{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // nixpkgs // cache
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Nothing here was ever truly lost; the matrix kept every shape it had
--    once been shown, ready in an instant."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   A content-addressed cache wrapping the 'EvalBackend' seam — the durable asset
--   under symbol/type completion. An eval of @pkgs.<pkg>@ costs ~62ms; a cache hit
--   costs a map lookup. The key is the SHA-256 of the package's @package.nix@
--   CONTENT (not the nixpkgs revision), so:
--
--     * a store-path nixpkgs (immutable) never invalidates — its bytes never change;
--     * a flake-lock bump re-evals ONLY the files whose content actually changed;
--     * editing the USER's own file invalidates nothing (it is not a package.nix).
--
--   This addresses the value's INTERFACE by the source that produces it. It can be
--   wrong only if a package's observable shape depends on bytes outside its own
--   @package.nix@ (a stdenv change, say) — accepted as "wrong extremely rarely",
--   by design, in exchange for a permanent cache.
--
--   Quota is evict-AFTER-insert: the just-computed entry always lands, then the
--   coldest entries are dropped until within 'ccMaxMemoryBytes' — a soft N+1 at the
--   boundary, never refusing storage for work we just paid for. The map survives
--   process exit on NVMe (content-keyed, so a stale on-disk entry simply never
--   matches once the file changes). The BFS warm layer sits
--   on top of this.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Nixpkgs.Cache (
  -- * Configuration
  CacheConfig (..),
  defaultCacheConfig,

  -- * The cache
  EvalCache,
  newEvalCache,
  cachingBackend,
  cacheStats,

  -- * Persistence
  defaultCachePath,
  saveCache,
  loadCacheFrom,
  flushCacheAsync,

  -- * Internals (exposed for testing)
  ContentHash (..),
  Sel (..),
  CacheKey (..),
  Cached (..),
  Entry (..),
  contentHashOf,
  estimateBytes,
)
where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, tryTakeMVar)
import Control.Concurrent.STM (
  STM,
  TVar,
  atomically,
  modifyTVar',
  newTVarIO,
  readTVar,
  readTVarIO,
  stateTVar,
  writeTVar,
 )
import Control.Exception (SomeException, finally, try)
import Control.Monad (void)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (minimumBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import System.Directory (
  XdgDirectory (XdgCache),
  createDirectoryIfMissing,
  doesFileExist,
  getXdgDirectory,
 )
import System.FilePath (takeDirectory, (</>))

import Data.Text.Encoding qualified as TE
import Narsil.Core.Span (spanFile)
import Narsil.Inference.Nix.Type (NixType)
import Narsil.Nixpkgs.Eval (EvalBackend (..), EvalError (..))
import Narsil.Nixpkgs.Index (NixpkgsIndex, lookupPackage, nixpkgsRoot)

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- keys and entries
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

-- | The SHA-256 (lowercase hex) of a source file's bytes — what we content-address on.
newtype ContentHash = ContentHash Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Which question about the value was asked: its attribute spine, or one field's type.
data Sel
  = SelSpine
  | SelType !Text
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Sel
instance ToJSON Sel

{- | A cache key: the content hash of the source that produces the value, the full
attribute path, and the selector. The hash is what makes the entry survive a lock
bump unchanged and expire the instant the file's bytes change.
-}
data CacheKey = CacheKey !ContentHash ![Text] !Sel
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON CacheKey
instance ToJSON CacheKey

-- | A cached answer: either an attribute spine or a single field type.
data Cached
  = CachedSpine ![Text]
  | CachedType !NixType
  deriving stock (Eq, Show, Generic)

instance FromJSON Cached
instance ToJSON Cached

{- | A stored value with its LRU recency tick and an approximate memory cost. The
tick is bumped on every read, so eviction drops the genuinely coldest entries.
-}
data Entry = Entry
  { entryValue :: !Cached
  , entryTick :: !Int
  , entryBytes :: !Int
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Entry
instance ToJSON Entry

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- configuration
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{- | The cache's two quotas: how much memory it may hold and how much it may
persist to disk. Both are sized in bytes; the LSP config knobs (@maxMemoryMB@,
@maxDiskMB@) convert into these. Quotas are soft at the boundary (evict-after-insert).
-}
data CacheConfig = CacheConfig
  { ccMaxMemoryBytes :: !Int
  , ccMaxDiskBytes :: !Int
  }
  deriving stock (Eq, Show)

-- | A sane default: 256 MiB resident, 512 MiB on disk.
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
  CacheConfig
    { ccMaxMemoryBytes = 256 * 1024 * 1024
    , ccMaxDiskBytes = 512 * 1024 * 1024
    }

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- the cache
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{- | The live cache: a content-keyed map, a monotonic clock for LRU ticks, and the
quota config. Shared across all backends and worker threads.
-}
data EvalCache = EvalCache
  { ecMap :: !(TVar (Map CacheKey Entry))
  , ecClock :: !(TVar Int)
  , ecConfig :: !CacheConfig
  , ecFlushLock :: !(MVar ())
  -- ^ a one-token mutex serialising background flushes (held = free to flush)
  }

-- | A fresh, empty cache for a given config.
newEvalCache :: CacheConfig -> IO EvalCache
newEvalCache cfg =
  EvalCache <$> newTVarIO Map.empty <*> newTVarIO 0 <*> pure cfg <*> newMVar ()

-- | The current entry count and total estimated bytes (introspection / tests).
cacheStats :: EvalCache -> IO (Int, Int)
cacheStats cache = atomically $ do
  m <- readTVar (ecMap cache)
  pure (Map.size m, totalBytes m)

{- | Wrap a backend so every answer it produces is memoised by source content. A
hit short-circuits the inner backend entirely; a miss runs it, stores a successful
result, and lets failures ('Left') fall through uncached so a transient error is
retried next time. A path whose source can't be resolved (so we can't content-
address it) bypasses the cache and delegates straight through — never wrong, just
not cached.
-}
cachingBackend :: EvalCache -> EvalBackend -> EvalBackend
cachingBackend cache inner =
  EvalBackend
    { backendName = "cache(" <> backendName inner <> ")"
    , evalSpine = \idx path ->
        lookupOrRun cache idx path SelSpine (evalSpine inner idx path) CachedSpine unwrapSpine
    , evalFieldType = \idx path field ->
        lookupOrRun
          cache
          idx
          path
          (SelType field)
          (evalFieldType inner idx path field)
          CachedType
          unwrapType
    }
 where
  unwrapSpine (CachedSpine xs) = Just xs
  unwrapSpine _ = Nothing
  unwrapType (CachedType t) = Just t
  unwrapType _ = Nothing

{- | The hit/miss core, generic over the two ops. Resolve the source hash; on a
hit return the stored value; on a miss run the inner backend and store a success.
-}
lookupOrRun ::
  EvalCache ->
  NixpkgsIndex ->
  [Text] ->
  Sel ->
  IO (Either EvalError a) ->
  (a -> Cached) ->
  (Cached -> Maybe a) ->
  IO (Either EvalError a)
lookupOrRun cache idx path sel run wrap unwrap = do
  mh <- contentHashFor idx path
  maybe run withKey mh
 where
  withKey h = do
    let key = CacheKey h path sel
    hit <- atomically (lookupBump cache key)
    maybe (miss key) (pure . Right) (hit >>= unwrap)
  miss key = run >>= either (pure . Left) (store key)
  store key a = atomically (insertEvict cache key (wrap a)) >> pure (Right a)

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- content hashing
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{- | The content hash a path is keyed under. If the head resolves to an indexed
package, hash that package's @package.nix@ (precise — a per-file edit invalidates
exactly it). Otherwise — every namespace member, @pkgs.python3Packages.requests@
and friends, whose head is not a by-name/all-packages entry — fall back to the
nixpkgs ROOT's identity: @sha256(root)@. For a store-path root the path embeds the
tree's own content hash, so the key is immutable (any change → new store path → new
key); for a mutable checkout it is stable, going stale only if a namespace member's
source is edited under the LSP — the accepted "wrong extremely rarely" tradeoff.
The empty path alone is uncacheable.
-}
contentHashFor :: NixpkgsIndex -> [Text] -> IO (Maybe ContentHash)
contentHashFor _ [] = pure Nothing
contentHashFor idx (pkg : _) =
  maybe (pure (Just (rootIdentityHash (nixpkgsRoot idx)))) contentHashOf indexed
 where
  indexed = lookupPackage idx pkg >>= spanFile

-- | A content hash standing in for the whole tree at @root@ — see 'contentHashFor'.
rootIdentityHash :: FilePath -> ContentHash
rootIdentityHash = ContentHash . hexEncode . SHA256.hash . TE.encodeUtf8 . T.pack

-- | SHA-256 (lowercase hex) of a file's bytes; 'Nothing' if it can't be read.
contentHashOf :: FilePath -> IO (Maybe ContentHash)
contentHashOf file = do
  ebytes <- try (BS.readFile file) :: IO (Either SomeException ByteString)
  pure (either (const Nothing) (Just . ContentHash . hexEncode . SHA256.hash) ebytes)

-- | Lowercase hex of a byte string.
hexEncode :: ByteString -> Text
hexEncode = T.pack . concatMap byteHex . BS.unpack
 where
  byteHex b = [hexDigit (b `shiftR` 4), hexDigit (b .&. 0x0f)]
  hexDigit d = "0123456789abcdef" !! fromIntegral d

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- STM operations
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

-- | Look a key up; on a hit, bump its recency tick and return the value.
lookupBump :: EvalCache -> CacheKey -> STM (Maybe Cached)
lookupBump cache key = do
  m <- readTVar (ecMap cache)
  maybe (pure Nothing) (bump m) (Map.lookup key m)
 where
  bump m e = do
    t <- nextTick cache
    writeTVar (ecMap cache) (Map.insert key e{entryTick = t} m)
    pure (Just (entryValue e))

{- | Insert a freshly computed value, then evict the coldest entries until within
the memory quota — but never the entry just inserted (soft N+1 at the boundary).
-}
insertEvict :: EvalCache -> CacheKey -> Cached -> STM ()
insertEvict cache key v = do
  t <- nextTick cache
  let entry = Entry v t (estimateBytes v)
      limit = ccMaxMemoryBytes (ecConfig cache)
  modifyTVar' (ecMap cache) (evictDown limit key . Map.insert key entry)

-- | A monotonic tick source for LRU recency.
nextTick :: EvalCache -> STM Int
nextTick cache = stateTVar (ecClock cache) (\t -> (t + 1, t + 1))

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- eviction (pure)
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{- | Drop the coldest entries until total bytes are within the limit, never
removing @keep@. If @keep@ is the only thing left over the limit, we stop — the
just-computed work always survives (the soft N+1).
-}
evictDown :: Int -> CacheKey -> Map CacheKey Entry -> Map CacheKey Entry
evictDown limit keep m
  | totalBytes m <= limit = m
  | otherwise = maybe m evictOne (oldestExcept keep m)
 where
  evictOne k = evictDown limit keep (Map.delete k m)

-- | Drop the coldest entries until total bytes are within the limit (no exception).
capBytes :: Int -> Map CacheKey Entry -> Map CacheKey Entry
capBytes limit m
  | totalBytes m <= limit = m
  | otherwise = maybe m capOne (oldestKey m)
 where
  capOne k = capBytes limit (Map.delete k m)

-- | Total estimated resident bytes across the map.
totalBytes :: Map CacheKey Entry -> Int
totalBytes = Map.foldr ((+) . entryBytes) 0

-- | The coldest (lowest-tick) key other than @keep@, if any.
oldestExcept :: CacheKey -> Map CacheKey Entry -> Maybe CacheKey
oldestExcept keep = oldestOf . filter ((/= keep) . fst) . Map.toList

-- | The coldest (lowest-tick) key in the map, if any.
oldestKey :: Map CacheKey Entry -> Maybe CacheKey
oldestKey = oldestOf . Map.toList

-- | The key with the smallest recency tick among the given entries.
oldestOf :: [(CacheKey, Entry)] -> Maybe CacheKey
oldestOf [] = Nothing
oldestOf xs = Just (fst (minimumBy (comparing (entryTick . snd)) xs))

{- | A rough resident-byte estimate for a cached value. Approximate by design — the
quota it feeds is soft — but monotone in the real cost so eviction stays sensible.
-}
estimateBytes :: Cached -> Int
estimateBytes (CachedSpine xs) = sum (map ((+ 16) . T.length) xs) + 32
estimateBytes (CachedType _) = 48

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- persistence
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

-- | The on-disk snapshot envelope (a version tag plus the entries).
data Snapshot = Snapshot
  { snapVersion :: !Int
  , snapEntries :: ![(CacheKey, Entry)]
  }
  deriving stock (Generic)

instance FromJSON Snapshot
instance ToJSON Snapshot

-- | The current snapshot format version (bump to invalidate old on-disk caches).
snapshotVersion :: Int
snapshotVersion = 1

-- | The default NVMe location: @$XDG_CACHE_HOME/nix-compile/eval-cache.json@.
defaultCachePath :: IO FilePath
defaultCachePath = do
  dir <- getXdgDirectory XdgCache "narsil"
  pure (dir </> "eval-cache.json")

{- | Persist the cache to disk, trimmed to the disk quota (coldest-first). Failures
(unwritable path, full disk) are swallowed — persistence is best-effort.
-}
saveCache :: FilePath -> EvalCache -> IO ()
saveCache path cache = do
  m <- readTVarIO (ecMap cache)
  let trimmed = capBytes (ccMaxDiskBytes (ecConfig cache)) m
      snap = Snapshot snapshotVersion (Map.toList trimmed)
  createDirectoryIfMissing True (takeDirectory path)
  void (try (LBS.writeFile path (Aeson.encode snap)) :: IO (Either SomeException ()))

{- | Checkpoint the cache to disk in the background, but only if no flush is
already running — a save in flight means a concurrent one would just race on the
same file, so we drop it (the next checkpoint captures the newer state). Returns
immediately; the write happens on a spawned thread.
-}
flushCacheAsync :: FilePath -> EvalCache -> IO ()
flushCacheAsync path cache = do
  token <- tryTakeMVar (ecFlushLock cache)
  maybe (pure ()) (const (void (async flush))) token
 where
  flush = saveCache path cache `finally` putMVar (ecFlushLock cache) ()

{- | Load a cache from disk into a fresh 'EvalCache'. A missing, unreadable, or
malformed file (including a stale snapshot version) yields an empty cache — the
content-keyed entries are then re-warmed lazily. The loaded map is capped to the
memory quota and the clock advanced past the highest stored tick.
-}
loadCacheFrom :: CacheConfig -> FilePath -> IO EvalCache
loadCacheFrom cfg path = do
  cache <- newEvalCache cfg
  exists <- doesFileExist path
  prime cache exists
  pure cache
 where
  prime _cache False = pure ()
  prime cache True = do
    eraw <- try (LBS.readFile path) :: IO (Either SomeException LBS.ByteString)
    install cache (either (const []) snapshotEntries eraw)
  snapshotEntries raw = maybe [] entriesOf (Aeson.decode raw)
  entriesOf snap
    | snapVersion snap == snapshotVersion = snapEntries snap
    | otherwise = []
  install cache entries = atomically $ do
    let m = capBytes (ccMaxMemoryBytes cfg) (Map.fromList entries)
        maxTick = maximum (0 : map (entryTick . snd) entries)
    writeTVar (ecMap cache) m
    writeTVar (ecClock cache) maxTick
