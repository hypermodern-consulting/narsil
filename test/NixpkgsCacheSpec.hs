{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                      // tests // nixpkgs // cache
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He fed the box a question once, and forever after it knew."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The content-addressed eval cache, driven entirely by a COUNTING fake backend
--   (no Nix, no nix-repl) so every property is hermetic and instant:
--
--     * a repeat query is a hit — the inner backend runs once;
--     * rewriting a package's bytes invalidates it — same path, new hash, a miss;
--     * over the memory quota, the COLDEST entry is evicted (true LRU recency),
--       and the just-computed entry always survives (soft N+1);
--     * a path whose source can't be resolved bypasses the cache (never cached,
--       never wrong);
--     * the map round-trips through disk — a reload serves the same answer with
--       the backend never touched, and a post-save content change is a safe miss.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module NixpkgsCacheSpec (nixpkgsCacheTests) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Narsil.Inference.Nix.Type (NixType (..))
import Narsil.Nixpkgs.Cache (
  CacheConfig (..),
  cacheStats,
  cachingBackend,
  defaultCacheConfig,
  loadCacheFrom,
  newEvalCache,
  saveCache,
 )
import Narsil.Nixpkgs.Eval (EvalBackend (..))
import Narsil.Nixpkgs.Index (NixpkgsIndex, buildNixpkgsIndex)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

-- ── helpers ────────────────────────────────────────────────────────

-- | Lay down a @pkgs/by-name/<shard>/<name>/package.nix@ with explicit bytes.
seedPkg :: FilePath -> FilePath -> String -> String -> IO ()
seedPkg root shard name content = do
  let dir = root </> "pkgs" </> "by-name" </> shard </> name
  createDirectoryIfMissing True dir
  writeFile (dir </> "package.nix") content

-- | An inner backend that counts every call; spine answers @["x"]@, types 'TString'.
countingBackend :: IORef Int -> EvalBackend
countingBackend ref =
  EvalBackend
    { backendName = "counting"
    , evalSpine = \_ _ -> modifyIORef' ref (+ 1) >> pure (Right ["x"])
    , evalFieldType = \_ _ _ -> modifyIORef' ref (+ 1) >> pure (Right TString)
    }

-- | Build the by-name index over a seeded root.
indexOf :: FilePath -> IO NixpkgsIndex
indexOf = buildNixpkgsIndex

-- ── hit / miss ─────────────────────────────────────────────────────

-- | The same query twice runs the inner backend ONCE; the second is a cache hit.
testRepeatIsHit :: IO Bool
testRepeatIsHit =
  withSystemTempDirectory "nixpkgs-cache" $ \root -> do
    seedPkg root "he" "hello" "{ }\n"
    idx <- indexOf root
    ref <- newIORef 0
    cache <- newEvalCache defaultCacheConfig
    let be = cachingBackend cache (countingBackend ref)
    r1 <- evalSpine be idx ["hello"]
    r2 <- evalSpine be idx ["hello"]
    n <- readIORef ref
    (count, _) <- cacheStats cache
    pure (r1 == Right ["x"] && r2 == Right ["x"] && n == 1 && count == 1)

-- | Distinct packages are distinct entries — each is evaluated once.
testDistinctMiss :: IO Bool
testDistinctMiss =
  withSystemTempDirectory "nixpkgs-cache" $ \root -> do
    seedPkg root "he" "hello" "{ }\n"
    seedPkg root "wo" "world" "{ }\n"
    idx <- indexOf root
    ref <- newIORef 0
    cache <- newEvalCache defaultCacheConfig
    let be = cachingBackend cache (countingBackend ref)
    _ <- evalSpine be idx ["hello"]
    _ <- evalSpine be idx ["world"]
    n <- readIORef ref
    (count, _) <- cacheStats cache
    pure (n == 2 && count == 2)

-- | A field-type query is memoised just like a spine query.
testFieldTypeCached :: IO Bool
testFieldTypeCached =
  withSystemTempDirectory "nixpkgs-cache" $ \root -> do
    seedPkg root "he" "hello" "{ }\n"
    idx <- indexOf root
    ref <- newIORef 0
    cache <- newEvalCache defaultCacheConfig
    let be = cachingBackend cache (countingBackend ref)
    t1 <- evalFieldType be idx ["hello"] "pname"
    t2 <- evalFieldType be idx ["hello"] "pname"
    n <- readIORef ref
    pure (t1 == Right TString && t2 == Right TString && n == 1)

-- ── invalidation ───────────────────────────────────────────────────

{- | Rewriting a package's bytes (same attr path) changes its content hash, so the
old entry never matches again and the query re-evaluates.
-}
testContentInvalidation :: IO Bool
testContentInvalidation =
  withSystemTempDirectory "nixpkgs-cache" $ \root -> do
    seedPkg root "he" "hello" "{ }\n"
    idx <- indexOf root
    ref <- newIORef 0
    cache <- newEvalCache defaultCacheConfig
    let be = cachingBackend cache (countingBackend ref)
    _ <- evalSpine be idx ["hello"]
    n1 <- readIORef ref
    seedPkg root "he" "hello" "{ meta = { }; }\n" -- different bytes → different hash
    _ <- evalSpine be idx ["hello"]
    n2 <- readIORef ref
    pure (n1 == 1 && n2 == 2)

-- ── eviction ───────────────────────────────────────────────────────

{- | A memory quota that holds two entries but not three: a third distinct insert
caps the resident set at two (evict-after-insert). One spine entry costs ~49 bytes
('estimateBytes' @["x"]@), so 100 bytes holds two, not three.
-}
tightConfig :: CacheConfig
tightConfig = defaultCacheConfig{ccMaxMemoryBytes = 100}

-- | Over quota, the resident set is capped — three distinct queries leave two.
testEvictionCapsSize :: IO Bool
testEvictionCapsSize =
  withSystemTempDirectory "nixpkgs-cache" $ \root -> do
    seedPkg root "aa" "aaa" "{ }\n"
    seedPkg root "bb" "bbb" "{ }\n"
    seedPkg root "cc" "ccc" "{ }\n"
    idx <- indexOf root
    ref <- newIORef 0
    cache <- newEvalCache tightConfig
    let be = cachingBackend cache (countingBackend ref)
    _ <- evalSpine be idx ["aaa"]
    _ <- evalSpine be idx ["bbb"]
    _ <- evalSpine be idx ["ccc"]
    (count, bytes) <- cacheStats cache
    pure (count == 2 && bytes <= 100)

{- | Eviction is by recency, not insertion order. Touch @aaa@ before inserting
@ccc@ so @bbb@ is the coldest; the sequence then proves @aaa@ and @ccc@ stayed
hot (no re-eval) while @bbb@ was dropped (a re-eval). Final inner count == 4 holds
iff re-aaa and re-ccc were hits and re-bbb was a miss.
-}
testLruRecency :: IO Bool
testLruRecency =
  withSystemTempDirectory "nixpkgs-cache" $ \root -> do
    seedPkg root "aa" "aaa" "{ }\n"
    seedPkg root "bb" "bbb" "{ }\n"
    seedPkg root "cc" "ccc" "{ }\n"
    idx <- indexOf root
    ref <- newIORef 0
    cache <- newEvalCache tightConfig
    let be = cachingBackend cache (countingBackend ref)
    _ <- evalSpine be idx ["aaa"] -- miss (1) → {aaa}
    _ <- evalSpine be idx ["bbb"] -- miss (2) → {aaa,bbb}
    _ <- evalSpine be idx ["aaa"] -- hit, bumps aaa → bbb now coldest
    _ <- evalSpine be idx ["ccc"] -- miss (3), evicts bbb → {aaa,ccc}
    _ <- evalSpine be idx ["aaa"] -- hit (aaa survived)
    _ <- evalSpine be idx ["ccc"] -- hit (ccc survived)
    _ <- evalSpine be idx ["bbb"] -- miss (4) — bbb had been evicted
    n <- readIORef ref
    (count, _) <- cacheStats cache
    pure (n == 4 && count == 2)

-- ── nested / root-identity caching ─────────────────────────────────

{- | A nested / namespace path whose head is not an indexed package (e.g.
@python3Packages.requests@) is still cached — keyed by the nixpkgs root identity —
so a repeat is a hit, not a re-eval.
-}
testNestedCachedViaRoot :: IO Bool
testNestedCachedViaRoot =
  withSystemTempDirectory "nixpkgs-cache" $ \root -> do
    seedPkg root "he" "hello" "{ }\n"
    idx <- indexOf root
    ref <- newIORef 0
    cache <- newEvalCache defaultCacheConfig
    let be = cachingBackend cache (countingBackend ref)
    _ <- evalSpine be idx ["python3Packages", "requests"] -- head not in the index
    _ <- evalSpine be idx ["python3Packages", "requests"]
    n <- readIORef ref
    (count, _) <- cacheStats cache
    pure (n == 1 && count == 1)

{- | The empty path is the one genuinely uncacheable case: it delegates straight
through every time.
-}
testEmptyPathBypass :: IO Bool
testEmptyPathBypass =
  withSystemTempDirectory "nixpkgs-cache" $ \root -> do
    seedPkg root "he" "hello" "{ }\n"
    idx <- indexOf root
    ref <- newIORef 0
    cache <- newEvalCache defaultCacheConfig
    let be = cachingBackend cache (countingBackend ref)
    _ <- evalSpine be idx []
    _ <- evalSpine be idx []
    n <- readIORef ref
    (count, _) <- cacheStats cache
    pure (n == 2 && count == 0)

-- ── persistence ────────────────────────────────────────────────────

{- | The cache round-trips through disk: warm one cache, save it, then a FRESH
cache loaded from that file serves the same answer without ever calling its
backend (the second counter stays at zero).
-}
testPersistRoundTrip :: IO Bool
testPersistRoundTrip =
  withSystemTempDirectory "nixpkgs-cache" $ \root -> do
    seedPkg root "he" "hello" "{ }\n"
    idx <- indexOf root
    let cachePath = root </> "eval-cache.json"
    -- warm and save
    refA <- newIORef 0
    warm <- newEvalCache defaultCacheConfig
    _ <- evalSpine (cachingBackend warm (countingBackend refA)) idx ["hello"]
    saveCache cachePath warm
    -- reload into a fresh cache; its backend must never run
    refB <- newIORef 0
    reloaded <- loadCacheFrom defaultCacheConfig cachePath
    r <- evalSpine (cachingBackend reloaded (countingBackend refB)) idx ["hello"]
    nB <- readIORef refB
    (count, _) <- cacheStats reloaded
    pure (r == Right ["x"] && nB == 0 && count == 1)

{- | A content change AFTER a save is a safe miss on reload: the on-disk entry is
keyed by the old hash and simply never matches the new bytes.
-}
testPersistStaleIsSafe :: IO Bool
testPersistStaleIsSafe =
  withSystemTempDirectory "nixpkgs-cache" $ \root -> do
    seedPkg root "he" "hello" "{ }\n"
    idx <- indexOf root
    let cachePath = root </> "eval-cache.json"
    refA <- newIORef 0
    warm <- newEvalCache defaultCacheConfig
    _ <- evalSpine (cachingBackend warm (countingBackend refA)) idx ["hello"]
    saveCache cachePath warm
    seedPkg root "he" "hello" "{ meta = { }; }\n" -- bytes change after save
    refB <- newIORef 0
    reloaded <- loadCacheFrom defaultCacheConfig cachePath
    _ <- evalSpine (cachingBackend reloaded (countingBackend refB)) idx ["hello"]
    nB <- readIORef refB
    pure (nB == 1) -- the stale on-disk entry did not match → recomputed

-- ── runner ─────────────────────────────────────────────────────────

-- | The content-addressed eval-cache tests (hermetic; fake backend only).
nixpkgsCacheTests :: [(String, IO Bool)]
nixpkgsCacheTests =
  [ ("cache_repeat_is_hit", testRepeatIsHit)
  , ("cache_distinct_miss", testDistinctMiss)
  , ("cache_field_type_cached", testFieldTypeCached)
  , ("cache_content_invalidation", testContentInvalidation)
  , ("cache_eviction_caps_size", testEvictionCapsSize)
  , ("cache_lru_recency", testLruRecency)
  , ("cache_nested_cached_via_root", testNestedCachedViaRoot)
  , ("cache_empty_path_bypass", testEmptyPathBypass)
  , ("cache_persist_round_trip", testPersistRoundTrip)
  , ("cache_persist_stale_is_safe", testPersistStaleIsSafe)
  ]
