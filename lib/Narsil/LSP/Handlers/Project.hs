{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                     // lsp // handlers // project
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The whole of the matrix, the sum of all the corporate data."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Project-wide state and cross-module typing for the LSP: the global,
--   content-addressed project cache and the (legacy) per-root module-graph
--   cache with in-flight build dedup, plus the cross-module 'TypeEnv' /
--   'Scope.ScopeGraph' builders the position features consult. All the
--   `unsafePerformIO` CAFs that back the server's caches live HERE, behind a
--   small functional surface — nothing else touches the mutable state.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.LSP.Handlers.Project (
  getProjectCache,
  buildCrossEnv,
  buildCrossScopeGraphWith,
  invalidateModuleGraphCache,
  voidProjectDiags,
  lookupNixpkgsIndex,
  warmNixpkgsIndex,
  latestNixpkgsIndex,
  resolveNixpkgsRoot,
  nixpkgsRootFromLock,
)
where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Data.Aeson (Value (..), decode)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types (Uri, uriToFilePath)
import Narsil.Inference.Nix (TypeEnv (..), builtinEnv, extendImport)
import Narsil.LSP.ProjectCache qualified as PC
import Narsil.Layout.Closure qualified as Closure
import Narsil.Layout.Graph qualified as Mod
import Narsil.Layout.Scope qualified as Scope
import Narsil.Nixpkgs.Index qualified as Nixpkgs
import Narsil.Nixpkgs.StorePath (fixedOutputSourcePath)
import Nix.Expr.Types.Annotated (NExprLoc)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE moduleGraphCache #-}
moduleGraphCache :: MVar (Map.Map FilePath Mod.ModuleGraph)
moduleGraphCache = unsafePerformIO $ newMVar Map.empty

{-# NOINLINE inflightCache #-}

{- | Tracks an in-flight graph build per project root so concurrent requests
don't both rebuild the same graph (Race-A from the audit).
-}
inflightCache :: MVar (Map.Map FilePath (Async (Maybe Mod.ModuleGraph)))
inflightCache = unsafePerformIO $ newMVar Map.empty

{-# NOINLINE projectCacheRef #-}

{- | Per-file, content-addressed project cache. Built lazily and incrementally
in the background; lookups never block. Replaces the all-or-nothing
moduleGraphCache for hover/inlay/completion paths.
-}
projectCacheRef :: MVar (Maybe PC.ProjectCache)
projectCacheRef = unsafePerformIO (newMVar Nothing)

{- | Get the project cache, creating it (and starting workers) the first time.
Subsequent calls return the same cache.
-}
getProjectCache :: IO PC.ProjectCache
getProjectCache = modifyMVar projectCacheRef orCreate
 where
  orCreate (Just pc) = pure (Just pc, pc)
  orCreate Nothing = do
    pc <- PC.newProjectCache
    PC.startWorkers pc
    pure (Just pc, pc)

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- nixpkgs symbol index (the cross-jump into nixpkgs)
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{-# NOINLINE nixpkgsIndexCache #-}

{- | Built nixpkgs symbol indices, keyed by checkout root. Like 'moduleGraphCache',
an unsafePerformIO CAF behind a small functional surface. A store-path root never
changes, so an entry here is effectively permanent for the session.
-}
nixpkgsIndexCache :: MVar (Map.Map FilePath Nixpkgs.NixpkgsIndex)
nixpkgsIndexCache = unsafePerformIO (newMVar Map.empty)

{-# NOINLINE nixpkgsIndexInflight #-}

{- | Roots whose index is being built, so concurrent first-requests don't each
re-scan @by-name@.
-}
nixpkgsIndexInflight :: MVar (Set FilePath)
nixpkgsIndexInflight = unsafePerformIO (newMVar Set.empty)

{-# NOINLINE latestIndexRef #-}

{- | The most-recently-built nixpkgs index, for consumers that have no URI in hand
(the background-warm pool). One project per session is the norm, so
"latest" is the right index; multi-root sessions simply warm against whichever
resolved last.
-}
latestIndexRef :: MVar (Maybe Nixpkgs.NixpkgsIndex)
latestIndexRef = unsafePerformIO (newMVar Nothing)

-- | The most-recently-built nixpkgs index, or 'Nothing' before any has resolved.
latestNixpkgsIndex :: IO (Maybe Nixpkgs.NixpkgsIndex)
latestNixpkgsIndex = readMVar latestIndexRef

{- | Resolve the nixpkgs checkout to index, WITHOUT evaluating Nix. Sources, in
order: the @NIX_COMPILE_NIXPKGS@ env var (explicit override); the project's
@flake.lock@ (the locked nixpkgs input → its realized @/nix/store@ path, computed
purely from the @narHash@); then a @nixpkgs=@ entry in @NIX_PATH@. Returns the
first that names an existing directory.
-}
resolveNixpkgsRoot :: Uri -> IO (Maybe FilePath)
resolveNixpkgsRoot uri = firstJustM [fromEnvVar, fromFlakeLock, fromNixPath]
 where
  firstJustM [] = pure Nothing
  firstJustM (a : as) = a >>= maybe (firstJustM as) (pure . Just)
  fromEnvVar = lookupEnv "NIX_COMPILE_NIXPKGS" >>= maybe (pure Nothing) keepDir
  fromNixPath = lookupEnv "NIX_PATH" >>= maybe (pure Nothing) (keepFirstDir . nixpkgsPaths)
  nixpkgsPaths np =
    [ T.unpack (T.drop 8 e)
    | e <- T.splitOn ":" (T.pack np)
    , "nixpkgs=" `T.isPrefixOf` e
    ]
  fromFlakeLock = findProjectRoot uri >>= maybe (pure Nothing) viaLock
  viaLock projRoot = do
    let lockPath = projRoot </> "flake.lock"
    present <- doesFileExist lockPath
    if not present
      then pure Nothing
      else do
        raw <- LBS.readFile lockPath
        maybe (pure Nothing) keepDir (nixpkgsRootFromLock raw)
  keepFirstDir [] = pure Nothing
  keepFirstDir (d : ds) = keepDir d >>= maybe (keepFirstDir ds) (pure . Just)
  keepDir d = do
    ok <- doesDirectoryExist d
    pure (if ok then Just d else Nothing)

{- | Pure: from flake.lock bytes, the nixpkgs checkout directory — either the
@locked.path@ (a path-type input) or the @/nix/store@ path computed from
@locked.narHash@ (a github/tarball input). Navigates the lock JSON by hand
(@root.inputs.nixpkgs@ → node → @locked@); 'Nothing' if absent or malformed.
-}
nixpkgsRootFromLock :: LBS.ByteString -> Maybe FilePath
nixpkgsRootFromLock raw = do
  Object top <- decode raw
  Object nodes <- KM.lookup "nodes" top
  rootName <- asText =<< KM.lookup "root" top
  Object rootNode <- KM.lookup (K.fromText rootName) nodes
  Object inputs <- KM.lookup "inputs" rootNode
  npName <- inputName =<< KM.lookup "nixpkgs" inputs
  Object npNode <- KM.lookup (K.fromText npName) nodes
  Object locked <- KM.lookup "locked" npNode
  let fromPath = T.unpack <$> (asText =<< KM.lookup "path" locked)
      fromNar = fixedOutputSourcePath =<< (asText =<< KM.lookup "narHash" locked)
  fromPath <|> fromNar
 where
  asText (String s) = Just s
  asText _ = Nothing
  -- a direct input is the node-name string; a `follows` is an array (take head).
  inputName (String s) = Just s
  inputName (Array a) = asText =<< listToMaybe (toList a)
  inputName _ = Nothing

{- | Non-blocking lookup of the nixpkgs symbol index for the project. Returns the
built index if ready; otherwise kicks a background build (once per root) and
returns 'Nothing' so the caller falls back to its normal behaviour. Never blocks
— same discipline as the per-file cache. The @uri@ locates the project (its
flake.lock) for root resolution.
-}
lookupNixpkgsIndex :: Uri -> IO (Maybe Nixpkgs.NixpkgsIndex)
lookupNixpkgsIndex uri = do
  mRoot <- resolveNixpkgsRoot uri
  maybe (pure Nothing) viaRoot mRoot
 where
  viaRoot root = do
    cache <- readMVar nixpkgsIndexCache
    maybe (warmNixpkgsIndex root >> pure Nothing) (pure . Just) (Map.lookup root cache)

{- | Build the nixpkgs index for a root on a background thread (once per root).
Idempotent: concurrent calls dedup via the inflight set; a failed build is
swallowed (nav falls back to single-file behaviour).
-}
warmNixpkgsIndex :: FilePath -> IO ()
warmNixpkgsIndex root = do
  won <- modifyMVar nixpkgsIndexInflight claim
  when won (void (async build))
 where
  claim s = pure (if Set.member root s then (s, False) else (Set.insert root s, True))
  build = do
    result <- try (Nixpkgs.buildNixpkgsIndex root) :: IO (Either SomeException Nixpkgs.NixpkgsIndex)
    either (const (pure ())) install result
    modifyMVar_ nixpkgsIndexInflight (pure . Set.delete root)
  install idx = do
    modifyMVar_ nixpkgsIndexCache (pure . Map.insert root idx)
    modifyMVar_ latestIndexRef (const (pure (Just idx)))

{- | The URI's project root, by THE shared policy ('Closure.findProjectRootFrom'
— flake.nix / .nix-compile.dhall / .git; this used to be a second, divergent
walker that did not know about .git and capped at 64 levels).
-}
findProjectRoot :: Uri -> IO (Maybe FilePath)
findProjectRoot uri = maybe (pure Nothing) fromPath (uriToFilePath uri)
 where
  fromPath fp = do
    canon <- canonicalizePath fp
    Closure.findProjectRootFrom (takeDirectory canon)

{- | Build a TypeEnv enriched with cross-module type information from the
per-file project cache. Non-blocking by construction:

  1. The URI's own cache entry (when present) contributes its dependency edges
     through the shared 'Closure.extendDeps' — keyed by the RAW path text as
     written, which is the key inference's 'lookupImport' actually queries, plus
     the canonical path; @callPackage@ deps land in 'envCallPackageTypes'.
  2. Every other 'Fresh' entry still contributes its type under its canonical
     path (belt-and-braces for absolute-path imports); 'Stale'/missing entries
     are simply absent (inference treats absent imports as opaque and proceeds).
  3. 'builtinEnv' underlies everything.

If the cache is still cold for this project we kick the cross-module graph build
off in the BACKGROUND (for the navigation path) and return immediately — hover /
completion get single-file precision now and richer cross-module types as the
per-file workers fill the cache. This function never blocks on a build.
-}
buildCrossEnv :: Uri -> IO TypeEnv
buildCrossEnv uri = do
  pc <- getProjectCache
  snap <- PC.snapshotFiles pc
  -- Cold cache: warm the cross-module graph in the background for the nav path,
  -- but never block this request on the build.
  when (Map.null snap) (voidProjectDiags uri)
  let base = Map.foldlWithKey' addFresh builtinEnv snap
      freshOf e = if PC.feStatus e == PC.Fresh then Just (PC.feType e) else Nothing
      types = Map.mapMaybe freshOf snap
      viaEntry fp = do
        canon <- canonicalizePath fp
        pure $ maybe base (Closure.extendDeps base types . PC.feImports) (Map.lookup canon snap)
  maybe (pure base) viaEntry (uriToFilePath uri)
 where
  addFresh acc fp entry
    | PC.feStatus entry == PC.Fresh = extendImport fp (PC.feType entry) acc
    | otherwise = acc

{- | Build a cross-module 'Scope.ScopeGraph' for the URI's project, splicing in
the caller's current (possibly unsaved) expression for that file. Non-blocking:
if the module graph has already been built we use it for full cross-file
precision; otherwise we warm it in the BACKGROUND and answer NOW from the
current file alone, so within-file navigation works immediately and cross-file
results arrive on a later request. The single-file fallback needs no project
root, so within-file go-to-def/references work even outside a flake.
-}
buildCrossScopeGraphWith :: Uri -> Maybe NExprLoc -> IO Scope.ScopeGraph
buildCrossScopeGraphWith uri mCurrentExpr = do
  mMg <- lookupModuleGraph uri
  maybe onCold (pure . crossGraph) mMg
 where
  -- Not built yet: warm in the background and answer from the current file now.
  onCold = voidProjectDiags uri >> pure singleFileGraph
  currentFile = uriToFilePath uri
  singleFileGraph = maybe Scope.empty (Scope.fromNixExpr currentFile) mCurrentExpr
  crossGraph mg =
    let exprs = Map.map Mod.modExpr (Mod.mgModules mg)
        exprs' =
          maybe exprs (\(f, e) -> Map.insert f e exprs) ((,) <$> currentFile <*> mCurrentExpr)
     in Scope.fromModuleGraph exprs'

{- | Non-blocking: the cached module graph for the URI's project if one has
already been built, else Nothing. NEVER triggers a build — warming the cache is
the background path's job ('voidProjectDiags').
-}
lookupModuleGraph :: Uri -> IO (Maybe Mod.ModuleGraph)
lookupModuleGraph uri = do
  mRoot <- findProjectRoot uri
  maybe (pure Nothing) (\root -> Map.lookup root <$> readMVar moduleGraphCache) mRoot

{- | Look up or build the module graph for a project root.
n.b. fixes from review-2:
  * exception-safe (catches StackOverflow from hnix, IO errors)
  * in-flight dedup: concurrent requests share a single build
  * negative cache via try @SomeException so a failing build doesn't loop
-}
getOrBuildModuleGraph :: Uri -> IO (Maybe Mod.ModuleGraph)
getOrBuildModuleGraph uri = do
  mRoot <- findProjectRoot uri
  maybe (pure Nothing) withRoot mRoot
 where
  withRoot root = do
    cache <- readMVar moduleGraphCache
    maybe (joinOrStartBuild root) (pure . Just) (Map.lookup root cache)

joinOrStartBuild :: FilePath -> IO (Maybe Mod.ModuleGraph)
joinOrStartBuild root = do
  -- Check inflight or claim it atomically; whoever wins starts the build.
  action <- modifyMVar inflightCache claim
  let asyncHandle = either id id action
  waitResult <- waitCatch asyncHandle
  -- Clean up inflight entry no matter what.
  modifyMVar_ inflightCache (pure . Map.delete root)
  either (const (pure Nothing)) pure waitResult
 where
  -- Check inflight or claim it atomically; whoever wins starts the build.
  claim m = maybe (start m) (\a -> pure (m, Right a)) (Map.lookup root m)
  start m = do
    a <- async (startBuild root)
    pure (Map.insert root a m, Left a)

startBuild :: FilePath -> IO (Maybe Mod.ModuleGraph)
startBuild root = do
  let flakePath = root </> "flake.nix"
  hasFlake <- doesFileExist flakePath
  if not hasFlake
    then pure Nothing
    else do
      -- Catch every exception: hnix parser stack overflow, IO errors,
      -- whatever buildModuleGraph might throw beyond its Either return.
      outcome <- try (Mod.buildModuleGraph flakePath)
      either
        (const (pure Nothing))
        (either (const (pure Nothing)) cacheIt)
        (outcome :: Either SomeException (Either Text Mod.ModuleGraph))
 where
  cacheIt mg = do
    modifyMVar moduleGraphCache (\m -> pure (Map.insert root mg m, ()))
    pure (Just mg)

{- | Invalidate the module-graph cache for the project containing the given URI.
n.b. fixed from review-2: invalidate on ANY save in the project, not just flake.nix.
-}
invalidateModuleGraphCache :: Uri -> IO ()
invalidateModuleGraphCache uri = do
  mRoot <- findProjectRoot uri
  maybe (pure ()) (\root -> modifyMVar_ moduleGraphCache (pure . Map.delete root)) mRoot

{- | Eagerly warm the module-graph cache for the URI's project.
n.b. fixed from review-2 (B5 voidProjectDiags was a no-op stub):
  * actually populates the cache so subsequent hover/definition are warm
  * exception-safe via getOrBuildModuleGraph's try/catch
  * uses the inflight-dedup machinery so we don't race the foreground request
-}
voidProjectDiags :: Uri -> IO ()
voidProjectDiags uri = do
  _ <- async $ do
    _result <- try (getOrBuildModuleGraph uri) :: IO (Either SomeException (Maybe Mod.ModuleGraph))
    pure ()
  pure ()
