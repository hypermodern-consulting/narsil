{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // layout // closure
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He followed the thing back along every wire it touched, until the whole
--    shape of it stood in his head at once."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   ONE reachability closure, so @check@ / @infer@ / @lsp@ stop disagreeing
--   about which files a file depends on and what those files are. The CLI paths
--   call 'closureEnv' / 'closureEnvShared' directly; the LSP's async ProjectCache
--   walks the SAME edges ('Narsil.Layout.Edge') and seeds envs through the
--   SAME 'extendDeps', keeping its own non-blocking worker pool.
--
--   The graph follows three edge kinds, each discovered from the AST with no
--   evaluation ('discoverEdges'):
--
--     * 'EImport'      — @import ./path@ (the canonical 'findImports' walker);
--     * 'EFlakeImport' — a flake-parts @imports = [ ./a.nix … ]@ list;
--     * 'ECallPackage' — a top-level @x = callPackage ./path { }@ binding.
--
--   'buildTypeClosure' walks that graph from a root file — bounded to the
--   enclosing project (flake.nix / .git), eval-free, cycle-guarded, resolving
--   @./dir@ to @./dir/default.nix@ — and infers every reachable file's type in
--   dependency order, threading each file's dependency types into the next. The
--   result populates two env maps: 'envImportTypes' (what @import ./p@ yields) and
--   'envCallPackageTypes' (the RESULT of @callPackage ./p { }@ — its function
--   applied to its auto-filled args), so a one-shot CLI @infer@ resolves both
--   cross-module forms synchronously, no async project cache needed.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Closure (
  -- * Edges
  EdgeKind (..),
  Edge (..),
  discoverEdges,

  -- * The closure
  Closure (..),
  Dep,
  buildTypeClosure,

  -- * Consuming it
  importEnvFor,
  closureEnv,
  extendDeps,
  resolveExisting,

  -- * Sweep sharing
  ClosureCache,
  newClosureCache,
  closureEnvShared,

  -- * Project-root policy
  findProjectRootFrom,
)
where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Safety (renderSafetyError, safeAnalyze, safeParseNixFile)
import Narsil.Inference.Nix (
  TypeEnv,
  builtinEnv,
  extendCallPackage,
  extendImport,
  inferExprWithEnv,
 )
import Narsil.Inference.Nix.Type (NixType (..))
import Narsil.Layout.Edge (Edge (..), EdgeKind (..), discoverEdges)
import Narsil.Layout.Edge qualified as Edge
import Nix.Expr.Types.Annotated (NExprLoc)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, makeAbsolute)
import System.FilePath (normalise, pathSeparator, takeDirectory, (</>))

-- ── the closure ─────────────────────────────────────────────────────

{- | A resolved dependency: its kind, the canonical path it points at (an existing
file inside the project), and the raw path text as written at the use site.
-}
type Dep = (EdgeKind, FilePath, Text)

-- | One reachable file: its parsed expression and its resolved dependencies.
data Node = Node
  { nodeExpr :: !NExprLoc
  , nodeDeps :: ![Dep]
  }

{- | The synchronous, file-rooted type closure: every file reachable from the root
(bounded to the enclosing project), each with its parsed expression, inferred
type, and resolved dependency edges — plus the files that failed on the way, so
product layers ('Narsil.Layout.Graph') can report them.
-}
data Closure = Closure
  { clTypes :: !(Map FilePath NixType)
  -- ^ canonical path ↦ inferred type of that file's expression
  , clDeps :: !(Map FilePath [Dep])
  -- ^ canonical path ↦ its resolved dependency edges
  , clExprs :: !(Map FilePath NExprLoc)
  -- ^ canonical path ↦ the parsed expression (the LSP scope graph feeds on these)
  , clFailures :: !(Map FilePath Text)
  -- ^ canonical path ↦ why the file was skipped (parse error, depth bomb)
  , clTruncated :: !Bool
  -- ^ the walk hit 'maxClosureNodes' with work left — coverage is partial
  , clRoot :: !FilePath
  -- ^ the canonical root the closure was built from
  }

{- | Build the type closure rooted at a file. Eval-free, cycle-guarded,
depth-guarded ('safeAnalyze' on every dependency, not just the root), bounded to
the enclosing project root (the nearest @flake.nix@ / @.git@ ancestor, else the
file's own directory) so it never wanders into the Nix store, and truncated at
'maxClosureNodes' files. Files that fail to parse (or exceed the depth bound)
contribute nothing and are skipped — attempted once, memoised; a dependency that
fails to type-check simply does not enrich its importers (degrade, never lie).
-}
buildTypeClosure :: FilePath -> IO Closure
buildTypeClosure = buildTypeClosureWith True

{- | Truncation bound for the reachability walk. A hub-reaching root (an
all-packages.nix-style file) fans out to thousands of @callPackage@ edges; past
the bound the walk stops cold — enrichment degrades on gigantic graphs, it never
hangs the CLI on a synchronous parse+infer of half of nixpkgs.
-}
maxClosureNodes :: Int
maxClosureNodes = 512

{- | 'buildTypeClosure', optionally skipping inference of the root file itself.
'closureEnv' consumes only DEPENDENCY types — its caller re-infers the root with
the enriched env — so inferring the root inside the closure would run the most
expensive file twice for nothing. Sweep-style consumers keep the root's type.
-}
buildTypeClosureWith :: Bool -> FilePath -> IO Closure
buildTypeClosureWith inferRoot rootPath = do
  -- Edges resolve against the LEXICAL absolute path (dots collapsed textually,
  -- symlinks NOT resolved): Nix resolves `../` from the path as invoked, so
  -- canonicalizing first would type a different file than Nix reads whenever a
  -- directory on the way is a symlink. Canonical paths serve only as node
  -- identity (dedup / map keys).
  lexRoot <- Edge.collapseDots . normalise <$> makeAbsolute rootPath
  canonRoot <- canonicalizePath lexRoot
  projRoot <- projectRootFor lexRoot
  (nodes, truncated) <- loadReachable projRoot Map.empty [(canonRoot, lexRoot)]
  let live = Map.mapMaybe (either (const Nothing) Just) nodes
      failures = Map.mapMaybe (either Just (const Nothing)) nodes
      allOrder = topoOrder canonRoot live
      order = if inferRoot then allOrder else filter (/= canonRoot) allOrder
      types = foldl' (inferOne live) Map.empty order
  pure
    Closure
      { clTypes = types
      , clDeps = Map.map nodeDeps live
      , clExprs = Map.map nodeExpr live
      , clFailures = failures
      , clTruncated = truncated
      , clRoot = canonRoot
      }

{- | Parse, depth-check, and edge-discover every file transitively reachable from
the worklist of (canonical identity, lexical path) pairs, following all three edge
kinds. A file that fails (unparseable, depth bomb) is memoised as 'Left' with the
rendered reason so a broken shared dep is attempted once, not once per referrer —
and so product layers can report it. The walk truncates at 'maxClosureNodes';
truncation is reported, never silent.
-}
loadReachable ::
  FilePath ->
  Map FilePath (Either Text Node) ->
  [(FilePath, FilePath)] ->
  IO (Map FilePath (Either Text Node), Bool)
loadReachable _ acc [] = pure (acc, False)
loadReachable projRoot acc ((canon, lexPath) : rest)
  | canon `Map.member` acc = loadReachable projRoot acc rest
  | Map.size acc >= maxClosureNodes = pure (acc, True)
  | otherwise = do
      parsed <- safeParseNixFile lexPath
      -- n.b. 'safeAnalyze' is the depth guard: a dependency AST that parses but
      -- nests pathologically must not reach 'discoverEdges' / inference (the
      -- root file is guarded by every entry point; deps are guarded HERE).
      either skip viaExpr (parsed >>= safeAnalyze)
 where
  skip err = loadReachable projRoot (Map.insert canon (Left (renderSafetyError err)) acc) rest
  viaExpr expr = do
    rdeps <- catMaybes <$> mapM (resolveDep projRoot) (discoverEdges (takeDirectory lexPath) expr)
    let node = Node{nodeExpr = expr, nodeDeps = [(k, c, r) | (k, c, _, r) <- rdeps]}
        next = [(c, l) | (_, c, l, _) <- rdeps] ++ rest
    loadReachable projRoot (Map.insert canon (Right node) acc) next

{- | Resolve one edge to an existing in-project file (or drop it): @./dir@ becomes
@./dir/default.nix@, the target must exist and live under the project root. Yields
the edge's kind and raw text with the target's canonical identity AND the lexical
path the target's own edges must resolve against.
-}
resolveDep :: FilePath -> Edge -> IO (Maybe (EdgeKind, FilePath, FilePath, Text))
resolveDep projRoot (Edge kind path raw) = do
  mFile <- resolveExisting path
  maybe (pure Nothing) underProject mFile
 where
  underProject file = do
    canon <- canonicalizePath file
    pure (if underRoot projRoot file then Just (kind, canon, file, raw) else Nothing)

-- | A path as-is if it is a file, its @default.nix@ if it is a directory, else nothing.
resolveExisting :: FilePath -> IO (Maybe FilePath)
resolveExisting path = do
  isFile <- doesFileExist path
  if isFile then pure (Just path) else viaDir
 where
  viaDir = do
    isDir <- doesDirectoryExist path
    if not isDir then pure Nothing else viaDefault
  viaDefault = do
    let dn = path </> "default.nix"
    hasDn <- doesFileExist dn
    pure (if hasDn then Just dn else Nothing)

-- | Is the canonical path at or under the project root (separator-guarded)?
underRoot :: FilePath -> FilePath -> Bool
underRoot projRoot p = p == projRoot || (projRoot ++ [pathSeparator]) `isPrefixOf` p

{- | THE project-root policy — one marker set, one walker, shared by the
closure and the LSP (they used to diverge: closure knew @flake.nix@/@.git@,
the LSP knew @flake.nix@/@.nix-compile.dhall@ with a 64-level cap — same
file, two different "projects"). A directory is a root when it holds a
@flake.nix@, a @.nix-compile.dhall@, or a @.git@; the walk terminates at
the filesystem root (@takeDirectory@ is its own fixpoint there), so no
depth cap is needed.
-}
findProjectRootFrom :: FilePath -> IO (Maybe FilePath)
findProjectRootFrom = go
 where
  go dir = do
    hasFlake <- doesFileExist (dir </> "flake.nix")
    hasNarsil <- doesFileExist (dir </> ".narsil.dhall")
    hasLegacy <- doesFileExist (dir </> ".nix-compile.dhall")
    let hasConfig = hasNarsil || hasLegacy
    hasGit <- doesDirectoryExist (dir </> ".git")
    let parent = takeDirectory dir
    if hasFlake || hasConfig || hasGit
      then pure (Just dir)
      else if parent == dir then pure Nothing else go parent

-- | The nearest marked ancestor ('findProjectRootFrom'); the file's own dir if none.
projectRootFor :: FilePath -> IO FilePath
projectRootFor file =
  fromMaybe (takeDirectory file) <$> findProjectRootFrom (takeDirectory file)

-- | Dependency-first (post-order) traversal of the reachable graph from the root.
topoOrder :: FilePath -> Map FilePath Node -> [FilePath]
topoOrder root nodes = reverse (snd (go Set.empty [] root))
 where
  go visited acc path
    | path `Set.member` visited = (visited, acc)
    | otherwise =
        let visited' = Set.insert path visited
            deps = maybe [] (map depTarget . nodeDeps) (Map.lookup path nodes)
            (visited'', acc') = foldl' step (visited', acc) deps
         in (visited'', path : acc')
  step (v, a) = go v a
  depTarget (_, t, _) = t

-- | Infer one file's type with its already-inferred dependencies in scope.
inferOne :: Map FilePath Node -> Map FilePath NixType -> FilePath -> Map FilePath NixType
inferOne nodes acc path = maybe acc viaNode (Map.lookup path nodes)
 where
  viaNode node =
    either
      (const acc)
      (\(t, _) -> Map.insert path t acc)
      (inferExprWithEnv (extendDeps builtinEnv acc (nodeDeps node)) (nodeExpr node))

-- ── consuming the closure ───────────────────────────────────────────

{- | Extend a base env with the cross-module types a file depends on. An @import@ /
flake-import contributes the dependency's own type, keyed by BOTH the canonical path
and the raw text as written (inference's 'lookupImport' keys on the literal). A
@callPackage ./p@ contributes the package's RESULT type — the dependency is a
function @{ … }: package@, and @callPackage@ applies it, so the call site has the
function's result — keyed by the raw path under 'envCallPackageTypes'.
-}
extendDeps :: TypeEnv -> Map FilePath NixType -> [Dep] -> TypeEnv
extendDeps base known = foldl' add base
 where
  add env (ECallPackage, canon, raw) =
    maybe env (\t -> extendCallPackage (T.unpack raw) (callResult t) env) (Map.lookup canon known)
  add env (_, canon, raw) =
    maybe env extend (Map.lookup canon known)
   where
    extend t = extendImport (T.unpack raw) t (extendImport canon t env)

{- | The result of applying a package function: @callPackage f@ supplies @f@'s
arguments, so the call yields @f@'s codomain. A non-function degrades to itself.
-}
callResult :: NixType -> NixType
callResult (TFun _ r) = r
callResult t = t

-- | The cross-module inference env for one file already in a closure.
importEnvFor :: TypeEnv -> Closure -> FilePath -> TypeEnv
importEnvFor base cl file =
  extendDeps base (clTypes cl) (Map.findWithDefault [] file (clDeps cl))

{- | Build the closure rooted at a file and return its cross-module inference env: a
one-call seam for the CLI @infer@ / @check@ path. Best-effort — a file with no
in-project imports just yields @base@ unchanged. Skips inferring the root file
inside the closure (the caller is about to infer it against this very env).
-}
closureEnv :: TypeEnv -> FilePath -> IO TypeEnv
closureEnv base file = do
  cl <- buildTypeClosureWith False file
  pure (importEnvFor base cl (clRoot cl))

-- ── sweep sharing ───────────────────────────────────────────────────

{- | A sweep-shared accumulator: each file's closure is built at most once and
merged in, so a sweep over N files of one project pays one parse + inference per
reachable file — not one full closure rebuild per swept file (quadratic on shared
deps). Thread-safe: the merge is atomic; concurrent misses may duplicate work,
never corrupt it.
-}
newtype ClosureCache = ClosureCache (IORef SharedClosure)

data SharedClosure = SharedClosure
  { scTypes :: !(Map FilePath NixType)
  , scDeps :: !(Map FilePath [Dep])
  }

-- | An empty cache: one per sweep.
newClosureCache :: IO ClosureCache
newClosureCache = ClosureCache <$> newIORef (SharedClosure Map.empty Map.empty)

{- | 'closureEnv' through a 'ClosureCache': a file some earlier closure already
covered is served from the merge (its edges and its transitive deps' types are
already there); a miss builds a FULL closure — the root's own type is exactly
what later files in the sweep want — and merges it.
-}
closureEnvShared :: ClosureCache -> TypeEnv -> FilePath -> IO TypeEnv
closureEnvShared (ClosureCache ref) base file = do
  lexRoot <- Edge.collapseDots . normalise <$> makeAbsolute file
  canon <- canonicalizePath lexRoot
  cached <- readIORef ref
  case Map.lookup canon (scDeps cached) of -- CASE-OK: shape dispatch
    Just deps -> pure (extendDeps base (scTypes cached) deps)
    Nothing -> do
      cl <- buildTypeClosure lexRoot
      merged <- atomicModifyIORef' ref (dup . merge cl)
      pure (extendDeps base (scTypes merged) (Map.findWithDefault [] canon (scDeps merged)))
 where
  dup st = (st, st)
  -- first-computed entries win (Map.union is left-biased): a file's types/deps
  -- never flap between closure builds that raced.
  merge cl st =
    SharedClosure
      { scTypes = Map.union (scTypes st) (clTypes cl)
      , scDeps = Map.union (scDeps st) (clDeps cl)
      }
