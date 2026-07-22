{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                // layout // graph
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "And the next. And ever was."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The module-graph PRODUCT over the one reachability engine
--   ('Narsil.Layout.Closure'): build the closure — parse, depth-guard, all
--   three edge kinds, cross-module types in dependency order — then derive what
--   the graph consumers need on top: per-file lint findings for the CI graph
--   phase, parse/depth failures with their reasons, and the parsed expressions
--   the LSP scope graph feeds on.
--
--   This module used to carry its own walker + topo sort + cross-module
--   inference; that engine now lives ONLY in Closure, so check / infer / lsp /
--   ci can no longer disagree about which files a file depends on.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Graph (
  -- * Module graph
  ModuleGraph (..),
  Module (..),
  ParseFailure (..),
  LintFailure (..),

  -- * Building
  buildModuleGraph,
  buildModuleGraphFromFlake,

  -- * Queries
  moduleTypes,
)
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Inference.Nix.Type
import Narsil.Layout.Closure qualified as Closure
import Narsil.Lint.Nix (NixViolation, findNixViolations)
import Nix.Expr.Types.Annotated
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- types
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | one node in the module graph: its path, parsed AST, inferred type, and dependency edges.
data Module = Module
  { modPath :: !FilePath
  , modExpr :: !NExprLoc
  , modType :: !NixType
  , modDeps :: ![Closure.Dep]
  }
  deriving (Show)

-- | a file that failed to parse (or tripped the safety gate), with the error text.
data ParseFailure = ParseFailure
  { pfPath :: !FilePath
  , pfError :: !Text
  }
  deriving (Show)

-- | a file's lint violations (recorded only when non-empty).
data LintFailure = LintFailure
  { lfPath :: !FilePath
  , lfViolations :: ![NixViolation]
  }
  deriving (Show)

{- | the whole module graph: modules by canonical path, the root, the failure
categories, the cross-module inferred types, and whether the underlying walk was
truncated at the closure's node bound (partial coverage — report it, never hide it).
-}
data ModuleGraph = ModuleGraph
  { mgModules :: !(Map FilePath Module)
  , mgRoot :: !FilePath
  , mgFailures :: ![ParseFailure]
  , mgLintFailures :: ![LintFailure]
  , mgModuleTypes :: !(Map FilePath NixType)
  , mgTruncated :: !Bool
  }
  deriving (Show)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- building
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | build a complete module graph starting from a root nix file: one
'Closure.buildTypeClosure' walk (imports, flake-parts imports, callPackage
edges; lexical path resolution; depth-guarded; cycle-guarded; project-bounded),
then per-file lint over the parsed expressions. The 'Either' is kept for API
stability — the closure itself degrades instead of failing.
-}
buildModuleGraph :: FilePath -> IO (Either Text ModuleGraph)
buildModuleGraph rootPath = do
  cl <- Closure.buildTypeClosure rootPath
  let toModule path expr =
        Module
          { modPath = path
          , modExpr = expr
          , modType = Map.findWithDefault TAny path (Closure.clTypes cl)
          , modDeps = Map.findWithDefault [] path (Closure.clDeps cl)
          }
      lintOf (path, expr) =
        case findNixViolations expr of -- CASE-OK: shape dispatch
          [] -> Nothing
          vs -> Just (LintFailure path vs)
  pure $
    Right
      ModuleGraph
        { mgModules = Map.mapWithKey toModule (Closure.clExprs cl)
        , mgRoot = Closure.clRoot cl
        , mgFailures = [ParseFailure p e | (p, e) <- Map.toList (Closure.clFailures cl)]
        , mgLintFailures = [lf | Just lf <- map lintOf (Map.toList (Closure.clExprs cl))]
        , mgModuleTypes = Closure.clTypes cl
        , mgTruncated = Closure.clTruncated cl
        }

-- | build module graph starting from flake.nix in the given directory
buildModuleGraphFromFlake :: FilePath -> IO (Either Text ModuleGraph)
buildModuleGraphFromFlake dir = do
  let flakePath = dir </> "flake.nix"
  exists <- doesFileExist flakePath
  if exists
    then buildModuleGraph flakePath
    else pure $ Left $ "No flake.nix found in " <> T.pack dir

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- queries
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | the cross-module inferred type of every reachable module
moduleTypes :: ModuleGraph -> Map FilePath NixType
moduleTypes = mgModuleTypes
