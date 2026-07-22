{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                 // layout // edge
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Every wire that left the box went somewhere; he learned to read them
--    all the same way."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The single home for eval-free dependency-edge discovery — the three ways one
--   Nix file reaches another, recognized syntactically from the AST. A PURE leaf
--   (hnix + 'Narsil.Layout.Import' only, no inference), so every consumer —
--   the type closure ('Narsil.Layout.Closure'), the module system, the
--   nixpkgs index — depends DOWN on this one definition instead of each carrying
--   its own copy of the same scan.
--
--     * 'EImport'      — @import ./path@, via the canonical 'findImports' walker;
--     * 'EFlakeImport' — a flake-parts @imports = [ ./a.nix … ]@ list element
--       ('flakeImportPaths');
--     * 'ECallPackage' — a top-level @x = callPackage ./path { }@ binding
--       ('callPackageTargetOf' on the bound value).
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Edge (
  -- * Edges
  EdgeKind (..),
  Edge (..),
  discoverEdges,

  -- * The individual scans (for consumers that want one kind)
  flakeImportPaths,
  callPackageTargetOf,
  callPackageHeadOf,
  topBindings,
  findAttr,

  -- * Path resolution
  collapseDots,
)
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Layout.Import (Import (..), findImports)
import Narsil.Syntax.Annotation (varNameText, pattern Layer)
import Nix.Expr.Types
import Nix.Expr.Types.Annotated
import Nix.Utils qualified as NixPath
import System.FilePath (joinPath, normalise, splitDirectories, (</>))

-- ── edges ───────────────────────────────────────────────────────────

{- | Which kind of dependency an edge encodes — the three ways one Nix file
reaches another without evaluation.
-}
data EdgeKind
  = -- | @import ./path@ (also @import ./path args@, @builtins.import@)
    EImport
  | -- | a flake-parts @imports = [ ./a.nix … ]@ list element
    EFlakeImport
  | -- | a top-level @x = callPackage ./path { }@ binding
    ECallPackage
  deriving (Eq, Ord, Show)

{- | One discovered edge: its kind, the path it resolves to (against the source
file's directory, before any existence check), and the path text as written
(the key inference's 'lookupImport' looks an @import@ up under).
-}
data Edge = Edge
  { edgeKind :: !EdgeKind
  , edgePath :: !FilePath
  , edgeRaw :: !Text
  }
  deriving (Eq, Show)

{- | Every dependency edge of an expression, all three kinds, eval-free. The
@import@ case delegates to the canonical 'findImports' walker; the flake-parts and
@callPackage@ cases use the scans below.
-}
discoverEdges :: FilePath -> NExprLoc -> [Edge]
discoverEdges baseDir expr =
  [Edge EImport (collapseDots (impPath i)) (impRawPath i) | i <- findImports baseDir expr]
    ++ [Edge EFlakeImport (resolveEdge baseDir p) (T.pack p) | p <- flakeImportPaths expr]
    ++ [Edge ECallPackage (resolveEdge baseDir (T.unpack raw)) raw | raw <- callPackagePaths expr]

-- ── the individual scans ─────────────────────────────────────────────

-- | flake-parts module imports: the literal paths of a top-level @imports = [ … ]@.
flakeImportPaths :: NExprLoc -> [FilePath]
flakeImportPaths expr = maybe [] extractPaths (findAttr "imports" (topBindings expr))
 where
  extractPaths (Layer (NList es)) = mapMaybe litPath es
  extractPaths (Layer (NApp f a)) = extractPaths f ++ extractPaths a
  extractPaths _ = []
  litPath (Layer (NLiteralPath (NixPath.Path p))) = Just p
  litPath (Layer (NStr (DoubleQuoted [Plain t]))) = Just (T.unpack t)
  litPath _ = Nothing

-- | The literal paths of every top-level @x = callPackage ./path { … }@ binding.
callPackagePaths :: NExprLoc -> [Text]
callPackagePaths = mapMaybe binding . topBindings
 where
  binding (NamedVar _ rhs _) = callPackageTargetOf rhs
  binding _ = Nothing

{- | The literal path of a @callPackage ./path@ / @callPackages ./path@ application,
if the expression is one (the package file an @x = callPackage ./p { }@ binding
pulls in). Bare @callPackage@ head only, by design (matching nixpkgs all-packages).
-}
callPackageTargetOf :: NExprLoc -> Maybe Text
callPackageTargetOf (Layer (NApp headExpr _)) = T.pack <$> callPackageHeadOf headExpr
callPackageTargetOf _ = Nothing

{- | The literal path of a bare @callPackage ./path@ \/ @callPackages ./path@ HEAD —
the one-argument prefix of the full application. The closure walker matches whole
bindings ('callPackageTargetOf'); inference intercepts at the application node,
where the function position is exactly this shape. ONE name-set and ONE path
matcher, so the seeding side and the lookup side can never drift apart.
-}
callPackageHeadOf :: NExprLoc -> Maybe FilePath
callPackageHeadOf (Layer (NApp headF (Layer (NLiteralPath (NixPath.Path p)))))
  | isCallPackageName headF = Just p
 where
  -- bare `callPackage` or one select deep (`pkgs.callPackage`,
  -- `python3Packages.callPackage`) — the qualified form is the same
  -- protocol and must intercept identically or the seam drifts
  isCallPackageName (Layer (NSym f)) = varNameText f `elem` cpNames
  isCallPackageName (Layer (NSelect Nothing _ (StaticKey k :| []))) = varNameText k `elem` cpNames
  isCallPackageName _ = False
  cpNames = ["callPackage", "callPackages"] :: [Text]
callPackageHeadOf _ = Nothing

-- ── small AST helpers ───────────────────────────────────────────────

-- | Top-level bindings, unwrapping lambda / let / with wrappers.
topBindings :: NExprLoc -> [Binding NExprLoc]
topBindings (Layer (NSet _ bs)) = bs
topBindings (Layer (NAbs _ body)) = topBindings body
topBindings (Layer (NLet _ body)) = topBindings body
topBindings (Layer (NWith _ body)) = topBindings body
topBindings _ = []

{- | The value of a named static binding, if present (shared by the flake-parts
scan and the module system's option/config queries).
-}
findAttr :: Text -> [Binding NExprLoc] -> Maybe NExprLoc
findAttr name = foldr check Nothing
 where
  check (NamedVar (StaticKey k :| []) v _) acc
    | varNameText k == name = Just v
    | otherwise = acc
  check _ acc = acc

-- | Resolve a raw import path against a base directory (absolute paths pass through).
resolveEdge :: FilePath -> FilePath -> FilePath
resolveEdge _ path@('/' : _) = collapseDots path
resolveEdge baseDir path = collapseDots (normalise (baseDir </> path))

{- | Collapse @.@ \/ @..@ segments lexically — the way Nix resolves path literals:
@a\/link\/..\/b@ is @a\/b@ even when @link@ is a symlink somewhere else entirely.
Purely textual, never consults the filesystem: resolving through the filesystem
('canonicalizePath', or letting @doesFileExist@ chase the dots) would follow the
symlink and type a DIFFERENT file than Nix evaluates.
-}
collapseDots :: FilePath -> FilePath
collapseDots = joinPath . reverse . foldl' step [] . splitDirectories
 where
  step acc "." = acc
  step (d : ds) ".." | d /= "/", d /= ".." = ds
  step acc seg = seg : acc
