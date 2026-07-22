{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // nixpkgs // index
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "She knew the names of all the streets, the addresses of every door,
--    though she had never walked there once."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   A static symbol index over a nixpkgs checkout — attribute name to defining
--   file — built WITHOUT evaluating Nix. The killer-feature substrate: from a
--   `pkgs.<name>` reference in any file we hop straight to the package's source.
--
--   Source 1 (here): `pkgs/by-name/<shard>/<name>/package.nix`, where the shard
--   is the first two characters of the name, lowercased. This is pure path math
--   — a directory scan, no parser — and covers the bulk of modern nixpkgs
--   (20k+ packages). Sources 2 (all-packages.nix callPackage parse) and 3 (the
--   lib graph) layer on later; the by-name map alone is the dominant target set.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Nixpkgs.Index (
  -- * Index
  NixpkgsIndex (..),
  Location,
  emptyIndex,
  buildNixpkgsIndex,

  -- * Sources
  byNameEntries,
  allPackagesEntries,

  -- * Query
  lookupPackage,
)
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Safety qualified as Safety
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Layout.Edge qualified as Edge
import Narsil.Syntax.Annotation (varNameText)
import Nix.Expr.Types (Binding (..), NKeyName (..))
import Nix.Expr.Types.Annotated (NExprLoc)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

{- | Where a nixpkgs symbol is defined: a file with a span. For by-name packages
the span is the head of @package.nix@ (refined to the derivation later).
-}
type Location = Span

{- | A static, eval-free symbol index over one nixpkgs checkout. Keyed by the
checkout root so a cache can hold several. Currently carries the by-name package
map; lib and all-packages maps join it as further sources land.
-}
data NixpkgsIndex = NixpkgsIndex
  { nixpkgsRoot :: !FilePath
  , pkgsByName :: !(Map Text Location)
  -- ^ package attribute name → its @pkgs/by-name/…/package.nix@
  }

-- | An empty index for a root (no entries scanned).
emptyIndex :: FilePath -> NixpkgsIndex
emptyIndex root = NixpkgsIndex root Map.empty

{- | Build the index for a nixpkgs checkout. by-name (a directory walk, no parse)
is authoritative; all-packages.nix (a syntactic callPackage parse, no eval) fills
in legacy names absent from by-name. No Nix is evaluated.
-}
buildNixpkgsIndex :: FilePath -> IO NixpkgsIndex
buildNixpkgsIndex root = do
  byName <- byNameEntries root
  allPkgs <- allPackagesEntries root
  -- 'Map.union' is left-biased, so by-name wins any name collision.
  pure (NixpkgsIndex root (Map.union (Map.fromList byName) (Map.fromList allPkgs)))

{- | Scan @pkgs/by-name@ into @(name, location)@ pairs. The layout is
@pkgs/by-name/<shard>/<name>/package.nix@ with @shard = toLower (take 2 name)@;
we read the directory structure directly rather than reproduce the sharding, so
the rule never drifts. Missing @by-name@ (older nixpkgs) yields an empty list.
-}
byNameEntries :: FilePath -> IO [(Text, Location)]
byNameEntries root = do
  let byNameDir = root </> "pkgs" </> "by-name"
  present <- doesDirectoryExist byNameDir
  if not present
    then pure []
    else do
      shards <- listDirectory byNameDir
      concat <$> mapM (shardEntries byNameDir) shards
 where
  shardEntries byNameDir shard = do
    let shardDir = byNameDir </> shard
    isDir <- doesDirectoryExist shardDir
    if not isDir
      then pure []
      else do
        names <- listDirectory shardDir
        catMaybes <$> mapM (pkgEntry shardDir) names
  pkgEntry shardDir name = do
    let pkgFile = shardDir </> name </> "package.nix"
    ok <- doesFileExist pkgFile
    pure (if ok then Just (T.pack name, headSpan pkgFile) else Nothing)
  headSpan f = Span (Loc 1 1) (Loc 1 1) (Just f)

{- | Parse @pkgs/top-level/all-packages.nix@ for @name = callPackage <path> …@
bindings — the legacy packages not yet migrated to by-name. Purely syntactic
(no Nix evaluation): descend the leading lambda/let/with wrappers to the package
attrset, then read each binding whose RHS is a @callPackage@/@callPackages@
applied to a literal path. Paths are relative to @pkgs/top-level@; a directory
path resolves to its @default.nix@. A parse failure yields an empty list.
-}
allPackagesEntries :: FilePath -> IO [(Text, Location)]
allPackagesEntries root = do
  let apFile = root </> "pkgs" </> "top-level" </> "all-packages.nix"
      topDir = root </> "pkgs" </> "top-level"
  present <- doesFileExist apFile
  if not present
    then pure []
    else do
      parsed <- Safety.safeParseNixFile apFile
      either (const (pure [])) (resolveAll topDir) parsed
 where
  resolveAll topDir expr = catMaybes <$> mapM (resolveEntry topDir) (callPackageBindings expr)
  resolveEntry topDir (name, relPath) = do
    let raw = topDir </> T.unpack relPath
    isFile <- doesFileExist raw
    target <- pickTarget raw isFile
    maybe (pure Nothing) (clean name) target
  -- Prefer the path as-is; a directory callPackage target resolves to default.nix.
  pickTarget raw True = pure (Just raw)
  pickTarget raw False = do
    let dflt = raw </> "default.nix"
    isDflt <- doesFileExist dflt
    pure (if isDflt then Just dflt else Nothing)
  -- canonicalise so the `../..` from the relative path collapses to a clean path.
  clean name t = do
    c <- canonicalizePath t
    pure (Just (name, headSpan c))
  headSpan f = Span (Loc 1 1) (Loc 1 1) (Just f)

{- | The @(name, relative-path)@ pairs of every @callPackage@/@callPackages@
binding at the top level of all-packages.nix.
-}
callPackageBindings :: NExprLoc -> [(Text, Text)]
callPackageBindings = concatMap binding . Edge.topBindings
 where
  binding (NamedVar (StaticKey k :| []) rhs _) =
    maybe [] (\p -> [(varNameText k, p)]) (Edge.callPackageTargetOf rhs)
  binding _ = []

-- | Look up a package attribute name in the index.
lookupPackage :: NixpkgsIndex -> Text -> Maybe Location
lookupPackage idx name = Map.lookup name (pkgsByName idx)
