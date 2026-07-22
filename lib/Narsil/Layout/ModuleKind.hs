{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                    // module kind
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "He'd found her, one rainy night, in an arcade."
--
--                                                                                     — Neuromancer
--

{- | What kind of Nix file is this? A @.nix@ file carries no declared type, yet
the layout checker, the module-graph builder, and the type checker all need to
know whether they are looking at a package, a NixOS module, a flake-parts module,
an overlay, and so on. This module answers that question from the file's path and
parsed structure, and reports how sure it is.

The strategy is a cascade, most-authoritative first:

  1. An explicit @_class = "nixos"@ attribute — the author told us; we believe
     them (confidence 100).
  2. The filename — @flake.nix@, @package.nix@, @overlay.nix@ are strong signals.
  3. The shape — parameter names (@{ stdenv, ... }@) and body attributes
     (@options@/@config@, @mkDerivation@, @perSystem@, @final: prev:@).

Filename and shape each emit a list of weighted hints; 'selectBest' takes the
highest-confidence one. The recognized kinds:

  * 'NixOSModule' — @{ config, lib, pkgs, ... }: { options = …; config = …; }@
  * 'HomeModule' — the same shape, for home-manager
  * 'DarwinModule' — the same shape, for nix-darwin
  * 'Package' — @{ lib, stdenv, ... }: stdenv.mkDerivation { … }@
  * 'Overlay' — @final: prev: { … }@
  * 'FlakeModule' — a flake-parts module (@perSystem@ / @flake@ / @imports@)
  * 'Library' — @{ lib }: { … }@ exporting functions
  * 'Flake' — the @flake.nix@ file itself
  * 'Shell' — a devShell / @shell.nix@
  * 'Unknown' — no signal strong enough to commit
-}
module Narsil.Layout.ModuleKind (
  -- * Types
  ModuleKind (..),
  Detection (..),

  -- * Detection
  detectKind,
  detectKindFromFile,
  detectClassValue,

  -- * Queries
  isNixOSModule,
  isPackage,
  isOverlay,
  isFlakeModule,
)
where

import Data.Coerce (coerce)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Narsil.Syntax.Annotation (pattern Layer)
import Narsil.Syntax.Parse (parseNix)
import Nix.Expr.Types
import Nix.Expr.Types.Annotated
import System.FilePath (takeFileName)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                          // types
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | The kind of Nix file, as recognized by 'detectKind'.
data ModuleKind
  = -- | NixOS configuration module
    NixOSModule
  | -- | home-manager module
    HomeModule
  | -- | nix-darwin module
    DarwinModule
  | -- | derivation / package definition
    Package
  | -- | nixpkgs overlay (@final: prev: …@)
    Overlay
  | -- | flake-parts module
    FlakeModule
  | -- | part of a flake (@perSystem@, etc.)
    FlakePart
  | -- | library of functions
    Library
  | -- | the @flake.nix@ file itself
    Flake
  | -- | devShell or @shell.nix@
    Shell
  | -- | test file
    Test
  | -- | no signal strong enough to commit
    Unknown
  deriving (Eq, Show, Ord)

{- | A classification plus how confident we are and why. The evidence list is
kept so diagnostics can explain the verdict ("detected as Package because: has
stdenv param, calls mkDerivation").
-}
data Detection = Detection
  { detectedKind :: !ModuleKind
  , detectedConf :: !Int
  -- ^ confidence, 0–100
  , detectedEvidence :: ![Text]
  -- ^ the hints that produced this kind
  }
  deriving (Eq, Show)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                      // detection
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Parse a file from disk and classify it; 'Unknown' if it does not parse.
detectKindFromFile :: FilePath -> IO Detection
detectKindFromFile path = do
  content <- TIO.readFile path
  pure (either parseFailed (detectKind path) (parseNix path content))
 where
  parseFailed _ = Detection Unknown 0 ["parse failed"]

{- | Classify an already-parsed expression. A declared @_class@ wins outright;
otherwise we pool the filename and structural hints and take the best.
-}
detectKind :: FilePath -> NExprLoc -> Detection
detectKind path expr
  | Just cls <- detectClassValue expr = fromClass cls
  | otherwise = selectBest (detectFromFileName (takeFileName path) ++ detectFromStructure expr)
 where
  -- An explicit `_class = "..."` is the author speaking directly: authoritative
  -- (confidence 100), even when the value is one we don't recognize (in which
  -- case we surface that rather than silently guessing from structure).
  fromClass cls
    | Just kind <- classToKind cls = Detection kind 100 ["_class = \"" <> cls <> "\""]
    | otherwise = Detection Unknown 0 ["unknown _class = \"" <> cls <> "\""]

{- | Extract a top-level @_class@ string, if present. We look through the leading
function parameters and @let@/@with@ wrappers a module may have before its body
attrset, since @{ ... }: { _class = "nixos"; … }@ is the common shape.
-}
detectClassValue :: NExprLoc -> Maybe Text
detectClassValue = go
 where
  go (Layer (NSet _ bindings)) = listToMaybe (mapMaybe classBinding bindings)
  go (Layer (NAbs _ body)) = go body
  go (Layer (NLet _ body)) = go body
  go (Layer (NWith _ body)) = go body
  go _ = Nothing

  -- the value of a `_class = "..."` binding, if this is one
  classBinding (NamedVar (StaticKey name :| []) valExpr _)
    | coerce name == ("_class" :: Text) = staticString valExpr
  classBinding _ = Nothing

  -- a string literal with no interpolation (both quoting styles)
  staticString (Layer (NStr (DoubleQuoted [Plain t]))) = Just t
  staticString (Layer (NStr (Indented _ [Plain t]))) = Just t
  staticString _ = Nothing

-- | Map a declared @_class@ value to the kind it names.
classToKind :: Text -> Maybe ModuleKind
classToKind "flake" = Just FlakeModule
classToKind "nixos" = Just NixOSModule
classToKind "home" = Just HomeModule
classToKind "homeManager" = Just HomeModule
classToKind "darwin" = Just DarwinModule
classToKind "package" = Just Package
classToKind "overlay" = Just Overlay
classToKind "lib" = Just Library
classToKind "shell" = Just Shell
classToKind _ = Nothing

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                             // filename detection
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Hints from the bare filename. @default.nix@ is deliberately silent — it can
be anything, so we let the structural pass decide.
-}
detectFromFileName :: String -> [(ModuleKind, Int, Text)]
detectFromFileName "flake.nix" = [(Flake, 100, "filename is flake.nix")]
detectFromFileName "shell.nix" = [(Shell, 90, "filename is shell.nix")]
detectFromFileName "default.nix" = []
detectFromFileName "package.nix" = [(Package, 80, "filename is package.nix")]
detectFromFileName "module.nix" = [(NixOSModule, 60, "filename is module.nix")]
detectFromFileName "overlay.nix" = [(Overlay, 80, "filename is overlay.nix")]
detectFromFileName "test.nix" = [(Test, 80, "filename is test.nix")]
detectFromFileName name
  | "-test.nix" `T.isSuffixOf` T.pack name = [(Test, 70, "filename ends in -test.nix")]
  | "-module.nix" `T.isSuffixOf` T.pack name = [(NixOSModule, 60, "filename ends in -module.nix")]
  | otherwise = []

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                            // structure detection
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Hints from the parsed shape. Three shapes carry signal: the two-argument
@final: prev:@ overlay, a function (whose parameters and body we mine), and a
bare attrset.
-}
detectFromStructure :: NExprLoc -> [(ModuleKind, Int, Text)]
detectFromStructure (Layer (NAbs param1 body1))
  -- `final: prev: { … }` — an overlay. n.b. a single overlay-shaped parameter
  -- that is NOT followed by a second one is no signal (an empty list), and must
  -- not fall through to the general-function clause below.
  | isOverlayParam param1
  , Layer (NAbs param2 _) <- body1
  , isOverlayParam param2 =
      [(Overlay, 95, "two-argument function (final: prev:)")]
  | isOverlayParam param1 = []
detectFromStructure (Layer (NAbs param body)) =
  detectFromParams (getParamNames param) ++ detectFromBody body
detectFromStructure (Layer (NSet _ bindings)) = detectFromBindings bindings
detectFromStructure _ = []

-- | Does this parameter name an overlay's fixpoint argument?
isOverlayParam :: Params NExprLoc -> Bool
isOverlayParam (Param name) = coerce name `elem` (["final", "prev", "self", "super"] :: [Text])
isOverlayParam _ = False

-- | The names bound by a function's parameter (one name, or a whole @{ … }@ set).
getParamNames :: Params NExprLoc -> [Text]
getParamNames (Param name) = [coerce name]
getParamNames (ParamSet _ _ params) = map (coerce . fst) params

{- | Hints from parameter names alone. The module-system trio (@config@ + @lib@
or @pkgs@) reads as a NixOS module; build inputs (@stdenv@, fetchers) as a
package; the flake-parts argument set as a flake module.
-}
detectFromParams :: [Text] -> [(ModuleKind, Int, Text)]
detectFromParams params
  | hasNixOSParams = [(NixOSModule, 70, "has config/lib/pkgs params")]
  | hasPackageParams = [(Package, 70, "has stdenv/fetchurl params")]
  | hasFlakeModuleParams = [(FlakeModule, 60, "has flake-parts params")]
  | otherwise = []
 where
  has names = all (`elem` params) names
  hasNixOSParams = has ["config", "lib"] || has ["config", "pkgs"]
  hasPackageParams = any (`elem` params) ["stdenv", "mkDerivation", "fetchurl", "fetchFromGitHub"]
  hasFlakeModuleParams = has ["config", "lib", "flake-parts-lib"] || has ["self", "inputs"]

-- | Hints from a function body: look through @let@/@with@ to the attrset inside.
detectFromBody :: NExprLoc -> [(ModuleKind, Int, Text)]
detectFromBody (Layer (NSet _ bindings)) = detectFromBindings bindings
detectFromBody (Layer (NLet _ inner)) = detectFromBody inner
detectFromBody (Layer (NWith _ inner)) = detectFromBody inner
detectFromBody _ = []

-- | Hints from an attrset's top-level attribute names.
detectFromBindings :: [Binding NExprLoc] -> [(ModuleKind, Int, Text)]
detectFromBindings = detectFromAttrNames . mapMaybe bindingName
 where
  bindingName (NamedVar (StaticKey name :| []) _ _) = Just (coerce name)
  bindingName _ = Nothing

{- | Hints from attribute names, ranked by how diagnostic each shape is. Ordering
matters: @options@+@config@ pins a module hard (85) before the weaker package and
flake heuristics get a look.
-}
detectFromAttrNames :: [Text] -> [(ModuleKind, Int, Text)]
detectFromAttrNames names
  | hasOptions && hasConfig = [(NixOSModule, 85, "has options and config attrs")]
  | hasOptions = [(NixOSModule, 60, "has options attr")]
  | hasMkDeriv = [(Package, 90, "calls mkDerivation or similar")]
  | hasPname && hasVersion = [(Package, 75, "has pname and version")]
  | hasFlakeConfig = [(FlakeModule, 80, "has flake-parts config pattern")]
  | hasShellAttrs = [(Shell, 70, "has shell-like attrs")]
  | hasLibExports = [(Library, 50, "exports library functions")]
  | otherwise = []
 where
  named = (`elem` names)
  hasOptions = named "options"
  hasConfig = named "config"
  hasMkDeriv =
    any named ["mkDerivation", "stdenv.mkDerivation", "buildPythonPackage", "buildGoModule"]
  hasPname = named "pname"
  hasVersion = named "version"
  -- `imports` (with none of the stronger signals above) marks a flake-parts
  -- module that only wires children — common in the all-flake-module layout.
  hasFlakeConfig = any named ["perSystem", "flake", "imports"]
  hasShellAttrs = named "buildInputs" && named "shellHook"
  hasLibExports = any named ["mkOption", "mkIf", "mapAttrs", "filterAttrs"]

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                        // ranking
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Collapse a pool of weighted hints into a single verdict: the highest
confidence wins, and the winner's evidence (across all hints that agree on the
kind) is retained. No hints at all means 'Unknown'.
-}
selectBest :: [(ModuleKind, Int, Text)] -> Detection
selectBest [] = Detection Unknown 0 []
selectBest hints = Detection kind conf evidence
 where
  -- nub before ranking so a kind asserted by both filename and structure doesn't
  -- get double-counted; reverse so structural hints (added last) outrank a
  -- weaker filename hint at equal confidence.
  (kind, conf, _) = maximumByConfidence (reverse (nub hints))
  evidence = [e | (k, _, e) <- hints, k == kind]

{- | The element with the greatest confidence (the middle field). Total on the
non-empty lists 'selectBest' hands it; the empty case cannot arise there.
-}
maximumByConfidence :: [(ModuleKind, Int, Text)] -> (ModuleKind, Int, Text)
maximumByConfidence [] = (Unknown, 0, "")
maximumByConfidence (hint : hints) = foldl keepHigher hint hints
 where
  keepHigher best@(_, bestConf, _) candidate@(_, conf, _)
    | conf > bestConf = candidate
    | otherwise = best

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                        // queries
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | A NixOS / home-manager / nix-darwin module (the option-system family).
isNixOSModule :: Detection -> Bool
isNixOSModule d = detectedKind d `elem` [NixOSModule, HomeModule, DarwinModule]

-- | A derivation / package definition.
isPackage :: Detection -> Bool
isPackage d = detectedKind d == Package

-- | A nixpkgs overlay (@final: prev: …@).
isOverlay :: Detection -> Bool
isOverlay d = detectedKind d == Overlay

-- | A flake-parts module or flake part.
isFlakeModule :: Detection -> Bool
isFlakeModule d = detectedKind d `elem` [FlakeModule, FlakePart]
