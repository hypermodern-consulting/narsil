{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                         // naming
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "Case was twenty-four. At twenty-two, he'd been a cowboy, a rustler,
--      one of the best in the Sprawl."
--
--                                                                                     — Neuromancer
--

{- | Identifier naming convention enforcement.

Enforces naming conventions for:
  * Attribute names
  * Let bindings
  * Function parameters
  * Option names (NixOS modules)

Conventions:
  * 'LispCase' (kebab-case): forces-code-through-prelude
  * 'SnakeCase': python_style_names
  * 'CamelCase': nixpkgsStyleNames
-}
module Narsil.Layout.Naming (
  -- * Conventions
  NamingConvention (..),

  -- * Validation
  findNamingViolations,
  checkIdentifier,

  -- * Results
  NamingViolation (..),

  -- * Utilities
  toKebabCase,
  toSnakeCase,
  toCamelCase,
  isKebabCase,
  isSnakeCase,
  isCamelCase,
)
where

import Data.Char (isAlphaNum, isLower, isUpper, toLower, toUpper)
import Data.Coerce (coerce)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Syntax.Annotation (pattern Layer, pattern LayerAnn)
import Nix.Expr.Types
import Nix.Expr.Types.Annotated
import Nix.Utils (Path (..))

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                          // types
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Naming convention.
data NamingConvention
  = -- | kebab-case (straylight)
    LispCase
  | -- | snake_case
    SnakeCase
  | -- | camelCase (nixpkgs)
    CamelCase
  | NoConvention
  deriving (Eq, Show)

-- | A naming violation.
data NamingViolation = NamingViolation
  { nvIdentifier :: !Text
  , nvExpected :: !NamingConvention
  , nvSpan :: !Span
  , nvContext :: !Text
  -- ^ Where it appears (attr, let, param, option)
  , nvSuggestion :: !Text
  -- ^ Corrected name
  }
  deriving (Eq, Show)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                     // validation
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Find all naming violations in an expression.
findNamingViolations :: NamingConvention -> NExprLoc -> [NamingViolation]
findNamingViolations NoConvention _ = []
findNamingViolations conv expr = go expr
 where
  go :: NExprLoc -> [NamingViolation]
  -- Let bindings: let foo = ...; in ...
  go (LayerAnn srcSpan (NLet bindings body)) =
    concatMap (checkBinding conv "let binding" srcSpan) bindings ++ go body
  -- Attribute set: { foo = ...; bar = ...; }
  go (LayerAnn srcSpan (NSet _ bindings)) =
    concatMap (checkBinding conv "attribute" srcSpan) bindings
  -- Function with pattern: { foo, bar, ... }: ...
  go (LayerAnn srcSpan (NAbs (ParamSet _ _ params) body)) =
    mapMaybe (checkParam conv srcSpan) params ++ go body
  -- Recurse into children
  go (Layer (NAbs _ body)) = go body
  go (Layer (NWith _ body)) = go body
  go (Layer (NIf cond t f)) = go cond ++ go t ++ go f
  go (Layer (NApp f x)) = go f ++ go x
  go (Layer (NSelect _ e1 _)) = go e1
  go (Layer (NHasAttr e1 _)) = go e1
  go (Layer (NList xs)) = concatMap go xs
  go (Layer (NUnary _ e1)) = go e1
  go (Layer (NBinary _ l r)) = go l ++ go r
  go (Layer (NAssert cond body)) = go cond ++ go body
  go _ = []

-- | Check a binding name.
checkBinding :: NamingConvention -> Text -> SrcSpan -> Binding NExprLoc -> [NamingViolation]
checkBinding conv ctx srcSpan (NamedVar (StaticKey name :| []) val _) =
  checkIdentifier conv ctx (coerce name :: Text) (toSpan srcSpan) ++ valViolations
 where
  valViolations
    | NoConvention <- conv = []
    | otherwise = findNamingViolations conv val
checkBinding conv _ _ (NamedVar _ val _) = findNamingViolations conv val
checkBinding _ _ _ (Inherit _ _ _) = []

-- | Check a parameter name.
checkParam :: NamingConvention -> SrcSpan -> (VarName, Maybe NExprLoc) -> Maybe NamingViolation
checkParam conv srcSpan (name, _) =
  listToMaybe (checkIdentifier conv "parameter" (coerce name :: Text) (toSpan srcSpan))

-- | Check a single identifier against the convention.
checkIdentifier :: NamingConvention -> Text -> Text -> Span -> [NamingViolation]
checkIdentifier conv ctx ident sp
  | isExempt ident = []
  | matchesConvention conv (T.unpack ident) = []
  | otherwise =
      [ NamingViolation
          { nvIdentifier = ident
          , nvExpected = conv
          , nvSpan = sp
          , nvContext = ctx
          , nvSuggestion = T.pack $ convertTo conv (T.unpack ident)
          }
      ]

-- | Check if identifier matches convention.
matchesConvention :: NamingConvention -> String -> Bool
matchesConvention NoConvention _ = True
matchesConvention LispCase s = isKebabCase s
matchesConvention SnakeCase s = isSnakeCase s
matchesConvention CamelCase s = isCamelCase s

-- | Convert identifier to convention.
convertTo :: NamingConvention -> String -> String
convertTo NoConvention s = s
convertTo LispCase s = toKebabCase s
convertTo SnakeCase s = toSnakeCase s
convertTo CamelCase s = toCamelCase s

-- | Identifiers exempt from naming rules.
isExempt :: Text -> Bool
isExempt ident = ident `elem` exemptList || T.isPrefixOf "_" ident
 where
  exemptList =
    [ -- Nix builtins and common attrs
      "inherit"
    , "self"
    , "super"
    , "final"
    , "prev"
    , "config"
    , "lib"
    , "pkgs"
    , "options"
    , "imports"
    , "stdenv"
    , "fetchurl"
    , "fetchFromGitHub"
    , "fetchgit"
    , "buildInputs"
    , "nativeBuildInputs"
    , "propagatedBuildInputs"
    , "pname"
    , "version"
    , "src"
    , "meta"
    , "description"
    , "homepage"
    , "license"
    , "maintainers"
    , "platforms"
    , "configurePhase"
    , "buildPhase"
    , "installPhase"
    , "checkPhase"
    , "postInstall"
    , "postBuild"
    , "preBuild"
    , "preInstall"
    , "passthru"
    , "overrideAttrs"
    , "override"
    , -- flake-parts
      "perSystem"
    , "flake"
    , "withSystem"
    , "inputsFrom"
    , -- home-manager / NixOS
      "enable"
    , "package"
    , "extraConfig"
    , "extraOptions"
    , "systemd"
    , "services"
    , "programs"
    , "environment"
    , -- Common functions
      "mkShell"
    , "writeShellApplication"
    , "mkDerivation"
    , "mkOption"
    , "mkEnableOption"
    , "shellHook"
    , "runtimeInputs"
    , "haskellPackages"
    , "mkCheck"
    , "mkHook"
    , "mkPreCommitHook"
    , "mkIntegration"
    , -- Flake outputs
      "packages"
    , "checks"
    , "devShells"
    , "apps"
    , "overlays"
    , "nixosModules"
    , "homeModules"
    , "flakeModules"
    , -- Special
      "true"
    , "false"
    , "null"
    , "or"
    , "if"
    , "then"
    , "else"
    , "let"
    , "in"
    , "with"
    , "rec"
    , "assert"
    , "inherit"
    ]

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                 // case utilities
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Check if string is kebab-case.
isKebabCase :: String -> Bool
isKebabCase [] = False
isKebabCase s@(c : _) =
  all validChar s
    && not ("--" `isInfixOf` s)
    && c /= '-'
    && last s /= '-'
 where
  validChar ch = isLower ch || ch == '-' || (ch >= '0' && ch <= '9')
  isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  isPrefixOf [] _ = True
  isPrefixOf _ [] = False
  isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys
  tails [] = [[]]
  tails xs@(_ : xs') = xs : tails xs'

-- | Check if string is snake_case.
isSnakeCase :: String -> Bool
isSnakeCase [] = False
isSnakeCase s@(c : _) =
  all validChar s
    && not ("__" `isInfixOf` s)
    && c /= '_'
    && last s /= '_'
 where
  validChar ch = isLower ch || ch == '_' || (ch >= '0' && ch <= '9')
  isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  isPrefixOf [] _ = True
  isPrefixOf _ [] = False
  isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys
  tails [] = [[]]
  tails xs@(_ : xs') = xs : tails xs'

-- | Check if string is camelCase.
isCamelCase :: String -> Bool
isCamelCase [] = False
isCamelCase (c : cs) = isLower c && all validChar cs
 where
  validChar x = isAlphaNum x

-- | Convert to kebab-case.
toKebabCase :: String -> String
toKebabCase = intercalate "-" . splitWords . map toLower

-- | Convert to snake_case.
toSnakeCase :: String -> String
toSnakeCase = intercalate "_" . splitWords . map toLower

-- | Convert to camelCase.
toCamelCase :: String -> String
toCamelCase s = camel (splitWords s)
 where
  camel [] = ""
  camel (w : ws) = map toLower w ++ concatMap capitalize ws
  capitalize [] = []
  capitalize (c : cs) = toUpper c : map toLower cs

-- | Split a string into words (handles camelCase, snake_case, kebab-case).
splitWords :: String -> [String]
splitWords [] = []
splitWords s =
  let (word, rest) = span isWordChar $ dropWhile isSeparator s
   in if null word
        then splitCamel rest
        else word : splitWords rest
 where
  isSeparator c = c == '-' || c == '_'
  isWordChar c = isAlphaNum c && not (isUpper c)

  -- Handle camelCase by splitting on uppercase
  splitCamel [] = []
  splitCamel (c : cs)
    | isUpper c =
        let (upper, rest) = span isUpper cs
            (lower, rest') = span isLower rest
         in (toLower c : map toLower upper ++ lower) : splitWords rest'
    | otherwise = splitWords (c : cs)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                        // helpers
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

toSpan :: SrcSpan -> Span
toSpan srcSpan =
  let begin = getSpanBegin srcSpan
      end = getSpanEnd srcSpan
   in Span
        { spanStart = Loc (sourceLine begin) (sourceCol begin)
        , spanEnd = Loc (sourceLine end) (sourceCol end)
        , spanFile = sourceFile begin
        }
 where
  sourceLine (NSourcePos _ (NPos l) _) = fromIntegral (unPos l)
  sourceCol (NSourcePos _ _ (NPos c)) = fromIntegral (unPos c)
  sourceFile (NSourcePos path _ _) = Just (coerce path)
