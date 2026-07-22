{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                               // layout // convention // validate
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "Wintermute was hive mind, decision maker, effecting change in
--      the world outside."
--
--                                                                                     — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The validation engine: given a 'Convention', check a file (by path +
--   detected kind, or by parsed AST) against its location rules, forbidden
--   locations, file-name policy, and flake-module requirement, plus the
--   universal checks that apply to every convention — banned @_index.nix@ /
--   @_main.nix@ names and the @_class@ attribute that must match the
--   directory. Produces a list of 'LayoutError'.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Convention.Validate (
  -- * Validation
  validateLayout,
  validateFile,
  validateFileExpr,
  validateFileFromExpr,

  -- * Universal checks
  isIndexFile,
  isMainFile,
  checkBannedFiles,
  checkClassAttr,
)
where

import Data.Coerce (coerce)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Span (Span)
import Narsil.Layout.Convention.Naming (dropNixExtension, isValidName, suggestName)
import Narsil.Layout.Convention.Types
import Narsil.Layout.ModuleKind
import Narsil.Syntax.Annotation (srcSpanToSpan, pattern Layer, pattern LayerAnn)
import Nix.Expr.Types
import Nix.Expr.Types.Annotated
import System.FilePath (makeRelative, splitDirectories, takeFileName)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                               // universal checks
-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Checks that apply regardless of convention: banned file names, _class
-- attribute validation.

-- | is this a banned @_index.nix@ file?
isIndexFile :: FilePath -> Bool
isIndexFile path = takeFileName path == "_index.nix"

-- | is this a banned @_main.nix@ file?
isMainFile :: FilePath -> Bool
isMainFile path = takeFileName path == "_main.nix"

-- | flag @_index.nix@ / @_main.nix@ files, which are banned regardless of convention.
checkBannedFiles :: FilePath -> [LayoutError]
checkBannedFiles path
  | isIndexFile path =
      [ LayoutError
          { errCode = E007
          , errPath = path
          , errKind = Unknown
          , errMessage =
              "_index.nix files are banned; module graph is derived from directory structure"
          , errExpected = Nothing
          }
      ]
  | isMainFile path =
      [ LayoutError
          { errCode = E008
          , errPath = path
          , errKind = Unknown
          , errMessage = "_main.nix files are banned; use explicit imports in flake.nix"
          , errExpected = Nothing
          }
      ]
  | otherwise = []

classForPath :: FilePath -> Maybe Text
classForPath path = findClass (splitDirectories path)
 where
  findClass (x : y : _)
    | x == "modules" = classForDir (dropSlash y)
  findClass (_ : rest) = findClass rest
  findClass [] = Nothing
  dropSlash s
    | "/" `isSuffixOf` s = dropSlash (take (length s - 1) s)
    | otherwise = s
  classForDir "flake" = Just "flake"
  classForDir "nixos" = Just "nixos"
  classForDir "home" = Just "home"
  classForDir "home-manager" = Just "home"
  classForDir "darwin" = Just "darwin"
  classForDir _ = Nothing

findClassAttrWithSpan :: NExprLoc -> Maybe (Text, Span)
findClassAttrWithSpan = go
 where
  go (Layer (NSet _ bindings)) = listToMaybe (mapMaybe extractClass bindings)
  go (Layer (NAbs _ body)) = go body
  go (Layer (NLet _ body)) = go body
  go (Layer (NWith _ body)) = go body
  go _ = Nothing

  extractClass :: Binding NExprLoc -> Maybe (Text, Span)
  extractClass (NamedVar (StaticKey name :| []) valExpr _)
    | varNameText name == "_class" = extractStringValue valExpr
  extractClass _ = Nothing

  extractStringValue :: NExprLoc -> Maybe (Text, Span)
  extractStringValue (LayerAnn srcSpan (NStr (DoubleQuoted [Plain t]))) =
    Just (t, srcSpanToSpan srcSpan)
  extractStringValue (LayerAnn srcSpan (NStr (Indented _ [Plain t]))) =
    Just (t, srcSpanToSpan srcSpan)
  extractStringValue _ = Nothing

  varNameText :: VarName -> Text
  varNameText = coerce

{- | for a module whose directory implies a @_class@, flag a missing or mismatched
@_class@ attribute.
-}
checkClassAttr :: FilePath -> NExprLoc -> [LayoutError]
checkClassAttr path expr = maybe [] check (classForPath path)
 where
  check expected = maybe (missing expected) (matched expected) (findClassAttrWithSpan expr)

  missing expected =
    [ LayoutError
        { errCode = E009
        , errPath = path
        , errKind = Unknown
        , errMessage = "Module missing _class attribute; expected _class = \"" <> expected <> "\""
        , errExpected = Just expected
        }
    ]

  matched expected (actual, _sp)
    | actual /= expected =
        [ LayoutError
            { errCode = E010
            , errPath = path
            , errKind = Unknown
            , errMessage = "Wrong _class: got \"" <> actual <> "\", expected \"" <> expected <> "\""
            , errExpected = Just expected
            }
        ]
    | otherwise = []

{- | Validate a file with its parsed AST, running universal checks only.
For use by the module graph builder which already has the AST.
-}
validateFileExpr :: FilePath -> NExprLoc -> [LayoutError]
validateFileExpr path expr = checkBannedFiles path ++ checkClassAttr path expr

{- | Full validation: convention-specific rules plus universal checks.
Takes the project root for relative path computation.
-}
validateFileFromExpr :: Convention -> FilePath -> FilePath -> NExprLoc -> [LayoutError]
validateFileFromExpr conv root path expr =
  validateFile conv root path (detectKind path expr)
    ++ checkClassAttr path expr

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                     // validation
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Validate a single file against a convention.
validateFile :: Convention -> FilePath -> FilePath -> Detection -> [LayoutError]
validateFile conv root path detection =
  let relPath = makeRelative root path
      kind = detectedKind detection
      components = splitDirectories relPath
      fileName = takeFileName path
   in concat
        [ checkBannedFiles path
        , validateLocation conv relPath components kind
        , validateForbidden conv relPath components kind
        , validateFileName conv relPath fileName
        , validateFlakeModReq conv relPath kind detection
        ]

-- | Validate multiple files.
validateLayout :: Convention -> FilePath -> [(FilePath, Detection)] -> [LayoutError]
validateLayout conv root = concatMap (uncurry (validateFile conv root))

validateLocation :: Convention -> FilePath -> [String] -> ModuleKind -> [LayoutError]
validateLocation conv relPath components kind =
  maybe [] check (findRuleForKind (convRules conv) kind)
 where
  check rule
    | matchesPattern (rulePattern rule) components = []
    | otherwise =
        [ LayoutError
            { errCode = E001
            , errPath = relPath
            , errKind = kind
            , errMessage = "File in wrong location for " <> T.pack (show kind)
            , errExpected = Just $ patternDescription (rulePattern rule)
            }
        ]

validateForbidden :: Convention -> FilePath -> [String] -> ModuleKind -> [LayoutError]
validateForbidden conv relPath components kind =
  maybe [] check (findRuleForKind (convRules conv) kind)
 where
  check rule = map toError (filter (`matchesPattern` components) (ruleForbidden rule))
  toError pat =
    LayoutError
      { errCode = E002
      , errPath = relPath
      , errKind = kind
      , errMessage = "File in forbidden location"
      , errExpected = Just $ "not in " <> patternDescription pat
      }

validateFileName :: Convention -> FilePath -> String -> [LayoutError]
validateFileName conv relPath fileName =
  let baseName = dropNixExtension fileName
   in [ LayoutError
          { errCode = E003
          , errPath = relPath
          , errKind = Unknown
          , errMessage = "File name violates naming convention"
          , errExpected = Just $ T.pack $ suggestName (convFileNaming conv) baseName <> ".nix"
          }
      | not (isValidName (convFileNaming conv) baseName)
      ]

validateFlakeModReq :: Convention -> FilePath -> ModuleKind -> Detection -> [LayoutError]
validateFlakeModReq conv relPath kind _detection =
  -- Under a uniform-structure convention every recognized file must be a flake
  -- module, the flake itself, or a package.nix leaf; anything else (a stray
  -- NixOS module, overlay, bare attrset, or raw expression) is rejected.
  [ LayoutError
      { errCode = E006
      , errPath = relPath
      , errKind = kind
      , errMessage =
          "File must be a flake module or package (convention requires uniform structure)"
      , errExpected = Just "flake-parts module or package.nix"
      }
  | convRequireFlakeMod conv && kind `notElem` [Flake, FlakeModule, Package]
  ]

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                        // helpers
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

findRuleForKind :: [ConventionRule] -> ModuleKind -> Maybe ConventionRule
findRuleForKind rules kind = find ((== kind) . ruleKind) rules

matchesPattern :: PathPattern -> [String] -> Bool
matchesPattern None _ = True
matchesPattern (Exact expected) actual = actual == expected
matchesPattern (Prefix expected) actual = expected `isPrefixOf` actual
matchesPattern (Contains expected) actual = any (`elem` actual) expected
matchesPattern (AnyOf patterns) actual = any (`matchesPattern` actual) patterns

patternDescription :: PathPattern -> Text
patternDescription None = "anywhere"
patternDescription (Exact comps) = T.pack $ joinPath comps
patternDescription (Prefix comps) = T.pack $ joinPath comps <> "/..."
patternDescription (Contains comps) = "containing " <> T.pack (show comps)
patternDescription (AnyOf pats) = T.intercalate " or " (map patternDescription pats)

joinPath :: [String] -> String
joinPath = foldr1 (\a b -> a ++ "/" ++ b)
