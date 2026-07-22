{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                 // layout // convention // naming
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "He'd given up trying to pronounce the names; the shapes of them were
--      enough."
--
--                                                                                     — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Naming policy: decide whether a string obeys a 'NamingConvention'
--   (kebab / snake / camel / Pascal), convert between cases, and turn a
--   bad attribute or identifier name into a 'LayoutError' carrying the
--   suggested fix. Pure string work over the convention vocabulary.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Convention.Naming (
  NamingConvention (..),
  isValidName,
  toKebabCase,
  toSnakeCase,
  suggestName,
  dropNixExtension,
  validateAttrName,
  validateIdentifier,
)
where

import Data.Char (isAlphaNum, isDigit, isLower, isUpper, toLower)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Layout.Convention.Types
import Narsil.Layout.ModuleKind (ModuleKind (Unknown))

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                              // naming validation
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Check if a name is valid for a convention.
isValidName :: NamingConvention -> String -> Bool
isValidName NoNaming _ = True
isValidName KebabCase s = isKebabCase s
isValidName SnakeCase s = isSnakeCase s
isValidName CamelCase s = isCamelCase s
isValidName PascalCase s = isPascalCase s

isKebabCase :: String -> Bool
isKebabCase [] = False
isKebabCase s = all validChar s && not (badPattern s)
 where
  validChar c = isLower c || isDigit c || c == '-'
  badPattern x =
    "--" `isPrefixOf` x
      || "--" `isSuffixOf` x
      || "-" `isPrefixOf` x
      || "-" `isSuffixOf` x

isSnakeCase :: String -> Bool
isSnakeCase [] = False
isSnakeCase s = all validChar s && not (badPattern s)
 where
  validChar c = isLower c || isDigit c || c == '_'
  badPattern x = "__" `isPrefixOf` x || "__" `isSuffixOf` x

isCamelCase :: String -> Bool
isCamelCase [] = False
isCamelCase (c : cs) = isLower c && all isAlphaNum cs

isPascalCase :: String -> Bool
isPascalCase [] = False
isPascalCase (c : cs) = isUpper c && all isAlphaNum cs

-- | Convert to kebab-case.
toKebabCase :: String -> String
toKebabCase = go False
 where
  go _ [] = []
  go prev (c : cs)
    | isUpper c = (if prev then ['-', toLower c] else [toLower c]) ++ go True cs
    | c == '_' = '-' : go False cs
    | otherwise = c : go (isLower c) cs

-- | Convert to snake_case.
toSnakeCase :: String -> String
toSnakeCase = go False
 where
  go _ [] = []
  go prev (c : cs)
    | isUpper c = (if prev then ['_', toLower c] else [toLower c]) ++ go True cs
    | c == '-' = '_' : go False cs
    | otherwise = c : go (isLower c) cs

-- | Suggest a corrected name under a convention (identity when not enforced).
suggestName :: NamingConvention -> String -> String
suggestName KebabCase s = toKebabCase s
suggestName SnakeCase s = toSnakeCase s
suggestName _ s = s

-- | Strip a trailing @.nix@ extension if present.
dropNixExtension :: String -> String
dropNixExtension s
  | ".nix" `isSuffixOf` s = take (length s - 4) s
  | otherwise = s

-- | Validate an attribute name.
validateAttrName :: Convention -> Text -> Maybe LayoutError
validateAttrName conv name =
  let s = T.unpack name
   in if isValidName (convAttrNaming conv) s
        then Nothing
        else
          Just $
            LayoutError
              { errCode = E004
              , errPath = ""
              , errKind = Unknown
              , errMessage = "Attribute name '" <> name <> "' violates naming convention"
              , errExpected = Just $ T.pack $ suggestName (convAttrNaming conv) s
              }

-- | Validate an identifier.
validateIdentifier :: Convention -> Text -> Maybe LayoutError
validateIdentifier conv name =
  let s = T.unpack name
   in if isValidName (convIdentNaming conv) s
        then Nothing
        else
          Just $
            LayoutError
              { errCode = E005
              , errPath = ""
              , errKind = Unknown
              , errMessage = "Identifier '" <> name <> "' violates naming convention"
              , errExpected = Just $ T.pack $ suggestName (convIdentNaming conv) s
              }
