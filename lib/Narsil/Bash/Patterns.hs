{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                               // bash // patterns
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He'd learned to value what little she did say, but he'd learned to
--    value what little she did say, and, always, she held him. And
--    listened."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                // bash // parsing
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Bash.Patterns (
  -- * Parameter expansion
  ParamExpansion (..),
  parseParamExpansion,

  -- * Config assignment
  ConfigAssignment (..),
  parseConfigAssignment,
  parseConfigValue,
  validConfigPath,

  -- * Literals
  parseLiteral,
  isNumericLiteral,
  isBoolLiteral,
  isStorePathSafe,
  safeParseInt,

  -- * Shell escaping
  escapeForParamExpansion,
  escapeForSingleQuoted,
  isSafeDefaultValue,
)
where

import Control.Monad (guard)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Bash.Types
import Text.Read (readMaybe)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Parameter Expansion
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | a parsed bash parameter expansion form (@${var:-default}@, @${var:?err}@, plain @$var@, …).
data ParamExpansion
  = DefaultValue Text (Maybe Text)
  | AssignDefault Text (Maybe Text)
  | ErrorIfUnset Text (Maybe Text)
  | UseAlternate Text (Maybe Text)
  | SimpleRef Text
  deriving (Eq, Show)

-- ── top-level dispatch: ${...} or $VAR ───────────────────────────

-- | parse ${expr} or $VAR into a ParamExpansion
parseParamExpansion :: Text -> Maybe ParamExpansion
parseParamExpansion text
  | "${" `T.isPrefixOf` text && "}" `T.isSuffixOf` text =
      parseExpansionBody (T.dropEnd 1 (T.drop 2 text))
  | "$" `T.isPrefixOf` text =
      Just (SimpleRef (T.drop 1 text))
  | otherwise = Nothing

-- ── ${body} parser: with or without : modifier ───────────────────

{- | parse the inner body of ${...} into the specific expansion form
dispatches based on presence of : separator (e.g. ${var:-default} vs ${var-default})
-}
parseExpansionBody :: Text -> Maybe ParamExpansion
parseExpansionBody body
  | (variable, remaining) <- T.breakOn ":" body
  , ":" `T.isPrefixOf` remaining =
      -- \${var:-word}, ${var:=word}, ${var:?word}, ${var:+word}
      parseOpWithColon variable (T.drop 1 remaining)
  | otherwise =
      -- \${var-word}, ${var=word}, ${var?word}, ${var+word}
      parseOpWithoutColon body
 where
  -- ── : variants (colon prefix) ──
  -- these use ":" before the operator character. (${var:} is just $var.)
  parseOpWithColon variable remaining = do
    guard (isVarName variable)
    colonOp (T.uncons remaining)
   where
    colonOp (Just ('-', defaultValue)) = Just (DefaultValue variable (Just defaultValue))
    colonOp (Just ('=', defaultValue)) = Just (AssignDefault variable (Just defaultValue))
    colonOp (Just ('?', message)) = Just (ErrorIfUnset variable (nonEmpty message))
    colonOp (Just ('+', alternate)) = Just (UseAlternate variable (nonEmpty alternate))
    colonOp _ = Just (SimpleRef variable)

  -- ── non-: variants (operator immediately after var name) ──
  parseOpWithoutColon expansionBody = do
    let (variable, remaining) = T.break isOpChar expansionBody
    guard (isVarName variable)
    plainOp variable (T.uncons remaining)
   where
    plainOp variable (Just ('-', defaultValue)) = Just (DefaultValue variable (Just defaultValue))
    plainOp variable (Just ('=', defaultValue)) = Just (AssignDefault variable (Just defaultValue))
    plainOp variable (Just ('?', message)) = Just (ErrorIfUnset variable (nonEmpty message))
    plainOp variable (Just ('+', alternate)) = Just (UseAlternate variable (nonEmpty alternate))
    plainOp variable Nothing = Just (SimpleRef variable) -- plain ${var}
    plainOp _ _ = Nothing

  -- ── helpers ──
  nonEmpty text = if T.null text then Nothing else Just text

  isOpChar character = character == '-' || character == '=' || character == '?' || character == '+'

  isVarName text =
    not (T.null text)
      && isValidStart text
      && T.all isVarChar text

  isValidStart text =
    maybe False (\(character, _) -> isAsciiAlpha character || character == '_') (T.uncons text)

  isVarChar character = isAsciiAlphaNum character || character == '_'

  isAsciiAlpha character =
    (character >= 'A' && character <= 'Z') || (character >= 'a' && character <= 'z')
  isAsciiAlphaNum character = isAsciiAlpha character || (character >= '0' && character <= '9')

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Config Assignment
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | a parsed @config.*@ assignment: the key path, its value (variable or literal), and quoting.
data ConfigAssignment = ConfigAssignment
  { configPath :: [Text]
  , configValue :: Either Text Literal
  , configQuoted :: Quoted
  }
  deriving (Eq, Show)

-- ── config assignment dispatching ────────────────────────────────

-- | try config[...]= then config.= syntax, in that order
parseConfigAssignment :: Text -> Maybe ConfigAssignment
parseConfigAssignment line =
  parseConfigArraySyntax line <|> parseConfigDotSyntax line
 where
  (<|>) Nothing b = b
  (<|>) a _ = a

-- ── config[...]=value syntax ─────────────────────────────────────

-- | parse config[path.to.key]=value
parseConfigArraySyntax :: Text -> Maybe ConfigAssignment
parseConfigArraySyntax line = do
  let (leftHandSide, rest) = T.breakOn "=" line
  inner <- T.stripPrefix "config[" leftHandSide
  guard ("]" `T.isSuffixOf` inner)
  let pathText = T.dropEnd 1 inner
  guard (not (T.null rest))
  let rightHandSide = T.drop 1 rest
  let pathParts = T.splitOn "." pathText
  guard (validConfigPath pathParts)
  (value, quoted) <- parseConfigValue rightHandSide
  Just
    ConfigAssignment
      { configPath = pathParts
      , configValue = value
      , configQuoted = quoted
      }

-- ── config.=value syntax ────────────────────────────────────────

-- | parse config.path.to.key=value
parseConfigDotSyntax :: Text -> Maybe ConfigAssignment
parseConfigDotSyntax line = do
  let (leftHandSide, rest) = T.breakOn "=" line
  path <- T.stripPrefix "config." leftHandSide
  guard (not (T.null rest))
  let rightHandSide = T.drop 1 rest
  let pathParts = T.splitOn "." path
  guard (validConfigPath pathParts)
  (value, quoted) <- parseConfigValue rightHandSide
  Just
    ConfigAssignment
      { configPath = pathParts
      , configValue = value
      , configQuoted = quoted
      }

-- ── config path validation ───────────────────────────────────────

-- | a valid config path is non-empty, each segment is non-empty and alpha-num-safe
validConfigPath :: [Text] -> Bool
validConfigPath parts =
  not (null parts) && all (\part -> not (T.null part) && T.all isSafeConfigChar part) parts

isSafeConfigChar :: Char -> Bool
isSafeConfigChar character =
  character == '_'
    || character == '-'
    || (character >= 'A' && character <= 'Z')
    || (character >= 'a' && character <= 'z')
    || (character >= '0' && character <= '9')

-- ── value side parser ────────────────────────────────────────────

{- | parse the RHS of a config assignment into (variable-name or literal, quoting)
handles quoted "${...}", "$VAR", plain strings, and ${...} expansions
-}

-- | valid bash variable name: @^[A-Za-z_][A-Za-z0-9_]*$@ (ASCII only)
isValidVarName :: Text -> Bool
isValidVarName text = maybe False valid (T.uncons text)
 where
  valid (c, rest) = startChar c && T.all varChar rest
  startChar c = isAsciiUpper c || isAsciiLower c || c == '_'
  varChar c = startChar c || isDigit c

-- | parse a config RHS into (variable name or literal, quoting); handles @"${…}"@, @"$VAR"@, plain.
parseConfigValue :: Text -> Maybe (Either Text Literal, Quoted)
parseConfigValue text
  | "\"${" `T.isPrefixOf` text && "\"" `T.isSuffixOf` text =
      let inner = T.dropEnd 1 (T.drop 1 text)
       in if "${" `T.isPrefixOf` inner && "}" `T.isSuffixOf` inner
            then
              Just
                ( maybe (Right (parseLiteralValue inner)) leftVar (parseParamExpansion inner)
                , Quoted
                )
            else Just (Right (parseLiteralValue inner), Quoted)
  | "\"$" `T.isPrefixOf` text && "\"" `T.isSuffixOf` text =
      -- "$VAR" — only a var ref if VAR is a valid name (#21/#22: `"$|"` must
      -- NOT be extracted as a variable). Otherwise it's a quoted literal.
      let varName = T.dropEnd 1 (T.drop 2 text)
       in if isValidVarName varName
            then Just (Left varName, Quoted)
            else Just (Right (LitString (T.dropEnd 1 (T.drop 1 text))), Quoted)
  | "\"" `T.isPrefixOf` text && "\"" `T.isSuffixOf` text =
      Just (Right (LitString (T.dropEnd 1 (T.drop 1 text))), Quoted)
  | "${" `T.isPrefixOf` text && "}" `T.isSuffixOf` text =
      Just (maybe (Right (parseLiteralValue text)) leftVar (parseParamExpansion text), Unquoted)
  | "$" `T.isPrefixOf` text =
      -- \$VAR — validate the name; a non-name (`$|`, `$\n; id`, …) is a literal
      let varName = T.drop 1 text
       in if isValidVarName varName
            then Just (Left varName, Unquoted)
            else Just (Right (parseLiteralValue text), Unquoted)
  | otherwise =
      Just (Right (parseLiteralValue text), Unquoted)
 where
  -- extract variable name from any expansion form (we only care about the name)
  leftVar (SimpleRef variable) = Left variable
  leftVar (DefaultValue variable _) = Left variable
  leftVar (AssignDefault variable _) = Left variable
  leftVar (ErrorIfUnset variable _) = Left variable
  leftVar (UseAlternate variable _) = Left variable

-- | naive literal parsing: bool, int, or string
parseLiteralValue :: Text -> Literal
parseLiteralValue text
  | text == "true" = LitBool True
  | text == "false" = LitBool False
  | isNumericLiteral text = maybe (LitString text) LitInt (safeParseInt text)
  | otherwise = LitString text

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Literals
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- ── full literal parsing (with store path detection) ──────────────

-- | parse a text literal: bool, int, store path, or plain string
parseLiteral :: Text -> Literal
parseLiteral text
  | text == "true" = LitBool True
  | text == "false" = LitBool False
  | isNumericLiteral text = maybe (LitString text) LitInt (safeParseInt text)
  | isStorePathSafe text = LitPath (StorePath text)
  | otherwise = LitString text

-- ── store path validation ────────────────────────────────────────

-- | safe store path: starts with /nix/store/, no .. or // traversal
isStorePathSafe :: Text -> Bool
isStorePathSafe text =
  "/nix/store/" `T.isPrefixOf` text
    && not (".." `T.isInfixOf` text)
    && not ("//" `T.isInfixOf` text)

-- ── numeric literal validation ───────────────────────────────────

{- | is this text a valid Int64 literal?
n.b. we check digit count to reject values wider than Int64
-}
isNumericLiteral :: Text -> Bool
isNumericLiteral text =
  not (T.null text)
    && T.all isDigitOrSign text
    && T.any isDigit text
    && validMinus text
    && validLength text
    && fitsInt64 text
 where
  isDigitOrSign character = isDigit character || character == '-'
  validMinus sourceText
    | Just ('-', remaining) <- T.uncons sourceText =
        not (T.null remaining) && not (T.any (== '-') remaining)
    | otherwise = not (T.any (== '-') sourceText)
  validLength sourceText = T.length (T.dropWhile (== '-') sourceText) <= 19
  fitsInt64 sourceText =
    maybe False inRange (readMaybe (T.unpack sourceText) :: Maybe Integer)
   where
    inRange parsedInteger =
      parsedInteger >= -9223372036854775808 && parsedInteger <= 9223372036854775807

-- | parse text as Int, returning Nothing if out of range
safeParseInt :: Text -> Maybe Int
safeParseInt text
  | Just parsedInteger <- (readMaybe (T.unpack text) :: Maybe Integer)
  , parsedInteger >= fromIntegral (minBound :: Int)
  , parsedInteger <= fromIntegral (maxBound :: Int) =
      Just (fromInteger parsedInteger)
  | otherwise = Nothing

-- | is this text exactly "true" or "false"?
isBoolLiteral :: Text -> Bool
isBoolLiteral text = text == "true" || text == "false"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Shell escaping (closes C1 from review-2)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{- | Escape text for safe inclusion inside @${VAR:-DEFAULT}@ or @${VAR:+ALT}@
in a double-quoted bash context. We backslash-escape every character bash would
otherwise interpret. Crucially:
  * @$@ → @\\\$@   (prevents parameter / command substitution)
  * @\`@ → @\\\`@  (prevents legacy command substitution)
  * @\\@ → @\\\\@  (preserves literal backslashes)
  * @\"@ → @\\\"@  (prevents premature termination of the surrounding @\"…\"@)
  * @\}@ → @\\}@   (prevents premature termination of the @${…}@ expansion)
After escaping, bash treats the value as a verbatim string. See review-2 C1 for
the attack vectors this blocks (e.g. @${UNSET:-$(touch /tmp/pwn)}@).
-}
escapeForParamExpansion :: Text -> Text
escapeForParamExpansion = T.concatMap escape
 where
  escape '$' = "\\$"
  escape '`' = "\\`"
  escape '\\' = "\\\\"
  escape '"' = "\\\""
  escape '}' = "\\}"
  escape '\n' = " "
  escape '\r' = " "
  escape c = T.singleton c

{- | Escape text for safe inclusion inside a bash single-quoted string.
The standard idiom for embedding @'@ inside @'…'@ is @'\\''@: close the string,
emit a backslash-quote, reopen. Defensive secondary layer to 'escapeForParamExpansion'.
-}
escapeForSingleQuoted :: Text -> Text
escapeForSingleQuoted = T.replace "'" "'\\''"

{- | Predicate: is this text safe to embed in 'ConfigVarDefault' / 'ConfigVarAlternate'
without escaping? Safe characters are alphanumerics, dash, underscore, dot,
colon, slash, and space. Used by tests to assert escape coverage.
-}
isSafeDefaultValue :: Text -> Bool
isSafeDefaultValue = T.all isSafeChar
 where
  isSafeChar c =
    (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c == '-'
      || c == '_'
      || c == '.'
      || c == ':'
      || c == '/'
      || c == ' '
