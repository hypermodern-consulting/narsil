{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                         // bash // facts // value
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Some articulated structure shifting to accommodate her course through
--    the city."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The config-value sublanguage: pull a plain variable name out of a
--   reference, and parse the right-hand side of a @config.*@ assignment —
--   from either a ShellCheck token stream or raw text — into a literal, a
--   single variable, or a mixed template ('ConfigValueDynamic'), then project
--   that to the corresponding 'Fact'. Pure; sits above
--   "Narsil.Bash.Facts.Token".
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Bash.Facts.Value (
  extractVarRef,
  ConfigValueDynamic,
  configValueFact,
  selectValueParser,
  parseConfigValueDynamic,
  parseConfigTemplate,
)
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Bash.Facts.Token (tokenToText)
import Narsil.Bash.Patterns
import Narsil.Bash.Types
import Narsil.Core.Span (Span)
import ShellCheck.AST qualified as SA

-- ── variable reference extraction ────────────────────────────────

{- | extract a plain variable name from ${VAR}, $VAR, or just VAR
n.b. rejects $(...) command substitutions and empty strings
-}
extractSimpleVar :: Text -> Maybe Text
extractSimpleVar text
  | "${" `T.isPrefixOf` text && "}" `T.isSuffixOf` text =
      let name = T.dropEnd 1 (T.drop 2 text)
       in if isValidName name then Just name else Nothing
  | "$" `T.isPrefixOf` text
      && not ("$(" `T.isPrefixOf` text)
      && not ("${" `T.isPrefixOf` text) =
      let name = T.drop 1 text
       in if isValidName name then Just name else Nothing
  | isValidName text =
      Just text
  | otherwise =
      Nothing
 where
  isValidName name =
    not (T.null name)
      && T.all isVarChar name
      && not (isNumericLiteral name)
      && not (isBoolLiteral name)
  isVarChar character =
    character == '_'
      || isAsciiUpper character
      || isAsciiLower character
      || isDigit character

-- | extract a variable reference that starts with $ (either $VAR or ${VAR})
extractVarRef :: Text -> Maybe Text
extractVarRef text
  | "${" `T.isPrefixOf` text && "}" `T.isSuffixOf` text = extractSimpleVar text
  | "$" `T.isPrefixOf` text = extractSimpleVar text
  | otherwise = Nothing

-- ── config value dynamic representation ──────────────────────────

-- | a parsed config RHS: a single variable, a plain literal, or a mixed text/var template.
data ConfigValueDynamic
  = -- | single variable reference
    CVDVar Text
  | -- | plain literal
    CVDLit Literal
  | -- | template with mixed text/vars
    CVDTemplate [ConfigPart]

-- | convert a dynamic value to the corresponding Fact constructor
configValueFact :: ConfigPath -> Quoted -> Span -> ConfigValueDynamic -> Fact
configValueFact configPath quoted sourceSpan (CVDVar variable) =
  ConfigAssign configPath variable quoted sourceSpan
configValueFact configPath _quoted sourceSpan (CVDLit literal) =
  ConfigLit configPath literal sourceSpan
configValueFact configPath quoted sourceSpan (CVDTemplate templateParts) =
  ConfigTemplate configPath templateParts quoted sourceSpan

-- ── value parser selection ───────────────────────────────────────

{- | choose the appropriate value parser based on token structure
empty token list → text fallback; non-empty → try template / var / dynamic
-}
selectValueParser :: [SA.Token] -> Text -> Quoted -> Maybe ConfigValueDynamic
selectValueParser [] rhsText quoted =
  parseConfigValueDynamic rhsText quoted
selectValueParser tokens _ quoted = classify (parseConfigTemplateTokens tokens)
 where
  classify (Just [ConfigVar variable]) = Just (CVDVar variable)
  classify (Just templateParts) = Just (CVDTemplate templateParts)
  classify Nothing = parseConfigValueDynamic (T.concat (map tokenToText tokens)) quoted

-- ── dynamic text-level parser ────────────────────────────────────

-- | parse a config value from raw text (fallback when token parser fails)
parseConfigValueDynamic :: Text -> Quoted -> Maybe ConfigValueDynamic
parseConfigValueDynamic rawText _quoted
  | T.null strippedText = Nothing
  | otherwise = classify (parseConfigTemplate strippedText)
 where
  classify (Just [ConfigVar variable]) = Just (CVDVar variable)
  classify (Just templateParts) = Just (CVDTemplate templateParts)
  classify Nothing = Just (CVDLit (parseLiteral strippedText))
  strippedText
    | "\"" `T.isPrefixOf` rawText && "\"" `T.isSuffixOf` rawText = T.dropEnd 1 (T.drop 1 rawText)
    | otherwise = rawText

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- token → config template
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- -- token sequence → config parts -- --
-- ShellCheck tokenizes `"$A-$B"` as a sequence of literal+var tokens.
-- We reconstruct the template structure from that token stream.

parseConfigTemplateTokens :: [SA.Token] -> Maybe [ConfigPart]
parseConfigTemplateTokens tokens =
  let parts = mergeTextParts (concatMap tokenParts tokens)
   in if any isVarPart parts then Just parts else Nothing
 where
  -- ── classify: if any part is a variable, it's a template ──
  isVarPart (ConfigText _) = False
  isVarPart _ = True

  -- ── token → [ConfigPart] ──
  tokenParts (SA.OuterToken _ innerToken) = innerParts innerToken

  -- ── inner token → flat part list ──
  -- n.b. Literal, SingleQuoted, Glob all become ConfigText
  -- DollarBraced tries expansionPart first
  innerParts (SA.Inner_T_Literal content) = [ConfigText (T.pack content)]
  innerParts (SA.Inner_T_SingleQuoted content) = [ConfigText (T.pack content)]
  innerParts (SA.Inner_T_Glob content) = [ConfigText (T.pack content)]
  innerParts (SA.Inner_T_NormalWord subParts) = concatMap tokenParts subParts
  innerParts (SA.Inner_T_DoubleQuoted subParts) = concatMap tokenParts subParts
  innerParts (SA.Inner_T_DollarBraced _ body) = expansionPart ("${" <> tokenToText body <> "}")
  innerParts _ = []

  -- ── ${...} → ConfigVar / ConfigVarDefault / ConfigVarRequired ──
  expansionPart text = classify (parseParamExpansion text)
   where
    classify (Just (SimpleRef variable)) = [ConfigVar variable]
    classify (Just (DefaultValue variable defaultValue)) =
      [ConfigVarDefault variable (fromMaybe "" defaultValue)]
    classify (Just (AssignDefault variable defaultValue)) =
      [ConfigVarDefault variable (fromMaybe "" defaultValue)]
    classify (Just (ErrorIfUnset variable _)) = [ConfigVarRequired variable]
    classify (Just (UseAlternate variable alternate)) =
      [ConfigVarAlternate variable (fromMaybe "" alternate)]
    classify Nothing = [ConfigText text]

  -- ── merge adjacent ConfigText parts ──
  mergeTextParts = foldr step []
   where
    step (ConfigText a) (ConfigText b : xs) = ConfigText (a <> b) : xs
    step part xs = part : xs

-- -- text → config parts -- --
-- Parses raw text like "$A-${B:-default}" into
-- [ConfigVar "A", ConfigText "-", ConfigVarDefault "B" "default"]
-- n.b. this is the text-level fallback when token-level parsing didn't apply

parseConfigTemplate :: Text -> Maybe [ConfigPart]
parseConfigTemplate sourceText =
  let parts = parseParts sourceText
   in if any isVarPart parts then Just (mergeTextParts parts) else Nothing
 where
  -- any part that carries a variable counts — not just bare $VAR. Without the
  -- default/required/alternate cases, a template built entirely of
  -- `${VAR:-default}` parts was misclassified as a plain literal. (REVIEW-3 #24)
  isVarPart (ConfigVar _) = True
  isVarPart (ConfigVarDefault _ _) = True
  isVarPart (ConfigVarRequired _) = True
  isVarPart (ConfigVarAlternate _ _) = True
  isVarPart (ConfigText _) = False

  -- ── main parser: dispatch on first character ──
  parseParts remainingText
    | T.null remainingText = []
    | "${" `T.isPrefixOf` remainingText =
        -- \${...} expansion: extract name, try param expansion, fallback to text
        parseDollarBrace remainingText
    | "$" `T.isPrefixOf` remainingText =
        -- \$VAR simple variable: grab identifier chars
        parseDollarVar remainingText
    | otherwise =
        -- plain text: scan forward to the next $
        splitText remainingText

  -- ── ${...} handler ──
  -- extract the name between ${ and }, then try each expansion form
  parseDollarBrace text =
    let textAfterDollarBrace = T.drop 2 text
        (name, textAfterName) = T.breakOn "}" textAfterDollarBrace
        rest = parseParts (T.drop 1 textAfterName)
        classify (Just (SimpleRef variable)) = ConfigVar variable : rest
        classify (Just (DefaultValue variable defaultValue)) =
          ConfigVarDefault variable (fromMaybe "" defaultValue) : rest
        classify (Just (AssignDefault variable defaultValue)) =
          ConfigVarDefault variable (fromMaybe "" defaultValue) : rest
        classify (Just (ErrorIfUnset variable _)) = ConfigVarRequired variable : rest
        classify (Just (UseAlternate variable alternate)) =
          ConfigVarAlternate variable (fromMaybe "" alternate) : rest
        classify Nothing = splitText text
     in if "}" `T.isPrefixOf` textAfterName
          then classify (parseParamExpansion ("${" <> name <> "}"))
          else splitText text

  -- ── $VAR handler ──
  parseDollarVar text =
    let textAfterDollar = T.drop 1 text
        (name, textAfterName) = T.span isVarChar textAfterDollar
     in if isVarName name
          then ConfigVar name : parseParts textAfterName
          else splitText text

  -- ── text chunk: find the next $, emit as ConfigText ──
  splitText text =
    let (textBefore, textAfter) = T.breakOn "$" text
     in if T.null textBefore
          then ConfigText (T.take 1 textAfter) : parseParts (T.drop 1 textAfter)
          else ConfigText textBefore : parseParts textAfter

  -- ── identifier validation ──
  isVarName name =
    not (T.null name)
      && not (isNumericLiteral name)
      && not (isBoolLiteral name)
      && T.all isVarChar name

  isVarChar character =
    character == '_'
      || isAsciiUpper character
      || isAsciiLower character
      || isDigit character

  -- ── merge adjacent ConfigText parts (post-processing) ──
  mergeTextParts = foldr step []
   where
    step (ConfigText a) (ConfigText b : xs) = ConfigText (a <> b) : xs
    step part xs = part : xs
