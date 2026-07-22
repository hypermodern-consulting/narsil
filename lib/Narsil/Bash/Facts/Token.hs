{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                         // bash // facts // token
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "One limb, a delicate probe or palp."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Leaf helpers over ShellCheck tokens: flatten a token to its text, decide
--   whether it was quoted, read a parameter-expansion or literal out of it, and
--   resolve a node's source 'Span' from the position map. No fact logic — just
--   the primitives every other Facts module reaches through.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Bash.Facts.Token (
  tokenToText,
  isQuotedToken,
  extractParamExpansion,
  extractLiteral,
  mkSpan,
)
where

import Control.Monad.Reader (Reader, asks)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Bash.Patterns
import Narsil.Bash.Types
import Narsil.Core.Span (Loc (..), Span (..))
import ShellCheck.AST qualified as SA
import ShellCheck.Interface (Position (..))

-- ── quoting detection ────────────────────────────────────────────

-- | determine if a token is quoted or unquoted (for config value semantics)
isQuotedToken :: SA.Token -> Quoted
isQuotedToken (SA.OuterToken _ (SA.Inner_T_DoubleQuoted _)) = Quoted
isQuotedToken
  (SA.OuterToken _ (SA.Inner_T_NormalWord [SA.OuterToken _ (SA.Inner_T_DoubleQuoted _)])) = Quoted
isQuotedToken _ = Unquoted

-- ── token → parameter expansion / literal ────────────────────────

-- | try to parse a token's text as a parameter expansion expression
extractParamExpansion :: SA.Token -> Maybe ParamExpansion
extractParamExpansion token =
  parseParamExpansion (tokenToText token)

-- | try to extract a literal value from a token
extractLiteral :: SA.Token -> Maybe Literal
extractLiteral token =
  let text = tokenToText token
   in if T.null text then Nothing else Just (parseLiteral text)

-- ── token → text conversion ──────────────────────────────────────

-- | convert a ShellCheck token to its text representation
tokenToText :: SA.Token -> Text
tokenToText (SA.OuterToken _ inner) = innerToText inner

-- | convert a ShellCheck inner token to text, recursing into child tokens
innerToText :: SA.InnerToken SA.Token -> Text
innerToText (SA.Inner_T_Literal content) = T.pack content
innerToText (SA.Inner_T_SingleQuoted content) = T.pack content
innerToText (SA.Inner_T_Glob content) = T.pack content
innerToText (SA.Inner_T_NormalWord parts) = T.concat (map tokenToText parts)
innerToText (SA.Inner_T_DoubleQuoted parts) = T.concat (map tokenToText parts)
innerToText (SA.Inner_T_DollarBraced _ token) = "${" <> tokenToText token <> "}"
innerToText (SA.Inner_T_DollarSingleQuoted content) = T.pack content
innerToText (SA.Inner_T_BraceExpansion parts) = T.concat (map tokenToText parts)
-- arithmetic-context tokens — used for array subscripts like `config[server]`
-- (the key parses as a TA_Variable inside a TA_Sequence). (REVIEW-3 #24)
innerToText (SA.Inner_TA_Variable name _) = T.pack name
innerToText (SA.Inner_TA_Sequence parts) = T.concat (map tokenToText parts)
innerToText _ = ""

-- ── span construction ────────────────────────────────────────────

-- | look up a ShellCheck node's position in the position map and produce a Span
mkSpan :: SA.Id -> Reader (Map SA.Id (Position, Position)) Span
mkSpan shellCheckId = asks (maybe noSpan toSpan . Map.lookup shellCheckId)
 where
  noSpan = Span (Loc 0 0) (Loc 0 0) Nothing
  -- n.b. ShellCheck positions are 1-based (per the Interface module);
  -- this matches megaparsec's positions so no adjustment is needed.
  toSpan (start, end) =
    Span
      (Loc (fromIntegral $ posLine start) (fromIntegral $ posColumn start))
      (Loc (fromIntegral $ posLine end) (fromIntegral $ posColumn end))
      (Just (posFile start))
