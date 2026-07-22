{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                             // lsp // handlers // semantic tokens
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Colors. Cyberspace, the way it used to be, before the ICE got clever."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Semantic-token highlighting: walk the AST, classify each leaf (keyword /
--   builtin function / variable / string / number), then delta-encode the tokens
--   into the LSP wire format. The 'semanticLegend' names the token types and
--   modifiers the encoding indexes into. Pure.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.LSP.Handlers.SemanticTokens (
  semanticLegend,
  semanticTokens,
)
where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Inference.Nix (TypeEnv (..), builtinEnv)
import Narsil.LSP.Handlers.Cursor (childExprs)
import Narsil.Syntax.Annotation (srcSpanToSpan, varNameText, pattern LayerAnn)
import Nix.Atoms (NAtom (..))
import Nix.Expr.Types (NExprF (..))
import Nix.Expr.Types.Annotated (NExprLoc)

{- | The token-type and modifier legend the encoded tokens index into; declared
  to the client at registration so it can map indices back to names.
-}
semanticLegend :: SemanticTokensLegend
semanticLegend =
  SemanticTokensLegend
    ["keyword", "function", "variable", "parameter", "type", "string", "number", "property"]
    ["definition", "readonly", "defaultLibrary"]

data RawToken = RawToken
  { rtLine :: Int
  , rtCol :: Int
  , rtLen :: Int
  , rtType :: SemanticTokenTypes
  , rtMods :: [SemanticTokenModifiers]
  }
  deriving (Eq, Show)

instance Ord RawToken where
  compare a b = compare (rtLine a, rtCol a) (rtLine b, rtCol b)

{- | Pure: classify every AST leaf and delta-encode the tokens into the LSP
  wire format, ordered by position. Token types per 'semanticLegend'.
-}
semanticTokens :: NExprLoc -> SemanticTokens
semanticTokens expr =
  let raw = collectTokens expr
      sorted = sort raw
      encoded = encToken sorted
   in SemanticTokens Nothing encoded

collectTokens :: NExprLoc -> [RawToken]
collectTokens = go
 where
  go (LayerAnn srcSpan e) =
    let sp = srcSpanToSpan srcSpan
        l = locLine (spanStart sp)
        c = locCol (spanStart sp)
        el = locLine (spanEnd sp)
        ec = locCol (spanEnd sp)
        len = max 1 (if l == el then ec - c else 0)
     in localToken e l c len ++ concatMap go (childExprs e)

  localToken (NSym name) l c len
    | varNameText name `elem` reservedWords = [RawToken l c len SemanticTokenTypes_Keyword []]
    | Map.member (varNameText name) (envBindings builtinEnv) =
        [RawToken l c len SemanticTokenTypes_Function [SemanticTokenModifiers_DefaultLibrary]]
    | otherwise = [RawToken l c len SemanticTokenTypes_Variable []]
  localToken (NStr _) l c len = [RawToken l c len SemanticTokenTypes_String []]
  localToken (NConstant (NInt _)) l c len = [RawToken l c len SemanticTokenTypes_Number []]
  localToken (NConstant (NFloat _)) l c len = [RawToken l c len SemanticTokenTypes_Number []]
  localToken (NConstant (NBool _)) l c len = [RawToken l c len SemanticTokenTypes_Keyword []]
  localToken (NConstant NNull) l c len = [RawToken l c len SemanticTokenTypes_Keyword []]
  localToken (NLiteralPath _) l c len = [RawToken l c len SemanticTokenTypes_String []]
  localToken (NEnvPath _) l c len = [RawToken l c len SemanticTokenTypes_String []]
  localToken _ _ _ _ = []

reservedWords :: [Text]
reservedWords = ["if", "then", "else", "let", "in", "with", "rec", "inherit", "assert", "import"]

encToken :: [RawToken] -> [UInt]
encToken tokens = go tokens (0, 0) []
 where
  go [] _ acc = reverse acc
  go (t : ts) (prevLine, prevCol) acc =
    let dLine = fromIntegral (rtLine t - prevLine)
        dCol =
          if rtLine t == prevLine
            then fromIntegral (rtCol t - prevCol)
            else fromIntegral (rtCol t)
        tIdx = fromIntegral (tokenTypeIndex (rtType t))
        bits = sum [modifierBit m | m <- rtMods t]
     in go
          ts
          (rtLine t, rtCol t)
          (acc ++ [dLine, dCol, fromIntegral (rtLen t), tIdx, fromIntegral bits])

tokenTypeIndex :: SemanticTokenTypes -> Int
tokenTypeIndex = idx . toEnumBaseType
 where
  idx "keyword" = 0
  idx "function" = 1
  idx "variable" = 2
  idx "parameter" = 3
  idx "type" = 4
  idx "string" = 5
  idx "number" = 6
  idx "property" = 7
  idx _ = 0

modifierBit :: SemanticTokenModifiers -> Int
modifierBit = bit . toEnumBaseType
 where
  bit "definition" = 1
  bit "readonly" = 2
  bit "defaultLibrary" = 4
  bit _ = 0
