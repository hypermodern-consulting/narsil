-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                               // Narsil.Lint.Patterns // lint
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "There was some magic chemistry in that impending darkness, something that
--    let him glimpse the infinite desirability of that room"
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                // Nix // pattern-based lint rules
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Narsil.Lint.Patterns (
  PatternViolationType (..),
  PatternViolation (..),
  findPatternViolations,
  formatPatternViolations,
  patternViolationDiagnostic,
)
where

import Data.List.NonEmpty (toList)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Katip (Severity (WarningS))
import Narsil.Core.Diagnostic (Diagnostic (..))
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Syntax.Annotation (srcSpanToSpan, varNameText, pattern Layer, pattern LayerAnn)
import Nix.Atoms (NAtom (..))
import Nix.Expr.Types
import Nix.Expr.Types.Annotated

-- | Pattern (heuristic) violation as a unified 'Diagnostic' (a warning).
patternViolationDiagnostic :: PatternViolation -> Diagnostic
patternViolationDiagnostic pv =
  Diagnostic
    { diagSeverity = WarningS
    , diagCode = if T.null code then Nothing else Just code
    , diagSpan = Just (pvSpan pv)
    , diagSummary = desc
    , diagHelp = lastLine (formatPatternNote (pvType pv))
    , diagSnippet = Nothing
    }
 where
  full = formatPatternErrorCode (pvType pv)
  (codePart, rest) = T.breakOn ": " full
  (code, desc) = if T.null rest then ("", full) else (codePart, T.drop 2 rest)
  lastLine note = take 1 (reverse (filter (not . T.null) (map T.strip (T.lines note))))

{- | a heuristic pattern smell: an @or null@ attribute fallback, or an
attribute-translation call (@translateAttrs@/@mapAttrsToList@/…).
-}
data PatternViolationType
  = VOrNullFallback
  | VAttrTranslation
  deriving (Eq, Show)

-- | one pattern violation: its kind, source span, and a short context label.
data PatternViolation = PatternViolation
  { pvType :: !PatternViolationType
  , pvSpan :: !Span
  , pvContext :: !Text
  }
  deriving (Eq, Show)

-- ── entry point ────────────────────────────────────────────────────

-- | walk an expression and collect every pattern violation.
findPatternViolations :: NExprLoc -> [PatternViolation]
findPatternViolations = traversePatternExpr

-- ── tree walk ──────────────────────────────────────────────────────
-- Local-prioritized traversal: each node's pattern violations are
-- emitted before sub-expression violations.

traversePatternExpr :: NExprLoc -> [PatternViolation]
traversePatternExpr (LayerAnn srcSpan expression) =
  localPatternViolations srcSpan expression
    ++ concatMap traversePatternExpr (patternSubExprs expression)

-- ── local node checks ──────────────────────────────────────────────
-- Two pattern rules fire at a single AST node:
--   1. `or null` fallback on attribute selection
--   2. Translation function calls (translateAttrs, mapAttrsToList, etc.)

localPatternViolations :: SrcSpan -> NExprF NExprLoc -> [PatternViolation]
localPatternViolations sourceSpan (NSelect (Just defaultExpr) base path)
  | isNullExpr defaultExpr =
      [ PatternViolation
          { pvType = VOrNullFallback
          , pvSpan = srcSpanToSpan sourceSpan
          , pvContext = fmtSelect base path
          }
      ]
localPatternViolations sourceSpan (NApp function _)
  | isTranslateCall function =
      [ PatternViolation
          { pvType = VAttrTranslation
          , pvSpan = srcSpanToSpan sourceSpan
          , pvContext = fmtCall function
          }
      ]
localPatternViolations _ _ = []

-- ── null-expression detection ──────────────────────────────────────
-- n.b. `NSym "null"` is included because the parser may or may not
-- resolve `null` to `NConstant NNull` depending on context.
-- !? is there a case where the binder shadows `null`? That would be
-- pathological but technically possible.

isNullExpr :: NExprLoc -> Bool
isNullExpr (Layer (NConstant NNull)) = True
isNullExpr (Layer (NSym name)) = varNameText name == "null"
isNullExpr _ = False

-- ── translation-function detection ─────────────────────────────────
-- Matches bare calls (`translateAttrs ...`) and qualified calls
-- (`lib.translateAttrs ...`). Only the final key is checked.

isTranslateCall :: NExprLoc -> Bool
isTranslateCall (Layer (NSym name)) = varNameText name `elem` translateFuncNames
isTranslateCall (Layer (NSelect _ _ path))
  | StaticKey k <- NE.last path = varNameText k `elem` translateFuncNames
  | otherwise = False
isTranslateCall _ = False

translateFuncNames :: [Text]
translateFuncNames = ["translateAttrs", "mapAttrsToList", "mapAttrsFlatten"]

-- ── formatting helpers ─────────────────────────────────────────────
-- Produce human-readable summaries of the offending expression for
-- embedding in the violation context.

fmtSelect :: NExprLoc -> NE.NonEmpty (NKeyName NExprLoc) -> Text
fmtSelect base path =
  prettyShort base <> "." <> attrPathText (toList path) <> " or null"

attrPathText :: [NKeyName NExprLoc] -> Text
attrPathText [StaticKey k] = varNameText k
attrPathText (StaticKey k : ks) = varNameText k <> "." <> attrPathText ks
attrPathText (_ : ks) = "‥." <> attrPathText ks
attrPathText [] = ""

fmtCall :: NExprLoc -> Text
fmtCall (Layer (NSym name)) = varNameText name <> " call"
fmtCall (Layer (NSelect _ _ path))
  | StaticKey k <- NE.last path = varNameText k <> " call"
  | otherwise = "translateAttrs call"
fmtCall _ = "translateAttrs call"

prettyShort :: NExprLoc -> Text
prettyShort (Layer (NSym name)) = varNameText name
prettyShort (Layer (NSelect _ b path)) =
  maybe "‥" (\k -> prettyShort b <> "." <> k) (lastStaticKey path)
prettyShort _ = "‥"

lastStaticKey :: NE.NonEmpty (NKeyName NExprLoc) -> Maybe Text
lastStaticKey path
  | StaticKey k <- NE.last path = Just (varNameText k)
  | otherwise = Nothing

-- ── sub-expression enumeration ─────────────────────────────────────
-- Maps each NExpr constructor to its list of child expressions that
-- need recursive linting. This is the traversal "shape" — every node
-- type must be listed or it won't be visited.

patternSubExprs :: NExprF NExprLoc -> [NExprLoc]
patternSubExprs (NList xs) = xs
patternSubExprs (NSet _ bindings) = concatMap patternBindingExprs bindings
patternSubExprs (NLet bindings body) = body : concatMap patternBindingExprs bindings
patternSubExprs (NIf c t f) = [c, t, f]
patternSubExprs (NWith s b) = [s, b]
patternSubExprs (NAssert c b) = [c, b]
patternSubExprs (NAbs _ b) = [b]
patternSubExprs (NApp f x) = [f, x]
patternSubExprs (NSelect mDef b path) =
  b : maybeToList mDef ++ [e | DynamicKey (Antiquoted e) <- toList path]
patternSubExprs (NHasAttr b path) = b : [e | DynamicKey (Antiquoted e) <- toList path]
patternSubExprs (NUnary _ x) = [x]
patternSubExprs (NBinary _ x y) = [x, y]
-- leaves (constants, strings, symbols, paths, holes): no sub-expressions
patternSubExprs _ = []

patternBindingExprs :: Binding NExprLoc -> [NExprLoc]
patternBindingExprs (NamedVar _ expr _) = [expr]
patternBindingExprs (Inherit (Just scope) _ _) = [scope]
patternBindingExprs (Inherit Nothing _ _) = []

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // output formatting
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | render a list of pattern violations as human-readable text blocks.
formatPatternViolations :: [PatternViolation] -> Text
formatPatternViolations = T.unlines . map formatOnePatternViolation

formatOnePatternViolation :: PatternViolation -> Text
formatOnePatternViolation pv =
  T.unlines
    [ formatPatternLoc (pvSpan pv) <> ": " <> formatPatternErrorCode (pvType pv)
    , "  " <> pvContext pv
    , ""
    , formatPatternNote (pvType pv)
    ]

formatPatternLoc :: Span -> Text
formatPatternLoc span' = maybe loc (\f -> T.pack f <> ":" <> loc) (spanFile span')
 where
  loc = T.pack (show (locLine (spanStart span'))) <> ":" <> T.pack (show (locCol (spanStart span')))

formatPatternErrorCode :: PatternViolationType -> Text
formatPatternErrorCode VOrNullFallback = "ALEPH-N009: `or null` fallback"
formatPatternErrorCode VAttrTranslation = "ALEPH-N010: attribute translation call"

formatPatternNote :: PatternViolationType -> Text
formatPatternNote VOrNullFallback =
  T.unlines
    [ "  Implicit `or null` fallbacks silently swallow attribute errors."
    , "  This can mask real bugs when expected fields are missing."
    , ""
    , "  Instead, use the attribute dot operator @. to surface"
    , "  type-checkable errors, or use explicit null checks."
    , ""
    , "  Before:  x.y or null"
    , "  After:   if x ? y then x.y else null"
    ]
formatPatternNote VAttrTranslation =
  T.unlines
    [ "  Attribute translation functions should only be used in prelude files."
    , "  translateAttrs/mapAttrsToList circumvents the type system and"
    , "  should be centralized in the designated prelude directory."
    , ""
    , "  Move translation logic to lib/prelude/ or use"
    , "  known attribute sets instead."
    ]
