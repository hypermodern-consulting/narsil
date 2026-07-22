{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                             // Narsil.Lint.Derivation // lint
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He was good as new. How good was that?"
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                           // Nix // derivation quality lint rules
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Lint.Derivation (
  DerivViolationType (..),
  DerivViolation (..),
  findDerivViolations,
  formatDerivViolations,
  derivViolationDiagnostic,
  derivRuleId,
)
where

import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Katip (Severity (WarningS))
import Narsil.Core.Diagnostic (Diagnostic (..))
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Syntax.Annotation (srcSpanToSpan, varNameText, pattern Layer, pattern LayerAnn)
import Nix.Expr.Types
import Nix.Expr.Types.Annotated (NExprLoc, SrcSpan)

-- | Derivation-quality violation as a unified 'Diagnostic' (a warning).
derivViolationDiagnostic :: DerivViolation -> Diagnostic
derivViolationDiagnostic dv =
  Diagnostic
    { diagSeverity = WarningS
    , diagCode = if T.null code then Nothing else Just code
    , diagSpan = Just (dvSpan dv)
    , diagSummary = desc
    , diagHelp = lastLine (formatDerivNote (dvType dv))
    , diagSnippet = Nothing
    }
 where
  full = formatDerivErrorCode (dvType dv)
  (codePart, rest) = T.breakOn ": " full
  (code, desc) = if T.null rest then ("", full) else (codePart, T.drop 2 rest)
  lastLine note = take 1 (reverse (filter (not . T.null) (map T.strip (T.lines note))))

{- | a derivation-quality defect: a missing @meta@ attribute, or missing
@description@ within @meta@.
-}
data DerivViolationType
  = VMissingMeta
  | VMissingDescription
  deriving (Eq, Show)

-- | one derivation-quality violation: its kind, originating file, and source span.
data DerivViolation = DerivViolation
  { dvType :: !DerivViolationType
  , dvPath :: !FilePath
  , dvSpan :: !Span
  }
  deriving (Eq, Show)

-- ── entry point ────────────────────────────────────────────────────

-- | walk an expression, flagging @mkDerivation@ calls missing @meta@/@description@.
findDerivViolations :: FilePath -> NExprLoc -> [DerivViolation]
findDerivViolations = traverseDerivExpr

-- ── tree walk ──────────────────────────────────────────────────────
-- We need the file path threaded through for diagnostic messages, so
-- it's passed explicitly rather than captured in a closure.

traverseDerivExpr :: FilePath -> NExprLoc -> [DerivViolation]
traverseDerivExpr filePath (LayerAnn srcSpan (NApp func arg)) =
  checkDerivCall filePath srcSpan func arg
    ++ traverseDerivExpr filePath func
    ++ traverseDerivExpr filePath arg
traverseDerivExpr filePath (Layer (NSet _ bindings)) =
  concatMap (traverseDerivBinding filePath) bindings
traverseDerivExpr filePath (Layer (NLet bindings body)) =
  concatMap (traverseDerivBinding filePath) bindings
    ++ traverseDerivExpr filePath body
traverseDerivExpr filePath (Layer (NList xs)) = concatMap (traverseDerivExpr filePath) xs
traverseDerivExpr filePath (Layer (NIf c t f)) =
  traverseDerivExpr filePath c
    ++ traverseDerivExpr filePath t
    ++ traverseDerivExpr filePath f
traverseDerivExpr filePath (Layer (NAssert c b)) =
  traverseDerivExpr filePath c ++ traverseDerivExpr filePath b
traverseDerivExpr filePath (Layer (NAbs _ b)) = traverseDerivExpr filePath b
traverseDerivExpr filePath (Layer (NWith scope body)) =
  traverseDerivExpr filePath scope ++ traverseDerivExpr filePath body
traverseDerivExpr filePath (Layer (NSelect alt b _)) =
  maybe [] (traverseDerivExpr filePath) alt ++ traverseDerivExpr filePath b
traverseDerivExpr filePath (Layer (NHasAttr b _)) = traverseDerivExpr filePath b
traverseDerivExpr filePath (Layer (NUnary _ x)) = traverseDerivExpr filePath x
traverseDerivExpr filePath (Layer (NBinary _ x y)) =
  traverseDerivExpr filePath x ++ traverseDerivExpr filePath y
traverseDerivExpr _ _ = []

traverseDerivBinding :: FilePath -> Binding NExprLoc -> [DerivViolation]
traverseDerivBinding filePath (NamedVar _ expr _) = traverseDerivExpr filePath expr
traverseDerivBinding filePath (Inherit (Just scope) _ _) = traverseDerivExpr filePath scope
traverseDerivBinding _ (Inherit Nothing _ _) = []

-- ── mkDerivation inspection ────────────────────────────────────────
-- When we spot an `NApp` whose function is `mkDerivation` (or
-- `stdenv.mkDerivation`, etc.), we inspect the argument for required
-- metadata fields.

checkDerivCall :: FilePath -> SrcSpan -> NExprLoc -> NExprLoc -> [DerivViolation]
checkDerivCall filePath sourceSpan function argument
  | isMkDerivationCall function = checkDerivArg filePath sourceSpan argument
  | otherwise = []

-- ── mkDerivation call detection ────────────────────────────────────
-- Matches both bare `mkDerivation` and qualified `attrset.mkDerivation`.

isMkDerivationCall :: NExprLoc -> Bool
isMkDerivationCall (Layer (NSym name)) = varNameText name == "mkDerivation"
-- the FINAL key of the path is what's applied, so `a.b.c.mkDerivation` counts —
-- not just a single-key `x.mkDerivation` (REVIEW-3 #26)
isMkDerivationCall (Layer (NSelect _ _ path))
  | StaticKey key <- NE.last path = varNameText key == "mkDerivation"
  | otherwise = False
isMkDerivationCall _ = False

-- ── argument inspection ────────────────────────────────────────────
-- The argument to `mkDerivation` must be an attrset. If it's a
-- variable reference (NSym), we skip — the attrset might be defined
-- elsewhere and we can't verify statically.

checkDerivArg :: FilePath -> SrcSpan -> NExprLoc -> [DerivViolation]
checkDerivArg filePath sourceSpan (Layer (NSet _ bindings)) =
  checkDerivMeta filePath sourceSpan bindings
checkDerivArg _ _ (Layer (NSym _)) = []
checkDerivArg _ _ _ = []

-- ── meta-attribute validation ──────────────────────────────────────
-- Two checks in one pass:
--   1. Does the attrset have a `meta` binding at all?
--   2. If meta exists, does it contain a `description` key?
-- The meta value itself is also traversed for nested violations.

checkDerivMeta :: FilePath -> SrcSpan -> [Binding NExprLoc] -> [DerivViolation]
checkDerivMeta filePath sourceSpan bindings = checkFoundMeta
 where
  found = findMetaBinding bindings

  checkFoundMeta
    | Nothing <- found = [missingMetaViolation]
    | Just (NamedVar _ metaValue _) <- found =
        checkDerivDescription filePath metaValue ++ traverseDerivExpr filePath metaValue
    | otherwise = []

  missingMetaViolation =
    DerivViolation
      { dvType = VMissingMeta
      , dvPath = filePath
      , dvSpan = srcSpanToSpan sourceSpan
      }

-- ── description key check ──────────────────────────────────────────
-- If `meta` resolves to an attrset literal, verify it has a
-- `description` key. Dynamic or referenced meta values are skipped
-- (conservative — we only flag what we can prove).

checkDerivDescription :: FilePath -> NExprLoc -> [DerivViolation]
checkDerivDescription filePath metaValue
  | LayerAnn metaSpan (NSet _ metaBindings) <- metaValue
  , not (any isDescriptionBinding metaBindings) =
      [ DerivViolation
          { dvType = VMissingDescription
          , dvPath = filePath
          , dvSpan = srcSpanToSpan metaSpan
          }
      ]
  | otherwise = []

findMetaBinding :: [Binding NExprLoc] -> Maybe (Binding NExprLoc)
findMetaBinding = find isMetaBinding
 where
  isMetaBinding (NamedVar (StaticKey bindingName :| []) _ _) = varNameText bindingName == "meta"
  isMetaBinding _ = False

isDescriptionBinding :: Binding NExprLoc -> Bool
isDescriptionBinding (NamedVar (StaticKey bindingName :| []) _ _) =
  varNameText bindingName == "description"
isDescriptionBinding _ = False

-- | the stable short rule id string for a derivation violation type.
derivRuleId :: DerivViolationType -> Text
derivRuleId VMissingMeta = "missing-meta"
derivRuleId VMissingDescription = "missing-description"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // output formatting
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | render a list of derivation violations as human-readable text blocks.
formatDerivViolations :: [DerivViolation] -> Text
formatDerivViolations = T.unlines . map formatOneDerivViolation

formatOneDerivViolation :: DerivViolation -> Text
formatOneDerivViolation dv =
  T.unlines
    [ formatDerivLoc (dvSpan dv) <> ": " <> formatDerivErrorCode (dvType dv)
    , "  " <> formatDerivContext (dvType dv)
    , ""
    , formatDerivNote (dvType dv)
    ]

formatDerivLoc :: Span -> Text
formatDerivLoc span' = maybe loc (\f -> T.pack f <> ":" <> loc) (spanFile span')
 where
  loc = T.pack (show (locLine (spanStart span'))) <> ":" <> T.pack (show (locCol (spanStart span')))

formatDerivErrorCode :: DerivViolationType -> Text
formatDerivErrorCode VMissingMeta = "NARSIL-N013: missing `meta`"
formatDerivErrorCode VMissingDescription = "NARSIL-N014: missing `description` in meta"

formatDerivContext :: DerivViolationType -> Text
formatDerivContext VMissingMeta = "mkDerivation call without meta attribute"
formatDerivContext VMissingDescription = "meta = { ... } without description key"

formatDerivNote :: DerivViolationType -> Text
formatDerivNote VMissingMeta =
  T.unlines
    [ "  Derivations should include a `meta` attribute for package metadata."
    , ""
    , "  Add:  meta = with lib; { ... };"
    ]
formatDerivNote VMissingDescription =
  T.unlines
    [ "  The `meta` attribute should include a `description`."
    , ""
    , "  Add:  meta = with lib; { description = \"...\"; ... };"
    ]
