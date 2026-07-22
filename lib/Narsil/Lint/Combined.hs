{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                               // Narsil.Lint.Combined // walk
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He touched the jaws and cheekbones, and it was like she was standing
--    right there, the actual woman, behind the face like a wall."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                               // Nix // single-pass combined lint
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Lint.Combined (
  LintBundle (..),
  LintResult (..),
  emptyBundle,
  combinedLint,
  combinedLintSafe,
)
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Safety qualified as Safety
import Narsil.Lint.Derivation (
  DerivViolation (..),
  DerivViolationType (VMissingDescription, VMissingMeta),
 )
import Narsil.Lint.Nix (
  NixViolation (..),
  ViolationType (
    VLongInlineString,
    VRawMkDerivation,
    VRawRunCommand,
    VRawWriteShellApplication,
    VRec,
    VSubstituteAll,
    VWith,
    VWriteShellScript
  ),
  checkLetBindingName,
 )
import Narsil.Lint.Patterns (
  PatternViolation (..),
  PatternViolationType (VAttrTranslation, VOrNullFallback),
 )
import Narsil.Syntax.Annotation (srcSpanToSpan, varNameText, pattern Layer, pattern LayerAnn)
import Nix.Atoms (NAtom (..))
import Nix.Expr.Types
import Nix.Expr.Types.Annotated

{- | the three violation categories collected in a single AST walk: Nix-idiom,
derivation-quality, and pattern violations.
-}
data LintBundle = LintBundle
  { lbNix :: ![NixViolation]
  , lbDeriv :: ![DerivViolation]
  , lbPattern :: ![PatternViolation]
  }
  deriving (Eq, Show)

-- | a bundle with no violations in any category.
emptyBundle :: LintBundle
emptyBundle = LintBundle [] [] []

{- | LintBundle | DepthExceeded — callers can distinguish "no violations"
from "skipped because too deep". Past depth, return Left so the caller can
report a hard error instead of silently dropping violations.
-}
data LintResult
  = LintOk !LintBundle
  | LintDepthExceeded !Safety.DepthError
  deriving (Eq, Show)

-- ── entry point ────────────────────────────────────────────────────
-- Single AST walk collecting all three violation categories.
-- Replaces three separate traversals.

-- | Legacy entry: returns 'emptyBundle' on depth-exceeded for backwards compat.
combinedLint :: FilePath -> NExprLoc -> LintBundle
combinedLint filePath expr = orEmpty (combinedLintSafe filePath expr)
 where
  orEmpty (LintOk b) = b
  orEmpty (LintDepthExceeded _) = emptyBundle

{- | Safer entry: distinguishes "no violations" from "depth-exceeded".
n.b. depth limit shared with 'Narsil.Core.Safety.maxRecursionDepth'.
-}
combinedLintSafe :: FilePath -> NExprLoc -> LintResult
combinedLintSafe filePath = walkExpr (0 :: Int)
 where
  walkExpr depth (LayerAnn srcSpan expression)
    | depth > Safety.maxRecursionDepth = LintDepthExceeded (Safety.DepthError depth (T.pack "lint"))
    | otherwise = mergeResults local rest
   where
    d = depth + 1
    local = LintOk (localViolations filePath srcSpan expression)
    -- 'childExprs' already descends into binding RHSs (NSet/NLet), so we must
    -- NOT also walk 'bindingsOf' — doing both traversed every binding value
    -- twice, i.e. 2^depth re-walks of nested attrsets (duplicate violations +
    -- exponential blowup).
    rest = combineResults (map (walkExpr d) (childExprs expression))

  mergeResults (LintDepthExceeded e) _ = LintDepthExceeded e
  mergeResults _ (LintDepthExceeded e) = LintDepthExceeded e
  mergeResults (LintOk a) (LintOk b) = LintOk (combineBundle a b)

  combineResults = foldr mergeResults (LintOk emptyBundle)

-- ── per-node violation checks ──────────────────────────────────────

localViolations :: FilePath -> SrcSpan -> NExprF NExprLoc -> LintBundle
localViolations filePath srcSpan expression =
  mconcat
    [ nixViolations srcSpan expression
    , derivViolations filePath srcSpan expression
    , patternViolations srcSpan expression
    ]

-- ── Nix lint checks ────────────────────────────────────────────────

nixViolations :: SrcSpan -> NExprF NExprLoc -> LintBundle
nixViolations srcSpan = go
 where
  go (NWith _scope _body) = LintBundle [nv VWith "with ..."] [] []
  go (NSet Recursive _) = LintBundle [nv VRec "rec { ... }"] [] []
  go (NApp func _arg)
    | null banned = emptyBundle
    | otherwise = LintBundle banned [] []
   where
    banned = bannedApp srcSpan func
  go (NStr (DoubleQuoted parts)) = LintBundle (longString srcSpan parts) [] []
  -- author-owned naming (mirrors 'Narsil.Lint.Nix.checkLetBindingName' —
  -- the two walkers must agree on the rule set)
  go (NLet bindings _body) = LintBundle (concatMap checkLetBindingName bindings) [] []
  go _ = emptyBundle

  nv typ ctx = NixViolation{nvType = typ, nvSpan = srcSpanToSpan srcSpan, nvContext = ctx}

maxInlineStringLength :: Int
maxInlineStringLength = 120

longString :: SrcSpan -> [Antiquoted Text NExprLoc] -> [NixViolation]
longString srcSpan parts
  | len > maxInlineStringLength =
      [ NixViolation
          (VLongInlineString len)
          (srcSpanToSpan srcSpan)
          ("inline string of length " <> T.pack (show len))
      ]
  | otherwise = []
 where
  len = sum (map partLen parts)
  partLen (Plain t) = T.length t
  partLen _ = 0

bannedApp :: SrcSpan -> NExprLoc -> [NixViolation]
bannedApp srcSpan f = dispatch (leafName f)
 where
  dispatch (Just "substituteAll") = [mkNV VSubstituteAll "substituteAll ..."]
  dispatch (Just "mkDerivation") = [mkNV VRawMkDerivation "mkDerivation { ... }"]
  dispatch (Just "runCommand") = [mkNV VRawRunCommand "runCommand ..."]
  dispatch (Just "writeShellApplication") =
    [mkNV VRawWriteShellApplication "writeShellApplication { ... }"]
  dispatch (Just n)
    | n == "writeShellScript" || n == "writeShellScriptBin" = [mkNV VWriteShellScript (n <> " ...")]
  dispatch _ = []

  mkNV typ = NixViolation typ (srcSpanToSpan srcSpan)

-- ── Derivation lint checks ─────────────────────────────────────────

derivViolations :: FilePath -> SrcSpan -> NExprF NExprLoc -> LintBundle
derivViolations filePath srcSpan = go
 where
  go (NApp func arg) | isMkDerivationCall func = checkDerivMeta filePath srcSpan arg
  go _ = emptyBundle

isMkDerivationCall :: NExprLoc -> Bool
isMkDerivationCall (Layer (NSym name)) = varNameText name == "mkDerivation"
isMkDerivationCall (Layer (NSelect _ _ (StaticKey key :| _))) = varNameText key == "mkDerivation"
isMkDerivationCall _ = False

checkDerivMeta :: FilePath -> SrcSpan -> NExprLoc -> LintBundle
checkDerivMeta filePath srcSpan (Layer (NSet _ bindings)) =
  LintBundle [] (missingMeta ++ missingDesc) []
 where
  hasMeta = any isMetaBinding bindings
  metaBody = findMetaBody bindings
  hasDesc = maybe False hasDescription metaBody
  missingMeta = [DerivViolation VMissingMeta filePath (srcSpanToSpan srcSpan) | not hasMeta]
  missingDesc =
    [DerivViolation VMissingDescription filePath (srcSpanToSpan srcSpan) | hasMeta && not hasDesc]

  isMetaBinding (NamedVar (StaticKey name :| _) _ _) = varNameText name == "meta"
  isMetaBinding _ = False
  findMetaBody [] = Nothing
  findMetaBody (NamedVar (StaticKey n :| _) e _ : _) | varNameText n == "meta" = Just e
  findMetaBody (_ : rest) = findMetaBody rest
  hasDescription (Layer (NSet _ bss)) = any isDescBinding bss
  hasDescription _ = False
  isDescBinding (NamedVar (StaticKey n :| _) _ _) = varNameText n == "description"
  isDescBinding _ = False
checkDerivMeta _ srcSpan _ =
  LintBundle [] [DerivViolation VMissingMeta "<buffer>" (srcSpanToSpan srcSpan)] []

-- ── Pattern lint checks ────────────────────────────────────────────

patternViolations :: SrcSpan -> NExprF NExprLoc -> LintBundle
patternViolations srcSpan = go
 where
  go (NSelect (Just defaultExpr) _ _) -- NSelect alt base path
    | isNullExpr defaultExpr =
        LintBundle
          []
          []
          [PatternViolation VOrNullFallback (srcSpanToSpan srcSpan) "or null fallback"]
  go (NApp func _)
    | isTranslateCall func =
        LintBundle
          []
          []
          [PatternViolation VAttrTranslation (srcSpanToSpan srcSpan) "attribute translation call"]
  go _ = emptyBundle

  isNullExpr (Layer (NConstant NNull)) = True
  isNullExpr (Layer (NSym name)) = varNameText name == ("null" :: Text)
  isNullExpr _ = False

  isTranslateCall (Layer (NSym name)) = varNameText name `elem` translators
  isTranslateCall (Layer (NSelect _ _ (StaticKey key :| _))) = varNameText key `elem` translators
  isTranslateCall _ = False

  translators = ["translateAttrs", "mapAttrsToList", "mapAttrsFlatten"] :: [Text]

-- ── helpers ────────────────────────────────────────────────────────

-- | Extract all immediate child expressions from a node (same pattern across all linters).
childExprs :: NExprF NExprLoc -> [NExprLoc]
childExprs (NConstant _) = []
childExprs (NStr parts) = stringExprs parts
childExprs (NLiteralPath _) = []
childExprs (NEnvPath _) = []
childExprs (NSym _) = []
childExprs (NList xs) = xs
childExprs (NSet _ bindings) = concatMap bindingExprs bindings
childExprs (NLet bindings body) = concatMap bindingExprs bindings ++ [body]
childExprs (NIf c t f) = [c, t, f]
childExprs (NWith scope body) = [scope, body]
childExprs (NAssert c b) = [c, b]
childExprs (NAbs _ b) = [b]
childExprs (NApp f a) = [f, a]
childExprs (NSelect alt b path) = b : maybe id (:) alt [] ++ pathExprs path
childExprs (NHasAttr b path) = b : pathExprs path
childExprs (NUnary _ x) = [x]
childExprs (NBinary _ x y) = [x, y]
childExprs (NSynHole _) = []

bindingExprs :: Binding NExprLoc -> [NExprLoc]
bindingExprs (NamedVar _ e _) = [e]
bindingExprs (Inherit (Just scope) _ _) = [scope]
bindingExprs (Inherit Nothing _ _) = []

stringExprs :: NString NExprLoc -> [NExprLoc]
stringExprs (DoubleQuoted parts) = [e | Antiquoted e <- parts]
stringExprs (Indented _ parts) = [e | Antiquoted e <- parts]

pathExprs :: NAttrPath NExprLoc -> [NExprLoc]
pathExprs path = [e | DynamicKey (Antiquoted e) <- NE.toList path]

-- | Leaf symbol name of an expression (for banned-function detection).
leafName :: NExprLoc -> Maybe Text
leafName (Layer (NSym name)) = Just (varNameText name)
leafName (Layer (NSelect _ _ (StaticKey key :| _))) = Just (varNameText key)
leafName _ = Nothing

-- ── Bundle combinators ─────────────────────────────────────────────

combineBundle :: LintBundle -> LintBundle -> LintBundle
combineBundle (LintBundle n1 d1 p1) (LintBundle n2 d2 p2) =
  LintBundle (n1 ++ n2) (d1 ++ d2) (p1 ++ p2)

instance Semigroup LintBundle where
  (<>) = combineBundle

instance Monoid LintBundle where
  mempty = emptyBundle
