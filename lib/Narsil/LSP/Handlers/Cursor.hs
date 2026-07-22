{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                      // lsp // handlers // cursor
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He found the spot, the exact point where the data lived."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Cursor ↔ AST plumbing: find the smallest expression enclosing an editor
--   (line, col), walk a node's children, and infer the type at a cursor. Pure;
--   shared by every position-driven feature (hover, signature, completion,
--   option lookup, semantic tokens).
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.LSP.Handlers.Cursor (
  findExprAt,
  childExprs,
  inferExprAt,
  inferExprAtWithEnv,
  exprName,
  selectAtCursor,
)
where

import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Inference.Nix (TypeEnv, builtinEnv, inferExprWithEnv)
import Narsil.Inference.Nix qualified as Infer
import Narsil.Inference.Nix.Type qualified as NT
import Narsil.Syntax.Annotation (srcSpanToSpan, varNameText, pattern Layer, pattern LayerAnn)
import Nix.Expr.Types (Binding (..), NExprF (..), NKeyName (..), Params (..))
import Nix.Expr.Types.Annotated (NExprLoc)

-- | The smallest expression whose span contains the editor (line, col), if any.
findExprAt :: Int -> Int -> NExprLoc -> Maybe NExprLoc
findExprAt l c = go
 where
  targetLine = l + 1
  targetCol = c + 1
  spContains (Span (Loc sl sc) (Loc el ec) _) =
    (sl < targetLine || (sl == targetLine && sc <= targetCol))
      && (el > targetLine || (el == targetLine && ec >= targetCol))
  getSpan (LayerAnn sp _) = srcSpanToSpan sp
  go e
    | not (spContains (getSpan e)) = Nothing
    | otherwise = Just (fromMaybe e (listToMaybe (mapMaybe go (childExprs' e))))
  childExprs' (Layer (NConstant _)) = []
  childExprs' (Layer (NStr _)) = []
  childExprs' (Layer (NLiteralPath _)) = []
  childExprs' (Layer (NEnvPath _)) = []
  childExprs' (Layer (NSym _)) = []
  childExprs' (Layer (NList es)) = es
  childExprs' (Layer (NSet _ bs)) = concatMap bindingExprs bs
  childExprs' (Layer (NLet bs b)) = concatMap bindingExprs bs ++ [b]
  childExprs' (Layer (NIf cond t f')) = [cond, t, f']
  childExprs' (Layer (NWith s b)) = [s, b]
  childExprs' (Layer (NAssert cond body)) = [cond, body]
  childExprs' (Layer (NAbs (Param _) b)) = [b]
  childExprs' (Layer (NAbs (ParamSet _ _ formals) b)) = [d | (_, Just d) <- formals] ++ [b]
  childExprs' (Layer (NApp f' a)) = [f', a]
  childExprs' (Layer (NSelect mDef obj _path)) = maybeToList mDef ++ [obj]
  childExprs' (Layer (NHasAttr e1 _)) = [e1]
  childExprs' (Layer (NUnary _ e1)) = [e1]
  childExprs' (Layer (NBinary _ e1 e2)) = [e1, e2]
  childExprs' (Layer (NSynHole _)) = []
  bindingExprs (NamedVar _ e _) = [e]
  bindingExprs (Inherit mScope _ _) = maybeToList mScope

-- | The immediate sub-expressions of one AST node (one level deep).
childExprs :: NExprF NExprLoc -> [NExprLoc]
childExprs (NConstant _) = []
childExprs (NStr _) = []
childExprs (NLiteralPath _) = []
childExprs (NEnvPath _) = []
childExprs (NSym _) = []
childExprs (NList es) = es
childExprs (NSet _ bs) = concatMap bindExprs bs
childExprs (NLet bs b) = concatMap bindExprs bs ++ [b]
childExprs (NIf cond t f') = [cond, t, f']
childExprs (NWith s b) = [s, b]
childExprs (NAssert cond body) = [cond, body]
childExprs (NAbs _ b) = [b]
childExprs (NApp f' a) = [f', a]
childExprs (NSelect _ b _) = [b]
childExprs (NHasAttr b _) = [b]
childExprs (NUnary _ e1) = [e1]
childExprs (NBinary _ e1 e2) = [e1, e2]
childExprs (NSynHole _) = []

bindExprs :: Binding NExprLoc -> [NExprLoc]
bindExprs (NamedVar _ e _) = [e]
bindExprs (Inherit mScope _ _) = maybeToList mScope

{- | Pretty type of the expression at the cursor, inferred against the builtin
  env only. See 'inferExprAtWithEnv'.
-}
inferExprAt :: NExprLoc -> Int -> Int -> Maybe Text
inferExprAt = inferExprAtWithEnv builtinEnv

{- | Pretty type of the expression at the cursor, inferred against @env@. Prefers
  the binding type when the cursor names a let/attr binding; falls back to
  inferring the target sub-expression. Yields @"TYPE_ERROR"@ on inference failure.
-}
inferExprAtWithEnv :: TypeEnv -> NExprLoc -> Int -> Int -> Maybe Text
inferExprAtWithEnv env expr l c = do
  target <- findExprAt l c expr
  either (const (inferTarget' target)) (fromBindings target) (inferExprWithEnv env expr)
 where
  fromBindings target (_, bindings) =
    maybe (inferTarget' target) (fromName target bindings) (exprName target)
  fromName target bindings name =
    maybe (inferTarget' target) namedType (find (\(Infer.Binding n _ _) -> n == name) bindings)
   where
    namedType (Infer.Binding _ t _sp) = Just (NT.prettyType t)
  inferTarget' te =
    either
      (const (Just "TYPE_ERROR"))
      (\(t, _) -> Just (NT.prettyType t))
      (inferExprWithEnv builtinEnv te)

{- | The identifier an expression refers to: a bare symbol or the final
  static key of a select. 'Nothing' for anything else.
-}
exprName :: NExprLoc -> Maybe Text
exprName (Layer (NSym name)) = Just $ varNameText name
exprName (Layer (NSelect _ _ (StaticKey k :| _))) = Just $ varNameText k
exprName _ = Nothing

{- | If the editor (line, col) sits on an attribute select whose base is a bare
  symbol — e.g. the cursor anywhere within @pkgs.ripgrep@ — return
  @(baseName, firstKey)@: the base identifier (@pkgs@) and the first attribute
  after it (@ripgrep@). The caller decides whether @baseName@ denotes the nixpkgs
  package set. Finds the INNERMOST enclosing such select, so nested selects like
  @(pkgs.lib).foo@ resolve to the closest one; works whether the cursor is on the
  base or on a key (hnix gives the whole select one span). 'Nothing' otherwise.
-}
selectAtCursor :: Int -> Int -> NExprLoc -> Maybe (Text, Text)
selectAtCursor l c = go
 where
  targetLine = l + 1
  targetCol = c + 1
  contains (Span (Loc sl sc) (Loc el ec) _) =
    (sl < targetLine || (sl == targetLine && sc <= targetCol))
      && (el > targetLine || (el == targetLine && ec >= targetCol))
  spanOf (LayerAnn sp _) = srcSpanToSpan sp
  kids (Layer ef) = childExprs ef
  go e
    | not (contains (spanOf e)) = Nothing
    | otherwise = maybe (thisSelect e) Just (listToMaybe (mapMaybe go (kids e)))
  thisSelect (Layer (NSelect _ (Layer (NSym base)) (StaticKey k :| _))) =
    Just (varNameText base, varNameText k)
  thisSelect _ = Nothing
