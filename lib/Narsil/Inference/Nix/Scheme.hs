{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                     // inference // nix // scheme
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "A scheme, a plan, a thing of beauty and precision."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The two halves of Hindley-Milner let-polymorphism: 'instantiate' hands each
--   use-site a fresh copy of a scheme's quantified vars, and 'generalize' closes
--   a type over the vars the environment does not mention. Both run in the Infer
--   monad ('Constraint').
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Inference.Nix.Scheme (
  instantiate,
  generalize,
  applyCurrentSubstScheme,
)
where

import Control.Monad.State.Strict (gets)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Narsil.Inference.Nix.Constraint
import Narsil.Inference.Nix.Environment
import Narsil.Inference.Nix.Type

{- | instantiate a polymorphic scheme by replacing each quantified var with a fresh type var
this is HM-style let-polymorphism: each use-site gets its own copy

n.b. the rename is applied ONE step, never chain-following: 'applySubst'
chases var→var bindings, so when a fresh id collides with a still-unrenamed
source id the chains compose (1→2, 2→3 ⟹ 1→3) and DISTINCT quantified vars
collapse into one — a rename is a bijection, not a substitution.
-}
instantiate :: Scheme -> Infer NixType
instantiate (Forall vars t) = do
  freshVars <- mapM (const freshVar) vars
  let rename = Map.fromList (zip vars freshVars)
  pure $ applyRename rename t

-- | apply a var→fresh-var rename exactly one step (no chasing)
applyRename :: Map.Map TypeVar NixType -> NixType -> NixType
applyRename s = go
 where
  go (TVar v) = Map.findWithDefault (TVar v) v s
  go (TList e) = TList (go e)
  go (TFun a b) = TFun (go a) (go b)
  go (TRec m tl) = TRec (Map.map (\(ft, o) -> (go ft, o)) m) (goTail tl)
  go (TUnion ts) = TUnion (map go ts)
  go other = other
  goTail (ROpen r) | Just (TVar r') <- Map.lookup r s = ROpen r'
  goTail tl = tl

{- | generalize (close over) free type vars not free in the environment
this implements HM let-polymorphism: only quantify vars the env doesn't mention
-}
generalize :: TypeEnv -> NixType -> Infer Scheme
generalize environment t = do
  t' <- applyCurrentSubst t
  envSchemes <- mapM applyCurrentSubstScheme (Map.elems (envBindings environment))
  let freeInEnv = Set.unions (map freeTypeVarsScheme envSchemes)
  let freeInT = freeTypeVars t'
  let vars = Set.toList (freeInT `Set.difference` freeInEnv)
  pure $ Forall vars t'

-- | apply current subst to all type variables in a scheme
applyCurrentSubstScheme :: Scheme -> Infer Scheme
applyCurrentSubstScheme s = do
  subst <- gets inferSubst
  pure $ applySubstScheme subst s
