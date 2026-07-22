{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                 // inference // nix // constraint
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He had a sense of the thing as a vast and intricate machine, and himself
--    a single moving part."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The inference monad and its state: a `State`+`Except` stack (no IO — that
--   is load-bearing, it is why the oracle can replay inference). Fresh-var
--   supply, the triangular substitution, the accumulated output bindings, the
--   current source span, and the `with`-scope memo all live here, along with
--   the primitive operations (freshVar / addSubst / applyCurrentSubst /
--   withSpan / throwTypeError / emitBinding) that the unifier and the
--   inference core are built from.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Inference.Nix.Constraint (
  -- * The accumulated output
  Binding (..),

  -- * Monad + state
  InferState (..),
  Infer,
  runInfer,
  runInferBinds,

  -- * Primitives
  emitBinding,
  withSpan,
  throwTypeError,
  catchInfer,
  freshVar,
  freshTypeVar,
  mkOpenRec,
  applyCurrentSubst,
  addSubst,

  -- * Row lacks-constraints (Gaster–Jones)
  addLacks,
  getLacks,
)
where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Inference.Nix.Type

-- | a single typed binding (name, resolved type, source location)
data Binding = Binding
  { bindName :: !Text
  , bindType :: !NixType
  , bindSpan :: !Span
  }
  deriving (Eq, Show)

{- | inference monad state
supply = fresh type-var counter; subst = current unifier substitution
binds = accumulated (name, type, span) triples for the output
span = current source location (for error messages)
withMemo = cache for `with` scope field lookups (avoids re-unification)
-}
data InferState = InferState
  { inferSupply :: !Int
  , inferSubst :: !Subst
  , inferBinds :: ![Binding]
  , inferSpan :: !(Maybe Span)
  , inferWithMemo :: !(Map Text NixType)
  , inferLacks :: !(Map TypeVar (Set Text))
  {- ^ Gaster–Jones lacks-constraints: for an open row variable @r@, the labels
  @r@ must NOT contain — exactly the labels owned by every record that has @r@
  as its tail. Maintained at each 'unify' entry and checked when a row var is
  bound, so the field-merge in 'applySubst' is disjoint by enforcement (the row is
  refused any label its record already owns, at a possibly-different type).
  -}
  }

-- | inference runs in EitherT over State: errors abort, state persists
type Infer a = ExceptT Text (State InferState) a

{- | run the inference monad, extracting final bindings
starts with empty substitution / fresh-var counter at 0
-}
runInfer :: Infer a -> Either Text (a, [Binding])
runInfer inference =
  let (eitherResult, inferState) =
        runState (runExceptT inference) (InferState 0 emptySubst [] Nothing Map.empty Map.empty)
   in (\res -> (res, inferBinds inferState)) <$> eitherResult

{- | Like 'runInfer', but the accumulated bindings SURVIVE an inference error —
everything emitted before the failure point is real, typed information (a
single type error must not blank a whole file's inlay hints).
-}
runInferBinds :: Infer a -> (Either Text a, [Binding])
runInferBinds inference =
  let (eitherResult, inferState) =
        runState (runExceptT inference) (InferState 0 emptySubst [] Nothing Map.empty Map.empty)
   in (eitherResult, inferBinds inferState)

-- ── emit a binding into the result list (prepended, reversed later) ──
emitBinding :: Text -> NixType -> Span -> Infer ()
emitBinding name t sp = modify $ \s ->
  s{inferBinds = Binding name t sp : inferBinds s}

-- ── run an action with a specific source span for error reporting ──
withSpan :: Span -> Infer a -> Infer a
withSpan sp action = do
  old <- gets inferSpan
  modify $ \s -> s{inferSpan = Just sp}
  res <- action
  modify $ \s -> s{inferSpan = old}
  pure res

-- ── abort inference with a type error annotated by source location ──
throwTypeError :: Text -> Infer a
throwTypeError msg = do
  mSpan <- gets inferSpan
  maybe (throwError msg) located mSpan
 where
  located (Span (Loc l c) _ _) =
    throwError $ T.pack (show l) <> ":" <> T.pack (show c) <> ": " <> msg

{- | Run a SPECULATIVE action: if it throws, restore the full inference state
(substitution, emitted bindings, memos) as of entry and run the fallback.
The rollback is what makes speculation sound — a failed attempt must not
leave its partial constraints behind.
-}
catchInfer :: Infer a -> Infer a -> Infer a
catchInfer action fallback = do
  saved <- get
  action `catchError` \_ -> put saved >> fallback

-- ── allocate a fresh type variable (monotonically increasing id) ──
freshVar :: Infer NixType
freshVar = TVar <$> freshTypeVar

-- | allocate a fresh type/row variable (the raw 'TypeVar', for row tails)
freshTypeVar :: Infer TypeVar
freshTypeVar = do
  s <- get
  put s{inferSupply = inferSupply s + 1}
  pure $ TypeVar (inferSupply s)

-- | build an open record with the given known fields and a FRESH row tail var
mkOpenRec :: Map Text (NixType, Bool) -> Infer NixType
mkOpenRec m = do
  r <- freshTypeVar
  pure (TRec m (ROpen r))

-- ── apply the current substitution to a type (idempotent with current subst) ──
applyCurrentSubst :: NixType -> Infer NixType
applyCurrentSubst t = do
  s <- gets inferSubst
  pure $ applySubst s t

{- | Extend the current substitution with @v ↦ t@.

We keep a TRIANGULAR substitution (a plain insert) rather than eagerly
composing. The old @composeSubst@ form re-walked and rewrote the entire
accumulated substitution on every bind — O(n) per bind, O(n²) over a program
with n unifications (RC4). 'applySubst' already chases transitively (the @TVar@
case recurses through bound vars), so resolution still fully normalises on read.

Soundness invariant: every caller binds @v@ to a @t@ that has already been
resolved against the current substitution ('applyCurrentSubst' in 'unify' /
'mergeTypes' / 'unifyRec'), and 'bindVar'/'bindRowVar' run the occurs check on
that resolved @t@. So @v@ is unbound and @t@ is ground w.r.t. current bindings at
insert time — the substitution stays acyclic and the on-read chase terminates.
-}
addSubst :: TypeVar -> NixType -> Infer ()
addSubst v t = modify $ \s ->
  s{inferSubst = Map.insert v t (inferSubst s)}

{- | Record that an open row variable must LACK the given labels — accumulated
(union), since a row var can tail several records over a run. The anonymous
sentinel ('isAnonRowVar') is never bound, so it carries no constraint. Empty sets
are a no-op.
-}
addLacks :: TypeVar -> Set Text -> Infer ()
addLacks r labels
  | isAnonRowVar r = pure ()
  | Set.null labels = pure ()
  | otherwise = modify $ \s -> s{inferLacks = Map.insertWith Set.union r labels (inferLacks s)}

-- | The labels an open row variable must lack (empty if unconstrained).
getLacks :: TypeVar -> Infer (Set Text)
getLacks r = gets (Map.findWithDefault Set.empty r . inferLacks)
