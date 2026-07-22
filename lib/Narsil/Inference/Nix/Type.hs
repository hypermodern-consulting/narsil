{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                   // nix // types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The box was a universe, a poem."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The vocabulary of the type system — the data every module in this subtree
--   manipulates. Four pieces, in the Hindley–Milner tradition:
--
--     * 'NixType' — the language of types a Nix expression can have: base scalars
--       (Int / Float / Bool / String / Path / Null / Derivation), lists, functions,
--       records, unions, the type variable 'TVar', and the dynamic escape 'TAny'.
--     * 'Scheme' — a type closed over quantified variables (@Forall vars t@, a ∀).
--       This is what makes `let`-bound names polymorphic: the scheme is
--       INSTANTIATED afresh at each use site (see "Narsil.Inference.Nix.Scheme").
--     * 'Subst' — a finite map from type variable to type. Unification's whole job
--       is to BUILD one of these; 'applySubst' then rewrites a type under it,
--       chasing variables transitively to a normal form.
--     * 'Constraint' — an equality goal @t1 :~: t2@ the solver must make hold.
--
--   Records are where we leave textbook HM: each carries a ROW ('RowTail') that is
--   either 'RClosed' (exactly these fields) or @'ROpen' r@ (these fields AND whatever
--   the row variable @r@ later resolves to). Row polymorphism (Wand / Rémy / Leijen)
--   is what lets a partially-known attrset GAIN fields as it flows through the
--   program — essential for Nix, where attrsets are passed half-built and completed
--   by overlays and `//`.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Inference.Nix.Type (
  -- * Types
  NixType (..),
  RowTail (..),
  pattern TAttrs,
  pattern TAttrsOpen,
  tRecOpenAnon,
  anonRowVar,
  isAnonRowVar,
  rowTailVars,
  TypeVar (..),

  -- * Type schemes (polymorphic types)
  Scheme (..),

  -- * Constraints
  Constraint (..),

  -- * Substitution
  Subst,
  emptySubst,
  singleSubst,
  composeSubst,
  applySubst,
  applySubstScheme,

  -- * Free variables
  freeTypeVars,
  freeTypeVarsScheme,

  -- * Pretty printing
  prettyType,
  prettyScheme,
)
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- types
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | A type variable — a placeholder for a not-yet-known type, identified by a
unique id from the inference fresh-var supply. Unification's job is to
discover what each one stands for.
-}
newtype TypeVar = TypeVar {unTypeVar :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

{- | A monomorphic Nix type — inference assigns one of these to every
expression. 'TVar' is an unknown to be solved for; 'TAny' is a deliberate
dynamic (it unifies with anything — our top); the rest are ordinary structure
(scalars, lists, functions, records, unions). Polymorphism is NOT here — it
lives one level up, in 'Scheme'.
-}
data NixType
  = TVar !TypeVar
  | TInt
  | TFloat
  | TBool
  | TString
  | TStrLit !Text
  | TPath
  | TNull
  | TList !NixType
  | -- | records: known fields (type, isOptional) + a row tail
    TRec !(Map Text (NixType, Bool)) !RowTail
  | TFun !NixType !NixType
  | TDerivation
  | TUnion ![NixType]
  | TAny
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON NixType

instance ToJSON NixType

{- | A record's row tail: closed (exactly the known fields) or open with a row
**variable** standing for "at least these fields, plus whatever @r@ resolves to".
The row var lets open records accumulate fields across unifications (RC1); its
lacks-constraints (which labels it must NOT gain — the labels its record already
owns) live in a side store in the inference state (@inferLacks@ in
"Narsil.Inference.Nix.Constraint") and are ENFORCED when the row var is bound
("Narsil.Inference.Nix.Unify"'s @bindRowVar@ rejects a binding that would
supply a lacked label). That is what makes the field merge in 'applySubst' below
provably disjoint rather than disjoint by happenstance.
-}
data RowTail = RClosed | ROpen !TypeVar
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON RowTail

instance ToJSON RowTail

{- | Closed-record view: bidirectional, so @TAttrs m@ both matches and builds
@TRec m RClosed@.
-}
pattern TAttrs :: Map Text (NixType, Bool) -> NixType
pattern TAttrs fields = TRec fields RClosed

{- | Open-record view. Matching ignores the row variable; building uses the
anonymous sentinel ('anonRowVar') — fine for pure/display and test construction.
Inference sites that need field accumulation build @TRec m (ROpen r)@ with a
FRESH @r@ instead (see 'Narsil.Inference.Nix.mkOpenRec').
-}
pattern TAttrsOpen :: Map Text (NixType, Bool) -> NixType
pattern TAttrsOpen fields <- TRec fields (ROpen _)
  where
    TAttrsOpen fields = TRec fields (ROpen anonRowVar)

{-# COMPLETE
  TVar
  , TInt
  , TFloat
  , TBool
  , TString
  , TStrLit
  , TPath
  , TNull
  , TList
  , TAttrs
  , TAttrsOpen
  , TFun
  , TDerivation
  , TUnion
  , TAny
  #-}

{- | A polymorphic type scheme: a type closed over a list of quantified
variables — @Forall vars t@ reads as @∀ vars. t@. This is the seat of
let-polymorphism: a @let@-bound name is generalised to a scheme, and each use
site instantiates it with fresh variables. @Forall []@ is an ordinary
monotype. (Generalise / instantiate live in "Narsil.Inference.Nix.Scheme".)
-}
data Scheme = Forall ![TypeVar] !NixType
  deriving stock (Eq, Show, Generic)

instance FromJSON Scheme

instance ToJSON Scheme

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- constraints
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | An equality constraint: @t1 :~: t2@ is a goal asserting the two types must
be made equal. The engine can either collect these and solve them in a batch,
or (as we do) unify eagerly; either way @:~:@ is the unit of work.
-}
data Constraint
  = NixType :~: NixType
  deriving stock (Eq, Show, Generic)

infix 4 :~:

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- substitution
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | A substitution: what each solved type variable has been resolved to.
Unification's entire output is one of these; 'applySubst' uses it to turn a
type-with-unknowns into its current best-known form.
-}
type Subst = Map TypeVar NixType

-- | The identity substitution — resolves nothing.
emptySubst :: Subst
emptySubst = Map.empty

-- | The substitution binding exactly one variable.
singleSubst :: TypeVar -> NixType -> Subst
singleSubst = Map.singleton

{- | Compose two substitutions. @composeSubst s1 s2@ first rewrites the range of
@s2@ through @s1@, then unions (with @s1@ winning on conflict) — so it means
\"apply @s2@, then @s1@\". n.b. the inference engine deliberately avoids this
(it keeps a triangular substitution and chases on read instead — O(n) not
O(n²); see 'Narsil.Inference.Nix.Constraint.addSubst').
-}
composeSubst :: Subst -> Subst -> Subst
composeSubst substitution1 substitution2 =
  Map.map (applySubst substitution1) substitution2 `Map.union` substitution1

-- | the row variable in an open tail, if any
rowTailVars :: RowTail -> Set TypeVar
rowTailVars (ROpen r) = Set.singleton r
rowTailVars RClosed = Set.empty

{- | Sentinel row variable for "anonymous open" records built in PURE contexts
(flake/module display types) that have no fresh-var supply. Unification must never
bind it (see 'isAnonRowVar'), so such records behave like the old tail-less open
attrset — no field accumulation. Negative id can never collide with the inference
supply (which counts up from 0).
-}
anonRowVar :: TypeVar
anonRowVar = TypeVar (-1)

{- | Is this the anonymous sentinel row variable? Unification consults this and
refuses to bind it, so anonymous-open records never accumulate fields.
-}
isAnonRowVar :: TypeVar -> Bool
isAnonRowVar v = v == anonRowVar

{- | build an open record with the anonymous tail (pure-context helper for
flake/module types; the inference engine uses fresh row vars instead)
-}
tRecOpenAnon :: Map Text (NixType, Bool) -> NixType
tRecOpenAnon m = TRec m (ROpen anonRowVar)

{- | Apply a substitution to a type: replace every bound variable by what it
resolved to, recursively, so the result is normalised w.r.t. the
substitution. The variable case chases transitively (a var bound to a var
bound to a type follows the whole chain), which is what lets the engine keep
a cheap triangular substitution and still read fully-resolved types.
-}
applySubst :: Subst -> NixType -> NixType
applySubst s = go
 where
  go (TVar v) = resolveVar v (Map.lookup v s)
  go (TList t) = TList (go t)
  go (TRec m tail_) = resolveRec (Map.map (\(t, o) -> (go t, o)) m) tail_
  go (TFun a b) = TFun (go a) (go b)
  -- unions are SETS: members that resolve equal collapse (a join of two
  -- field vars that both land on Int is Int, not `Int | Int`), and nested
  -- unions flatten
  go (TUnion ts) = case dedupe (concatMap (flat . go) ts) of -- CASE-OK: shape dispatch
    [t] -> t
    ts' -> TUnion ts'
   where
    flat (TUnion us) = us
    flat t = [t]
    dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
  go t = t

  -- a self-map {v ↦ TVar v} is the identity; returning it (instead of chasing)
  -- avoids an infinite loop. `instantiate` produces such maps whenever a fresh
  -- var collides with a scheme's quantified var index (both draw from 0,1,…),
  -- which is why applying ANY polymorphic builtin (head/map/filter/…) to an
  -- argument used to hang inference.
  resolveVar v (Just (TVar v')) | v' == v = TVar v
  resolveVar _ (Just t) = go t
  resolveVar v Nothing = TVar v

  resolveRec m' RClosed = TRec m' RClosed
  resolveRec m' (ROpen r) = resolveRow m' r (Map.lookup r s)

  resolveRow m' r Nothing = TRec m' (ROpen r)
  resolveRow m' r (Just (TVar r')) | r' == r = TRec m' (ROpen r) -- self-map: identity
  resolveRow m' _ (Just (TVar r')) = TRec m' (ROpen r') -- tail var renamed
  -- row var bound to a record: merge known fields and continue resolving the
  -- bound row's own tail. The 'Map.union' is disjoint by ENFORCEMENT: the
  -- lacks-check in 'bindRowVar' refuses to bind a row var to a record sharing a
  -- label with the record it tails, so a colliding field is a type error there and
  -- never reaches this merge to be silently dropped.
  resolveRow m' _ (Just (TRec m2 tail2)) = go (TRec (Map.union m' m2) tail2)
  resolveRow m' r (Just _) = TRec m' (ROpen r) -- defensive: non-row binding

{- | Apply a substitution to a scheme — but NOT to its quantified variables.
Those are bound by the @forall@; substituting them would be variable capture
(the textbook hazard of substituting under a binder), so we drop them from
the substitution first.
-}
applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme s (Forall vars t) =
  Forall vars (applySubst (foldr Map.delete s vars) t)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- free type variables
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | The type variables occurring free in a type — including row-tail variables.
Generalisation quantifies over exactly the ones the environment does not also
mention.
-}
freeTypeVars :: NixType -> Set TypeVar
freeTypeVars (TVar v) = Set.singleton v
freeTypeVars (TList t) = freeTypeVars t
freeTypeVars (TRec m tail_) =
  Set.unions (map (freeTypeVars . fst) (Map.elems m)) `Set.union` rowTailVars tail_
freeTypeVars (TFun a b) = freeTypeVars a `Set.union` freeTypeVars b
freeTypeVars (TUnion ts) = Set.unions (map freeTypeVars ts)
freeTypeVars _ = Set.empty

{- | The free type variables of a scheme: free in the body but NOT quantified by
the @forall@. (Those are the ones still tied to the surrounding context.)
-}
freeTypeVarsScheme :: Scheme -> Set TypeVar
freeTypeVarsScheme (Forall vars t) =
  freeTypeVars t `Set.difference` Set.fromList vars

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- pretty printing
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Render a type for display, numbering its free variables @a@, @b@, … in
order of appearance (so the same type always prints the same way).
-}
prettyType :: NixType -> Text
prettyType t = prettyTypeWith mapping t
 where
  vars = Set.toAscList (freeTypeVars t)
  names = map T.singleton ['a' .. 'z'] ++ ["t" <> T.pack (show i) | i <- [1 ..] :: [Int]]
  mapping = Map.fromList $ zip vars names

{- | Render a scheme as @forall a b. …@, or just the type when it is monomorphic
(no quantified variables).
-}
prettyScheme :: Scheme -> Text
prettyScheme (Forall [] t) = prettyType t
prettyScheme (Forall vars t) =
  let
    free = Set.toAscList (freeTypeVars t `Set.difference` Set.fromList vars)
    allVars = vars ++ free
    names = map T.singleton ['a' .. 'z'] ++ ["t" <> T.pack (show i) | i <- [1 ..] :: [Int]]
    mapping = Map.fromList $ zip allVars names

    prettyVar v = Map.findWithDefault "?" v mapping
   in
    "forall " <> T.intercalate " " (map prettyVar vars) <> ". " <> prettyTypeWith mapping t

prettyTypeWith :: Map TypeVar Text -> NixType -> Text
prettyTypeWith mapping = go
 where
  go (TVar v) = Map.findWithDefault ("t" <> T.pack (show (unTypeVar v))) v mapping
  go TInt = "Int"
  go TFloat = "Float"
  go TBool = "Bool"
  go TString = "String"
  go (TStrLit s) = "\"" <> truncLit s <> "\""
  go TPath = "Path"
  go TNull = "Null"
  go (TList t) = "[" <> go t <> "]"
  go (TRec m RClosed) = prettyAttrs m
  go (TRec m (ROpen r)) = prettyAttrs m <> " | " <> Map.findWithDefault ".." r mapping
  go (TFun a b) = prettyArg a <> " -> " <> go b
  go TDerivation = "Derivation"
  go (TUnion ts) = T.intercalate " | " (map go ts)
  go TAny = "Any"

  prettyArg t@(TFun _ _) = "(" <> go t <> ")"
  prettyArg t = go t

  -- A 'TStrLit' carries the literal's full text; cap it in type display so a
  -- giant string literal doesn't become a giant type (e.g. `infer` on a file
  -- with a 200 KB string was emitting a 200 KB `# :: "…"` annotation).
  truncLit s
    | T.length s <= 40 = s
    | otherwise = T.take 39 s <> "…"

  prettyAttrs m
    | Map.null m = "{}"
    | otherwise = "{ " <> T.intercalate ", " (map prettyField (Map.toList m)) <> " }"

  prettyField (k, (v, opt)) = k <> (if opt then "?" else "") <> " : " <> go v
