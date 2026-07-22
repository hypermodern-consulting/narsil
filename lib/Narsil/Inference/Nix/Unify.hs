{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                      // inference // nix // unify
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The matrix has its roots in primitive arcade games, in early graphics
--    programs and military experimentation with cranial jacks."
--
--                                                                                     — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Unification — the heart of inference. To UNIFY two types is to find the most
--   general substitution that makes them equal: a variable unifies with anything
--   (and we record the binding); two constructors unify when their heads match
--   and their children unify recursively; anything else is a type error. 'unify'
--   first normalises both sides through the current substitution, then matches
--   structurally ('unify''). Three wrinkles past the textbook:
--
--     * the OCCURS CHECK ('occursCheck') refuses to bind @a@ to a type that
--       CONTAINS @a@ (e.g. @a ~ [a]@) — that is an infinite type, and the bug it
--       guards against is a non-terminating substitution.
--     * RECORDS unify row-by-row ('unifyRec'): closed/closed is exact; open/
--       closed lets the open side's row variable absorb the closed side's extra
--       fields; open/open binds both tails to a SHARED fresh row so the field
--       UNION survives. This is the row polymorphism 'Narsil.Inference.Nix.Type'
--       sets up.
--     * 'mergeTypes' is the JOIN (least upper bound), NOT unification: where
--       'unify' ASSERTS that two types are equal, 'mergeTypes' COMBINES two types
--       into one that covers both — the arms of an `if`, the elements of a list.
--       Equal types collapse; incompatible ones become a 'TUnion'.
--
--   Runs in the Infer monad ('Constraint'); knows nothing of the AST.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Inference.Nix.Unify (
  unify,
  mergeTypes,
  fieldConstraint,
  bindRowVar,
  derivationWitness,
)
where

import Control.Monad (forM, forM_, unless)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Inference.Nix.Constraint
import Narsil.Inference.Nix.Type

-- ── unify: the core constraint solver ────────────────────────────

-- | report a mismatch between expected and actual types
typeMismatch :: NixType -> NixType -> Infer a
typeMismatch type1 type2 =
  throwTypeError $ "type mismatch: expected " <> prettyType type1 <> ", got " <> prettyType type2

{- | handle __functor protocol: if attrs has __functor, unify against its return type
n.b. this is how nix makes callable attribute sets
-}
unifyFunctor :: NixType -> NixType -> Infer ()
unifyFunctor funT attrsT
  | TAttrs m <- attrsT = lookupFunctor funT m False
  | TAttrsOpen m <- attrsT = lookupFunctor funT m True
  | otherwise = typeMismatch funT attrsT
 where
  lookupFunctor ft m open = dispatch (Map.lookup "__functor" m)
   where
    dispatch (Just (TFun _ innerT, _)) = unify innerT ft
    dispatch (Just (ftFunctor, _)) =
      throwTypeError $ "__functor must be a function, got " <> prettyType ftFunctor
    -- an OPEN record's unknown tail may carry __functor
    -- (makeOverridable/setFunctionArgs callables) — only a CLOSED record
    -- provably lacks it
    dispatch Nothing
      | open = pure ()
      | otherwise = typeMismatch ft attrsT

-- | apply current subst, then unify the normalised forms
unify :: NixType -> NixType -> Infer ()
unify type1 type2 = do
  t1' <- applyCurrentSubst type1
  t2' <- applyCurrentSubst type2
  -- record each side's lacks-constraint BEFORE binding any tail: a tail var is
  -- only ever bound inside a unify that first saw its record, so this captures the
  -- labels every record tailing it owns (across instantiation, too).
  registerLacks t1'
  registerLacks t2'
  unify' t1' t2'

{- | Record the lacks-constraint a top-level open record imposes on its tail
variable: the tail must lack every label the record already owns. Nested records
are registered by the recursive 'unify' calls that reach them.
-}
registerLacks :: NixType -> Infer ()
registerLacks (TRec m (ROpen r)) = addLacks r (Map.keysSet m)
registerLacks _ = pure ()

{- | structural unification — must be applied AFTER current substitution.
n.b. clause order is load-bearing: the patterns overlap (TFun/TFun before
the functor-protocol TFun/attrs, TUnion before that), exactly as the old
top-to-bottom `case (type1, type2)` required.
-}
unify' :: NixType -> NixType -> Infer ()
-- a variable meeting a UNION: join-produced unions carry the join's own
-- variables as members (`a | Any -> a`), so bind through the union with
-- self-references stripped — `a ~ a | X` constrains a to X, and a union still
-- self-referential after stripping (only joins make these) is left
-- unconstrained rather than fabricating an "infinite type" on valid Nix.
unify' (TVar v) u@(TUnion _) = bindVarUnion v u
unify' u@(TUnion _) (TVar v) = bindVarUnion v u
-- variable cases: bind one to the other (with occurs check)
unify' (TVar v) t = bindVar v t
unify' t (TVar v) = bindVar v t
-- TAny unifies with everything (dynamic / unknown)
unify' TAny _ = pure ()
unify' _ TAny = pure ()
-- base types: identical only
unify' TInt TInt = pure ()
unify' TFloat TFloat = pure ()
unify' TBool TBool = pure ()
unify' TString TString = pure ()
unify' (TStrLit _) (TStrLit _) = pure ()
unify' TString (TStrLit _) = pure ()
unify' (TStrLit _) TString = pure ()
unify' TPath TPath = pure ()
unify' TNull TNull = pure ()
unify' TDerivation TDerivation = pure ()
-- a record carrying (or able to carry) a string-coercion witness passes at
-- Derivation — the SAME rule union membership already applies
-- ('unionMemberAccepts'): oracle-typed packages and row-constrained formals
-- (`inherit (clang-unwrapped) version` ⟹ `{ version : a | ρ }`) are records,
-- not TDerivation, yet flow into Derivation positions (`lib.getExe pkg`)
unify' TDerivation r@(TRec _ _) | derivationWitness r = pure ()
unify' r@(TRec _ _) TDerivation | derivationWitness r = pure ()
-- derivations coerce to STRINGS (outPath) — accepted only where String is
-- the EXPECTED side (`concatStringsSep sep [pkg]`); the reverse (a string
-- where a Derivation is required, `lib.getExe ""`) stays an error
unify' TString TDerivation = pure ()
unify' TString r@(TRec _ _) | derivationWitness r = pure ()
-- compound types: recurse structurally
unify' (TList a) (TList b) = unify a b
unify' (TFun a1 b1) (TFun a2 b2) = unify a1 a2 >> unify b1 b2
unify' (TRec m1 tl1) (TRec m2 tl2) = unifyRec m1 tl1 m2 tl2
-- union: check membership
unify' (TUnion ts) t = unifyUnion ts t
unify' t (TUnion ts) = unifyUnion ts t
-- function vs attrset: try functor protocol
unify' (TFun argT retT) attrsT = unifyFunctor (TFun argT retT) attrsT
unify' attrsT (TFun argT retT) = unifyFunctor (TFun argT retT) attrsT
unify' type1 type2 = typeMismatch type1 type2

-- | bind a variable to a union, stripping the variable's own occurrences first
bindVarUnion :: TypeVar -> NixType -> Infer ()
bindVarUnion v (TUnion ts) =
  case filter (/= TVar v) (concatMap flat ts) of -- CASE-OK: shape dispatch
    [] -> pure ()
    rest
      | any (occursCheck v) rest -> pure ()
    [t] -> bindVar v t
    rest -> bindVar v (TUnion rest)
 where
  flat (TUnion us) = concatMap flat us
  flat t = [t]
bindVarUnion v t = bindVar v t

{- | Bind a type variable to a concrete type. The occurs check protects the
SUBSTITUTION from cycles — it does NOT reject the program: Nix is lazy, so
self-referential VALUES are legal (`let x = [ x ]`, the `overrideScope :
self -> …` fixpoint APIs, `getconf = if isGnu then libc else getconf`) and
`a ~ [a]` is an equirecursive type we cannot represent, not a bug. The
variable is left UNCONSTRAINED — the same degrade 'bindVarUnion' applies to
self-referential join unions. (Throwing here manufactured the "infinite
type" sweep class: 19 files, all valid.)
-}
bindVar :: TypeVar -> NixType -> Infer ()
bindVar v t
  | t == TVar v = pure ()
  | occursCheck v t = pure ()
  | otherwise = addSubst v t

-- | occurs check: does v appear free inside t? (prevents infinite types)
occursCheck :: TypeVar -> NixType -> Bool
occursCheck v (TVar typeVariable') = v == typeVariable'
occursCheck v (TList t) = occursCheck v t
occursCheck v (TFun a b) = occursCheck v a || occursCheck v b
occursCheck v (TRec m tail_) =
  any (occursCheck v . fst) (Map.elems m) || rowHas tail_
 where
  rowHas (ROpen r) = v == r
  rowHas RClosed = False
occursCheck v (TUnion ts) = any (occursCheck v) ts
occursCheck _ _ = False

-- | unify two closed attr sets: all keys must match, required fields must exist
unifyAttrs :: Map Text (NixType, Bool) -> Map Text (NixType, Bool) -> Infer ()
unifyAttrs m1 m2 = forM_ (Set.toList allKeys) reconcile
 where
  allKeys = Set.union (Map.keysSet m1) (Map.keysSet m2)
  reconcile k = step (Map.lookup k m1) (Map.lookup k m2)
   where
    step (Just (t1, o1)) (Just (t2, o2))
      | o1 || o2 = lenientFieldUnify t1 t2
      | otherwise = unify t1 t2
    step (Just (_, False)) Nothing = throwTypeError $ "missing required field: " <> k
    step Nothing (Just (_, False)) = throwTypeError $ "unexpected field (required in other): " <> k
    step _ _ = pure ()

{- | Unify an OPTIONAL (defaulted) record field. A caller's value REPLACES the
default rather than having to agree with it — `kernelParam { module = "msr"; }`
against formals `{ module ? null }` is valid Nix (the "expected Null, got …"
sweep classes: a default RIGIDIFIED its formal and rejected every real
caller). A still-free side is bound (keeps precision); concrete disagreement
is accepted.
-}
lenientFieldUnify :: NixType -> NixType -> Infer ()
lenientFieldUnify t1 t2 = do
  t1' <- applyCurrentSubst t1
  t2' <- applyCurrentSubst t2
  case (t1', t2') of -- CASE-OK: shape dispatch
    (TVar _, _) -> unify t1' t2'
    (_, TVar _) -> unify t1' t2'
    _ -> pure ()

{- | Unify two records, row-variable aware (RC1 core).

  * closed/closed: exact — delegated to 'unifyAttrs'.
  * open/closed: the open side's own required fields must exist in the closed
    side; the open tail var then absorbs the closed side's extra fields and is
    bound CLOSED.
  * open/open: common fields unified, and the two tail vars are bound to a
    SHARED fresh tail carrying each side's extra fields — so the field UNION is
    preserved across the unification (the old 'unifyAttrsOpenOpen' discarded it).

  The anonymous sentinel row var ('isAnonRowVar') is never bound, so pure
  flake/module display types keep their old open-world behavior.
-}
unifyRec :: Map Text (NixType, Bool) -> RowTail -> Map Text (NixType, Bool) -> RowTail -> Infer ()
unifyRec m1 tl1 m2 tl2 = dispatch tl1 tl2
 where
  dispatch RClosed RClosed = unifyAttrs m1 m2
  dispatch (ROpen r1) RClosed = unifyCommon >> closeAgainst r1 only1 only2
  dispatch RClosed (ROpen r2) = unifyCommon >> closeAgainst r2 only2 only1
  dispatch (ROpen r1) (ROpen r2)
    | isAnonRowVar r1 || isAnonRowVar r2 = unifyCommon
    | otherwise = do
        unifyCommon
        r3 <- freshTypeVar
        bindRowVar r1 (TRec only2 (ROpen r3))
        bindRowVar r2 (TRec only1 (ROpen r3))
  only1 = Map.difference m1 m2 -- fields known only on the left
  only2 = Map.difference m2 m1 -- fields known only on the right
  unifyCommon =
    mapM_
      ( \((t1, o1), (t2, o2)) ->
          if o1 || o2 then lenientFieldUnify t1 t2 else unify t1 t2
      )
      (Map.elems (Map.intersectionWith (,) m1 m2))
  -- an open record (tail var r, own-only fields `openOnly`) meeting a closed
  -- side whose extras are `closedExtra`
  closeAgainst r openOnly closedExtra = do
    forM_ (Map.toList openOnly) $ \(k, (_, optional)) ->
      unless optional $
        throwTypeError ("closed record missing field required by open record: " <> k)
    unless (isAnonRowVar r) $ bindRowVar r (TRec closedExtra RClosed)

-- | bind a row variable (with row-occurs check + lacks check; never binds the anon sentinel)
bindRowVar :: TypeVar -> NixType -> Infer ()
bindRowVar r t
  | isAnonRowVar r = pure ()
  -- like 'bindVar': the occurs check protects the SUBSTITUTION, not the
  -- program — lazy self-referential attrsets (crate2nix self/build
  -- co-recursion, the stdenv booter fold) legitimately produce
  -- equirecursive rows we cannot represent; leave the tail unconstrained
  | occursCheck r t = pure ()
  | otherwise = lacksCheck r t >> addSubst r t

{- | Reject binding a row variable to a record that supplies a label the row is
required to LACK — i.e. a label its own record already owns. Without this, the
field merge in 'applySubst' ('resolveRow') would silently keep one type and drop
the other; here it is a type error, which is the sound outcome — the same field
of the same record constrained two incompatible ways. By construction the bind
targets ('Map.difference' extras, a fresh tail) are disjoint from the row's record,
so on well-typed programs this never fires; it ENFORCES that invariant rather than
leaving it to coincidence.
-}
lacksCheck :: TypeVar -> NixType -> Infer ()
lacksCheck r (TRec m2 _) = do
  lacked <- getLacks r
  let clash = Set.intersection (Map.keysSet m2) lacked
  unless (Set.null clash) $
    throwTypeError $
      "conflicting row constraint: field(s) "
        <> T.intercalate ", " (Set.toList clash)
        <> " required absent from an open record but supplied by unification"
lacksCheck _ _ = pure ()

{- | unify a union (sum) type against a concrete type
single-element unions delegate; multi-element checks membership
-}
unifyUnion :: [NixType] -> NixType -> Infer ()
unifyUnion [] _ = pure ()
unifyUnion [t'] t = unify t' t
unifyUnion ts t = do
  t' <- applyCurrentSubst t
  ts' <- mapM applyCurrentSubst ts
  checkUnionMembership t' ts'
 where
  -- flatten nested unions so membership sees the leaves (REVIEW-3 #25) —
  -- on BOTH sides: a union-typed value (an `if` merging Int and Float, say)
  -- is a member when every leaf it could be is a member.
  flatten (TUnion us) = concatMap flatten us
  flatten x = [x]
  -- n.b. 'unify'' routes BOTH argument orders here, so the union may be either
  -- side of the equation: the EXPECTED type (toString's domain — accept when
  -- every leaf of t' fits some member) or the VALUE's type (a list of string
  -- literals used at [String] — accept when every member fits t' as the
  -- expected type). Rejecting the second direction false-positived
  -- e.g. `concatStringsSep "," [ "a" "b" ]`.
  checkUnionMembership t' ts'
    | all leafAccepted (flatten t') = pure ()
    | all (\m -> unionMemberAccepts m t') members = pure ()
    | otherwise =
        throwTypeError $
          "type mismatch: expected one of "
            <> T.intercalate " | " (map prettyType ts)
            <> ", got "
            <> prettyType t'
   where
    members = concatMap flatten ts'
    leafAccepted (TVar _) = True
    leafAccepted leaf = any (unionMemberAccepts leaf) members

{- | Does a union member accept a value of the given type? A member subsumes the
value when it is 'TAny'; when it is 'TString' and the value is a string literal;
when both are lists and the element is accepted; when it is 'TDerivation' and the
value is an attrset carrying a string-coercion witness (@__toString@ / @outPath@ —
how Nix itself decides a set coerces, and how oracle-typed packages appear, since
the pkgs oracle produces records, not 'TDerivation') — otherwise exact equality.
Plain equality wrongly rejected e.g. @toString <list>@: a @[Any]@ member never
structurally equals @["x"]@, yet it should. A bare set (no witness) is still
rejected, since no member subsumes it.
-}
unionMemberAccepts :: NixType -> NixType -> Bool
unionMemberAccepts _ TAny = True
-- a VARIABLE member is unconstrained — it could be the value (join-produced
-- unions like `a | Any -> a` carry vars by construction; two such unions from
-- separate joins must unify, e.g. `optCall lhs … // optCall rhs …`)
unionMemberAccepts _ (TVar _) = True
-- …and symmetrically: an UNRESOLVED leaf could be anything. Top-level var
-- leaves short-circuit in 'checkUnionMembership' already, but a var NESTED
-- in a constructor (`[a]` meeting the declared `Null | [String]`) reaches
-- this table through the list recursion.
unionMemberAccepts (TVar _) _ = True
unionMemberAccepts (TStrLit _) TString = True
-- two DIFFERENT literals: membership between literal unions comes from
-- branch-merged flag lists (`[ "--a" ] ++ optionals c [ "--b" ]`) — demanding
-- literal equality there false-positives on ordinary package files; a literal
-- fits where some literal was expected
unionMemberAccepts (TStrLit _) (TStrLit _) = True
unionMemberAccepts (TList a) (TList b) = unionMemberAccepts a b
-- NESTED unions (a union inside a list/field, e.g. `[String]` meeting the
-- declared `[Path | String]`): only TOP-level unions are flattened before
-- membership, so the recursion must handle a union on either side —
-- compatible when any pairing of leaves is. Falling through to equality
-- here rejected e.g. a `[String]` definition of a `listOf types.path`
-- option (whose honest element type is Path | String).
unionMemberAccepts (TUnion xs) t = any (`unionMemberAccepts` t) xs
unionMemberAccepts t (TUnion ys) = any (unionMemberAccepts t) ys
unionMemberAccepts r@(TRec _ _) TDerivation = derivationWitness r
-- record-vs-record membership is STRUCTURAL, not equality: an OPEN member
-- ("has at least these fields") absorbs any record whose known overlap
-- fits; a CLOSED member accepts an open value when the value's known
-- fields all exist in it at accepted types (the value's tail may narrow
-- to the member). Equality here rejected every union-of-records meeting
-- a row-polymorphic lambda (`map (p: p.name)` over filtered unions).
unionMemberAccepts (TRec m1 (ROpen _)) (TRec m2 _) =
  and
    [ unionMemberAccepts t1 t2
    | (k, (t2, _)) <- Map.toList m2
    , Just (t1, _) <- [Map.lookup k m1]
    ]
unionMemberAccepts (TRec m1 RClosed) (TRec m2 (ROpen _)) =
  and
    [ maybe False (\(t1, _) -> unionMemberAccepts t1 t2) (Map.lookup k m1)
    | (k, (t2, _)) <- Map.toList m2
    ]
unionMemberAccepts a b = a == b

{- | Can this record stand in for a derivation? A CLOSED record must carry a
string-coercion witness (@__toString@ / @outPath@ — how Nix itself decides a
set coerces); an OPEN record's unknown tail may always carry one.
-}
derivationWitness :: NixType -> Bool
derivationWitness (TRec fields RClosed) =
  any (`Map.member` fields) (["__toString", "outPath"] :: [Text])
derivationWitness (TRec _ (ROpen _)) = True
derivationWitness _ = False

-- ── type merging (for branches / polymorphic result combination) ──

{- | merge two types into their least upper bound (join)
differs from unify in that it produces a result rather than asserting equality
-}
mergeTypes :: NixType -> NixType -> Infer NixType
mergeTypes type1 type2 = do
  t1' <- applyCurrentSubst type1
  t2' <- applyCurrentSubst type2
  merge t1' t2'
 where
  -- variable on either side: bind and return — UNLESS the other side already
  -- contains the variable. `if lib.isFunction f then f x else f` (the result ∨
  -- the function itself) and `[ flutter flutter.cacheDir ]` (a value ∨ its own
  -- field) are valid Nix whose least upper bound is the UNION; binding would
  -- turn the join into an occurs-check failure, and a join must produce a
  -- bound, never an error (the nixpkgs sweep's "infinite type" FP class).
  -- a variable joined with NULL keeps its freedom: `if c then f x else null`
  -- must not bind a shared function-result var to Null forever (downstream
  -- calls then "expected Null"); same for two DISTINCT variables — a join
  -- of unknowns is their union, not an alias that collapses independent
  -- formals into one
  merge (TVar v) TNull = pure (TUnion [TVar v, TNull])
  merge TNull (TVar v) = pure (TUnion [TNull, TVar v])
  merge (TVar v) (TVar u) | v /= u = pure (TUnion [TVar v, TVar u])
  merge (TVar v) t
    | v `Set.member` freeTypeVars t = pure (TUnion [TVar v, t])
    | otherwise = bindVar v t >> pure t
  merge t (TVar v)
    | v `Set.member` freeTypeVars t = pure (TUnion [t, TVar v])
    | otherwise = bindVar v t >> pure t
  -- TAny absorbs anything
  merge TAny _ = pure TAny
  merge _ TAny = pure TAny
  -- records: merge field-by-field; the result is open as soon as either
  -- side is (an if of two open records is an open record, NOT a union that
  -- later explodes at `//` or map sites). Tails are joined ANONYMOUSLY — a
  -- join must not constrain either operand's row.
  merge (TRec m1 tl1) (TRec m2 tl2) = mergeAttrsTailed (joinTail tl1 tl2) m1 m2
  merge (TList e1) (TList e2) = TList <$> mergeTypes e1 e2
  -- a JOIN of functions joins the domains too — `unify a1 a2` here asserted
  -- domain EQUALITY, exploding every heterogeneous pipeline-stage list
  -- (`[ attrNames (map f) ... ]`)
  merge (TFun a1 b1) (TFun a2 b2) = do
    dom <- mergeTypes a1 a2
    res <- mergeTypes b1 b2
    pure $ TFun dom res
  -- identical base types: return as-is
  merge a b | a == b = pure a
  -- otherwise: produce a union
  merge a b = pure $ TUnion [a, b]

-- | the tail of a record JOIN: open as soon as either side is open
joinTail :: RowTail -> RowTail -> RowTail
joinTail RClosed RClosed = RClosed
joinTail _ _ = ROpen anonRowVar

{- | merge two record types field-by-field, marking optional any field present
in only one; the caller supplies the result tail (via 'joinTail')
-}
mergeAttrsTailed :: RowTail -> Map Text (NixType, Bool) -> Map Text (NixType, Bool) -> Infer NixType
mergeAttrsTailed tl m1 m2 = do
  fields <- forM (Set.toList keys) field
  pure $ TRec (Map.fromList fields) tl
 where
  keys = Set.union (Map.keysSet m1) (Map.keysSet m2)
  field k = combine (Map.lookup k m1) (Map.lookup k m2)
   where
    combine (Just (t1, o1)) (Just (t2, o2)) = do
      t <- mergeTypes t1 t2
      pure (k, (t, o1 || o2))
    combine (Just (t1, _)) Nothing = pure (k, (t1, True))
    combine Nothing (Just (t2, _)) = pure (k, (t2, True))
    combine Nothing Nothing =
      throwTypeError $ "internal error: key " <> k <> " missing from both attr sets"

{- | constrain a field in a scope type to a specific type
used by `with` scope resolution
-}
fieldConstraint :: Text -> NixType -> NixType -> Infer ()
fieldConstraint name scopeT valueT
  | TAttrs m <- scopeT = lookupAndUnify name valueT m
  | TAttrsOpen m <- scopeT = lookupAndUnify name valueT m
  | TVar _ <- scopeT = do
      r <- freshTypeVar
      let fieldType = TRec (Map.singleton name (valueT, False)) (ROpen r)
      unify scopeT fieldType
  | otherwise = pure ()
 where
  lookupAndUnify k v m = maybe (pure ()) (\(ft, _) -> unify v ft) (Map.lookup k m)
