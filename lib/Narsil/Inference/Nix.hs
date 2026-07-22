{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                   // nix // infer
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "They set a slamhound on Turner's trail."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The inference engine: a syntax-directed walk over the hnix AST ('infer') that
--   assigns a 'NixType' to every node, in the Hindley–Milner / Algorithm-W
--   tradition. Three rules carry the polymorphism:
--
--     * at a VARIABLE use ('inferSymbol') the variable's 'Scheme' is INSTANTIATED
--       — a fresh copy, so each use can specialise differently.
--     * at an APPLICATION ('inferApp') the function's parameter type is UNIFIED
--       with the argument's type, and the result type falls out.
--     * at a `let` ('inferLet' / 'inferLetGroup') each binding's inferred type is
--       GENERALISED into a 'Scheme' (closed over the vars the environment does not
--       mention), so the bound name is polymorphic downstream. Bindings are first
--       grouped into strongly-connected components ('stronglyConnComp'), so a set
--       of mutually-recursive definitions is solved together and the rest in
--       dependency order.
--
--   The Nix-specific parts layer on top: attrsets are row-polymorphic records
--   (open until proven closed); `with` opens a dynamically-scoped field lookup
--   (memoised in the state); `import` is resolved and its file typed; builtins
--   carry hand-written schemes ('builtinEnv'). The whole engine is pure ('Infer'
--   = State + Except, no IO) — which is exactly what lets the oracle replay it.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Inference.Nix (
  -- * Inference
  inferExpr,
  inferModuleExpr,
  inferExprWithEnv,
  inferExprBindingsPartial,
  inferFile,
  runInfer,
  unify,

  -- * Environment
  TypeEnv (..),
  emptyEnv,
  builtinEnv,
  extendEnv,
  extendImport,
  extendImports,
  extendCallPackage,
  lookupEnv,
  lookupImport,
  lookupCallPackage,

  -- * Results
  InferResult (..),
  Binding (..),
)
where

import Control.Exception (IOException, try)
import Control.Monad (foldM, forM, forM_, replicateM)
import Control.Monad.State.Strict (gets, modify)
import Data.Coerce (coerce)
import Data.Fix (Fix (..))
import Data.Foldable (toList)
import Data.Functor.Compose (Compose (..))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Inference.Nix.Builtins
import Narsil.Inference.Nix.Constraint
import Narsil.Inference.Nix.Environment
import Narsil.Inference.Nix.Module qualified as Module
import Narsil.Inference.Nix.Scheme
import Narsil.Inference.Nix.Type
import Narsil.Inference.Nix.Unify
import Narsil.Layout.Edge qualified as Edge
import Narsil.Layout.Import (checkImportBuiltin)
import Narsil.Syntax.Annotation (
  normalizeStaticKeys,
  srcSpanToSpan,
  varNameText,
  pattern Layer,
  pattern LayerAnn,
 )
import Nix.Atoms (NAtom (..))
import Nix.Expr.Types hiding (Binding)
import Nix.Expr.Types qualified as Nix
import Nix.Expr.Types.Annotated (AnnUnit (..), NExprLoc, nullSpan)
import Nix.Parser (parseNixFileLoc)
import Nix.Utils qualified as Nix

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- instantiation
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- inference: expression-level helpers
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | atom → type mapping (NAtom → NixType)
inferAtom :: NAtom -> Infer NixType
inferAtom = pure . atomType

-- | string: literal strings get TStrLit, interpolated strings get TString
inferStr :: NString NExprLoc -> Infer NixType
inferStr (DoubleQuoted [Plain t]) = pure $ TStrLit t
inferStr _ = pure TString

{- | list: infer element type from first element, merge remaining against it
empty list gets a fresh (unconstrained) element type variable
-}
inferList :: TypeEnv -> [NExprLoc] -> Infer NixType
inferList _environment [] = do
  elemType <- freshVar
  pure $ TList elemType
inferList environment (x : xs) = do
  elemType <- infer environment x
  finalElemType <- foldM (\acc e -> infer environment e >>= mergeListElems acc) elemType xs
  pure $ TList finalElemType

{- | The element JOIN for list literals and @++@: like 'mergeTypes', except a
still-FREE variable element is never bound to its concrete siblings — Nix
lists are heterogeneous by design (`imports = [ ./hw.nix extraConfig ]`,
`[ ./path param ]`), and binding rigidified a lambda formal or SCC
placeholder to the first concrete element for every OTHER use site too.
Concrete pairs still merge (homogeneous precision, field-level attr joins).
-}
mergeListElems :: NixType -> NixType -> Infer NixType
mergeListElems t1 t2 = do
  t1' <- applyCurrentSubst t1
  t2' <- applyCurrentSubst t2
  case (t1', t2') of -- CASE-OK: shape dispatch
    (TVar _, TVar _) | t1' == t2' -> pure t1'
    (TVar _, _) -> pure $ TUnion [t1', t2']
    (_, TVar _) -> pure $ TUnion [t1', t2']
    _ -> mergeTypes t1' t2'

{- | attrset: infer all bindings, wrap as closed TAttrs — unless a DYNAMIC
key binding (`{ ${name} = v; }`) is present: then the set has fields we
cannot name statically, so it is an OPEN record (anon row — selects
succeed without accumulating requirements). Previously dynamic bindings
were silently DROPPED, leaving a closed record missing real fields.
-}
inferAttrSet :: Recursivity -> TypeEnv -> [Nix.Binding NExprLoc] -> Infer NixType
inferAttrSet recursive environment bindings = do
  fields <- inferBindings (recursive == Recursive) environment bindings
  let fieldMap = Map.fromList $ map (\(k, t) -> (k, (t, False))) fields
  if any dynamicKeyBinding bindings
    then pure $ TRec fieldMap (ROpen anonRowVar)
    else pure $ TAttrs fieldMap
 where
  dynamicKeyBinding (Nix.NamedVar path _ _) = any isDynKey (toList path)
  dynamicKeyBinding _ = False
  isDynKey (DynamicKey _) = True
  isDynKey _ = False

{- | if-then-else: condition must be bool, branches merged under occurrence
narrowing — each branch sees variables refined by what the condition
establishes there (`x != null`, `builtins.isString x`, …)
-}
inferIf :: TypeEnv -> NExprLoc -> NExprLoc -> NExprLoc -> Infer NixType
inferIf environment cond thenE elseE = do
  condT <- infer environment cond
  unify condT TBool
  thenEnv <- narrowEnv environment cond True
  elseEnv <- narrowEnv environment cond False
  thenT <- infer thenEnv thenE
  elseT <- infer elseEnv elseE
  mergeTypes thenT elseT

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- inference: occurrence narrowing
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | The shape a condition can assert a variable has (or lacks). Mirrors the
`builtins.is*` predicate family plus null equality.
-}
data Shape
  = ShNull
  | ShInt
  | ShFloat
  | ShBool
  | ShString
  | ShList
  | ShAttrs
  | ShFunction
  | ShPath
  deriving (Eq, Show)

{- | a positive or negative shape fact about one variable, or the fact that
an attrset variable HAS a given field (`args ? hardeningDisable`)
-}
data TypeFact = MustBe Shape | MustNot Shape | HasField Text
  deriving (Eq, Show)

{- | The facts a condition establishes about PLAIN VARIABLES when it evaluates
to the given polarity. Purely syntactic: `x == null` / `x != null` (either
operand order), the `isNull`/`builtins.is*` predicates, `&&` under True, `||`
under False, and `!` flipping polarity. `&&` under False (and `||` under True)
yield nothing — we cannot know which conjunct failed. Facts about attribute
PATHS (`cfg.foo != null`) are not extracted yet; plain formals dominate the
sweep's null classes.
-}
narrowingsFrom :: NExprLoc -> Bool -> [(Text, TypeFact)]
narrowingsFrom (Layer (NBinary NEq a b)) pol
  | Just x <- symOf a, isNullLit b = [(x, if pol then MustBe ShNull else MustNot ShNull)]
  | Just x <- symOf b, isNullLit a = [(x, if pol then MustBe ShNull else MustNot ShNull)]
narrowingsFrom (Layer (NBinary NNEq a b)) pol
  | Just x <- symOf a, isNullLit b = [(x, if pol then MustNot ShNull else MustBe ShNull)]
  | Just x <- symOf b, isNullLit a = [(x, if pol then MustNot ShNull else MustBe ShNull)]
  -- `x.attr or null != null` proves x HAS the attr in the true branch
  | Just (x, k) <- selOrNullOf a, isNullLit b, pol = [(x, HasField k)]
  | Just (x, k) <- selOrNullOf b, isNullLit a, pol = [(x, HasField k)]
narrowingsFrom (Layer (NBinary NAnd a b)) True = narrowingsFrom a True ++ narrowingsFrom b True
narrowingsFrom (Layer (NBinary NOr a b)) False = narrowingsFrom a False ++ narrowingsFrom b False
narrowingsFrom (Layer (NUnary NNot e)) pol = narrowingsFrom e (not pol)
narrowingsFrom (Layer (NApp p (Layer (NSym x)))) pol
  | Just sh <- predicateShape p =
      [(varNameText x, if pol then MustBe sh else MustNot sh)]
-- `x ? field` under True: selecting the field in the guarded region must not
-- constrain the row to REQUIRE it — the guard proves presence at runtime, and
-- requiring it would reject every caller that legitimately omits it (the
-- `hardeningDisable` sweep class). Absence under False carries no usable fact.
narrowingsFrom (Layer (NHasAttr b (StaticKey k :| []))) True
  | Just x <- symOf b = [(x, HasField (varNameText k))]
narrowingsFrom _ _ = []

symOf :: NExprLoc -> Maybe Text
symOf (Layer (NSym x)) = Just (varNameText x)
symOf _ = Nothing

-- | @x.attr or null@ — the guarded-presence probe (variable base, one static key)
selOrNullOf :: NExprLoc -> Maybe (Text, Text)
selOrNullOf (Layer (NSelect (Just def) b (StaticKey k :| [])))
  | isNullLit def, Just x <- symOf b = Just (x, varNameText k)
selOrNullOf _ = Nothing

isNullLit :: NExprLoc -> Bool
isNullLit (Layer (NConstant NNull)) = True
isNullLit _ = False

{- | The shape asserted by a type predicate: bare (`isFunction`, via
@with lib@) or behind ANY select path ending in the predicate name
(`builtins.isString`, `lib.isFunction`, `pkgs.lib.isFunction` — the
make-test-python guard). Matching on the final key alone is safe: the
only effect is branch-local shadowing, never unification.
-}
predicateShape :: NExprLoc -> Maybe Shape
predicateShape (Layer (NSym n)) = shapeOfPredicate (varNameText n)
predicateShape (Layer (NSelect Nothing _ path))
  | StaticKey k <- NE.last path = shapeOfPredicate (varNameText k)
predicateShape _ = Nothing

shapeOfPredicate :: Text -> Maybe Shape
shapeOfPredicate "isNull" = Just ShNull
shapeOfPredicate "isInt" = Just ShInt
shapeOfPredicate "isFloat" = Just ShFloat
shapeOfPredicate "isBool" = Just ShBool
shapeOfPredicate "isString" = Just ShString
shapeOfPredicate "isList" = Just ShList
shapeOfPredicate "isAttrs" = Just ShAttrs
shapeOfPredicate "isFunction" = Just ShFunction
shapeOfPredicate "isPath" = Just ShPath
-- lib.isDerivation: a derivation IS an attrset (`_type = "derivation"`)
shapeOfPredicate "isDerivation" = Just ShAttrs
shapeOfPredicate _ = Nothing

-- | does a concrete type constructor match a shape?
shapeMatches :: Shape -> NixType -> Bool
shapeMatches ShNull TNull = True
shapeMatches ShInt TInt = True
shapeMatches ShFloat TFloat = True
shapeMatches ShBool TBool = True
shapeMatches ShString TString = True
shapeMatches ShString (TStrLit _) = True
shapeMatches ShList (TList _) = True
shapeMatches ShAttrs (TRec _ _) = True
shapeMatches ShAttrs TDerivation = True
shapeMatches ShFunction (TFun _ _) = True
shapeMatches ShPath TPath = True
shapeMatches _ _ = False

-- | the canonical type for a shape (used when narrowing has nothing to keep)
shapeType :: Shape -> Infer NixType
shapeType ShNull = pure TNull
shapeType ShInt = pure TInt
shapeType ShFloat = pure TFloat
shapeType ShBool = pure TBool
shapeType ShString = pure TString
shapeType ShList = TList <$> freshVar
shapeType ShAttrs = do
  v <- freshVar
  case v of -- CASE-OK: shape dispatch
    TVar row -> pure (TRec Map.empty (ROpen row))
    _ -> pure TAny
shapeType ShFunction = TFun <$> freshVar <*> freshVar
shapeType ShPath = pure TPath

{- | Refine a type by a fact, NEVER erroring: narrowing is advisory (the
refined binding merely SHADOWS the original in the branch env — no
unification happens, so nothing leaks out of the branch).

  * MustBe: keep the union leaves that could match (plus vars/Any); if
    nothing remains — including a concrete non-matching type, i.e. a branch
    the guard proves unreachable — fall back to the shape's canonical type.
  * MustNot: drop the matching leaves; a type that IS the shape (e.g. Null
    under `!= null`) becomes a fresh var: "some value we know nothing about,
    except that it isn't that shape".
-}
refineType :: TypeFact -> NixType -> Infer NixType
refineType fact t0 = do
  t <- applyCurrentSubst t0
  case (fact, t) of -- CASE-OK: shape dispatch
    (_, TAny) -> pure TAny
    -- field made OPTIONAL in the shadow: select finds it (no row extension),
    -- but callers omitting it stay accepted
    (HasField k, TRec fields row)
      | not (Map.member k fields) -> do
          fieldTy <- freshVar
          pure (TRec (Map.insert k (fieldTy, True) fields) row)
    (HasField _, other) -> pure other
    -- Under MustBe the guard PROVES the shape, so flexible leaves (vars/Any)
    -- add nothing: the concrete matching leaves are the description — or the
    -- shape itself when none match. Keeping a var leaf here let
    -- `if conf == null then stringLength conf …` slip through on `Null | α`.
    (MustBe sh, TUnion ts) -> case filter (shapeMatches sh) ts of -- CASE-OK: shape dispatch
      [] -> shapeType sh
      [one] -> pure one
      kept -> pure (TUnion kept)
    (MustBe sh, TVar _) -> shapeType sh
    (MustBe sh, other)
      | shapeMatches sh other -> pure other
      | otherwise -> shapeType sh
    (MustNot sh, TUnion ts) -> case filter (not . shapeMatches sh) ts of -- CASE-OK: shape dispatch
      [] -> freshVar
      [one] -> pure one
      kept -> pure (TUnion kept)
    -- a still-free var gets a FRESH shadow: constraints accumulated inside
    -- the branch must not bind the original var outside it
    (MustNot _, TVar _) -> freshVar
    (MustNot sh, other)
      | shapeMatches sh other -> freshVar
      | otherwise -> pure other

{- | Shadow every variable the condition narrows with its refined type. Only
monomorphic bindings (λ-formals, let-bound monotypes) are refined —
polymorphic schemes re-instantiate per use and don't carry a nullable
identity worth narrowing.
-}
narrowEnv :: TypeEnv -> NExprLoc -> Bool -> Infer TypeEnv
narrowEnv environment cond polarity =
  foldM app1 environment (narrowingsFrom cond polarity)
 where
  app1 env (x, fact)
    | Just (Forall [] t) <- lookupEnv x env = do
        t' <- refineType fact t
        pure (extendEnv x (Forall [] t') env)
    | otherwise = pure env

{- | with: scope expr provides attr type, body sees fields via dynamic lookup
n.b. the memo cache prevents repeated unification for the same field
-}
inferWith :: TypeEnv -> NExprLoc -> NExprLoc -> Infer NixType
inferWith environment scope body = do
  scopeT <- infer environment scope
  let environment' = environment{envWith = scopeT : envWith environment}
  oldMemo <- gets inferWithMemo
  modify $ \s -> s{inferWithMemo = Map.empty}
  resultT <- infer environment' body
  modify $ \s -> s{inferWithMemo = oldMemo}
  pure resultT

{- | assert: condition must be bool, then infer body — under the condition's
positive narrowing (`assert x != null; …` guarantees non-null in the body)
-}
inferAssert :: TypeEnv -> NExprLoc -> NExprLoc -> Infer NixType
inferAssert environment cond body = do
  condT <- infer environment cond
  unify condT TBool
  bodyEnv <- narrowEnv environment cond True
  infer bodyEnv body

{- | function application: unify func type as TFun arg result, return result.
n.b. intercepts two cross-module forms when the closure has precomputed their types:
@callPackage ./path { }@ (the head is @callPackage <literal>@) resolves to the
package's RESULT type, and @import ./path@ — the head must BE @import@ /
@builtins.import@ ('checkImportBuiltin'); any other function applied to a known
path literal (@dirOf ./dep.nix@) is ordinary application — resolves to the
imported file's type. Both fall back to ordinary unifying application when the
env has nothing for them. A resolved type is instantiated with fresh variables:
it was inferred by a separate run whose type variables are not ours, so splicing
it in verbatim captures variables (spurious infinite-type errors).
-}
inferAppWithImport :: TypeEnv -> NExprLoc -> NExprLoc -> Infer NixType
inferAppWithImport environment func arg =
  maybe viaImportArg viaCallPackage (callPackageLiteral func)
 where
  viaCallPackage cpPath =
    maybe unresolvedCallPackage useResolved (lookupCallPackage cpPath environment)
  -- an UNRESOLVED callPackage call must not fall through to ordinary
  -- application: that binds the one shared `callPackage` formal to
  -- `TFun TAny r`, entangling every call site's result in a single var
  -- (the first site's constraints then poison the rest). The result is a
  -- package we know nothing about — a fresh var per site, honestly.
  unresolvedCallPackage = do
    _ <- infer environment func
    _ <- infer environment arg
    freshVar
  viaImportArg
    | Just () <- checkImportBuiltin func =
        maybe fallback viaImport (extractImportPathLiteral arg)
    | otherwise = fallback
  viaImport importPath =
    maybe fallback useResolved (lookupImport importPath environment)
  fallback = inferApp environment func arg
  useResolved resolvedType = do
    _ <- infer environment func
    _ <- infer environment arg
    instantiated <- instantiate (Forall (Set.toList (freeTypeVars resolvedType)) resolvedType)
    applyCurrentSubst instantiated

-- | extract a literal file path from an expression (for import resolution)
extractImportPathLiteral :: NExprLoc -> Maybe FilePath
extractImportPathLiteral (Layer (NLiteralPath (Nix.Path p))) = Just p
extractImportPathLiteral (Layer (NStr (DoubleQuoted [Plain t]))) = Just (T.unpack t)
extractImportPathLiteral (Layer (NStr (Indented _ [Plain t]))) = Just (T.unpack t)
extractImportPathLiteral _ = Nothing

{- | the literal path of a @callPackage ./path@ / @callPackages ./path@ head —
the SAME matcher the closure walker seeds 'envCallPackageTypes' from
('Narsil.Layout.Edge'), so intercept and seed can never drift apart
-}
callPackageLiteral :: NExprLoc -> Maybe FilePath
callPackageLiteral = Edge.callPackageHeadOf

{- | The condition guarding an application argument, when the head is a
partially-applied conditional combinator: in
@lib.optionalString (x != null) x@ the second argument is only EVALUATED
when the condition held, so it may be inferred under the condition's
positive narrowing. Matches the lib combinators whose second argument is
condition-guarded, as a bare name (via @with lib@) or one @lib.@ select
deep.
-}
guardedCombArg :: NExprLoc -> Maybe NExprLoc
guardedCombArg (Layer (NApp combHead cond))
  | isGuardedComb combHead = Just cond
 where
  isGuardedComb (Layer (NSym n)) = varNameText n `elem` guardedCombs
  isGuardedComb (Layer (NSelect Nothing base (StaticKey k :| [])))
    | isNamespaceVar "lib" base = varNameText k `elem` guardedCombs
  isGuardedComb _ = False
  guardedCombs = ["optionalString", "optional", "optionals", "optionalAttrs", "mkIf"] :: [Text]
guardedCombArg _ = Nothing

-- | function application: unify func type as TFun arg result, return result
inferApp :: TypeEnv -> NExprLoc -> NExprLoc -> Infer NixType
inferApp environment func arg = do
  funcT <- infer environment func
  argT <- case guardedCombArg func of -- CASE-OK: shape dispatch
    Nothing -> infer environment arg
    -- a guarded argument only ever EVALUATES when the condition held; when
    -- the condition's correlation is beyond the narrowing table (a plain
    -- boolean flag guarding a field the flag implies), degrade the
    -- argument to unknown instead of reporting an error the runtime
    -- cannot reach ('catchInfer' rolls back the failed attempt's state)
    Just c -> do
      argEnv <- narrowEnv environment c True
      infer argEnv arg `catchInfer` freshVar
  resultT <- freshVar
  funcT' <- applyCurrentSubst funcT
  case funcT' of -- CASE-OK: shape dispatch
  -- An UNCONSTRAINED function var — a λ-bound formal like `symlinkJoin` /
  -- `fetchFromGitHub`, injected by callPackage. HM monomorphizes λ-bound
  -- vars, so unifying the argument here would rigidify the FIRST call
  -- site's record as THE domain and reject every differently-shaped call —
  -- the nixpkgs sweep's "unexpected field / missing required field" FP
  -- class (~180 files). These functions are used record-polymorphically in
  -- real Nix and we know nothing about their true domain, so constrain the
  -- var only to "function yielding resultT" (the argument was still
  -- inferred, so errors INSIDE it surface). Known functions — lambda
  -- literals, let-bound schemes, oracle-typed @pkgs.*@ — keep full domain
  -- checking.
    TVar _ -> unify funcT' (TFun TAny resultT) >> applyCurrentSubst resultT
    _ -> unify funcT' (TFun argT resultT) >> applyCurrentSubst resultT

{- | attribute select @e.name@: look up name in e's attr type.
n.b. fixes S2 from review-2: a missing key on a *closed* attrset is a type
error. Open attrsets may legitimately have more fields, so a miss there is
just a fresh polymorphic var.
The 'hasDefault' parameter (from @attrs.x or default@) suppresses the error,
because the source has explicitly declared "ok if missing".
-}

{- | If a @pkgs.<path>@ selection has a static prefix the caller precomputed (into
'envPkgsOracle'), start the selection fold from that prefix's type instead of from
the opaque @pkgs@ value — the seam where the nixpkgs eval backend enriches
inference. The LONGEST matching prefix wins, so a fully-precomputed path resolves
directly AND a path one step beyond a known (closed) record selects through it —
turning a bogus nixpkgs attribute into a real "missing attribute" error. Syntactic
on the @pkgs@ base, like 'builtinsFieldScheme'; a path with no matching prefix (or
a non-@pkgs@ base) falls back to the normal opaque handling.
-}
pkgsOracleStart ::
  TypeEnv ->
  NExprLoc ->
  NonEmpty (NKeyName NExprLoc) ->
  Maybe (NixType, [NKeyName NExprLoc])
pkgsOracleStart environment base path
  | isNamespaceVar "pkgs" base = longest (length staticPrefix)
  | otherwise = Nothing
 where
  keyList = toList path
  staticPrefix = leadingStaticKeys keyList
  longest 0 = Nothing
  longest i =
    maybe
      (longest (i - 1))
      (\t -> Just (t, drop i keyList))
      (Map.lookup (take i staticPrefix) (envPkgsOracle environment))

-- | The leading run of static attribute keys (as text), stopping at the first dynamic key.
leadingStaticKeys :: [NKeyName NExprLoc] -> [Text]
leadingStaticKeys (StaticKey k : rest) = varNameText k : leadingStaticKeys rest
leadingStaticKeys _ = []

inferSelect :: TypeEnv -> NExprLoc -> NonEmpty (NKeyName NExprLoc) -> Bool -> Infer NixType
inferSelect environment base path hasDefault =
  maybe fromBase fromOracle (pkgsOracleStart environment base path)
 where
  -- Fold the WHOLE dotted path, not just the first key. The dispatch used to
  -- match `(attr :| _)`, silently dropping `b.c` from `x.a.b.c` (so the genuine
  -- "cannot select b from an Int" error was never produced). `expr or default`
  -- suppresses the missing-key error at every level, matching Nix semantics.
  fromBase = do
    baseT <- infer environment base
    foldM selectStep baseT (toList path)
  -- pkgs.<path>: start from the oracle's longest-prefix type, then select the rest
  -- through it — so a bogus attribute hits the closed-record miss below.
  fromOracle (startTy, restKeys) = foldM selectStep startTy restKeys
  selectStep baseTy attr = do
    t' <- applyCurrentSubst baseTy
    resolve t' (keyOf attr)
  keyOf (StaticKey k) = Just (varNameText k)
  keyOf (DynamicKey _) = Nothing

  -- closed record: key must be present (unless `or default`)
  resolve (TRec fields RClosed) (Just k) = maybe closedMiss (pure . fst) (Map.lookup k fields)
   where
    closedMiss
      | hasDefault = freshVar
      | otherwise =
          throwTypeError $
            "attribute '"
              <> k
              <> "' missing on closed attribute set (keys: "
              <> T.intercalate ", " (Map.keys fields)
              <> ")"
  -- open record: a missing key EXTENDS the row through its tail var, so
  -- repeated selections accumulate (`x.a` then `x.b` ⟹ `{a,b|ρ}`).
  resolve (TRec fields (ROpen r)) (Just k) = maybe openMiss (pure . fst) (Map.lookup k fields)
   where
    openMiss
      | hasDefault || isAnonRowVar r = freshVar
      | otherwise = do
          fieldTy <- freshVar
          r' <- freshTypeVar
          bindRowVar r (TRec (Map.singleton k (fieldTy, False)) (ROpen r'))
          pure fieldTy
  -- selection on a VARIABLE emits a row constraint α ~ { k : β | ρ }
  -- (RC1 #2 — was a silent freshVar, so `(x: x.foo) 5` wrongly passed).
  resolve t'@(TVar _) (Just k)
    -- lenient mode is the definition re-checking path: unresolved scope
    -- names must not accumulate row constraints there
    | hasDefault || envLenient environment = freshVar
    | otherwise = do
        fieldTy <- freshVar
        r <- freshTypeVar
        unify t' (TRec (Map.singleton k (fieldTy, False)) (ROpen r))
        pure fieldTy
  resolve TAny (Just _) = freshVar
  -- a derivation is an open value bag: beyond the modeled core (outPath,
  -- meta, version, …) it carries arbitrary passthru attributes, so any
  -- select is legitimate (`codeium.meta`, `pkg.pytestFlagsArray`)
  resolve TDerivation (Just _) = freshVar
  -- selection from a UNION scrutinee (a join like `if isFunction f then f x
  -- else f`): valid Nix whenever SOME member is selectable at runtime. A
  -- unique concrete record member keeps precise selection (its missing-field
  -- errors are still real); several possibilities degrade to dynamic. Only
  -- when NO member could ever be an attrset is selection a genuine error.
  resolve (TUnion ts) (Just k) =
    case filter selectable leaves of -- CASE-OK: shape dispatch
      [] -> resolveMiss (TUnion ts) k
      [TRec fields tail_] -> resolve (TRec fields tail_) (Just k)
      _ -> freshVar
   where
    leaves = concatMap flattenU ts
    flattenU (TUnion us) = concatMap flattenU us
    flattenU t = [t]
    selectable (TRec _ _) = True
    selectable (TVar _) = True
    selectable TAny = True
    selectable TDerivation = True
    selectable _ = False
  -- a FUNCTION base may be a callable attrset (makeOverridable results
  -- carry .override / .overrideAttrs alongside __functor) — selection is
  -- legitimate, the field type unknown
  resolve (TFun _ _) (Just _) = freshVar
  -- n.b. selecting from a KNOWN Null stays an error by policy — the
  -- lazily-guarded null-base idiom (types.nix defaultTypeMerge) is in the
  -- accepted-FP ledger; trading it away would also mask `let x = null; in
  -- x.field`, which is the product.
  -- selecting a static key from a concrete non-attrset is a type error
  -- (e.g. `x.a.b` where `x.a : Int`)
  resolve t' (Just k) = resolveMiss t' k
  -- dynamic key (`x.${e}`): not statically resolvable
  resolve _ _ = freshVar
  resolveMiss t' k
    | hasDefault = freshVar
    | otherwise =
        throwTypeError $
          "cannot select attribute '" <> k <> "' from non-attrset type " <> prettyType t'

{- | @e ? attr@: returns Bool. We additionally check that any dynamic-key
antiquotations type-check correctly (S4 from review-2 — previously the path
was ignored entirely).
-}
inferHasAttr :: TypeEnv -> NExprLoc -> NAttrPath NExprLoc -> Infer NixType
inferHasAttr environment base attrPath = do
  _ <- infer environment base
  mapM_ checkKey attrPath
  pure TBool
 where
  checkKey (StaticKey _) = pure ()
  checkKey (DynamicKey (Plain _)) = pure ()
  checkKey (DynamicKey EscapedNewline) = pure ()
  checkKey (DynamicKey (Antiquoted e)) = do
    t <- infer environment e
    -- The antiquoted expression must be a string (Nix coerces here)
    unify t TString

-- | unary ops: negation requires a number (Float passes through), not requires bool
inferUnary :: TypeEnv -> NUnaryOp -> NExprLoc -> Infer NixType
inferUnary environment op e = do
  t <- infer environment e
  apply op t
 where
  apply NNeg t = do
    t' <- applyCurrentSubst t
    case t' of -- CASE-OK: shape dispatch
      TFloat -> pure TFloat
      _ -> unify t' TInt >> pure TInt
  apply NNot t = unify t TBool >> pure TBool

{- | symbol resolution: lookup in env, or fall through to `with` scope, or error.
n.b. fixes S3 from review-2: previously fell through to 'freshVar' for any unbound
name. Now we error unless the name is in env or under an enclosing 'with' scope.
'envWithBypass' lets call paths opt into the old behavior — used at the top
level where some legitimate imports/builtins arrive un-modeled.
-}
inferSymbol :: TypeEnv -> Text -> Infer NixType
inferSymbol environment symbolName
  | Just scheme <- lookupEnv symbolName environment = instantiate scheme
  | not (null (envWith environment)) = resolveWithScopes symbolName (envWith environment)
  | envLenient environment = freshVar
  | otherwise = throwTypeError $ "unbound variable: " <> symbolName
 where
  -- Resolve via the ENCLOSING `with` scopes, innermost first (Nix searches
  -- them all, inner shadowing outer). A CLOSED record without the field is a
  -- definite miss — fall through to the next scope. The innermost definite
  -- hit resolves precisely. When only indefinite scopes remain (open rows,
  -- vars, Any — the field MAY be there), a SOLE candidate keeps the precise
  -- constraining behavior ('fieldConstraint', memoized); several candidates
  -- degrade to a fresh var — the name could come from any of them, and
  -- constraining one would wrongly require it there.
  resolveWithScopes name scopeTypes = do
    resolved <- mapM applyCurrentSubst scopeTypes
    walk resolved
   where
    walk [] = throwTypeError $ "unbound variable: " <> symbolName
    walk (s : rest) = case definiteLookup s of -- CASE-OK: shape dispatch
      Hit t -> pure t
      Miss -> walk rest
      Indefinite
        | all definitelyMisses rest -> constrainScope name s
        | otherwise -> freshVar
    definitelyMisses s = case definiteLookup s of -- CASE-OK: shape dispatch
      Miss -> True
      _ -> False
    definiteLookup (TRec fields RClosed) =
      maybe Miss (Hit . fst) (Map.lookup name fields)
    definiteLookup (TRec fields (ROpen _))
      | Just (t, _) <- Map.lookup name fields = Hit t
    definiteLookup _ = Indefinite
  -- constrain the field in one scope type, memoized per with-body
  constrainScope name scopeType = do
    memo <- gets inferWithMemo
    maybe compute pure (Map.lookup name memo)
   where
    compute = do
      typeVar <- freshVar
      resolvedScope <- applyCurrentSubst scopeType
      fieldConstraint name resolvedScope typeVar
      resolvedType <- applyCurrentSubst typeVar
      modify $ \inferState ->
        inferState{inferWithMemo = Map.insert name resolvedType (inferWithMemo inferState)}
      pure resolvedType

-- | the three outcomes of statically looking a name up in one @with@ scope
data WithLookup = Hit NixType | Miss | Indefinite

-- | binary ops: each operator has specific type constraints
inferBinary :: TypeEnv -> NBinaryOp -> NExprLoc -> NExprLoc -> Infer NixType
inferBinary environment op left right = do
  leftT <- infer environment left
  -- `&&` and `||` are LAZY guards: the right operand only evaluates when
  -- the left held (resp. failed), so it is inferred under the left's
  -- narrowing — `pool != null && s ? ${pool}`, `!(l ? ssl) || l.ssl`
  rightEnv <- case op of -- CASE-OK: shape dispatch
    NAnd -> narrowEnv environment left True
    NImpl -> narrowEnv environment left True
    NOr -> narrowEnv environment left False
    _ -> pure environment
  rightT <- infer rightEnv right
  apply op leftT rightT
 where
  -- comparison: `==`/`!=` are TOTAL in Nix and never type-error, so we must
  -- NOT unify the operands — `x == null` with `x : Int` is legal and idiomatic.
  -- Operands are still inferred above (for their own checking); we just don't
  -- relate them. (Previously `unify leftT rightT` false-positived on `x == null`.)
  apply NEq _ _ = pure TBool
  apply NNEq _ _ = pure TBool
  -- ordering comparison: Nix `<` is POLYMORPHIC — numbers (Int/Float mixed),
  -- strings, paths, and lists compare; unifying both sides to Int (the old
  -- rule) rejected `version < "3.11"` all over nixpkgs. Like `+`, the check
  -- is non-binding: only two CONCRETE operands of different (or
  -- non-comparable) kinds error; a variable operand is left unconstrained.
  apply NLt l r = comparison l r
  apply NLte l r = comparison l r
  apply NGt l r = comparison l r
  apply NGte l r = comparison l r
  -- boolean logic
  apply NAnd l r = unify l TBool >> unify r TBool >> pure TBool
  apply NOr l r = unify l TBool >> unify r TBool >> pure TBool
  apply NImpl l r = unify l TBool >> unify r TBool >> pure TBool
  -- nix `+` is heterogeneous: Int/Float numeric add (Int+Float = Float),
  -- String concat, and Path concat (Path+String = Path, String+Path = String).
  -- When one side is still a variable, the result is the JOIN over what the
  -- var could legally be — `+` combines UNLIKE types, so unifying the
  -- operands is wrong twice over: `path + "/lib"` neither makes `path` a
  -- string literal nor forbids it being a Path (the old unify-to-propagate
  -- rejected exactly that, all over nixpkgs CI). When both sides are concrete
  -- we use the +-lattice instead of demanding equality.
  apply NPlus leftT rightT = do
    l <- applyCurrentSubst leftT
    r <- applyCurrentSubst rightT
    plus l r
   where
    plus TAny _ = pure TAny
    plus _ TAny = pure TAny
    plus lv@(TVar _) (TVar _) = pure lv
    -- a coercible operand meeting a VARIABLE coerces before the var-side
    -- machinery runs (`drv + x` — the record row contributes nothing to
    -- the +-table directly)
    plus l rv@(TVar _) | plusCoercible l = plus TString rv
    plus lv@(TVar _) r | plusCoercible r = plus lv TString
    plus lv@(TVar _) r = varSidePlus lv r (\k -> combine k (norm r))
    plus l rv@(TVar _) = varSidePlus rv l (combine (norm l))
    -- a UNION operand resolves per-member, non-binding: concrete members
    -- contribute their +-row against the known side (`Path | String +
    -- "/config"` — BOTH combine; `Null | String + " "` — the String arm
    -- does, the Null arm is the correlation-guarded case), and a flex leaf
    -- (join var) contributes the var side's surviving rows. A unique
    -- result is the answer; ambiguity goes fresh; only a union NONE of
    -- whose members could ever combine is an error.
    plus lu@(TUnion _) r = unionSidePlus lu (\k -> combine k (norm r))
    plus l ru@(TUnion _) = unionSidePlus ru (combine (norm l))
    -- a derivation-like operand (TDerivation, or a record carrying/able to
    -- carry the string-coercion witness) coerces to its outPath STRING under
    -- `+` — `clang-unwrapped + "/bin"` where the formal's row was constrained
    -- by an earlier select. A bare closed set stays an error.
    plus l r | plusCoercible l = plus TString r
    plus l r | plusCoercible r = plus l TString
    plus l r =
      maybe
        (throwTypeError $ "operator `+` cannot combine " <> prettyType l <> " and " <> prettyType r)
        pure
        (plusConcrete l r)
    -- One operand is a variable. A NUMERIC known side propagates (`x + 1` ⟹
    -- x : Int — this is what keeps `map (x: x + 1) [ "a" ]` a caught error).
    -- A STRING/PATH known side must NOT bind the var: `+` combines unlike
    -- types there (`path + "/lib"` neither makes path a string literal nor
    -- forbids it being a Path — the old unify-to-propagate rejected exactly
    -- that, all over nixpkgs CI). A unique surviving +-row is the result
    -- (`"a" + x` is String whatever x is); ambiguity (`x + "s"` — String or
    -- Path) FOLLOWS the unknown operand so a later binding resolves it.
    varSidePlus var known f
      | norm known `elem` [TInt, TFloat] = do
          unify var (norm known)
          pure (norm known)
      | otherwise = case dedupe [t | Just t <- map f [TInt, TFloat, TString, TPath]] of -- CASE-OK
          [] -> throwTypeError "operator `+` expects Int, Float, String, or Path operands"
          [t] -> pure t
          _ -> pure var
    dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
    leaves (TUnion us) = concatMap leaves us
    leaves u = [u]
    flexLeaf (TVar _) = True
    flexLeaf TAny = True
    flexLeaf _ = False
    unionSidePlus u f =
      let ms = leaves u
          concrete = [t | m <- ms, not (flexLeaf m), Just t <- [f (norm m)]]
          viaFlex
            | any flexLeaf ms = [t | Just t <- map f [TInt, TFloat, TString, TPath]]
            | otherwise = []
       in case dedupe (concrete ++ viaFlex) of -- CASE-OK: shape dispatch
            [] -> throwTypeError "operator `+` expects Int, Float, String, or Path operands"
            [t] -> pure t
            _ -> freshVar
    plusCoercible TDerivation = True
    plusCoercible r@(TRec _ _) = derivationWitness r
    plusCoercible _ = False
    -- both operands concrete: the legal +-combinations (TStrLit ≈ TString)
    plusConcrete a b = combine (norm a) (norm b)
    combine TInt TInt = Just TInt
    combine TInt TFloat = Just TFloat
    combine TFloat TInt = Just TFloat
    combine TFloat TFloat = Just TFloat
    combine TString TString = Just TString
    combine TString TPath = Just TString
    combine TPath TString = Just TPath
    combine TPath TPath = Just TPath
    -- a MIXED union that survived 'norm' expands member-wise: any member
    -- that combines contributes (`prefix + (Null | String)` — the Null arm
    -- is the correlation-guarded case, the String arm answers)
    combine a (TUnion ts) = combineOver (map (combine a . norm) ts)
    combine (TUnion ts) b = combineOver (map ((`combine` b) . norm) ts)
    combine _ _ = Nothing
    combineOver results = case dedupe [t | Just t <- results] of -- CASE-OK: shape dispatch
      [] -> Nothing
      [t] -> Just t
      ts' -> Just (TUnion ts')
    norm (TStrLit _) = TString
    -- a join-produced union normalises to its members' common base:
    -- `"a" + (if c then s else " ")` sees String | " ", which is String-ish
    norm (TUnion ts) = case map norm ts of -- CASE-OK: shape dispatch
      (n : rest) | all (== n) rest -> n
      _ -> TUnion ts
    norm t = t
  -- arithmetic: Int by default, Float-infectious like `+` (a Float operand
  -- makes the result Float without binding a var on the other side —
  -- `x - 1.0` leaves x free to be Int OR Float, both legal Nix)
  apply NMinus l r = numericBinary l r
  apply NMult l r = numericBinary l r
  apply NDiv l r = numericBinary l r
  -- list concatenation
  apply NConcat leftT rightT = do
    -- `l ++ r` requires two LISTS, but their element types need only a JOIN,
    -- not equality: `[ pkg ] ++ pkg.optional-dependencies.grpc` (ubiquitous
    -- in python-modules) entangles the list's element var with a field
    -- selected from its own element — unifying the element types manufactures
    -- an "infinite type" on valid Nix, merging yields the honest union.
    leftElem <- freshVar
    rightElem <- freshVar
    unify leftT (TList leftElem)
    unify rightT (TList rightElem)
    elemT <- mergeListElems leftElem rightElem
    TList <$> applyCurrentSubst elemT
  -- attrset update // operator. Co1 from review-2: the TVar fallback
  -- previously unified leftT against rightT, collapsing a polymorphic
  -- parameter to the right operand's exact shape. We now route TVar
  -- through TAttrsOpen instead so `\x. x // {a=1;}` stays polymorphic.
  apply NUpdate leftT rightT = do
    leftT' <- applyCurrentSubst leftT
    rightT' <- applyCurrentSubst rightT
    update leftT' rightT'
   where
    -- `drv // attrs` keeps derivation identity; a dynamic side wins outright;
    -- a UNION side resolves per-member (members that can update contribute,
    -- the rest are the correlation-guarded arms); the old fallthrough
    -- `unify leftT rightT` asserted EQUALITY of structurally unrelated
    -- operands.
    update TDerivation _ = pure TDerivation
    update TAny _ = pure TAny
    update _ TAny = pure TAny
    update (TAttrs l) (TAttrs r) = pure $ TAttrs (r `Map.union` l)
    update (TAttrsOpen l) (TAttrsOpen r) = mkOpenRec (r `Map.union` l)
    update (TAttrs l) (TAttrsOpen r) = mkOpenRec (r `Map.union` l)
    update (TAttrsOpen l) (TAttrs r) = mkOpenRec (r `Map.union` l)
    -- a VARIABLE left side is NOT unified into an attrset shape: callable
    -- attrsets (callPackage results carrying .override) legally flow
    -- through `//`, and the constraint rigidified them; the result still
    -- carries at least the right side's keys
    update (TVar _) (TRec r _) = mkOpenRec r
    update (TRec l _) (TVar _) = do
      mkOpenRec Map.empty >>= unify rightT
      mkOpenRec l
    update (TVar _) (TVar _) = do
      mkOpenRec Map.empty >>= unify leftT
      mkOpenRec Map.empty >>= unify rightT
      mkOpenRec Map.empty
    update (TUnion ts) r = do
      results <- forM ts $ \t -> (Just <$> update t r) `catchInfer` pure Nothing
      case [t | Just t <- results] of -- CASE-OK: shape dispatch
        [] -> unify leftT rightT >> applyCurrentSubst leftT
        [t] -> pure t
        ts' -> pure (TUnion ts')
    update l (TUnion _)
      | TRec fl _ <- l = mkOpenRec fl
      | TVar _ <- l = mkOpenRec Map.empty
    update _ _ = do
      unify leftT rightT
      applyCurrentSubst leftT
  numericBinary lT rT = do
    l <- applyCurrentSubst lT
    r <- applyCurrentSubst rT
    if l == TFloat || r == TFloat
      then mapM_ numOperand [l, r] >> pure TFloat
      else unify l TInt >> unify r TInt >> pure TInt
   where
    -- a var stays FREE next to a Float (it may be Int or Float); only a
    -- concrete non-number errors
    numOperand t = case t of -- CASE-OK: shape dispatch
      TInt -> pure ()
      TFloat -> pure ()
      TVar _ -> pure ()
      TAny -> pure ()
      TUnion _ -> pure ()
      other -> throwTypeError $ "arithmetic expects numeric operands, got " <> prettyType other
  comparison l r = do
    l' <- applyCurrentSubst l
    r' <- applyCurrentSubst r
    case (comparableKind l', comparableKind r') of -- CASE-OK: shape dispatch
      (Just a, Just b)
        | a /= b ->
            throwTypeError $
              "operator `<` cannot compare " <> prettyType l' <> " and " <> prettyType r'
      _ -> pure ()
    pure TBool
  -- Just = a concrete comparable kind; Nothing = no claim (vars, unions, Any,
  -- and kinds our inference reaches imprecisely — lenient by design)
  comparableKind :: NixType -> Maybe Text
  comparableKind TInt = Just "number"
  comparableKind TFloat = Just "number"
  comparableKind TString = Just "string"
  comparableKind (TStrLit _) = Just "string"
  comparableKind TPath = Just "path"
  comparableKind (TList _) = Just "list"
  comparableKind _ = Nothing

-- | lambda: fresh var for each param, infer body, produce TFun
inferLambda :: TypeEnv -> Params NExprLoc -> NExprLoc -> Infer NixType
-- simple param: just one binder
inferLambda environment (Param name) body = do
  paramT <- moduleParamVar environment (varNameText name)
  let environment' = extendEnv (varNameText name) (Forall [] paramT) environment
  resultT <- infer environment' body
  paramT' <- applyCurrentSubst paramT
  pure $ TFun paramT' resultT
-- set pattern: { name ? default, ... } @ name ->
inferLambda environment (ParamSet mName variadic paramList) body = do
  -- Nix gives every formal ONE mutually-recursive scope, so a default value
  -- (`doCheck ? lib.versionAtLeast …`, `x ? callPackage ./p { }`) may reference
  -- any sibling parameter — and the @-binding, which is also in scope for
  -- defaults (`{ a ? args.b, b ? 3 } @ args:` is legal Nix). Bind a var for
  -- each param FIRST, then infer the defaults against that full scope, unifying
  -- each into its param's var. Inferring a default in the bare outer env (as
  -- before) wrongly reported sibling params like `lib` / `callPackage` unbound,
  -- skipping the whole file.
  --
  -- A param WITH a default always gets a plain fresh var — never the module-mode
  -- 'TAny' widening — so the default's inferred type wins ('unify TAny dt' is an
  -- absorbing no-op that would silently discard it). The widening exists for
  -- externally-supplied params we can't see; a default is exactly the case
  -- where we CAN see the value.
  -- THE MODULE-SYSTEM SEAM (doc/design/module-system.md): when the body
  -- declares options, the `config` parameter is not opaque — it carries the
  -- declared paths at their REIFIED types (mkOption's `type = types.…`),
  -- anon-open everywhere else. Files with no declarations keep TAny.
  let optionTree
        | envModuleParams environment = Module.declaredOptions body
        | otherwise = Module.emptyTree
      configType
        | Module.nullTree optionTree = Nothing
        | otherwise = Just (Module.configRecordOf optionTree)
  freshParams <- forM paramList $ \(name, mDefault) -> do
    t <- case (varNameText name, mDefault) of -- CASE-OK: shape dispatch
      ("config", Nothing) | Just ct <- configType -> pure ct
      (n, Nothing) -> moduleParamVar environment n
      (_, Just _) -> freshVar
    pure (varNameText name, t, mDefault)
  -- The @-name is bound loosely (dynamic in module mode, else an unconstrained
  -- var) rather than unified with the formals' record: `{ x ? args } @ args`
  -- would otherwise fail the occurs check on valid Nix.
  defaultsScope <- case mName of -- CASE-OK: shape dispatch
    Nothing -> pure environment
    Just n -> do
      argT <- if envModuleParams environment then pure TAny else freshVar
      pure (extendEnv (varNameText n) (Forall [] argT) environment)
  let paramScope =
        foldr (\(n, t, _) e -> extendEnv n (Forall [] t) e) defaultsScope freshParams
      -- `cudaPackages ? { }` — a literal EMPTY attrset default — is the
      -- placeholder idiom: callPackage / callers supply the real (richer)
      -- set and the body freely selects fields the placeholder lacks. Typing
      -- it as the closed `{}` rejected every such select (the
      -- `backendStdenv`/`cudatoolkit` sweep classes), so the formal becomes
      -- an ANON open record instead: selects succeed, nothing accumulates.
      withDefault fresh d
        | Layer (NSet _ []) <- d = do
            unify fresh (TRec Map.empty (ROpen anonRowVar))
            applyCurrentSubst fresh
        | otherwise = do
            dt <- infer paramScope d
            dt' <- applyCurrentSubst dt
            case dt' of -- CASE-OK: shape dispatch
            -- a literal `null` default is the OTHER placeholder sentinel
            -- (`python ? null` + correlated `pythonSupport` flag): the
            -- formal is honestly `Null | α`, never rigidly Null — selects
            -- degrade to dynamic instead of "select from Null", and
            -- occurrence narrowing refines the union in guarded regions.
              TNull -> do
                alt <- freshVar
                unify fresh (TUnion [TNull, alt])
              -- literal bool defaults are flag SENTINELS too (`kernel ?
              -- false` overridden with a kernel set): same honest union
              TBool | Layer (NConstant _) <- d -> do
                alt <- freshVar
                unify fresh (TUnion [TBool, alt])
              _ -> unify fresh dt
            applyCurrentSubst fresh
  paramTypes <- forM freshParams $ \(name, fresh, mDefault) -> do
    t <- maybe (pure fresh) (withDefault fresh) mDefault
    pure (name, (t, isJust mDefault))

  attrsT <-
    if variadic == Variadic
      then mkOpenRec (Map.fromList paramTypes)
      else pure (TAttrs (Map.fromList paramTypes))

  -- all param names are in scope in the body
  let environment' = foldr (\(n, (t, _)) e -> extendEnv n (Forall [] t) e) environment paramTypes

  -- @-binding: the whole attrset is in scope. In module mode it's the
  -- externally-supplied input set (e.g. flake @inputs) — type it dynamic so
  -- self-references like `mkFlake { inherit inputs; }` don't form a cyclic
  -- (occurs-check-failing) row.
  let boundType
        | envModuleParams environment = TAny
        | otherwise = attrsT
  let environment'' =
        maybe
          environment'
          (\name -> extendEnv (varNameText name) (Forall [] boundType) environment')
          mName

  resultT <- infer environment'' body

  -- DEFINITIONS meet DECLARATIONS: for each declared option path that the
  -- same body defines (its `config` section, or the shorthand body), the
  -- definition's value must inhabit the declared type. `mkIf`/`mkDefault`/
  -- `mkForce`/`mkMerge` are type-transparent lib combinators, so guarded
  -- and prioritized definitions check through unchanged. Interiors we can't
  -- navigate (merged/wrapped) are skipped, never mis-checked.
  checkDeclaredDefinitions environment'' optionTree body

  pure $ TFun attrsT resultT

-- | unify every navigable definition site against its declared option type
checkDeclaredDefinitions :: TypeEnv -> Module.OptionTree -> NExprLoc -> Infer ()
checkDeclaredDefinitions env tree body
  | Module.nullTree tree = pure ()
  | otherwise = mapM_ checkOne (leafPaths [] tree)
 where
  leafPaths prefix (Module.OptionTree m) =
    concat
      [ case node of -- CASE-OK: shape dispatch
          Module.OptLeaf t -> [(prefix ++ [k], t)]
          Module.OptSub sub -> leafPaths (prefix ++ [k]) sub
      | (k, node) <- Map.toList m
      ]
  -- Definition sites sit under the body's lets/withs, whose bindings are
  -- not in the parameter env — re-inference runs LENIENT, so a def that
  -- references sibling scope degrades to a var (vacuous unify, no false
  -- positive) while literal definitions — the actual misconfiguration
  -- shape — keep full strength and their exact spans.
  checkOne (path, declaredT) =
    case Module.definitionSiteFor path body of -- CASE-OK: shape dispatch
      Nothing -> pure ()
      Just defExpr@(LayerAnn sp _) -> do
        defT <- infer env{envLenient = True} defExpr
        withSpan (srcSpanToSpan sp) (unify declaredT defT)

{- | Type for a lambda parameter. Normally a fresh inference var, but in module
mode ('envModuleParams') a parameter whose name is a well-known external module /
flake input is typed dynamically — those values come from the flake / module
system, so inferring them precisely only yields false positives. Matched by name
so ordinary inner lambdas keep precise inference.
-}
moduleParamVar :: TypeEnv -> Text -> Infer NixType
moduleParamVar environment name
  | envModuleParams environment && isExternalParam name = pure TAny
  -- the callPackage protocol is dynamic in EVERY mode: a formal named
  -- callPackage/callPackages is applied at many unrelated shapes per file,
  -- and one shared var entangles all its call-site results
  | name `elem` (["callPackage", "callPackages"] :: [Text]) = pure TAny
  | otherwise = freshVar

-- | Well-known parameter names supplied by the flake / module system.
isExternalParam :: Text -> Bool
isExternalParam name =
  name
    `elem` [ "self"
           , "inputs"
           , "self'"
           , "inputs'"
           , "config"
           , "options"
           , "lib"
           , "pkgs"
           , "pkgs'"
           , "final"
           , "prev"
           , "super"
           , "specialArgs"
           , "modulesPath"
           , "system"
           , "withSystem"
           , "moduleWithSystem"
           , "getSystem"
           , "flake-parts-lib"
           ]

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- inference: main dispatch
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | top-level inference: destructure AST node and dispatch to handler
infer :: TypeEnv -> NExprLoc -> Infer NixType
infer environment (LayerAnn sp expr) = withSpan (srcSpanToSpan sp) (go expr)
 where
  go (NConstant atom) = inferAtom atom
  go (NStr str) = inferStr str
  go (NLiteralPath _) = pure TPath
  go (NEnvPath _) = pure TPath
  go (NSym name) = inferSymbol environment (varNameText name)
  go (NList list) = inferList environment list
  go (NSet recursive bindings) = inferAttrSet recursive environment bindings
  go (NLet bindings body) = inferLet environment bindings body
  go (NIf cond thenE elseE) = inferIf environment cond thenE elseE
  go (NWith scope body) = inferWith environment scope body
  go (NAssert cond body) = inferAssert environment cond body
  go (NAbs params body) = inferLambda environment params body
  go (NApp func arg) = inferAppWithImport environment func arg
  -- `builtins.<name>`/`lib.<name>` get a fresh polymorphic instance; a
  -- `pkgs.<path>` is enriched from the oracle inside 'inferSelect' (longest-prefix).
  go (NSelect mDef base path) =
    maybe
      (inferSelect environment base path (isJust mDef))
      instantiate
      (builtinsFieldScheme base path)
  go (NHasAttr base attr) = inferHasAttr environment base attr
  go (NUnary op e) = inferUnary environment op e
  go (NBinary op left right) = inferBinary environment op left right
  go (NSynHole _) = freshVar

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- inference: bindings
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | infer all bindings in a recursive set — the SAME SCC machinery as
'inferLet' (dependency-ordered groups, generalization between groups), so a
helper function in a @rec@ set is polymorphic across its sibling call sites
exactly as it would be let-bound. The old pre-seeded flat scope monomorphized
every helper: the first caller's argument shape rigidified it for the rest.
n.b. desugars nested-path bindings (S5 from review-2) so @{ a.b = 1; }@ is
treated as @{ a = { b = 1; }; }@ before inference begins.
n.b. a group whose vars all resolve back to themselves (pure self-reference,
`rec { x = x; }`) is LEGAL lazy Nix — `typeOf` succeeds, forcing diverges
only if a consumer demands it; the bindings simply stay polymorphic.
-}
inferRecursiveBindings :: TypeEnv -> [Nix.Binding NExprLoc] -> Infer [(Text, NixType)]
inferRecursiveBindings environment bindings'' = do
  let bindings' = desugarNestedBindings bindings''
      named = concatMap parseBinding bindings'
      edges = map (buildEdge named) named
      sccs = stronglyConnComp edges
  (_, fieldsRev) <- foldM step (environment, []) sccs
  pure (reverse fieldsRev)
 where
  step (curEnv, acc) scc = do
    let groupBindings = sccBindings scc
        names = map (\(n, _, _) -> n) groupBindings
    freshVars <- replicateM (length names) freshVar
    let envRec = foldr (\(n, t) e -> extendEnv n (Forall [] t) e) curEnv (zip names freshVars)
    forM_ (zip groupBindings freshVars) $ \((name, expr, sp), typeVar) -> do
      t <- infer envRec expr
      unify typeVar t
      t' <- applyCurrentSubst t
      emitBinding name t' sp
    schemes <- mapM (generalize curEnv) freshVars
    fieldTys <- mapM applyCurrentSubst freshVars
    let curEnv' = foldr (\(n, s) e -> extendEnv n s e) curEnv (zip names schemes)
    pure (curEnv', reverse (zip names fieldTys) ++ acc)

{- | infer a single non-recursive binding and accumulate results
n.b. bindings in a non-recursive set are independent (each sees only
@environment@), so this is a plain per-binding map — the caller concats the
results. The old accumulator-with-@++@ form was O(n²) on wide attrsets.
-}
inferNonRecursiveBinding :: TypeEnv -> Nix.Binding NExprLoc -> Infer [(Text, NixType)]
inferNonRecursiveBinding environment (Nix.NamedVar (StaticKey name :| []) expr pos) = do
  let bindingName = varNameText name
  t <- infer environment expr
  t' <- applyCurrentSubst t
  emitBinding bindingName t' (posToSpan pos)
  pure [(bindingName, t)]
inferNonRecursiveBinding environment (Nix.Inherit Nothing keys _) =
  forM keys $ \key -> do
    let keyName = varNameText key
    t <- maybe freshVar instantiate (lookupEnv keyName environment)
    pure (keyName, t)
-- `inherit (scope) a b c` infers the scope expression ONCE, then selects
-- each key through a synthetic binding of its type — re-inferring the scope
-- per key compounded its constraints (and its cost) k-fold
inferNonRecursiveBinding environment (Nix.Inherit (Just scope) keys _) = do
  scopeT <- infer environment scope
  let scopeName = " %inherit-scope" -- unparseable: cannot collide
      envScope = extendEnv scopeName (Forall [] scopeT) environment
      LayerAnn scopeSp _ = scope
      baseExpr = Fix (Compose (AnnUnit scopeSp (NSym (coerce scopeName))))
  forM keys $ \key -> do
    t <-
      infer
        envScope
        (Fix (Compose (AnnUnit scopeSp (NSelect Nothing baseExpr (StaticKey key :| [])))))
    pure (varNameText key, t)
inferNonRecursiveBinding _ _ = pure []

-- | dispatch to recursive or non-recursive binding inference
inferBindings :: Bool -> TypeEnv -> [Nix.Binding NExprLoc] -> Infer [(Text, NixType)]
inferBindings recursive environment bindings
  | recursive = inferRecursiveBindings environment bindings
  | otherwise =
      concat <$> mapM (inferNonRecursiveBinding environment) (desugarNestedBindings bindings)

{- | Desugar nested-path bindings into top-level bindings whose value is a
synthesised attrset. Closes S5 from review-2: previously @{ a.b = 1; }@ was
silently dropped because the inference engine only matched singleton paths.
We also merge bindings that share a top-level key, so @{ a.b = 1; a.c = 2; }@
becomes @{ a = { b = 1; c = 2; }; }@.
-}
desugarNestedBindings :: [Nix.Binding NExprLoc] -> [Nix.Binding NExprLoc]
desugarNestedBindings = mergeByKey . map desugar1
 where
  desugar1 (Nix.NamedVar (StaticKey k :| (k2 : ks)) e pos) =
    let inner = Fix (Compose (AnnUnit nullSpan (NSet NonRecursive [Nix.NamedVar (k2 :| ks) e pos])))
     in Nix.NamedVar (StaticKey k :| []) inner pos
  desugar1 b = b

  -- Merge bindings sharing a top-level static key (e.g. desugared @a.b@/@a.c@)
  -- into one. Two O(n log n) passes: collect each key's later values, then
  -- emit each key's first occurrence merged with them (later occurrences are
  -- dropped, non-static bindings pass through in place). The old per-key
  -- partition scan was O(n²) on wide attrsets.
  mergeByKey bs =
    let (_, outRev) = foldl' emit (Set.empty, []) bs
     in reverse outRev
   where
    collectExtra acc (Nix.NamedVar (StaticKey k :| []) v _)
      | Map.member (varNameText k) acc = Map.insertWith (flip (++)) (varNameText k) [v] acc
      | otherwise = Map.insert (varNameText k) [] acc
    collectExtra acc _ = acc
    extras = foldl' collectExtra Map.empty bs
    emit (seen, out) (Nix.NamedVar kp@(StaticKey k :| []) val pos) =
      let kt = varNameText k
       in if kt `Set.member` seen
            then (seen, out)
            else
              let merged = foldr addAttrs val (Map.findWithDefault [] kt extras)
               in (Set.insert kt seen, Nix.NamedVar kp merged pos : out)
    emit (seen, out) b = (seen, b : out)

  addAttrs (Fix (Compose (AnnUnit s (NSet r bs1)))) (Fix (Compose (AnnUnit _ (NSet _ bs2)))) =
    Fix (Compose (AnnUnit s (NSet r (bs2 ++ bs1))))
  addAttrs additional original = original `mergeOrKeep` additional
  mergeOrKeep a _ = a

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- inference: let expressions
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | convert a Nix binding to (name, expr, span) tuples (1 per declared name)
inherit from scope desugars to a select expression
-}
parseBinding :: Nix.Binding NExprLoc -> [(Text, NExprLoc, Span)]
parseBinding (Nix.NamedVar (StaticKey name :| []) expr pos) =
  [(varNameText name, expr, posToSpan pos)]
parseBinding (Nix.Inherit mScope keys pos) = map synth keys
 where
  synth key = (varNameText key, expr, posToSpan pos)
   where
    spForSynth = maybe nullSpan scopeSpan mScope
    scopeSpan scope = let LayerAnn s _ = scope in s
    expr =
      maybe
        (Fix (Compose (AnnUnit spForSynth (NSym key))))
        (\scope -> Fix (Compose (AnnUnit spForSynth (NSelect Nothing scope (StaticKey key :| [])))))
        mScope
parseBinding _ = []

-- | build a graph edge: the name → [dependency names] for SCC analysis
buildEdge ::
  [(Text, NExprLoc, Span)] -> (Text, NExprLoc, Span) -> ((Text, NExprLoc, Span), Text, [Text])
buildEdge allBindings (name, expr, sp) =
  let free = collectFreeVars expr
      deps = [n | (n, _, _) <- allBindings, n `elem` free]
   in ((name, expr, sp), name, deps)

{- | infer one SCC (strongly connected component) group of let bindings
acyclic groups have 1 binding; cyclic groups share scope immediately
-}

-- | the members of an SCC as a plain list (acyclic = singleton, cyclic = group)
sccBindings :: SCC a -> [a]
sccBindings (AcyclicSCC x) = [x]
sccBindings (CyclicSCC list) = list

inferLetGroup :: TypeEnv -> TypeEnv -> SCC (Text, NExprLoc, Span) -> Infer TypeEnv
inferLetGroup _baseEnv currentEnv scc = do
  let groupBindings = sccBindings scc

  let names = map (\(n, _, _) -> n) groupBindings
  freshVars <- replicateM (length names) freshVar

  let envRecursive =
        foldr (\(n, t) e -> extendEnv n (Forall [] t) e) currentEnv (zip names freshVars)

  forM_ (zip groupBindings freshVars) $ \((name, expr, sp), typeVar) -> do
    t <- infer envRecursive expr
    unify typeVar t
    t' <- applyCurrentSubst t
    -- REBIND the placeholder to the authoritative body type: a self-call
    -- inside the body may have prematurely narrowed the placeholder (bound
    -- it to `Any -> α` via its own recursion), making the unify above a
    -- vacuous no-op. Sound because t' is fully substituted and the guard
    -- refuses a self-occurrence.
    case typeVar of -- CASE-OK: shape dispatch
      TVar v
        | t' /= typeVar
        , not (v `Set.member` freeTypeVars t') ->
            addSubst v t'
      _ -> pure ()
    emitBinding name t' sp

  -- generalize each binding for let-polymorphism
  -- (a group whose vars all self-resolve stays polymorphic — see
  -- 'inferRecursiveBindings' on why pure self-reference is not an error)
  schemes <- mapM (generalize currentEnv) freshVars

  pure $ foldr (\(n, s) e -> extendEnv n s e) currentEnv (zip names schemes)

{- | infer let ... in ... using SCC-based dependency analysis
bindings are grouped by mutual recursion, then each group is inferred in order
n.b. dotted-path bindings (@let a.b = 1;@) desugar exactly as in attrsets —
without this the top-level name never enters the SCC graph and every
reference reports unbound
-}
inferLet :: TypeEnv -> [Nix.Binding NExprLoc] -> NExprLoc -> Infer NixType
inferLet environment bindings body = do
  let namedBindings = concatMap parseBinding (desugarNestedBindings bindings)
  let edges = map (buildEdge namedBindings) namedBindings
  let sccs = stronglyConnComp edges
  environmentBody <- foldM (inferLetGroup environment) environment sccs
  infer environmentBody body

-- | collect free variable names from an expression (for dependency analysis)
collectFreeVars :: NExprLoc -> [Text]
collectFreeVars (Layer (NSym name)) = [varNameText name]
collectFreeVars (Layer (NList elems)) = concatMap collectFreeVars elems
collectFreeVars (Layer (NSet _ bindings)) = concatMap collectFreeVarsBinding bindings
collectFreeVars (Layer (NLet bindings body)) =
  concatMap collectFreeVarsBinding bindings ++ collectFreeVars body
collectFreeVars (Layer (NIf c t f)) = collectFreeVars c ++ collectFreeVars t ++ collectFreeVars f
collectFreeVars (Layer (NWith s b)) = collectFreeVars s ++ collectFreeVars b
collectFreeVars (Layer (NAssert c b)) = collectFreeVars c ++ collectFreeVars b
collectFreeVars (Layer (NAbs params b)) =
  let bound = paramNames params
      paramFreeVars = paramDefaults params
   in paramFreeVars ++ filter (`notElem` bound) (collectFreeVars b)
collectFreeVars (Layer (NApp f a)) = collectFreeVars f ++ collectFreeVars a
collectFreeVars (Layer (NSelect alt b _)) = maybe [] collectFreeVars alt ++ collectFreeVars b
collectFreeVars (Layer (NHasAttr b _)) = collectFreeVars b
collectFreeVars (Layer (NUnary _ e)) = collectFreeVars e
collectFreeVars (Layer (NBinary _ l r)) = collectFreeVars l ++ collectFreeVars r
collectFreeVars (Layer (NStr str)) = concatMap collectFreeVars (antiquotedExprs str)
collectFreeVars _ = []

-- | the antiquoted sub-expressions of a string literal (`"x${e}y"` → [e])
antiquotedExprs :: NString NExprLoc -> [NExprLoc]
antiquotedExprs str = [e | Antiquoted e <- parts str]
 where
  parts (DoubleQuoted ps) = ps
  parts (Indented _ ps) = ps

{- | collect free vars from a binding (recurse into the value expression).
n.b. inherit clauses reference names too: `inherit (scope) k` reads `scope`,
bare `inherit k` reads `k` from the enclosing scope. Missing these starved
the let-SCC dependency analysis, so a binding like
`pkg = f { inherit (cfg) addons; }` could be inferred before its group
established `cfg` — the "unbound variable: cfg" class in the nixpkgs sweep
(order-dependent: only bit when SCC emitted the reader first).
-}
collectFreeVarsBinding :: Nix.Binding NExprLoc -> [Text]
collectFreeVarsBinding (Nix.NamedVar _ expr _) = collectFreeVars expr
collectFreeVarsBinding (Nix.Inherit (Just scope) _ _) = collectFreeVars scope
collectFreeVarsBinding (Nix.Inherit Nothing keys _) = map varNameText keys

{- | extract the names bound by a function parameter pattern
n.b. this is used by collectFreeVars to scope variables (not type inference)
-}
paramNames :: Params NExprLoc -> [Text]
paramNames (Param name) = [varNameText name]
paramNames (ParamSet mName _ formals) =
  let formalNames = map (varNameText . fst) formals
   in formalNames ++ maybe [] (pure . varNameText) mName

{- | collect free vars from default expressions in a parameter pattern
the actual value of defaults may reference outer variables
-}
paramDefaults :: Params NExprLoc -> [Text]
paramDefaults (Param _) = []
paramDefaults (ParamSet _ _ formals) =
  concat [collectFreeVars e | (_, Just e) <- formals]

-- | map NAton to the corresponding NixType
atomType :: NAtom -> NixType
atomType (NInt _) = TInt
atomType (NFloat _) = TFloat
atomType (NBool _) = TBool
atomType NNull = TNull
atomType (NURI _) = TString

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- results
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- results
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | top-level inference result: all bindings + per-file function signatures
data InferResult = InferResult
  { irBindings :: ![Binding]
  , irFunctions :: ![(Text, NixType)]
  }
  deriving (Eq, Show)

-- | infer a single expression in the builtin environment
inferExpr :: NExprLoc -> Either Text (NixType, [Binding])
inferExpr expr = inferExprWithEnv builtinEnv expr

{- | Infer a module / flake expression: like 'inferExpr' but well-known external
parameter names (self, inputs, config, pkgs, the @-bound input set, …) are typed
as dynamic rather than inferred precisely. Used for files detected as a flake or
a module, whose top-level parameters are supplied by the flake / module system
(see 'envModuleParams').
-}
inferModuleExpr :: NExprLoc -> Either Text (NixType, [Binding])
inferModuleExpr = inferExprWithEnv builtinEnv{envModuleParams = True}

-- | infer an expression with a specific type environment (for cross-module inference)
inferExprWithEnv :: TypeEnv -> NExprLoc -> Either Text (NixType, [Binding])
inferExprWithEnv env expr =
  runInfer $ do
    t <- infer env expr
    applyCurrentSubst t

{- | The bindings inference emitted, INCLUDING on failure: everything typed
before the error point. Powers editor features that must degrade gracefully
(inlay hints around a type error) rather than blank the file.
-}
inferExprBindingsPartial :: TypeEnv -> NExprLoc -> [Binding]
inferExprBindingsPartial env expr =
  snd (runInferBinds (infer env expr >>= applyCurrentSubst))

-- | convert Nix source position to our Span type
posToSpan :: Nix.NSourcePos -> Span
posToSpan (Nix.NSourcePos path l c) =
  Span
    (Loc (unPos (coerce l)) (unPos (coerce c)))
    (Loc (unPos (coerce l)) (unPos (coerce c)))
    (Just (coerce path))

-- | parse and infer a file, returning bindings and overall type
inferFile :: FilePath -> IO (Either Text InferResult)
inferFile path = do
  result <- try (parseNixFileLoc (Nix.Path path))
  either onIOError onParsed result
 where
  onIOError (e :: IOException) = pure $ Left (T.pack $ show e)
  onParsed = either onDoc (onExpr . normalizeStaticKeys)
  onDoc doc = pure $ Left (T.pack $ show doc)
  onExpr expr = either (pure . Left) onInferred (inferExpr expr)
  onInferred (t, bindings) = pure $ Right $ InferResult bindings [(T.pack path, t)]
