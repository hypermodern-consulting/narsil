{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                     // tests // layout // closure
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He traced every wire the box touched, and the whole shape came clear."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The one reachability closure shared by check / infer / lsp. Two layers:
--
--     * 'discoverEdges' tags all three edge kinds from the AST, eval-free —
--       @import ./a@, a flake-parts @imports = [ … ]@ list, a top-level
--       @callPackage ./p { }@;
--     * 'closureEnv' walks a real temp tree from a root file and threads each
--       dependency's inferred type into the next, so a cross-module @import@
--       resolves to the dependency's actual record type (across a @../@ sibling,
--       which the old dir-bounded graph could not reach) — and a file with no
--       in-project imports leaves the base env untouched.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module ClosureSpec (closureTests) where

import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Narsil.Core.Safety (safeParseNixText)
import Narsil.Inference.Nix (TypeEnv (..), builtinEnv, inferExprWithEnv)
import Narsil.Inference.Nix.Type (prettyType)
import Narsil.Layout.Closure (Edge (..), EdgeKind (..), closureEnv, discoverEdges)
import Nix.Expr.Types.Annotated (NExprLoc)
import System.Directory (createDirectoryIfMissing, createDirectoryLink)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

-- ── helpers ────────────────────────────────────────────────────────

-- | The edges discovered in a snippet (parsed against a fixed base directory).
edgesOf :: Text -> IO [Edge]
edgesOf src = do
  parsed <- safeParseNixText src
  pure (either (const []) (discoverEdges "/proj") parsed)

{- | A two-file project under a temp root marked by @flake.nix@; returns the
cross-module env for @main.nix@.
-}
withTree :: Text -> Text -> (TypeEnv -> IO a) -> IO a
withTree dep main act =
  withSystemTempDirectory "closure" $ \dir -> do
    TIO.writeFile (dir </> "flake.nix") ""
    TIO.writeFile (dir </> "dep.nix") dep
    TIO.writeFile (dir </> "main.nix") main
    env <- closureEnv builtinEnv (dir </> "main.nix")
    act env

{- | Like 'withTree', but also hands the callback @main.nix@'s parsed expression
(so a test can infer it against the cross-module env). Parse failure ⇒ False.
-}
withTreeMain :: Text -> Text -> (TypeEnv -> NExprLoc -> IO Bool) -> IO Bool
withTreeMain dep main act =
  withSystemTempDirectory "closure" $ \dir -> do
    TIO.writeFile (dir </> "flake.nix") ""
    TIO.writeFile (dir </> "dep.nix") dep
    let mainPath = dir </> "main.nix"
    TIO.writeFile mainPath main
    env <- closureEnv builtinEnv mainPath
    parsed <- safeParseNixText main
    either (const (pure False)) (act env) parsed

-- | Infer a snippet against an env: 'Just' the pretty type, 'Nothing' on any failure.
inferSrcOk :: TypeEnv -> Text -> IO (Maybe Text)
inferSrcOk env src = do
  parsed <- safeParseNixText src
  pure $
    either
      (const Nothing)
      (either (const Nothing) (Just . prettyType . fst) . inferExprWithEnv env)
      parsed

-- ── edge discovery ─────────────────────────────────────────────────

-- | @import ./a.nix@ is one 'EImport' edge naming the raw path.
testImportEdge :: IO Bool
testImportEdge = do
  es <- edgesOf "import ./a.nix"
  pure (any (\e -> edgeKind e == EImport && edgeRaw e == "./a.nix") es)

-- | A flake-parts @imports = [ … ]@ list yields one 'EFlakeImport' per element.
testFlakeEdges :: IO Bool
testFlakeEdges = do
  es <- edgesOf "{ imports = [ ./m.nix ./n.nix ]; }"
  pure (length (filter ((== EFlakeImport) . edgeKind) es) == 2)

-- | A top-level @callPackage ./p { }@ binding is one 'ECallPackage' edge.
testCallPackageEdge :: IO Bool
testCallPackageEdge = do
  es <- edgesOf "{ foo = callPackage ./pkg.nix { }; }"
  pure (any (\e -> edgeKind e == ECallPackage && edgeRaw e == "./pkg.nix") es)

-- ── the type closure ───────────────────────────────────────────────

-- | A cross-module @import@ resolves to the dependency's actual record type.
testCrossModuleType :: IO Bool
testCrossModuleType =
  withTree "{ a = 1; b = \"x\"; }\n" "import ./dep.nix\n" $ \env ->
    pure (any recordWithFields (Map.elems (envImportTypes env)))
 where
  recordWithFields t = "a" `T.isInfixOf` rendered t && "b" `T.isInfixOf` rendered t
  rendered = prettyType

{- | A @callPackage ./dep.nix { }@ site resolves to the package's RESULT type (the
body of @dep.nix@'s function), not the function itself.
-}
testCallPackageResult :: IO Bool
testCallPackageResult =
  withTree depFn callerFn $ \env ->
    pure (any resultShaped (Map.elems (envCallPackageTypes env)))
 where
  depFn = "{ stdenv }: { nm = 1; }\n"
  callerFn = "{ callPackage }: { p = callPackage ./dep.nix { }; }\n"
  -- must be the APPLIED result `{ nm : … }`, not the raw function
  -- `{ stdenv : … } -> { nm : … }` (whose printed codomain also contains "nm")
  resultShaped t =
    let s = prettyType t
     in "nm" `T.isInfixOf` s && not ("stdenv" `T.isInfixOf` s)

{- | The check-path payoff: accessing a field the dependency's closed record
lacks is a cross-module type error the closure now catches (single-file inference,
seeing @import ./dep.nix@ as opaque, would have let it pass).
-}
testCrossModuleFieldError :: IO Bool
testCrossModuleFieldError =
  withTreeMain "{ a = 1; }\n" "(import ./dep.nix).b\n" $ \env expr ->
    pure (isLeft (inferExprWithEnv env expr))

-- | A file with no in-project imports leaves the base env's import types untouched.
testNoImportsIsBase :: IO Bool
testNoImportsIsBase =
  withTree "{ a = 1; }\n" "{ x = 1; }\n" $ \env ->
    pure (envImportTypes env == envImportTypes builtinEnv)

-- ── regressions from review-4 ──────────────────────────────────────

{- | REGRESSION: the import interception must check the HEAD is @import@ — with
the closure seeding raw path keys, @dirOf ./dep.nix@ used to be typed as
dep.nix's type instead of a path.
-}
testImportHeadGuard :: IO Bool
testImportHeadGuard =
  withTree "\"hello\"\n" "let x = import ./dep.nix; in dirOf ./dep.nix\n" $ \env -> do
    t <- inferSrcOk env "let x = import ./dep.nix; in dirOf ./dep.nix"
    pure (t == Just "Path")

{- | REGRESSION: a seeded dependency type carries type variables from a SEPARATE
inference run; splicing it in un-renamed captured the importer's variables
(spurious @infinite type@ on @let m = import ./dep.nix; in m@ for a
module-shaped dep). The resolved type must be instantiated fresh.
-}
testSeededTypeInstantiated :: IO Bool
testSeededTypeInstantiated =
  withTree "{ config, lib, pkgs, ... }: { val = config; }\n" "let m = import ./dep.nix; in m\n" $
    \env -> isJust <$> inferSrcOk env "let m = import ./dep.nix; in m"

{- | REGRESSION: @../@ in a path literal resolves LEXICALLY from the path as
invoked (as Nix does), not through a canonicalized symlink target. Reaching
main.nix via the @a/link@ symlink, @../dep.nix@ is @a/dep.nix@ (the lexical
sibling), never @other/dep.nix@ (the physical one).
-}
testLexicalSymlink :: IO Bool
testLexicalSymlink =
  withSystemTempDirectory "closure-sym" $ \dir -> do
    TIO.writeFile (dir </> "flake.nix") ""
    createDirectoryIfMissing True (dir </> "a")
    createDirectoryIfMissing True (dir </> "other" </> "real")
    TIO.writeFile (dir </> "a" </> "dep.nix") "{ lexical = 1; }\n"
    TIO.writeFile (dir </> "other" </> "dep.nix") "{ physical = 1; }\n"
    TIO.writeFile (dir </> "other" </> "real" </> "main.nix") "(import ../dep.nix).lexical\n"
    createDirectoryLink (dir </> "other" </> "real") (dir </> "a" </> "link")
    env <- closureEnv builtinEnv (dir </> "a" </> "link" </> "main.nix")
    viaLexical <- inferSrcOk env "(import ../dep.nix).lexical"
    viaPhysical <- inferSrcOk env "(import ../dep.nix).physical"
    pure (isJust viaLexical && isNothing viaPhysical)

{- | REGRESSION: the @-pattern name is in scope for sibling DEFAULTS
(@{ a ? args.b, b ? 3 } \@ args: a@ is legal Nix) — it used to report
@unbound variable: args@.
-}
testAtNameInDefaultScope :: IO Bool
testAtNameInDefaultScope =
  isJust <$> inferSrcOk builtinEnv "{ a ? args.b, b ? 3 } @ args: a"

{- | REGRESSION: in module mode, a well-known param WITH a default must keep the
default's inferred type — the TAny widening used to absorb it (@unify TAny t@ is
a no-op), silently missing bogus field accesses.
-}
testModuleDefaultTypePreserved :: IO Bool
testModuleDefaultTypePreserved = do
  bad <- inferSrcOk moduleEnv "{ lib ? { x = 1; }, ... }: lib.noSuchField"
  good <- inferSrcOk moduleEnv "{ lib ? { x = 1; }, ... }: lib.x"
  pure (isNothing bad && isJust good)
 where
  moduleEnv = builtinEnv{envModuleParams = True}

{- | REGRESSION: a union-typed VALUE (an @if@ merging Int and Float) is accepted
by @toString@ when every leaf is in the domain — the membership check used to
compare the whole union by equality and reject valid Nix.
-}
testToStringUnionValue :: IO Bool
testToStringUnionValue =
  isJust <$> inferSrcOk builtinEnv "{ c }: toString (if c then 1 else 1.5)"

{- | REGRESSION (review-5): a λ-bound formal applied with differently-shaped
records at two call sites (the @symlinkJoin@ / @fetchFromGitHub@ pattern) must
not rigidify the first site's record as THE domain — unknown-function
application keeps a dynamic domain.
-}
testFormalAppliedTwoShapes :: IO Bool
testFormalAppliedTwoShapes =
  isJust <$> inferSrcOk builtinEnv "{ mk }: { a = mk { x = 1; }; b = mk { y = 2; meta = 3; }; }"

{- | REGRESSION (review-5): joins where a variable meets a type containing it
(@if isFunction f then f x else f@; a list holding a value and its own field)
are valid Nix — the join yields a UNION, never an occurs-check error — and
selecting through such a union works.
-}
testJoinOccursIsUnion :: IO Bool
testJoinOccursIsUnion = do
  optCall <-
    inferSrcOk
      builtinEnv
      "let optCall = f: x: if builtins.isFunction f then f x else f; in optCall (y: y) 1"
  valueAndField <- inferSrcOk builtinEnv "{ p }: [ p p.cacheDir ]"
  unionUpdate <-
    inferSrcOk
      builtinEnv
      "let oc = f: x: if builtins.isFunction f then f x else f; in { c }: oc c { } // oc c { }"
  pure (all isJust [optCall, valueAndField, unionUpdate])

{- | REGRESSION (review-6): @++@ joins element types instead of unifying them —
@[ pkg ] ++ pkg.optional-dependencies.grpc@ (ubiquitous in python-modules)
entangles the list's element var with a field of its own element, which
manufactured an "infinite type" under element unification.
-}
testConcatEntangledElem :: IO Bool
testConcatEntangledElem =
  isJust <$> inferSrcOk builtinEnv "{ p, q }: [ p q ] ++ p.optional-dependencies.grpc"

{- | REGRESSION (review-6): a string/path known side of @+@ must not BIND the
variable operand (@path + \"/lib\"@ neither makes path a string literal nor
forbids Path — the formal's Path default must still unify), a literal plus a
join union is fine, and numeric propagation (`x + 1 ⟹ x : Int`) still catches
@map (x: x + 1) [ \"a\" ]@.
-}
testPlusVarStringUnbound :: IO Bool
testPlusVarStringUnbound = do
  pathDefault <- inferSrcOk builtinEnv "{ p ? ../.., x ? builtins.import (p + \"/lib\") }: x"
  litPlusUnion <- inferSrcOk builtinEnv "{ c }: \"a\" + (if c then \"b\" else \" \")"
  numericStillCaught <- inferSrcOk builtinEnv "map (x: x + 1) [ \"a\" ]"
  pure (isJust pathDefault && isJust litPlusUnion && isNothing numericStillCaught)

{- | REGRESSION (review-6, occurrence narrowing): a null-defaulted formal is
usable at a concrete type inside a branch its guard proves non-null —
@if module == null then \"\" else module + \".\"@ (x86-msr), predicate
narrowing (@if isString theme then stringLength theme …@, dwarf-fortress),
assert-guarded bodies, and negative narrowing must NOT leak: using the value
at String in the branch the guard proves IS null stays an error.
-}
testNarrowNullGuards :: IO Bool
testNarrowNullGuards = do
  eqElse <-
    inferSrcOk
      builtinEnv
      "{ module ? null }: (if module == null then \"\" else module + \".\") + \"name\""
  neqThen <-
    inferSrcOk
      builtinEnv
      "{ conf ? null }: if conf != null then builtins.stringLength conf else 0"
  predIf <-
    inferSrcOk
      builtinEnv
      "{ t ? null }: if builtins.isString t then builtins.stringLength t else 0"
  assertBody <-
    inferSrcOk
      builtinEnv
      "{ fn ? null }: assert fn != null; fn 3"
  wrongArm <-
    inferSrcOk
      builtinEnv
      "{ conf ? null }: if conf == null then builtins.stringLength conf else 0"
  pure (all isJust [eqElse, neqThen, predIf, assertBody] && isNothing wrongArm)

{- | REGRESSION (review-6, occurrence narrowing): @x ? field@ in guard
position makes the field select OPTIONAL in the guarded region — the open
formals row must not come to REQUIRE the field, so callers omitting it stay
accepted (the ocaml @hardeningDisable@ class).
-}
testNarrowHasField :: IO Bool
testNarrowHasField = do
  guarded <-
    inferSrcOk
      builtinEnv
      "({ x, ... }@args: x ++ (if args ? extra then args.extra else [ ])) { x = [ 1 ]; }"
  unguardedStillCaught <-
    inferSrcOk
      builtinEnv
      "({ x, ... }@args: x ++ args.extra) { x = [ 1 ]; }"
  pure (isJust guarded && isNothing unguardedStillCaught)

{- | REGRESSION (review-6): inherit clauses feed the let-SCC dependency
analysis — a binding reading @inherit (cfg) a@ must be inferred AFTER @cfg@
even when it appears first (order-independence of let bindings).
-}
testInheritSccDep :: IO Bool
testInheritSccDep = do
  scoped <-
    inferSrcOk
      builtinEnv
      "let pkg = (x: x) { inherit (cfg) a; }; cfg = { a = 1; }; in pkg"
  bare <-
    inferSrcOk
      builtinEnv
      "let pkg = (x: x) { inherit addons; }; addons = [ 1 ]; in pkg"
  interp <-
    inferSrcOk
      builtinEnv
      "let url = \"http://x/${ver}.tar\"; ver = \"1.0\"; in url"
  pure (all isJust [scoped, bare, interp])

{- | REGRESSION (review-6, round 2): four rigidity classes from the sweep.
A null (or any) DEFAULT must not rigidify its formal against real callers
(x86-msr's @kernelParam { module = "msr"; }@); ordering comparisons are
polymorphic (@version < "3.11"@) but still reject concretely-unlike operands;
derivations are open value bags (@drv.meta@ selects); an EMPTY-attrset
default is the callPackage placeholder idiom (@cudaPackages ? { }@ then
@cudaPackages.backendStdenv@) — while a NON-empty closed default still
rejects a bogus select.
-}
testDefaultsDontRigidify :: IO Bool
testDefaultsDontRigidify = do
  nullDefaultApplied <-
    inferSrcOk
      builtinEnv
      ( "let f = { module ? null }: if module == null then \"\" else module + \".\"; "
          <> "in f { module = \"msr\"; }"
      )
  cmpStrings <- inferSrcOk builtinEnv "\"3.13\" < \"3.11\""
  cmpUnlikeCaught <- inferSrcOk builtinEnv "1 < \"a\""
  drvSelect <- inferSrcOk builtinEnv "(builtins.derivation { name = \"x\"; }).meta.platforms"
  emptyPlaceholder <-
    inferSrcOk builtinEnv "{ cudaPackages ? { } }: cudaPackages.backendStdenv"
  nonEmptyStillCaught <-
    inferSrcOk builtinEnv "{ cfg ? { a = 1; } }: cfg.missingField"
  pure
    ( all isJust [nullDefaultApplied, cmpStrings, drvSelect, emptyPlaceholder]
        && all isNothing [cmpUnlikeCaught, nonEmptyStillCaught]
    )

{- | REGRESSION (review-6, round 3): a literal `null` DEFAULT types its formal
`Null | α` — the placeholder-sentinel idiom (`python ? null` with a
correlated `pythonSupport` flag): selects degrade to dynamic instead of
"select from Null", and unguarded string use passes. A LET-BOUND null (not a
default — nothing can replace it) still errors on select.
-}
testNullDefaultUnion :: IO Bool
testNullDefaultUnion = do
  selectDegrades <- inferSrcOk builtinEnv "{ python ? null }: python.pkgs"
  stringUseOk <- inferSrcOk builtinEnv "{ conf ? null }: \"pre\" + (builtins.toString conf)"
  letNullStillCaught <- inferSrcOk builtinEnv "let x = null; in x.field"
  pure (all isJust [selectDegrades, stringUseOk] && isNothing letNullStillCaught)

{- | REGRESSION (review-6): QUOTED constant keys (@"version" = …@, @h."key"@ —
generator output like graalvm's hashes.nix) are STATIC keys: they must bind
and select precisely, not silently type as the empty record.
-}
testQuotedStaticKeys :: IO Bool
testQuotedStaticKeys = do
  quotedBind <-
    inferSrcOk builtinEnv "let h = { \"hashes\" = { x = 1; }; }; in h.hashes.x + 1"
  quotedSelect <-
    inferSrcOk builtinEnv "let h = { version = \"v\"; }; in \"p\" + h.\"version\""
  pure (all isJust [quotedBind, quotedSelect])

{- | REGRESSION (review-6): derivation-like records — open rows (whose unknown
tail may carry the coercion witness) and closed sets WITH a witness — pass
at Derivation positions and coerce under @+@ (@clang-unwrapped + "/bin"@
after a row-constraining select). A bare closed set still does neither.
-}
testDerivationWitnessFlow :: IO Bool
testDerivationWitnessFlow = do
  openRecPlus <-
    inferSrcOk builtinEnv "{ p }: [ p.version (p + \"/bin\") ]"
  witnessAtDerivation <-
    inferSrcOk builtinEnv "{ p }: (d: d.meta) (builtins.derivation p) + \"\""
  bareSetPlusCaught <-
    inferSrcOk builtinEnv "{ a = 1; } + \"/bin\""
  pure (all isJust [openRecPlus, witnessAtDerivation] && isNothing bareSetPlusCaught)

{- | REGRESSION (review-6): nested @with@ scopes are a SEARCH STACK, not a
replacement — a name missing from the inner (closed) scope falls through to
the outer (`with lib; with builtins; mapAttrs'`), an inner definite hit
shadows, and a name in NO scope is still unbound when all scopes are closed.
-}
testNestedWithFallthrough :: IO Bool
testNestedWithFallthrough = do
  fallsThrough <-
    inferSrcOk
      builtinEnv
      ( "let lib = { extra = x: x + 1; }; in with lib; with builtins; "
          <> "[ (map (v: v) [ 1 ]) (extra 2) ]"
      )
  innerShadows <-
    inferSrcOk
      builtinEnv
      "with { x = \"s\"; }; with { x = 1; }; x + 1"
  allClosedMissCaught <-
    inferSrcOk
      builtinEnv
      "with { a = 1; }; with { b = 2; }; c"
  pure (all isJust [fallsThrough, innerShadows] && isNothing allClosedMissCaught)

{- | The MODULE-SYSTEM contract (doc/design/module-system.md): declarations
carry reified types (`mkOption { type = types.…; }`), and the same file's
definitions must inhabit them; `config` gets the declared spine (so
`cfg = config.services.foo` selects are precise); files with NO
declarations keep the opaque-TAny behavior unchanged.
-}
testModuleDefsMeetDecls :: IO Bool
testModuleDefsMeetDecls = do
  let modEnv = builtinEnv{envModuleParams = True}
      declBlock =
        "options.services.foo = { \
        \  enable = lib.mkEnableOption \"foo\"; \
        \  port = lib.mkOption { type = lib.types.int; default = 8080; }; \
        \  name = lib.mkOption { type = lib.types.nullOr lib.types.str; }; \
        \  features = lib.mkOption { type = lib.types.listOf lib.types.str; }; \
        \}; "
      modFile defs =
        "{ config, lib, pkgs, ... }: let cfg = config.services.foo; in { "
          <> declBlock
          <> "config = { "
          <> defs
          <> " }; }"
  goodDefs <-
    inferSrcOk modEnv (modFile "services.foo.port = 9090; services.foo.name = null;")
  badPort <- inferSrcOk modEnv (modFile "services.foo.port = \"9090\";")
  badElems <- inferSrcOk modEnv (modFile "services.foo.features = [ 1 2 ];")
  cfgPrecise <-
    inferSrcOk
      modEnv
      ( "{ config, lib, ... }: let cfg = config.services.foo; in { "
          <> declBlock
          <> "config = { services.foo.port = cfg.port + 1; }; }"
      )
  cfgWrongUse <-
    inferSrcOk
      modEnv
      ( "{ config, lib, ... }: let cfg = config.services.foo; in { "
          <> declBlock
          <> "config = { services.foo.name = builtins.stringLength cfg.port; }; }"
      )
  noDeclsUnchanged <-
    inferSrcOk
      modEnv
      "{ config, lib, ... }: { environment.systemPackages = [ config.boot.kernelPackages.perf ]; }"
  pure
    ( all isJust [goodDefs, cfgPrecise, noDeclsUnchanged]
        && all isNothing [badPort, badElems, cfgWrongUse]
    )

{- | REGRESSION: sets carrying a string-coercion witness (@__toString@ /
@outPath@) coerce under @toString@ — bare sets still do not.
-}
testToStringWitnessSets :: IO Bool
testToStringWitnessSets = do
  viaToString <- inferSrcOk builtinEnv "toString { __toString = self: \"v\"; }"
  viaOutPath <- inferSrcOk builtinEnv "toString { outPath = \"/nix/store/x\"; }"
  bare <- inferSrcOk builtinEnv "toString { plain = 1; }"
  pure (isJust viaToString && isJust viaOutPath && isNothing bare)

{- | REGRESSION (review-7, the tail sweep): dotted-path LET bindings desugar
exactly as attrset bindings — the name binds, siblings merge, and selects
through it are precise.
-}
testDottedLetBindings :: IO Bool
testDottedLetBindings = do
  dotted <- inferSrcOk builtinEnv "let a.b = 1; a.c = 2; in a.b + a.c"
  dottedTypo <- inferSrcOk builtinEnv "let a.b = 1; in a.z"
  pure (isJust dotted && isNothing dottedTypo)

{- | REGRESSION (review-7): heterogeneous LIST literals must not rigidify a
still-free element (lambda formal next to a Path literal — the NixOS
`imports = [ ./hw.nix extraConfig ]` idiom); homogeneous concrete lists
still merge precisely (`map (x: x + 1)` over strings stays caught — the
mutation ledger holds that pin).
-}
testListElemsNonBinding :: IO Bool
testListElemsNonBinding = do
  pathAndFormal <- inferSrcOk builtinEnv "let f = x: [ ./a.nix x ]; in f \"hello\""
  fnDomainsJoin <-
    inferSrcOk builtinEnv "[ builtins.attrNames (builtins.filter (s: s != \"\")) ]"
  pure (all isJust [pathAndFormal, fnDomainsJoin])

{- | REGRESSION (review-7): `if c then x else null` joins a FREE var with Null
as a union — the var stays usable as its eventual type; a KNOWN null select
is still the product (the testNullDefaultUnion pin holds that side).
-}
testNullJoinKeepsVarFree :: IO Bool
testNullJoinKeepsVarFree = do
  joined <-
    inferSrcOk builtinEnv "let f = c: x: if c then x else null; g = f true 5; in g"
  pure (isJust joined)

{- | REGRESSION (review-7): Float-infectious arithmetic (`2.0 / 4`, unary
negation of floats); Int-only ops still reject strings.
-}
testFloatArithmetic :: IO Bool
testFloatArithmetic = do
  floats <- inferSrcOk builtinEnv "[ (1.5 * 2.0) (3 - 1) (2.0 / 4) (-(1.5)) ]"
  divByString <- inferSrcOk builtinEnv "1 / \"a\""
  pure (isJust floats && isNothing divByString)

{- | REGRESSION (review-7): scheme instantiation renames each quantified var
ONE step — a two-param helper applied to unlike arguments must not collapse
its vars into one (the servarr name/port shape).
-}
testInstantiateRenameBijective :: IO Bool
testInstantiateRenameBijective = do
  twoParams <-
    inferSrcOk
      builtinEnv
      "let mk = name: port: { n = name + \"!\"; p = port + 1; }; in mk \"sonarr\" 8989"
  pure (isJust twoParams)

{- | REGRESSION (review-7): helpers in a REC set generalize per SCC group like
let bindings — one helper serving two differently-shaped siblings; a rec
binding referencing a missing sibling is still unbound.
-}
testRecSetGeneralizes :: IO Bool
testRecSetGeneralizes = do
  polyHelper <-
    inferSrcOk builtinEnv "rec { id2 = x: x; a = id2 1; b = id2 \"s\" + \"!\"; }"
  missingSibling <- inferSrcOk builtinEnv "rec { a = zzz; }"
  pure (isJust polyHelper && isNothing missingSibling)

{- | REGRESSION (review-7): UNRESOLVED `callPackage ./p { }` results are
per-site opaque (no shared result var across sites; `.override` selects
pass); derivations flow through `//` keeping their identity.
-}
testCallPackageOpaque :: IO Bool
testCallPackageOpaque = do
  perSite <-
    inferSrcOk
      builtinEnv
      ( "{ callPackage }: { a = callPackage ./a.nix { }; "
          <> "b = (callPackage ./b.nix { }).override { x = 1; }; "
          <> "c = callPackage ./a.nix { } // { extra = 1; }; }"
      )
  drvUpdate <-
    inferSrcOk
      builtinEnv
      "(builtins.derivation { name = \"x\"; } // { passthru = 1; }).drvPath"
  pure (all isJust [perSite, drvUpdate])

{- | REGRESSION (review-7): `&&`/`||` narrow their right operand (lazy guards);
derivations pass at String positions (expected-String side only).
-}
testLazyGuardsAndCoercions :: IO Bool
testLazyGuardsAndCoercions = do
  andGuard <-
    inferSrcOk
      builtinEnv
      "{ pool ? null }: pool != null && builtins.stringLength pool > 0"
  orGuard <-
    inferSrcOk
      builtinEnv
      "{ pool ? null }: pool == null || builtins.stringLength pool > 0"
  drvAtString <-
    inferSrcOk
      builtinEnv
      "builtins.concatStringsSep \":\" [ (builtins.derivation { name = \"x\"; }) ]"
  pure (all isJust [andGuard, orGuard, drvAtString])

-- ── runner ──────────────────────────────────────────────────────────

-- | The shared-closure tests (edge discovery hermetic; type flow on a temp tree).
closureTests :: [(String, IO Bool)]
closureTests =
  [ ("closure_discovers_import_edge", testImportEdge)
  , ("closure_discovers_flake_imports", testFlakeEdges)
  , ("closure_discovers_callpackage_edge", testCallPackageEdge)
  , ("closure_cross_module_type_flows", testCrossModuleType)
  , ("closure_callpackage_result_type_flows", testCallPackageResult)
  , ("closure_cross_module_field_error_caught", testCrossModuleFieldError)
  , ("closure_no_imports_is_base_env", testNoImportsIsBase)
  , ("closure_import_head_guard", testImportHeadGuard)
  , ("closure_seeded_type_instantiated", testSeededTypeInstantiated)
  , ("closure_lexical_symlink_resolution", testLexicalSymlink)
  , ("infer_at_name_in_default_scope", testAtNameInDefaultScope)
  , ("infer_module_default_type_preserved", testModuleDefaultTypePreserved)
  , ("infer_tostring_union_value", testToStringUnionValue)
  , ("infer_tostring_witness_sets", testToStringWitnessSets)
  , ("infer_formal_applied_two_shapes", testFormalAppliedTwoShapes)
  , ("infer_join_occurs_is_union", testJoinOccursIsUnion)
  , ("infer_concat_entangled_elem", testConcatEntangledElem)
  , ("infer_plus_var_string_unbound", testPlusVarStringUnbound)
  , ("infer_narrow_null_guards", testNarrowNullGuards)
  , ("infer_narrow_hasfield", testNarrowHasField)
  , ("infer_inherit_scc_dep", testInheritSccDep)
  , ("infer_defaults_dont_rigidify", testDefaultsDontRigidify)
  , ("infer_null_default_union", testNullDefaultUnion)
  , ("infer_quoted_static_keys", testQuotedStaticKeys)
  , ("infer_derivation_witness_flow", testDerivationWitnessFlow)
  , ("infer_nested_with_fallthrough", testNestedWithFallthrough)
  , ("infer_module_defs_meet_decls", testModuleDefsMeetDecls)
  , ("infer_dotted_let_bindings", testDottedLetBindings)
  , ("infer_list_elems_non_binding", testListElemsNonBinding)
  , ("infer_null_join_keeps_var_free", testNullJoinKeepsVarFree)
  , ("infer_float_arithmetic", testFloatArithmetic)
  , ("infer_instantiate_rename_bijective", testInstantiateRenameBijective)
  , ("infer_rec_set_generalizes", testRecSetGeneralizes)
  , ("infer_callpackage_opaque", testCallPackageOpaque)
  , ("infer_lazy_guards_and_coercions", testLazyGuardsAndCoercions)
  ]
