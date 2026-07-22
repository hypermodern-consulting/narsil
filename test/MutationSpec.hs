{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                             // tests // mutation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The street finds its own uses for things."
--
--                                                                                — Burning Chrome
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The FALSE-NEGATIVE scoreboard. The oracle sweep ratchets false positives
--   (stock nixpkgs must stay quiet); nothing there measures what we still
--   CATCH. Each entry here is a seed expression the checker accepts and a
--   mutation of it:
--
--     * MustCatch — the mutant is a genuine bug shape; rejecting it is the
--       product. If one of these starts passing, a leniency change went too
--       far — that is a regression even though no sweep number moved.
--     * AcceptedByDesign — the LEDGER of deliberate leniency trades
--       (defaulted fields, null placeholders, lazy self-reference,
--       non-binding comparisons). If one of these starts being CAUGHT, the
--       entry is stale: upgrade it to MustCatch and celebrate.
--
--   Both directions are exact assertions, so every future inference change
--   is forced to look this ledger in the eye.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module MutationSpec (mutationTests) where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Narsil.Core.Safety (safeParseNixText)
import Narsil.Inference.Nix (TypeEnv (..), builtinEnv, inferExprWithEnv)
import Narsil.Inference.Nix.Type (prettyType)

-- | 'Just' the pretty type when the expression parses and infers, else 'Nothing'.
inferOk :: TypeEnv -> Text -> IO (Maybe Text)
inferOk env src = do
  parsed <- safeParseNixText src
  pure $
    either
      (const Nothing)
      (either (const Nothing) (Just . prettyType . fst) . inferExprWithEnv env)
      parsed

-- | seed must infer; mutant must be REJECTED
mustCatch :: Text -> Text -> IO Bool
mustCatch = mustCatchIn builtinEnv

mustCatchIn :: TypeEnv -> Text -> Text -> IO Bool
mustCatchIn env seed mutant = do
  s <- inferOk env seed
  m <- inferOk env mutant
  pure (isJust s && isNothing m)

-- | seed must infer; mutant is ACCEPTED — a documented leniency trade
acceptedByDesign :: Text -> Text -> IO Bool
acceptedByDesign = acceptedByDesignIn builtinEnv

acceptedByDesignIn :: TypeEnv -> Text -> Text -> IO Bool
acceptedByDesignIn env seed mutant = do
  s <- inferOk env seed
  m <- inferOk env mutant
  pure (isJust s && isJust m)

-- | module-mode env: the module-system machinery (declarations, config spine)
moduleEnv :: TypeEnv
moduleEnv = builtinEnv{envModuleParams = True}

mutationTests :: [(String, IO Bool)]
mutationTests =
  [ -- ── MustCatch: the product ──────────────────────────────────────

    ( "mut_select_typo_closed_record"
    , mustCatch
        "let p = { pname = \"x\"; version = \"1\"; }; in p.pname"
        "let p = { pname = \"x\"; version = \"1\"; }; in p.pnmae"
    )
  ,
    ( "mut_missing_required_formal"
    , mustCatch
        "({ a, b }: a + b) { a = 1; b = 2; }"
        "({ a, b }: a + b) { a = 1; }"
    )
  ,
    ( "mut_unexpected_argument_field"
    , mustCatch
        "({ a, b }: a + b) { a = 1; b = 2; }"
        "({ a, b }: a + b) { a = 1; b = 2; c = 3; }"
    )
  ,
    ( "mut_map_numeric_over_strings"
    , mustCatch
        "map (x: x + 1) [ 1 2 ]"
        "map (x: x + 1) [ \"a\" ]"
    )
  ,
    ( "mut_apply_bare_attrset"
    , mustCatch
        "(f: f 1) (x: x)"
        "(f: f 1) { }"
    )
  ,
    ( "mut_not_on_int"
    , mustCatch "!true" "!1"
    )
  ,
    ( "mut_concat_non_list"
    , mustCatch "[ 1 ] ++ [ 2 ]" "[ 1 ] ++ 2"
    )
  ,
    ( "mut_builtin_domain_violation"
    , mustCatch "builtins.stringLength \"a\"" "builtins.stringLength 5"
    )
  ,
    ( "mut_tostring_bare_set"
    , mustCatch "toString { outPath = \"p\"; }" "toString { plain = 1; }"
    )
  ,
    ( "mut_unbound_typo"
    , mustCatch "let foo = 1; in foo" "let foo = 1; in fooo"
    )
  ,
    ( "mut_if_condition_int"
    , mustCatch "if true then 1 else 2" "if 1 then 1 else 2"
    )
  ,
    ( "mut_select_through_scalar"
    , mustCatch "{ a = { b = 1; }; }.a.b" "{ a = 1; }.a.b"
    )
  ,
    ( "mut_div_by_string"
    , mustCatch "1 / 2" "1 / \"a\""
    )
  ,
    ( "mut_compare_concrete_unlike"
    , mustCatch "\"3.13\" < \"3.11\"" "1 < \"a\""
    )
  ,
    ( "mut_null_guard_wrong_arm"
    , mustCatch
        "{ conf ? null }: if conf != null then builtins.stringLength conf else 0"
        "{ conf ? null }: if conf == null then builtins.stringLength conf else 0"
    )
  , -- ── AcceptedByDesign: the leniency ledger ───────────────────────
    -- lenient optional fields: a caller REPLACES a default, so a wrong-typed
    -- value through a defaulted field is not checked (review-6 trade for the
    -- "expected Null, got …" classes)

    ( "mutledger_defaulted_field_wrong_type"
    , acceptedByDesign
        "({ n ? 0 }: n + 1) { n = 2; }"
        "({ n ? 0 }: n + 1) { n = \"x\"; }"
    )
  , -- `? null` types as `Null | α`: selects degrade to dynamic (review-6
    -- trade for the select-from-Null classes)

    ( "mutledger_null_default_select"
    , acceptedByDesign
        "({ p ? null }: 1) { }"
        "({ p ? null }: p.pkgs) { }"
    )
  , -- lazy self-reference is legal Nix; nothing to catch

    ( "mutledger_rec_self_reference"
    , acceptedByDesign "rec { x = 1; }" "rec { x = x; }"
    )
  , -- non-binding comparisons: λ-formal operands stay unconstrained

    ( "mutledger_compare_var_operands"
    , acceptedByDesign "(a: b: a < b) 1 2" "(a: b: a < b) 1 \"x\""
    )
  , -- dynamic (antiquoted) select keys resolve to fresh vars, so a typo'd
    -- dynamic key is invisible

    ( "mutledger_dynamic_select_typo"
    , acceptedByDesign "{ a = 1; }.${\"a\"}" "{ a = 1; }.${\"b\"}"
    )
  , -- enum options: `unionMemberAccepts` treats any string literal as fitting
    -- any literal-union position (needed for branch-merged flag lists), so a
    -- typo'd enum VALUE passes; the enum's shape (string-ness) still checks

    ( "mutledger_enum_value_typo"
    , acceptedByDesignIn
        moduleEnv
        ( "({ config, lib, ... }: { options.m = lib.mkOption "
            <> "{ type = lib.types.enum [ \"fast\" \"safe\" ]; }; config.m = \"fast\"; })"
        )
        ( "({ config, lib, ... }: { options.m = lib.mkOption "
            <> "{ type = lib.types.enum [ \"fast\" \"safe\" ]; }; config.m = \"slow\"; })"
        )
    )
  , -- the module contract's MustCatch face: a definition must inhabit its
    -- declared reified type (doc/design/module-system.md)

    ( "mut_module_def_violates_decl"
    , mustCatchIn
        moduleEnv
        ( "({ config, lib, ... }: { options.m.port = lib.mkOption "
            <> "{ type = lib.types.int; }; config.m.port = 80; })"
        )
        ( "({ config, lib, ... }: { options.m.port = lib.mkOption "
            <> "{ type = lib.types.int; }; config.m.port = \"80\"; })"
        )
    )
  , -- empty-set placeholder default: selects on `? { }` formals succeed

    ( "mutledger_empty_placeholder_select"
    , acceptedByDesign
        "({ cudaPackages ? { } }: 1) { }"
        "({ cudaPackages ? { } }: cudaPackages.notAField) { }"
    )
  , -- REVIEW-7 POLICY PINS: the tail sweep confirmed real nixpkgs bugs in
    -- these shapes (doas/alot/fwknop `optionalString <list>`, `+` on lists
    -- in networkd/ale-py) — the leniency that would clear their FP cousins
    -- was deliberately NOT taken. If these start passing, that trade was
    -- silently re-made.

    ( "mut_optionalstring_list_arg"
    , mustCatch
        "{ lib }: lib.optionalString true \"--flag\""
        "{ lib }: lib.optionalString true [ \"--flag\" ]"
    )
  , -- String→Derivation coercion is DIRECTIONAL: a drv passes where String
    -- is expected (outPath), a string where a Derivation is required stays
    -- an error

    ( "mut_getexe_string_arg"
    , mustCatch
        "{ lib, pkg }: lib.getExe pkg"
        "{ lib }: lib.getExe \"\""
    )
  , -- `+` on two lists is `++` misspelled (networkd.nix, ale-py)

    ( "mut_plus_on_lists"
    , mustCatch
        "[ 1 ] ++ [ 2 ]"
        "[ 1 ] + [ 2 ]"
    )
  ]
