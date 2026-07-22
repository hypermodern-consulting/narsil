{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                   // tests // inference // oracle
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Give the box a fact and it reasons; the reasoning is only as good as
--    the facts you feed it."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The `pkgs.<path>` type oracle (`envPkgsOracle`) feeding inference — driven by
--   a hand-seeded map (no eval) so the enrichment is exercised purely. With the
--   oracle, a nixpkgs reference stops being opaque 'TAny':
--
--     * a precomputed leaf path gets its real scalar type (`pkgs.hello.pname` :
--       String) — better hover / decisions;
--     * a precomputed record lets a member access resolve, and a BOGUS member
--       becomes a real "missing attribute" error — better error messages;
--     * the now-typed value participates in unification, so a misuse
--       (`if pkgs.hello.pname then …`) is caught — sharper unification;
--     * with no oracle seeded, behaviour is unchanged (the seam is inert).
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module InferenceOracleSpec (inferenceOracleTests) where

import Data.Either (isLeft)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Inference.Nix (builtinEnv, inferExprWithEnv)
import Narsil.Inference.Nix.Environment (withPkgsOracle)
import Narsil.Inference.Nix.Type (NixType (..), pattern TAttrs)
import Nix.Parser (parseNixTextLoc)

-- ── helpers ────────────────────────────────────────────────────────

-- | Infer a snippet under a hand-seeded @pkgs@ oracle; the resulting type or error.
inferWith :: Map [Text] NixType -> Text -> Either Text NixType
inferWith oracle src = case parseNixTextLoc src of
  Left e -> Left (T.pack (show e))
  Right expr -> fst <$> inferExprWithEnv (withPkgsOracle oracle builtinEnv) expr

-- | A closed record over @(name, type)@ fields, all required.
closedRec :: [(Text, NixType)] -> NixType
closedRec fields = TAttrs (Map.fromList [(n, (t, False)) | (n, t) <- fields])

-- | `pkgs.hello` as a record of two string fields — the unit of the record cases.
helloOracle :: Map [Text] NixType
helloOracle = Map.singleton ["hello"] (closedRec [("pname", TString), ("version", TString)])

-- ── tests ──────────────────────────────────────────────────────────

-- | A precomputed leaf path gives its real type, not 'TAny'.
testLeafPrecise :: IO Bool
testLeafPrecise =
  pure (inferWith (Map.singleton ["hello", "pname"] TString) "pkgs.hello.pname" == Right TString)

-- | A precomputed record lets a member access resolve to the member's type.
testRecordMemberResolves :: IO Bool
testRecordMemberResolves =
  pure (inferWith helloOracle "let h = pkgs.hello; in h.pname" == Right TString)

{- | The record prefix resolves a DIRECT deep access too — @pkgs.hello.pname@
typed from the @pkgs.hello@ record even though that leaf was never precomputed.
-}
testDirectDeepFromRecord :: IO Bool
testDirectDeepFromRecord =
  pure (inferWith helloOracle "pkgs.hello.pname" == Right TString)

{- | A DIRECT deep access to a bogus attribute is now a real error (the prefix
record is closed) — not the silent 'TAny' it used to fall back to.
-}
testDirectDeepTypoErrors :: IO Bool
testDirectDeepTypoErrors =
  pure (isLeft (inferWith helloOracle "pkgs.hello.nope"))

-- | A bogus member of a precomputed (closed) record is a real type error.
testBogusMemberErrors :: IO Bool
testBogusMemberErrors =
  pure (isLeft (inferWith helloOracle "let h = pkgs.hello; in h.nope"))

-- | The now-typed value flows into unification: a String condition is rejected.
testUnificationCatch :: IO Bool
testUnificationCatch =
  pure (isLeft (inferWith helloOracle "let h = pkgs.hello; in if h.pname then 1 else 2"))

{- | The seam is inert without a seed: with no oracle, @pkgs@ is just an unbound
name (the pre-oracle behaviour), so the very query the oracle resolves is unresolved.
-}
testInertWithoutOracle :: IO Bool
testInertWithoutOracle =
  pure (isLeft (inferWith Map.empty "pkgs.hello.pname"))

-- | A dynamic key in the path falls through the oracle (only static paths key it).
testDynamicKeyFallsThrough :: IO Bool
testDynamicKeyFallsThrough =
  -- `pkgs.${x}` can't be a static path, so the oracle never matches → unbound pkgs.
  pure (isLeft (inferWith helloOracle "let x = \"hello\"; in pkgs.${x}"))

-- ── runner ─────────────────────────────────────────────────────────

-- | The pkgs-oracle inference-enrichment tests (hermetic; hand-seeded, no eval).
inferenceOracleTests :: [(String, IO Bool)]
inferenceOracleTests =
  [ ("oracle_leaf_precise", testLeafPrecise)
  , ("oracle_record_member_resolves", testRecordMemberResolves)
  , ("oracle_direct_deep_from_record", testDirectDeepFromRecord)
  , ("oracle_direct_deep_typo_errors", testDirectDeepTypoErrors)
  , ("oracle_bogus_member_errors", testBogusMemberErrors)
  , ("oracle_unification_catch", testUnificationCatch)
  , ("oracle_inert_without_seed", testInertWithoutOracle)
  , ("oracle_dynamic_key_falls_through", testDynamicKeyFallsThrough)
  ]
