{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                               // tests // inference // row lacks
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "What you forbid is as much a part of the shape as what you allow."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The Gaster–Jones lacks-constraint enforcement. The field-merge in 'applySubst'
--   ('resolveRow', a left-biased 'Map.union') is sound only if a row variable's
--   binding is DISJOINT from the record it tails. That invariant is now enforced:
--   'unify' records each open record's labels as its tail's lacks-set, and
--   'bindRowVar' rejects a binding that would supply a lacked label.
--
--   On well-typed programs the invariant holds by construction, so the check is
--   inert (the whole suite is green with it on) — which means it can't be
--   exercised from source. So we prove it two ways: SOURCE-level probes pin that a
--   shared-label type conflict is caught (by 'unifyCommon', upstream of the merge),
--   and a SYNTHETIC probe drives 'bindRowVar' against a hand-seeded lacks-set to
--   show the enforcement itself fires on collision and passes on disjoint fields.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module RowLacksSpec (rowLacksTests) where

import Data.Either (isLeft, isRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Narsil.Inference.Nix (inferExpr)
import Narsil.Inference.Nix.Constraint (Infer, addLacks, runInfer)
import Narsil.Inference.Nix.Type (NixType (..), TypeVar (..), pattern TAttrs)
import Narsil.Inference.Nix.Unify (bindRowVar)
import Nix.Parser (parseNixTextLoc)

-- ── source-level: shared-label type conflicts are caught ────────────

-- | Does inference REJECT this source (a type error somewhere)?
rejects :: Text -> Bool
rejects src = either (const False) (isLeft . inferExpr) (parseNixTextLoc src)

-- | Does inference ACCEPT this source?
accepts :: Text -> Bool
accepts src = either (const False) (isRight . inferExpr) (parseNixTextLoc src)

-- | A closed record meeting an open row that uses the shared field as a Bool.
testClosedOpenBoolConflict :: IO Bool
testClosedOpenBoolConflict =
  pure (rejects "(g: if g.pname then 1 else 2) { pname = \"x\"; version = \"y\"; }")

-- | A closed record meeting an open row that uses the shared field as an Int.
testClosedOpenIntConflict :: IO Bool
testClosedOpenIntConflict =
  pure (rejects "(g: g.pname + 1) { pname = \"x\"; }")

-- | The same field demanded at two incompatible types through one variable.
testSameLabelTwoTypes :: IO Bool
testSameLabelTwoTypes =
  pure (rejects "(x: [ (x.a + 1) (if x.a then 1 else 2) ])")

-- | The honest baseline: a consistent open selection still type-checks.
testConsistentOpenOk :: IO Bool
testConsistentOpenOk =
  pure (accepts "(x: x.a + 1) { a = 1; b = 2; }")

-- ── synthetic: the lacks-check itself ───────────────────────────────

-- | Bind a row var to a record that supplies a label it is required to lack.
lacksClashAction :: Infer ()
lacksClashAction = do
  let r = TypeVar 100
  addLacks r (Set.singleton "k")
  bindRowVar r (TAttrs (Map.singleton "k" (TInt, False)))

-- | Bind a row var to a record whose labels are disjoint from its lacks-set.
lacksDisjointAction :: Infer ()
lacksDisjointAction = do
  let r = TypeVar 101
  addLacks r (Set.singleton "k")
  bindRowVar r (TAttrs (Map.singleton "j" (TInt, False)))

{- | The enforcement FIRES: binding a row var to a record providing a lacked label
is a type error — the silent 'Map.union' drop made into a sound rejection.
-}
testLacksCheckFires :: IO Bool
testLacksCheckFires = pure (isLeft (runInfer lacksClashAction))

-- | The enforcement is PRECISE: a disjoint binding is accepted (no false positive).
testLacksCheckAllowsDisjoint :: IO Bool
testLacksCheckAllowsDisjoint = pure (isRight (runInfer lacksDisjointAction))

-- ── runner ──────────────────────────────────────────────────────────

-- | The row lacks-constraint enforcement tests (source probes + synthetic check).
rowLacksTests :: [(String, IO Bool)]
rowLacksTests =
  [ ("lacks_src_closed_open_bool_conflict", testClosedOpenBoolConflict)
  , ("lacks_src_closed_open_int_conflict", testClosedOpenIntConflict)
  , ("lacks_src_same_label_two_types", testSameLabelTwoTypes)
  , ("lacks_src_consistent_open_ok", testConsistentOpenOk)
  , ("lacks_check_fires_on_clash", testLacksCheckFires)
  , ("lacks_check_allows_disjoint", testLacksCheckAllowsDisjoint)
  ]
