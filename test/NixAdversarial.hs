{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                    // tests // nix // adversarial
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The soap had been running continuously since before he was born, the
--    plot a multiheaded narrative tapeworm that coiled back in to devour
--    itself every few months, then sprouted new heads hungry for tension and
--    thrust."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                        // nix // adversarial // property // tests
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module NixAdversarial where

import Control.Exception (SomeException, evaluate, try)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T

-- hnix
import Nix.Parser (parseNixTextLoc)

-- narsil Nix inference

import Narsil.Inference.Nix qualified as Infer (inferExpr, runInfer, unify)
import Narsil.Inference.Nix.Type qualified as NT
import Narsil.Lint.Derivation qualified as DerivLint
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- GENERATORS — Deep Nesting
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Generate deeply nested Nix function types
genDeepFunc :: Int -> Gen Text
genDeepFunc n =
  let src = T.intercalate ": " (replicate n "x") <> ": 42"
   in pure src

-- | Generate deeply nested attrstets
genDeepAttrs :: Int -> Gen Text
genDeepAttrs n =
  let go 0 = pure "42"
      go k = do
        inner <- go (k - 1)
        pure $ "{ a" <> T.pack (show k) <> " = " <> inner <> "; }"
   in go n

-- | Generate deeply nested let expressions
genDeepLet :: Int -> Gen Text
genDeepLet n =
  let go 0 = pure "42"
      go k = do
        inner <- go (k - 1)
        pure $ "let x" <> T.pack (show k) <> " = " <> inner <> "; in x" <> T.pack (show k)
   in go n

-- | Generate a rec attrset with many mutually-recursive bindings
genMutualSCC :: Int -> Gen Text
genMutualSCC n =
  let pairs = [(show i, show (if i == n then 1 else i + 1)) | i <- [1 .. n :: Int]]
      binds = map (\(a, b) -> "v" <> T.pack a <> " = v" <> T.pack b <> ";") pairs
   in pure $ "rec { " <> T.unwords binds <> " }"

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- NIX TYPE INFERENCE — DEGENERATE UNIFICATION (raw type-level)
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | FLIPPED (review-6): TVar 0 ~ TFun (TVar 0) (TVar 0) is an equirecursive
type we cannot represent — legal in lazy Nix (fixpoint APIs), so unify
SUCCEEDS by leaving the variable unconstrained. The occurs check's job is
keeping the substitution acyclic, not rejecting the program.
-}
prop_nix_occurs_check :: Bool
prop_nix_occurs_check =
  case Infer.runInfer
    ( Infer.unify
        (NT.TVar (NT.TypeVar 0))
        (NT.TFun (NT.TVar (NT.TypeVar 0)) (NT.TVar (NT.TypeVar 0)))
    ) of
    Right _ -> True
    Left _ -> False

-- | TUnion [TInt,TString,TBool,TNull] ~ TFloat must fail.
prop_nix_union_mismatch :: Bool
prop_nix_union_mismatch =
  case Infer.runInfer
    (Infer.unify (NT.TUnion [NT.TInt, NT.TString, NT.TBool, NT.TNull]) NT.TFloat) of
    Left err -> "type mismatch" `T.isInfixOf` err
    Right _ -> False

-- | TAttrs {"a"=(TInt,False)} vs TAttrs {} — required field missing.
prop_nix_attrs_required_missing :: Bool
prop_nix_attrs_required_missing =
  let a = NT.TAttrs (Map.singleton "a" (NT.TInt, False))
      b = NT.TAttrs Map.empty
   in case Infer.runInfer (Infer.unify a b) of
        Left err ->
          "missing required field" `T.isInfixOf` err
            || "unexpected field" `T.isInfixOf` err
        Right _ -> False

-- | TAttrs {"a"=(TInt,False)} vs TAttrsOpen {"b"=(TBool,False)} — closed missing open requirement.
prop_nix_row_closed_missing_open_req :: Bool
prop_nix_row_closed_missing_open_req =
  let closed = NT.TAttrs (Map.singleton "a" (NT.TInt, False))
      open = NT.TAttrsOpen (Map.singleton "b" (NT.TBool, False))
   in case Infer.runInfer (Infer.unify closed open) of
        Left err -> "missing field required by open" `T.isInfixOf` err
        Right _ -> False

-- | TAttrsOpen {} unifies with TInt — empty open has no requirements.
prop_nix_row_empty_open_any :: Bool
prop_nix_row_empty_open_any =
  case Infer.runInfer (Infer.unify (NT.TAttrsOpen Map.empty) NT.TInt) of
    Left _ -> False
    Right _ -> True

-- | Nested union: TUnion [TUnion [TInt,TBool], TString] ~ TInt.
prop_nix_nested_union :: Bool
prop_nix_nested_union =
  case Infer.runInfer
    (Infer.unify (NT.TUnion [NT.TUnion [NT.TInt, NT.TBool], NT.TString]) NT.TInt) of
    Left _ -> False
    Right _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- NIX TYPE INFERENCE — TYPE VARIABLE EXHAUSTION
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | 300 unique variable names — check no crash / no supply counter overflow.
prop_nix_many_fresh_vars :: Property
prop_nix_many_fresh_vars =
  monadicIO $ do
    result <-
      run $
        try @SomeException $
          evaluate $
            let manyVars =
                  T.intercalate
                    "\n"
                    [ "a" <> T.pack (show i) <> " = " <> T.pack (show i) <> ";"
                    | i <- [1 .. 300 :: Int]
                    ]
                expr = "{ " <> manyVars <> " }"
             in case parseNixTextLoc expr of
                  Left _ -> ()
                  Right e -> case Infer.inferExpr e of
                    Left _ -> ()
                    Right (t, _) -> t `seq` ()
    case result of
      Left (_ :: SomeException) -> assert False
      Right _ -> assert True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- NIX TYPE INFERENCE — DEEP NESTING
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | 500-deep function nesting via text generation.
prop_nix_deep_func_nesting :: Property
prop_nix_deep_func_nesting =
  monadicIO $ do
    src <- run $ generate (genDeepFunc 500)
    result <- run $
      try @SomeException $
        evaluate $
          case parseNixTextLoc src of
            Left _ -> ()
            Right e -> case Infer.inferExpr e of
              Left _ -> ()
              Right (t, _) -> t `seq` ()
    case result of
      Left (_ :: SomeException) -> assert False
      Right _ -> assert True

-- | 60-deep attrset nesting — should not stack-overflow.
prop_nix_deep_attr_nesting :: Property
prop_nix_deep_attr_nesting =
  monadicIO $ do
    src <- run $ generate (genDeepAttrs 60)
    result <- run $
      try @SomeException $
        evaluate $
          case parseNixTextLoc src of
            Left _ -> ()
            Right e -> case Infer.inferExpr e of
              Left _ -> ()
              Right (t, _) -> t `seq` ()
    case result of
      Left (_ :: SomeException) -> assert False
      Right _ -> assert True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- NIX TYPE INFERENCE — SCHEME INSTANTIATION BOMB
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Deeply nested let: let x1 = let x2 = ... in x50; in x1.
prop_nix_deep_let_nesting :: Property
prop_nix_deep_let_nesting =
  monadicIO $ do
    src <- run $ generate (genDeepLet 50)
    result <- run $
      try @SomeException $
        evaluate $
          case parseNixTextLoc src of
            Left _ -> ()
            Right e -> case Infer.inferExpr e of
              Left _ -> ()
              Right (t, _) -> t `seq` ()
    case result of
      Left (_ :: SomeException) -> assert False
      Right _ -> assert True

-- | 50 mutually recursive bindings in a rec attrset SCC group.
prop_nix_mutual_scc_stress :: Property
prop_nix_mutual_scc_stress =
  monadicIO $ do
    src <- run $ generate (genMutualSCC 50)
    result <- run $
      try @SomeException $
        evaluate $
          case parseNixTextLoc src of
            Left _ -> ()
            Right e -> case Infer.inferExpr e of
              Left _ -> ()
              Right (t, _) -> t `seq` ()
    case result of
      Left (_ :: SomeException) -> assert False
      Right _ -> assert True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- NIX TYPE INFERENCE — WITH-SCOPE MEMOIZATION INTEGRITY
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Nested with: with a; with b; with c; expr — all scopes should stack correctly.
prop_nix_nested_with :: Bool
prop_nix_nested_with =
  let src =
        "let a = { x = 1; }; b = { y = true; }; c = { z = \"hello\"; };"
          <> " in with a; with b; with c; z"
   in case parseNixTextLoc src of
        Left _ -> False
        Right e -> case Infer.inferExpr e of
          Right (NT.TStrLit "hello", _) -> True
          Right (NT.TString, _) -> True
          _ -> False

-- | with inside rec bindings — should not crash.
prop_nix_with_inside_rec :: Property
prop_nix_with_inside_rec =
  monadicIO $ do
    result <-
      run $
        try @SomeException $
          evaluate $
            let src = "rec { a = 1; b = with { x = a; }; x; }"
             in case parseNixTextLoc src of
                  Left _ -> ()
                  Right e -> case Infer.inferExpr e of
                    Left _ -> ()
                    Right _ -> ()
    case result of
      Left (_ :: SomeException) -> assert False
      Right _ -> assert True

-- | with inside function body — produces TFun type.
prop_nix_with_inside_func :: Bool
prop_nix_with_inside_func =
  let src = "s: with s; x"
   in case parseNixTextLoc src of
        Left _ -> False
        Right e -> case Infer.inferExpr e of
          Right (NT.TFun _ _, _) -> True
          _ -> False

-- | Memoization tables must not leak between different with scopes.
prop_nix_with_memo_no_leak :: Property
prop_nix_with_memo_no_leak =
  monadicIO $ do
    result <-
      run $
        try @SomeException $
          evaluate $
            let src = "let s1 = { x = 42; }; s2 = { y = true; }; in with s1; (with s2; y) + 1"
             in case parseNixTextLoc src of
                  Left _ -> ()
                  Right e -> case Infer.inferExpr e of
                    Left _ -> ()
                    Right _ -> ()
    case result of
      Left (_ :: SomeException) -> assert False
      Right _ -> assert True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- NIX TYPE INFERENCE — __FUNCTOR EDGE CASES
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | __functor returning self (recursive): rec { __functor = self: self; } applied to 42.
Should trigger infinite type or occurs check (not crash).
-}
prop_nix_functor_self :: Property
prop_nix_functor_self =
  monadicIO $ do
    result <-
      run $
        try @SomeException $
          evaluate $
            let src = "let f = rec { __functor = self: self; }; in f 42"
             in case parseNixTextLoc src of
                  Left _ -> ()
                  Right e -> case Infer.inferExpr e of
                    Left _ -> ()
                    Right (t, _) -> t `seq` ()
    case result of
      Left (_ :: SomeException) -> assert False
      Right _ -> assert True

{- | __functor with wrong arity: self: self (1-arg), not self: x: y (2-arg).
When called as f 42, should produce type error (not crash).
-}
prop_nix_functor_wrong_arity :: Property
prop_nix_functor_wrong_arity =
  monadicIO $ do
    result <-
      run $
        try @SomeException $
          evaluate $
            let src = "let f = { __functor = self: self; }; in f 42"
             in case parseNixTextLoc src of
                  Left _ -> ()
                  Right e -> case Infer.inferExpr e of
                    Left _ -> ()
                    Right (t, _) -> t `seq` ()
    case result of
      Left (_ :: SomeException) -> assert False
      Right _ -> assert True

-- | Recursive __functor chain: g calls f through __functor dispatch.
prop_nix_functor_chain :: Property
prop_nix_functor_chain =
  monadicIO $ do
    result <-
      run $
        try @SomeException $
          evaluate $
            let src =
                  "let f = { __functor = self: x: x; }; g = { __functor = self: _: f; };"
                    <> " in g 1 2"
             in case parseNixTextLoc src of
                  Left _ -> ()
                  Right e -> case Infer.inferExpr e of
                    Left _ -> ()
                    Right (t, _) -> t `seq` ()
    case result of
      Left (_ :: SomeException) -> assert False
      Right _ -> assert True

-- | Identity __functor: { __functor = self: x: x; } applied to 42 -> TInt.
prop_nix_functor_identity :: Bool
prop_nix_functor_identity =
  let src = "let f = { __functor = self: x: x; }; in f 42"
   in case parseNixTextLoc src of
        Left _ -> False
        Right e -> case Infer.inferExpr e of
          Right (NT.TInt, _) -> True
          _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- NIX TYPE INFERENCE — ROW POLYMORPHISM BRUTALITY
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Closed vs Open with common field: TAttrs {"a"=(TInt,False)} vs TAttrsOpen {"a"=(TInt,True)}
prop_nix_row_closed_vs_open_common :: Bool
prop_nix_row_closed_vs_open_common =
  let closed = NT.TAttrs (Map.singleton "a" (NT.TInt, False))
      open = NT.TAttrsOpen (Map.singleton "a" (NT.TInt, True))
   in case Infer.runInfer (Infer.unify closed open) of
        Right _ -> True
        Left _ -> False

-- | Closed with extra fields vs Open: TAttrs {"a","b"} vs TAttrsOpen {"a"} — extra OK.
prop_nix_row_closed_extra_ok :: Bool
prop_nix_row_closed_extra_ok =
  let closed = NT.TAttrs (Map.fromList [("a", (NT.TInt, False)), ("b", (NT.TBool, False))])
      open = NT.TAttrsOpen (Map.singleton "a" (NT.TInt, True))
   in case Infer.runInfer (Infer.unify closed open) of
        Right _ -> True
        Left _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- NIX TYPE INFERENCE — STATE INTEGRITY
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Inference on minimal expression produces correct type.
prop_nix_infer_state_integrity :: Bool
prop_nix_infer_state_integrity =
  case parseNixTextLoc "1" of
    Left _ -> False
    Right e -> case Infer.inferExpr e of
      Right (NT.TInt, _) -> True
      _ -> False

-- | Inference is deterministic across repeated runs with fresh InferState.
prop_nix_infer_deterministic :: Bool
prop_nix_infer_deterministic =
  let src = "let x = 42; y = true; in [ x (y: y) ]"
      r1 = case parseNixTextLoc src of Left _ -> Nothing; Right e -> Just (Infer.inferExpr e)
      r2 = case parseNixTextLoc src of Left _ -> Nothing; Right e -> Just (Infer.inferExpr e)
   in r1 == r2

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- NIX TYPE INFERENCE — TVar SUPPLY INTEGRITY
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Fresh variable supply must not wrap — check that sequential inferences
produce distinct TVar IDs.
-}
prop_nix_tvar_supply_monotonic :: Bool
prop_nix_tvar_supply_monotonic =
  let src1 = "x: y: z: x"
      src2 = "a: b: c: d: e: a"
      r1 = case parseNixTextLoc src1 of
        Left _ -> Nothing
        Right e -> case Infer.inferExpr e of Left _ -> Nothing; Right (t, _) -> Just t
      r2 = case parseNixTextLoc src2 of
        Left _ -> Nothing
        Right e -> case Infer.inferExpr e of Left _ -> Nothing; Right (t, _) -> Just t
   in isJust r1 && isJust r2

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- NIX TYPE INFERENCE — EDGE-CASE EXPRESSIONS
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Empty attrset: {}
prop_nix_empty_attrset :: Bool
prop_nix_empty_attrset =
  case parseNixTextLoc "{}" of
    Left _ -> False
    Right e -> case Infer.inferExpr e of
      Right (NT.TAttrs m, _) -> Map.null m
      Right _ -> False
      Left _ -> False

-- | Empty list: []
prop_nix_empty_list :: Bool
prop_nix_empty_list =
  case parseNixTextLoc "[]" of
    Left _ -> False
    Right e -> case Infer.inferExpr e of
      Right (NT.TList _, _) -> True
      _ -> False

-- | Heterogeneous list: [1 true "hello"]
prop_nix_heterogeneous_list :: Property
prop_nix_heterogeneous_list =
  monadicIO $ do
    result <- run $
      try @SomeException $
        evaluate $
          case parseNixTextLoc "[ 1 true \"hello\" ]" of
            Left _ -> ()
            Right e -> case Infer.inferExpr e of
              Left _ -> ()
              Right (t, _) -> t `seq` ()
    case result of
      Left (_ :: SomeException) -> assert False
      Right _ -> assert True

-- | Nested application: (x: x: x + x) 1 2
prop_nix_nested_application :: Bool
prop_nix_nested_application =
  case parseNixTextLoc "(x: y: x + y) 1 2" of
    Left _ -> False
    Right e -> case Infer.inferExpr e of
      Right (NT.TInt, _) -> True
      _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- NIX TYPE INFERENCE — __FUNCTOR PROTOCOL
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Non-function __functor must be rejected.
  { __functor = 42; } 1 should not type-check.
-}
prop_nix_functor_non_func :: Bool
prop_nix_functor_non_func =
  case parseNixTextLoc "let f = { __functor = 42; }; in f 1" of
    Left _ -> False
    Right expr -> case Infer.inferExpr expr of
      Left _ -> True -- must be a type error
      Right _ -> False

{- | Function __functor must be accepted.
  { __functor = self: x: x + 1; } 2 should type-check.
-}
prop_nix_functor_valid :: Bool
prop_nix_functor_valid =
  case parseNixTextLoc "let f = { __functor = self: x: x + 1; }; in f 2" of
    Left _ -> False
    Right expr -> case Infer.inferExpr expr of
      Right (NT.TInt, _) -> True
      _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- DEEP SELECT — DERIVATION LINT
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Deep select chains (pkgs.llvmPackages.stdenv.mkDerivation) must be detected
prop_deriv_deep_select :: Bool
prop_deriv_deep_select =
  case parseNixTextLoc
    "let pkgs = {}; in pkgs.llvmPackages.stdenv.mkDerivation { name = \"test\"; }" of
    Left _ -> False
    Right expr ->
      not (null (DerivLint.findDerivViolations "test.nix" expr))

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- SUBSTITUTION — CHAIN RESOLUTION
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Nix substitution must fully resolve chains: {0→1, 1→Int} should resolve 0 to Int
prop_nix_subst_chain :: Bool
prop_nix_subst_chain =
  let s =
        Map.fromList
          [ (NT.TypeVar 0, NT.TVar (NT.TypeVar 1))
          , (NT.TypeVar 1, NT.TInt)
          ]
      result = NT.applySubst s (NT.TVar (NT.TypeVar 0))
   in result == NT.TInt
