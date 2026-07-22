{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                       // oracle // fuzz
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Case fled up out of the octagon's rush, into the closed world of
--    consensual hallucination."
--
--                                                                                   — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The GENERATIVE oracle: a type-directed term generator whose output is
--   well-typed BY CONSTRUCTION, differentially checked against
--   `nix-instantiate`. The nixpkgs sweep covers what nixpkgs authors write;
--   this covers adversarial shapes they never do — deep closures, gnarly
--   record nesting, chained applications — where every checker rejection of
--   a term nix evaluates is a PROVEN false positive.
--
--   Verdicts (mirroring test/Oracle.hs):
--     FALSE-POSITIVE — nix evaluates it, the checker rejects it. FATAL.
--     CHECKER-CRASH  — inference threw. FATAL.
--     AGREE          — both accept.
--     TYPED-NOEVAL   — checker accepts, nix rejects (a miss — reported, not
--                      fatal: the generator can build diverging terms).
--
--   Deterministic: `--seed N` (default 20260718) reproduces a run exactly;
--   `--count N` scales it (default 400). Requires nix-instantiate on PATH.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.Process (readProcessWithExitCode)
import Test.QuickCheck.Gen (Gen, choose, elements, frequency, unGen, vectorOf)
import Test.QuickCheck.Random (mkQCGen)

import Narsil.Core.Safety (safeParseNixText)
import Narsil.Inference.Nix (builtinEnv, inferExprWithEnv)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- the type universe and its type-directed generator
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

data Ty
  = TyInt
  | TyBool
  | TyStr
  | TyList Ty
  | TyAttrs [(Text, Ty)]
  | TyFun Ty Ty
  deriving (Eq, Show)

-- | a random type, sized to keep terms readable
genTy :: Int -> Gen Ty
genTy 0 = elements [TyInt, TyBool, TyStr]
genTy n =
  frequency
    [ (3, elements [TyInt, TyBool, TyStr])
    , (1, TyList <$> genTy (n - 1))
    , (1, genAttrsTy)
    , (1, TyFun <$> genTy (n - 1) <*> genTy (n - 1))
    ]
 where
  genAttrsTy = do
    k <- choose (1, 3)
    names <- pure (take k ["alpha", "beta", "gamma"])
    tys <- vectorOf k (genTy (n - 1))
    pure (TyAttrs (zip names tys))

-- | in-scope variables, name → type
type Scope = [(Text, Ty)]

-- | a term OF the given type, in the given scope — well-typed by construction
genTerm :: Int -> Scope -> Ty -> Gen Text
genTerm depth scope ty =
  frequency $
    [(4, literal ty)]
      ++ [(6, elements varHits) | not (null varHits)]
      ++ [(w, g) | depth > 0, (w, g) <- structural]
 where
  varHits = [v | (v, t) <- scope, t == ty]
  freshName = "v" <> T.pack (show (length scope))
  structural =
    [ -- let-bind a subterm of some type, continue in extended scope

      ( 3
      , do
          bty <- genTy 1
          bound <- genTerm (depth - 1) scope bty
          body <- genTerm (depth - 1) ((freshName, bty) : scope) ty
          pure ("(let " <> freshName <> " = " <> bound <> "; in " <> body <> ")")
      )
    , -- apply a freshly built lambda: (x: body) arg

      ( 3
      , do
          aty <- genTy 1
          arg <- genTerm (depth - 1) scope aty
          body <- genTerm (depth - 1) ((freshName, aty) : scope) ty
          pure ("((" <> freshName <> ": " <> body <> ") (" <> arg <> "))")
      )
    , -- select the right field out of a bigger literal record

      ( 2
      , do
          other <- genTerm (depth - 1) scope TyInt
          inner <- genTerm (depth - 1) scope ty
          pure ("({ pick = " <> inner <> "; padding = " <> other <> "; }.pick)")
      )
    , -- both branches of an if

      ( 2
      , do
          c <- genTerm (depth - 1) scope TyBool
          t <- genTerm (depth - 1) scope ty
          e <- genTerm (depth - 1) scope ty
          pure ("(if " <> c <> " then " <> t <> " else " <> e <> ")")
      )
    ]
      ++ arithmetic
  arithmetic = case ty of
    TyInt ->
      [
        ( 2
        , do
            a <- genTerm (depth - 1) scope TyInt
            b <- genTerm (depth - 1) scope TyInt
            op <- elements ["+", "-", "*"]
            pure ("(" <> a <> " " <> op <> " " <> b <> ")")
        )
      ]
    TyStr ->
      [
        ( 2
        , do
            a <- genTerm (depth - 1) scope TyStr
            b <- genTerm (depth - 1) scope TyStr
            pure ("(" <> a <> " + " <> b <> ")")
        )
      ]
    TyList el ->
      [
        ( 2
        , do
            a <- genTerm (depth - 1) scope (TyList el)
            b <- genTerm (depth - 1) scope (TyList el)
            pure ("(" <> a <> " ++ " <> b <> ")")
        )
      ]
    TyBool ->
      [
        ( 2
        , do
            a <- genTerm (depth - 1) scope TyBool
            b <- genTerm (depth - 1) scope TyBool
            op <- elements ["&&", "||"]
            pure ("(" <> a <> " " <> op <> " " <> b <> ")")
        )
      ]
    _ -> []
  literal = \case
    TyInt -> T.pack . show <$> choose (0 :: Int, 99)
    TyBool -> elements ["true", "false"]
    TyStr -> do
      s <- elements ["a", "bb", "xyz", ""]
      pure ("\"" <> s <> "\"")
    TyList el -> do
      k <- choose (0, 3)
      xs <- vectorOf k (genTerm (max 0 (depth - 1)) scope el)
      pure ("[ " <> T.unwords xs <> " ]")
    TyAttrs fields -> do
      binds <- forM fields $ \(f, t) -> do
        v <- genTerm (max 0 (depth - 1)) scope t
        pure (f <> " = " <> v <> ";")
      pure ("{ " <> T.unwords binds <> " }")
    TyFun a b -> do
      body <- genTerm (max 0 (depth - 1)) ((freshName, a) : scope) b
      pure ("(" <> freshName <> ": " <> body <> ")")

-- | one complete case: a target type and a closed term of it
genCase :: Gen Text
genCase = do
  ty <- genTy 2
  -- functions can't go through `builtins.typeOf`-based eval comparison
  -- usefully at depth, but they still eval — keep them in the mix
  genTerm 3 [] ty

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- the two sides + verdicts
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

checkerAccepts :: Text -> IO (Either Text Bool)
checkerAccepts src = do
  r <- try $ do
    parsed <- safeParseNixText src
    either
      (\e -> pure (Left ("parse: " <> T.pack (show e))))
      (\e -> Right . either (const False) (const True) <$> evaluate (inferExprWithEnv builtinEnv e))
      parsed
  pure $ case r of
    Left (ex :: SomeException) -> Left (T.pack (show ex))
    Right v -> v

nixEvaluates :: Text -> IO Bool
nixEvaluates src = do
  (code, _, _) <-
    readProcessWithExitCode
      "nix-instantiate"
      ["--eval", "--expr", T.unpack ("builtins.seq (" <> src <> ") \"ok\"")]
      ""
  pure (code == ExitSuccess)

main :: IO ()
main = do
  args <- getArgs
  let grab flag def = maybe def read (lookup flag (zip args (drop 1 args)))
      seed = grab "--seed" (20260718 :: Int)
      count = grab "--count" (400 :: Int)
      cases = [unGen genCase (mkQCGen (seed + i)) 30 | i <- [1 .. count]]
  putStrLn ("oracle-fuzz: " ++ show count ++ " type-directed terms, seed " ++ show seed)

  results <- forM cases $ \src -> do
    ours <- checkerAccepts src
    nix <- nixEvaluates src
    pure (src, ours, nix)

  let crashes = [(s, e) | (s, Left e, _) <- results]
      falsePos = [s | (s, Right False, True) <- results]
      agree = length [() | (_, Right True, True) <- results]
      typedNoeval = [s | (s, Right True, False) <- results]
      agreeReject = length [() | (_, Right False, False) <- results]

  putStrLn
    ( "oracle-fuzz: agree="
        ++ show agree
        ++ " typed-noeval="
        ++ show (length typedNoeval)
        ++ " agree-reject="
        ++ show agreeReject
        ++ " FALSE-POSITIVES="
        ++ show (length falsePos)
        ++ " CRASHES="
        ++ show (length crashes)
    )
  mapM_ (\s -> putStrLn ("  FALSE-POSITIVE: " ++ T.unpack s)) (take 10 falsePos)
  mapM_ (\(s, e) -> putStrLn ("  CRASH: " ++ T.unpack s ++ "\n    " ++ T.unpack e)) (take 5 crashes)
  mapM_ (\s -> putStrLn ("  typed-noeval: " ++ T.unpack s)) (take 5 typedNoeval)

  if null falsePos && null crashes then exitSuccess else exitFailure
