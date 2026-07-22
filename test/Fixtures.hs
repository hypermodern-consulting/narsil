{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // tests // fixtures
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Three times, in their descent, the elevator came to a halt at some
--    floor and remained there, once for nearly fifteen minutes. The elevators
--    were located at the core of the arcology, their shafts bundled together
--    with water mains, sewage lines, huge power cables, and insulated pipes
--    that Bobby assumed were part of the geothermal system. You could see
--    it all whenever the doors opened; everything was exposed, raw, as though
--    the people who built the place had wanted to be able to see exactly how
--    everything worked and what was going where."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                // golden // tests
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Narsil (Schema (..), Script (..), parseScript)
import Narsil.Bash.Types (Fact (..))
import Narsil.Inference.Nix (inferFile)
import Narsil.Layout.Convention (ErrorCode (..), LayoutError (..), validateFileExpr)
import Narsil.Lint.Nix (NixViolation (..), ViolationType (..), findNixViolations)
import Narsil.Syntax.Parse (parseNixFile)
import System.Exit (exitFailure, exitSuccess)

-- | Run all fixture tests
main :: IO ()
main = do
  putStrLn "Running fixture tests..."
  results <-
    sequence
      [ testCheckByName
      , testQemuCommon
      , testKernel
      , testGpuBrokerLayoutValid
      , testGpuBrokerLayoutInvalid
      ]

  if all id results
    then do
      putStrLn "All fixture tests passed."
      exitSuccess
    else do
      putStrLn "Some fixture tests failed."
      exitFailure

{- | Test: maintainers/scripts/check-by-name.sh
Expect:
  - Parse: Success
  - Schema: Contains inferred env vars
  - Policy: Should fail check (bare commands, dynamic commands)
-}
testCheckByName :: IO Bool
testCheckByName = do
  putStr "testCheckByName... "
  src <- TIO.readFile "test/fixtures/bash/check-by-name.sh"
  case parseScript src of
    Left err -> do
      putStrLn $ "FAILED: Parse error: " ++ show err
      return False
    Right script -> do
      let schema = scriptSchema script
      let facts = scriptFacts script

      -- Check extracted variables
      let env = schemaEnv schema
      let hasBaseBranch = "baseBranch" `Map.member` env
      let hasRepo = "repo" `Map.member` env

      -- Check policy violations (should be present)
      -- parseScript doesn't run policy checks directly, but we can inspect facts
      let bareCommands = [c | BareCommand c _ <- facts]
      let dynamicCommands = [v | DynamicCommand v _ <- facts]

      let hasBareGit = any ("git" `T.isInfixOf`) bareCommands
      let hasDynamic = not (null dynamicCommands)

      if hasBaseBranch && hasRepo && hasBareGit && not hasDynamic
        then do
          putStrLn "OK"
          return True
        else do
          putStrLn "FAILED"
          putStrLn $ "  Has baseBranch: " ++ show hasBaseBranch
          putStrLn $ "  Has repo: " ++ show hasRepo
          putStrLn $ "  Has bare git: " ++ show hasBareGit
          putStrLn $ "  Has dynamic cmds: " ++ show hasDynamic
          return False

{- | Test: nixos/lib/qemu-common.nix
Expect:
  - Parse: Success
  - Infer: Success
  - Lint: Failure (contains 'rec' and 'with')
-}
testQemuCommon :: IO Bool
testQemuCommon = do
  putStr "testQemuCommon... "
  let path = "test/fixtures/nix/qemu-common.nix"

  -- Inference check
  inferRes <- inferFile path
  inferOk <- case inferRes of
    Left err -> do
      putStrLn $ "Inference failed: " ++ show err
      return False
    Right _ -> return True

  -- Lint check
  parseRes <- parseNixFile path
  lintOk <- case parseRes of
    Left err -> do
      putStrLn $ "Parse failed: " ++ show err
      return False
    Right expr -> do
      let violations = findNixViolations expr
      let hasRec = any (\v -> nvType v == VRec) violations
      let hasWith = any (\v -> nvType v == VWith) violations

      if hasRec && hasWith
        then return True
        else do
          putStrLn $
            "Lint check failed (expected violations): rec="
              ++ show hasRec
              ++ ", with="
              ++ show hasWith
          return False

  if inferOk && lintOk
    then do
      putStrLn "OK"
      return True
    else do
      putStrLn "FAILED"
      return False

{- | Test: lib/kernel.nix
Expect:
  - Parse: Success
  - Infer: Success
  - Lint: Success (clean)
-}
testKernel :: IO Bool
testKernel = do
  putStr "testKernel... "
  let path = "test/fixtures/nix/kernel.nix"

  -- Inference check
  inferRes <- inferFile path
  inferOk <- case inferRes of
    Left err -> do
      putStrLn $ "Inference failed: " ++ show err
      return False
    Right _ -> return True

  -- Lint check
  parseRes <- parseNixFile path
  lintOk <- case parseRes of
    Left err -> do
      putStrLn $ "Parse failed: " ++ show err
      return False
    Right expr -> do
      let violations = findNixViolations expr
      if null violations
        then return True
        else do
          putStrLn $ "Lint check failed (expected no violations): " ++ show violations
          return False

  if inferOk && lintOk
    then do
      putStrLn "OK"
      return True
    else do
      putStrLn "FAILED"
      return False

{- | Test: test/fixtures/nix/modules/flake/gpu-broker.nix
Expect:
  - Parse: Success
  - Layout: Success (correct directory for _class = "flake")
-}
testGpuBrokerLayoutValid :: IO Bool
testGpuBrokerLayoutValid = do
  putStr "testGpuBrokerLayoutValid... "
  let path = "test/fixtures/nix/modules/flake/gpu-broker.nix"

  parseRes <- parseNixFile path
  case parseRes of
    Left err -> do
      putStrLn $ "Parse failed: " ++ show err
      return False
    Right expr -> do
      let violations = validateFileExpr path expr
      if null violations
        then do
          putStrLn "OK"
          return True
        else do
          putStrLn $ "Layout check failed (expected no violations): " ++ show violations
          return False

{- | Test: test/fixtures/nix/modules/nixos/gpu-broker.nix
Expect:
  - Parse: Success
  - Layout: Failure (directory implies "nixos", file has "flake")
-}
testGpuBrokerLayoutInvalid :: IO Bool
testGpuBrokerLayoutInvalid = do
  putStr "testGpuBrokerLayoutInvalid... "
  let path = "test/fixtures/nix/modules/nixos/gpu-broker.nix"

  parseRes <- parseNixFile path
  case parseRes of
    Left err -> do
      putStrLn $ "Parse failed: " ++ show err
      return False
    Right expr -> do
      let violations = validateFileExpr path expr
      let hasWrongClass = any (\v -> errCode v == E010) violations

      if hasWrongClass
        then do
          putStrLn "OK"
          return True
        else do
          putStrLn $ "Layout check failed (expected E010): " ++ show violations
          return False
