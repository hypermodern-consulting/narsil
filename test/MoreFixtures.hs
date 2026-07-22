{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                      // tests // more // fixtures
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "'Is that Wig got a damn good price on them, somewhere in New York.
--    Money, I mean. But sometimes other things as well, things that came back
--    up...'
--
--    'What sort of things?'
--
--    'Software, I guess it was. He's a secretive old fuck when it comes to
--    what he thinks his voices are telling him to do... Once, it was
--    something he swore was biosoft, that new stuff...'"
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                 // additional // fixture // tests
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Narsil (Script (..), parseScript)
import Narsil.Bash.Parse (parseBash)
import Narsil.Bash.Types (Fact (..))
import Narsil.Lint.Forbidden (Violation (..), ViolationType (..), findViolations)
import Narsil.Lint.Nix (NixViolation (..), findNixViolations)
import Narsil.Lint.Nix qualified as NixLint
import Narsil.Syntax.Parse (BashScript (..), extractBashScripts, parseNixFile)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  putStrLn "Running more fixture tests..."
  results <-
    sequence
      [ testNativelinkIntegration
      , testIsospinMain
      ]

  if all id results
    then do
      putStrLn "All additional fixture tests passed."
      exitSuccess
    else do
      putStrLn "Some additional fixture tests failed."
      exitFailure

{- | Test: test/fixtures/bash/nativelink-integration.sh
Expect:
  - Parse: Success
  - Policy: Heavy violations (heredocs, bare commands)
-}
testNativelinkIntegration :: IO Bool
testNativelinkIntegration = do
  putStr "testNativelinkIntegration... "
  src <- TIO.readFile "test/fixtures/bash/nativelink-integration.sh"

  -- Check for forbidden constructs (heredocs, etc.)
  let lintViolations = case parseBash src of
        Right ast -> findViolations ast
        Left _ -> []

  let hasHeredoc = any (\v -> vType v == VHeredoc) lintViolations

  case parseScript src of
    Left err -> do
      putStrLn $ "FAILED: Parse error: " ++ show err
      return False
    Right script -> do
      let facts = scriptFacts script

      -- Check for specific expected facts
      let bareCommands = [c | BareCommand c _ <- facts]

      let hasDocker = any ("docker" `T.isInfixOf`) bareCommands
      let hasBazel = any ("bazel" `T.isInfixOf`) bareCommands

      -- Note: Arrays are currently not extracted as Facts, so we skip checking them explicitly
      -- via Facts.
      -- But the script parses successfully, which is part of the test.

      if hasDocker && hasBazel && hasHeredoc
        then do
          putStrLn "OK"
          return True
        else do
          putStrLn "FAILED"
          putStrLn $ "  Has docker: " ++ show hasDocker
          putStrLn $ "  Has bazel: " ++ show hasBazel
          putStrLn $ "  Has heredoc: " ++ show hasHeredoc
          return False

{- | Test: test/fixtures/nix/isospin-main.nix
Expect:
  - Parse: Success
  - Lint: Failure (rec, with)
  - Bash Extraction: Success (> 10 scripts)
-}
testIsospinMain :: IO Bool
testIsospinMain = do
  putStr "testIsospinMain... "
  let path = "test/fixtures/nix/isospin-main.nix"

  -- Parse Nix
  parseRes <- parseNixFile path
  case parseRes of
    Left err -> do
      putStrLn $ "FAILED: Parse error: " ++ show err
      return False
    Right expr -> do
      -- Lint check
      let violations = findNixViolations expr
      let hasRec = any (\v -> nvType v == NixLint.VRec) violations
      let hasWith = any (\v -> nvType v == NixLint.VWith) violations

      -- Bash extraction
      scriptsRes <- extractBashScripts path
      case scriptsRes of
        Left err -> do
          putStrLn $ "FAILED: Bash extraction error: " ++ show err
          return False
        Right scripts -> do
          let scriptCount = length scripts
          let hasManyScripts = scriptCount > 10

          -- Check a specific script (e.g., "boot-vm") for violations
          let checkScript = filter (\s -> bsName s == "boot-vm") scripts

          hasViolations <- case checkScript of
            [s] -> do
              let src = bsContent s
              case parseScript src of
                Right sc -> do
                  let facts = scriptFacts sc
                  let bare = [c | BareCommand c _ <- facts]
                  -- We expect 'sudo' and 'buck2' to be bare commands
                  return $ any ("sudo" `T.isInfixOf`) bare && any ("buck2" `T.isInfixOf`) bare
                Left _ -> return False
            _ -> do
              putStrLn $ "DEBUG: 'boot-vm' script not found. Scripts: " ++ show (map bsName scripts)
              return False

          if hasRec && hasWith && hasManyScripts && hasViolations
            then do
              putStrLn "OK"
              return True
            else do
              putStrLn "FAILED"
              putStrLn $ "  Has rec: " ++ show hasRec
              putStrLn $ "  Has with: " ++ show hasWith
              putStrLn $ "  Script count: " ++ show scriptCount
              putStrLn $ "  Script violations found: " ++ show hasViolations
              return False
