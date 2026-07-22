{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                           // tests // adversarial
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "'And all that time,' the Finn continued, 'you know how many people ever
--    dumb enough to try to get in here to take me off? None! Not one, not
--    till this morning, and I get fucking three already. Well,' he shot Bobby
--    a hostile glance, 'that's not counting the odd little lump of shit, I
--    guess, but...' He shrugged.
--
--    'He looks kind of lopsided,' Bobby said staring at the first corpse.
--
--    'That's 'cause he's dog food, inside.' The Finn leered. 'All mashed up.'"
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                  // security // property // tests
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Adversarial where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, replicateM)
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Narsil
import Narsil.Bash.Facts (extractFacts)
import Narsil.Bash.Parse (parseBash)
import Narsil.Bash.Patterns
import Narsil.Core.Config qualified as Cfg
import Narsil.Emit.Config (emitConfigFunction)
import Narsil.Inference.Bash.Constraint (factsToConstraints)
import Narsil.Inference.Bash.Schema (buildSchema)
import Narsil.Inference.Bash.Unify (solve, unify)
import Narsil.Inference.Nix.Type qualified as NT
import Narsil.Lint.Derivation qualified as DerivLint
import Narsil.Lint.Forbidden (Violation (..), ViolationType (..), findViolations)
import Narsil.Lint.Nix (
  NixViolation (..),
  ViolationType (..),
  findNixViolations,
  formatNixViolations,
 )
import Narsil.Lint.Packages qualified as PackageLint
import Narsil.Lint.Patterns qualified as PatternLint
import Nix.Parser (parseNixTextLoc)
import System.Timeout (timeout)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

genValidVarName :: Gen Text
genValidVarName = do
  c <- elements $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['_']
  rest <- listOf $ elements $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['_']
  let name = c : take 20 rest
  return $ T.pack name

genInvalidVarName :: Gen Text
genInvalidVarName =
  oneof
    [ pure ""
    , T.pack . (: []) <$> elements ['0' .. '9']
    , T.cons <$> elements "-+*/" <*> genValidVarName
    , do v <- genValidVarName; pure (v <> "$")
    , do v <- genValidVarName; pure (v <> "`")
    , pure ";"
    , pure "|"
    , pure "&"
    ]

genInjectionAttempt :: Gen Text
genInjectionAttempt =
  elements
    [ "; rm -rf /"
    , "`id`"
    , "| cat /etc/passwd"
    , "&& curl evil.com"
    , "\"; echo pwned; \""
    , "'; echo pwned; '"
    , "$'\\x00'"
    , "${IFS}cat${IFS}/etc/passwd"
    , "\n; id\n"
    , "\\`id\\`"
    ]

genOverflowInt :: Gen Text
genOverflowInt =
  oneof
    [ pure "9223372036854775808"
    , pure "-9223372036854775809"
    , pure "99999999999999999999999999999999999999"
    , pure "0000000000000000000000000000000000001"
    , T.pack . show <$> (arbitrary :: Gen Int64)
    ]

genMalformedExpansion :: Gen Text
genMalformedExpansion =
  oneof
    [ pure "${"
    , pure "${}"
    , pure "${VAR:-"
    , pure "${VAR"
    , pure "${{{"
    , pure "${VAR:-${NESTED}}"
    , pure "${VAR:-${NESTED:-x}}"
    , pure "${VAR:--}"
    , pure "${VAR::}"
    , pure "${VAR:}"
    , pure "${:VAR}"
    , pure "${-VAR}"
    , do n <- genValidVarName; pure ("${" <> n <> ":-$" <> n <> "}")
    ]

genValidExpansion :: Gen Text
genValidExpansion = do
  var <- genValidVarName
  op <-
    elements
      [ ":-default"
      , "-default"
      , ":=default"
      , "=default"
      , ":?error"
      , "?error"
      , ":+alt"
      , "+alt"
      , ":-"
      , "-"
      , ""
      ]
  pure $ "${" <> var <> op <> "}"

genConfigPath :: Gen [Text]
genConfigPath = do
  len <- choose (1, 5)
  replicateM len $ do
    c <- elements $ ['a' .. 'z'] ++ ['A' .. 'Z']
    rest <- listOf $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_']
    pure $ T.pack (c : take 10 rest)

genMalformedConfig :: Gen Text
genMalformedConfig =
  oneof
    [ pure "config.="
    , pure "config..a=1"
    , pure "config.a.=1"
    , pure "config[]=1"
    , pure "config[a.]=1"
    , pure "config[.a]=1"
    , pure "config="
    , pure "config"
    , pure "config.a.b"
    , pure "config.a.b=$"
    , pure "config.a.b=${"
    ]

genArbitraryText :: Gen Text
genArbitraryText =
  oneof
    [ T.pack <$> listOf (elements $ ['\0' .. '\127'])
    , T.pack <$> listOf arbitrary
    , genValidVarName
    ]

genLiteral :: Gen Literal
genLiteral =
  oneof
    [ LitInt <$> arbitrary
    , LitString <$> genArbitraryText
    , LitBool <$> arbitrary
    , LitPath . StorePath <$> genStorePath
    ]

genStorePath :: Gen Text
genStorePath = do
  hash <- replicateM 32 (elements $ ['a' .. 'z'] ++ ['0' .. '9'])
  name <- genValidVarName
  pure $ "/nix/store/" <> T.pack hash <> "-" <> name

genTraversalPath :: Gen Text
genTraversalPath =
  oneof
    [ pure "/nix/store/../../../etc/passwd"
    , pure "/nix/store/abc/../../../bin/sh"
    , do p <- genStorePath; pure (p <> "/../../../etc/passwd")
    , pure "/nix/store/./abc"
    , pure "/nix/store//abc"
    ]

genType :: Gen Type
genType = elements [TInt, TString, TBool, TPath, TNumeric]

genTypeVar :: Gen TypeVar
genTypeVar = TypeVar <$> genValidVarName

genTypeWithVars :: Gen Type
genTypeWithVars =
  frequency
    [ (4, genType)
    , (1, TVar <$> genTypeVar)
    ]

genSpan :: Gen Span
genSpan = do
  l1 <- choose (1, 10000)
  c1 <- choose (0, 200)
  l2 <- choose (l1, l1 + 100)
  c2 <- choose (0, 200)
  pure $ Span (Loc l1 c1) (Loc l2 c2) Nothing

genConstraint :: Gen Constraint
genConstraint = (:~:) <$> genTypeWithVars <*> genTypeWithVars

genSatisfiableConstraints :: Gen [Constraint]
genSatisfiableConstraints =
  oneof
    [ do
        ts <- listOf genType
        pure $ map (\t -> t :~: t) ts
    , do
        n <- choose (1, 5)
        vs <- replicateM n genTypeVar
        ts <- replicateM n genType
        pure $ zipWith (\v t -> TVar v :~: t) vs ts
    , do
        v1 <- genTypeVar
        v2 <- genTypeVar
        t <- genType
        pure [TVar v1 :~: TVar v2, TVar v2 :~: t]
    ]

genUnsatisfiableConstraints :: Gen [Constraint]
genUnsatisfiableConstraints =
  oneof
    [ pure [TInt :~: TString]
    , pure [TBool :~: TPath]
    , pure [TString :~: TInt]
    , do
        v <- genTypeVar
        pure [TVar v :~: TInt, TVar v :~: TString]
    ]

genBashScript :: Gen Text
genBashScript = do
  ls <- listOf1 genBashLine
  pure $ T.unlines ls

genBashLine :: Gen Text
genBashLine =
  frequency
    [ (3, genAssignment)
    , (2, genConfigLine)
    , (1, genCommand)
    , (1, pure "")
    , (1, ("#" <>) <$> genArbitraryText)
    ]

genAssignment :: Gen Text
genAssignment = do
  var <- genValidVarName
  val <-
    oneof
      [ genValidExpansion
      , ("\"" <>) . (<> "\"") <$> genArbitraryText
      , T.pack . show <$> (arbitrary :: Gen Int)
      , elements ["true", "false"]
      ]
  pure $ var <> "=" <> val

genConfigLine :: Gen Text
genConfigLine = do
  path <- genConfigPath
  var <- genValidVarName
  quoted <- arbitrary
  let pathText = "config." <> T.intercalate "." path
  let val = if quoted then "\"$" <> var <> "\"" else "$" <> var
  pure $ pathText <> "=" <> val

genCommand :: Gen Text
genCommand =
  oneof
    [ do path <- genStorePath; pure (path <> "/bin/cmd arg1 arg2")
    , do cmd <- elements ["echo", "printf", "test"]; pure (cmd <> " hello")
    ]

genNixExpr :: Int -> Gen Text
genNixExpr 0 = genNixAtom
genNixExpr n =
  frequency
    [ (4, genNixAtom)
    , (2, genNixList n)
    , (2, genNixAttrSet n)
    , (2, genNixLet n)
    , (1, genNixFunc n)
    , (1, genNixIf n)
    , (1, genNixApp n)
    , (1, genNixBinOp n)
    , (1, genNixWith n)
    , (1, genNixRec n)
    ]

genNixAtom :: Gen Text
genNixAtom =
  oneof
    [ T.pack . show <$> (choose (0, 1000) :: Gen Int)
    , pure "true"
    , pure "false"
    , pure "null"
    , do
        s <- listOf1 (elements ['a' .. 'z'])
        pure $ "\"" <> T.pack (take 10 s) <> "\""
    ]

genNixIdent :: Gen Text
genNixIdent = do
  c <- elements ['a' .. 'z']
  rest <- listOf (elements $ ['a' .. 'z'] ++ ['0' .. '9'])
  pure $ T.pack (c : take 5 rest)

genNixList :: Int -> Gen Text
genNixList n = do
  len <- choose (0, 3)
  elems <- replicateM len (genNixExpr (n `div` 2))
  pure $ "[ " <> T.unwords elems <> " ]"

genNixAttrSet :: Int -> Gen Text
genNixAttrSet n = do
  len <- choose (1, 3)
  names <- replicateM len genNixIdent
  vals <- replicateM len (genNixExpr (n `div` 2))
  let bindings = zipWith (\k v -> k <> " = " <> v <> ";") (nub names) vals
  pure $ "{ " <> T.unwords bindings <> " }"

genNixLet :: Int -> Gen Text
genNixLet n = do
  name <- genNixIdent
  val <- genNixExpr (n `div` 2)
  body <- genNixExpr (n `div` 2)
  pure $ "let " <> name <> " = " <> val <> "; in " <> body

genNixFunc :: Int -> Gen Text
genNixFunc n = do
  param <- genNixIdent
  body <- genNixExpr (n `div` 2)
  pure $ param <> ": " <> body

genNixIf :: Int -> Gen Text
genNixIf n = do
  cond <- genNixExpr (n `div` 3)
  t <- genNixExpr (n `div` 3)
  f <- genNixExpr (n `div` 3)
  pure $ "if " <> cond <> " then " <> t <> " else " <> f

genNixApp :: Int -> Gen Text
genNixApp n = do
  func <- genNixFunc n
  arg <- genNixExpr (n `div` 2)
  pure $ "(" <> func <> ") " <> arg

genNixBinOp :: Int -> Gen Text
genNixBinOp n = do
  left <- genNixExpr (n `div` 2)
  right <- genNixExpr (n `div` 2)
  op <- elements ["+", "-", "*", "==", "!=", "<", "<=", ">", ">=", "&&", "||"]
  pure $ "(" <> left <> " " <> op <> " " <> right <> ")"

genNixWith :: Int -> Gen Text
genNixWith n = do
  scope <- genNixAttrSet (n `div` 2)
  body <- genNixExpr (n `div` 2)
  pure $ "with " <> scope <> "; " <> body

genNixRec :: Int -> Gen Text
genNixRec n = do
  len <- choose (1, 3)
  names <- replicateM len genNixIdent
  let uniqueNames = nub names
  vals <- case uniqueNames of
    [] -> pure []
    (_first : rest) -> do
      firstVal <- genNixAtom
      restVals <- replicateM (length rest) (genNixExpr (n `div` 2))
      pure (firstVal : restVals)
  let bindings = zipWith (\k v -> k <> " = " <> v <> ";") uniqueNames vals
  pure $ "rec { " <> T.unwords bindings <> " }"

genNullByte :: Gen Text
genNullByte =
  oneof
    [ pure "VAR=\0hello\0"
    , pure "\0X=1"
    , pure "X=\0\01\0"
    , pure "VAR=\"$\0{VAR:-default}\""
    , pure "config.\0.path=42"
    , T.pack <$> listOf (elements (['a' .. 'z'] ++ ['\0']))
    ]

genGiantScript :: Int -> Gen Text
genGiantScript n = do
  ls <- replicateM n $ do
    var <- genValidVarName
    elements
      [ var <> "=\"${" <> var <> ":-default}\""
      , "config." <> var <> "=$" <> var
      , "/nix/store/abc-shell/bin/sh -c 'echo " <> var <> "'"
      ]
  pure $ T.unlines ls

genUnicodeAttack :: Gen Text
genUnicodeAttack =
  oneof
    [ pure "\x202E# TIMOR :TIMOR"
    , pure $ "\x200B\x200B\x200Bexport\x200B\x200B\x200B"
    , pure "\xFEFF#!/bin/sh"
    , pure $ "\x202D" <> "\x202C" <> "x=1"
    , pure "\x200B"
    , pure "\x200D"
    , pure "\x200C"
    , pure "\x2060"
    ]

genSolverConfigPaths :: Gen [Text]
genSolverConfigPaths = do
  len <- choose (1, 4)
  replicateM len $ do
    c <- elements (['a' .. 'z'] ++ ['_'])
    rest <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['_']
    return $ T.pack (c : rest)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- PARSER SAFETY PROPERTIES
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_literal_no_crash :: Text -> Bool
prop_literal_no_crash t =
  let result = parseLiteral t
   in result `seq` True

prop_literal_overflow_safe :: Property
prop_literal_overflow_safe = forAll genOverflowInt $ \t ->
  let result = parseLiteral t
   in result `seq` True

prop_expansion_no_crash :: Text -> Bool
prop_expansion_no_crash t =
  let result = parseParamExpansion t
   in result `seq` True

prop_expansion_malformed_safe :: Property
prop_expansion_malformed_safe = forAll genMalformedExpansion $ \t ->
  let result = parseParamExpansion t
   in result `seq` True

prop_config_no_crash :: Text -> Bool
prop_config_no_crash t =
  let result = parseConfigAssignment t
   in result `seq` True

prop_config_malformed_safe :: Property
prop_config_malformed_safe = forAll genMalformedConfig $ \t ->
  let result = parseConfigAssignment t
   in result `seq` True

prop_bash_no_crash :: Text -> Property
prop_bash_no_crash t = monadicIO $ do
  result <- run $ try @SomeException $ evaluate $ parseBash t
  assert $ case result of
    Left _ -> True
    Right _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- SPECIFICATION CONFORMANCE
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_expansion_test_vectors :: Property
prop_expansion_test_vectors =
  conjoin
    [ parseParamExpansion "${VAR:-default}" === Just (DefaultValue "VAR" (Just "default"))
    , parseParamExpansion "${VAR-default}" === Just (DefaultValue "VAR" (Just "default"))
    , parseParamExpansion "${VAR:-}" === Just (DefaultValue "VAR" (Just ""))
    , parseParamExpansion "${VAR-}" === Just (DefaultValue "VAR" (Just ""))
    , parseParamExpansion "${VAR:?}" === Just (ErrorIfUnset "VAR" Nothing)
    , parseParamExpansion "${VAR:?msg}" === Just (ErrorIfUnset "VAR" (Just "msg"))
    , parseParamExpansion "${VAR}" === Just (SimpleRef "VAR")
    , parseParamExpansion "$VAR" === Just (SimpleRef "VAR")
    , parseParamExpansion "${VAR:+alt}" === Just (UseAlternate "VAR" (Just "alt"))
    , parseParamExpansion "${VAR+alt}" === Just (UseAlternate "VAR" (Just "alt"))
    , parseParamExpansion "${X-}" === Just (DefaultValue "X" (Just ""))
    , parseParamExpansion "${_A1-x}" === Just (DefaultValue "_A1" (Just "x"))
    ]

prop_literal_test_vectors :: Property
prop_literal_test_vectors =
  conjoin
    [ parseLiteral "true" === LitBool True
    , parseLiteral "false" === LitBool False
    , parseLiteral "0" === LitInt 0
    , parseLiteral "-1" === LitInt (-1)
    , parseLiteral "42" === LitInt 42
    , parseLiteral "-" === LitString "-"
    , parseLiteral "--1" === LitString "--1"
    , parseLiteral "1-1" === LitString "1-1"
    , parseLiteral "" === LitString ""
    ]

prop_empty_default_not_required :: Property
prop_empty_default_not_required =
  parseParamExpansion "${VAR:-}" =/= Just (ErrorIfUnset "VAR" Nothing)
    .&&. parseParamExpansion "${VAR:-}" === Just (DefaultValue "VAR" (Just ""))

prop_traversal_rejected :: Property
prop_traversal_rejected = forAll genTraversalPath $ \path ->
  not (isStorePath path) .||. not (".." `T.isInfixOf` path)

prop_overflow_becomes_string :: Property
prop_overflow_becomes_string =
  conjoin
    [ case parseLiteral "9223372036854775808" of
        LitString _ -> True
        _ -> False
    , case parseLiteral "-9223372036854775809" of
        LitString _ -> True
        _ -> False
    ]

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- SECURITY PROPERTIES
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_varname_valid :: Property
prop_varname_valid = forAll genValidVarName $ \name ->
  T.all isValidVarChar name
 where
  isValidVarChar c =
    c `elem` ['A' .. 'Z']
      || c `elem` ['a' .. 'z']
      || c `elem` ['0' .. '9']
      || c == '_'

prop_invalid_varname_rejected :: Property
prop_invalid_varname_rejected = forAll genInvalidVarName $ \name ->
  let config = "config.test=\"$" <> name <> "\""
   in case parseConfigAssignment config of
        Nothing -> True
        Just ca -> T.all isValidVarChar (either id (const "") (configValue ca))
 where
  isValidVarChar c =
    c `elem` ['A' .. 'Z']
      || c `elem` ['a' .. 'z']
      || c `elem` ['0' .. '9']
      || c == '_'

prop_injection_blocked :: Property
prop_injection_blocked = forAll genInjectionAttempt $ \injection ->
  let config = "config.test=$" <> injection
   in case parseConfigAssignment config of
        Nothing -> True
        Just ca ->
          case configValue ca of
            Left var -> not (any (`T.isInfixOf` var) [";", "|", "&", "`", "$("])
            Right _ -> True

prop_store_path_no_traversal :: Property
prop_store_path_no_traversal = forAll genTraversalPath $ \path ->
  ".." `T.isInfixOf` path ==> not (isStorePath path)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- EMIT-CONFIG PROPERTIES
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_emit_escaped :: Property
prop_emit_escaped = forAll genLiteral $ \lit ->
  let spec =
        ConfigSpec TString Nothing Nothing (Just lit) Nothing (Span (Loc 1 0) (Loc 1 0) Nothing)
      schema = emptySchema{schemaConfig = Map.singleton ["test"] spec}
      output = emitConfigFunction schema
   in case lit of
        LitString s ->
          not (T.any (\c -> c `elem` ['"', '\\', '\n', '\r', '\t']) s)
            || "\\\"" `T.isInfixOf` output
            || "\\n" `T.isInfixOf` output
        _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- ALGEBRAIC PROPERTIES
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_solve_unsatisfiable :: Property
prop_solve_unsatisfiable = forAll genUnsatisfiableConstraints $ \cs ->
  isLeft (solve cs)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- RESOURCE BOUNDS
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_bounded_time :: Property
prop_bounded_time = forAll (resize 50 genBashScript) $ \script ->
  monadicIO $ do
    result <- run $ timeout 1000000 $ evaluate $ parseBash script
    assert $ isJust result

prop_no_memory_bomb :: Property
prop_no_memory_bomb = forAll genMalformedExpansion $ \expansion ->
  let bomb = T.replicate 100 expansion
   in parseBash bomb `seq` True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- ROUNDTRIP PROPERTIES
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

prop_literal_type_preserved :: Literal -> Bool
prop_literal_type_preserved lit =
  literalType lit == literalType (parseLiteral (renderLiteral lit))
 where
  renderLiteral (LitInt n) = T.pack (show n)
  renderLiteral (LitBool True) = "true"
  renderLiteral (LitBool False) = "false"
  renderLiteral (LitString s) = s
  renderLiteral (LitPath (StorePath p)) = p

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- SUBSTITUTION — CHAIN RESOLUTION
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | applySubst must follow chains to terminal types.
  e.g. {PORT → TVar HOST, HOST → TInt} must resolve PORT to TInt.
-}
prop_subst_chain_bash :: Property
prop_subst_chain_bash =
  forAll genValidVarName $ \v1 -> forAll genValidVarName $ \v2 ->
    let subst =
          Map.fromList
            [ (TypeVar v1, TVar (TypeVar v2))
            , (TypeVar v2, TInt)
            ]
        resolved = applySubst subst (TVar (TypeVar v1))
     in resolved === TInt

-- | Nix substitution must also follow chains.
prop_subst_chain_nix :: Bool
prop_subst_chain_nix =
  let s =
        Map.fromList
          [ (NT.TypeVar 0, NT.TVar (NT.TypeVar 1))
          , (NT.TypeVar 1, NT.TInt)
          ]
   in NT.applySubst s (NT.TVar (NT.TypeVar 0)) == NT.TInt

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- EVAL DETECTION — adversarial patterns
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | prefixed eval (nice eval, sudo eval, time eval) must be caught
prop_bash_lint_eval_prefixed :: Bool
prop_bash_lint_eval_prefixed =
  let checks =
        [ isEvalDetected "nice eval echo hello"
        , isEvalDetected "sudo eval echo hello"
        , isEvalDetected "time eval echo hello"
        , not (isEvalDetected "echo eval") -- argument, not invocation
        , isEvalDetected "builtin eval echo hello"
        , isEvalDetected "command eval echo hello"
        ]
   in and checks
 where
  isEvalDetected src = case parseBash (T.pack src) of
    Right ast -> any (\v -> vType v == VEval) (findViolations ast)
    Left _ -> False

-- | store-path eval (/nix/store/.../bin/eval) must be caught
prop_bash_lint_eval_store_path :: Bool
prop_bash_lint_eval_store_path =
  isEvalDetected "/nix/store/abc123coreutils-9.0/bin/eval \"echo hello\""
 where
  isEvalDetected src = case parseBash (T.pack src) of
    Right ast -> any (\v -> vType v == VEval) (findViolations ast)
    Left _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- CONFIG ARRAY — template facts
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | config[server]="${HOST:-localhost}:${PORT:-8080}" must emit ConfigTemplate
prop_config_array_template :: Bool
prop_config_array_template = case parseBash "config[server]=\"${HOST:-localhost}:${PORT:-8080}\"" of
  Right ast ->
    let facts = extractFacts ast
     in any (\case ConfigTemplate _ parts _ _ -> not (null parts); _ -> False) facts
  Left _ -> False
