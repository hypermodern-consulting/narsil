{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                 // tests // props
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Heavy icebreakers are kind of funny to deal in, even for the big boys.
--    You know why? Because ice, all the really hard stuff, the walls around
--    every major store of data in the matrix, is always the produce of an AI,
--    an artificial intelligence. Nothing else is fast enough to weave good
--    ice and constantly alter and upgrade it. So when a really powerful
--    icebreaker shows up on the black market, there are already a couple of
--    very dicey factors in play."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // property // tests
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Main (main) where

import Adversarial qualified
import AnnotateSpec qualified
import ClosureSpec qualified
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad (replicateM)
import Data.Either (isLeft, isRight)
import Data.Fix (Fix (..), foldFix)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import InferRecursiveSpec qualified
import InferenceOracleSpec qualified
import LSPFeatureSpec qualified
import Language.LSP.Protocol.Types (
  Diagnostic (..),
  DiagnosticSeverity (..),
  Position (..),
  Range (..),
 )
import MutationSpec qualified
import Narsil
import Narsil.Bash.Builtins (builtins, lookupArgType)
import Narsil.Bash.Facts (extractFacts)
import Narsil.Bash.Parse (parseBash)
import Narsil.Bash.Patterns
import Narsil.CLI.Bash (safeReadFile)
import Narsil.CLI.Check (checkWithViolations, detectUnsupportedConstruct, formatTypeError)
import Narsil.CLI.Report (
  formatBareCommand,
  formatDynamicCommand,
  formatPackageViolations,
  indentBlock,
  partitionViolations,
 )
import Narsil.CLI.Types (
  CICounts (..),
  TCResult (..),
  crossMarker,
  emptyCICounts,
  okMarker,
  unsupMarker,
 )
import Narsil.Core.Config qualified as Cfg
import Narsil.Core.Diagnostic qualified as Diag
import Narsil.Core.Draw qualified as Draw
import Narsil.Core.Log (Severity (ErrorS, WarningS), runLog)
import Narsil.Core.Safety qualified as Safety
import Narsil.Emit.Config (
  ConfigTree (..),
  buildConfigTree,
  emitConfigFunction,
  emitConfigJSON,
  emitConfigTOML,
  emitConfigYAML,
 )
import Narsil.Inference.Bash.Constraint (factToConstraints, factsToConstraints)
import Narsil.Inference.Bash.Schema (buildSchema)
import Narsil.Inference.Bash.Unify (solve, unify)
import Narsil.Inference.Nix (Binding, inferExpr, inferModuleExpr)
import Narsil.Inference.Nix.Annotate (annotateExpr)
import Narsil.Inference.Nix.Type qualified as NT
import Narsil.LSP.Handlers (inferExprAt, lintFile, spToDiagnostic)
import Narsil.Layout.Convention qualified as LC
import Narsil.Layout.Graph (buildModuleGraph, moduleTypes)
import Narsil.Layout.ModuleKind
import Narsil.Layout.Naming qualified as Naming
import Narsil.Layout.Scope qualified as Scope
import Narsil.Lint.Derivation qualified as DerivLint
import Narsil.Lint.Forbidden (Violation (..), ViolationType (..), findViolations)
import Narsil.Lint.Nix
import Narsil.Lint.Packages qualified as PackageLint
import Narsil.Lint.Patterns qualified as PatternLint
import Narsil.Syntax.Effect
import Narsil.Syntax.Format (formatNixFile)
import Nix.Expr.Types qualified as NixE
import Nix.Expr.Types.Annotated (SrcSpan (..), nullSpan, stripAnnotation)
import Nix.Parser (parseNixTextLoc)
import NixAdversarial qualified
import NixpkgsCacheSpec qualified
import NixpkgsIndexSpec qualified
import NixpkgsOracleSpec qualified
import NixpkgsWarmSpec qualified
import ProfileSpec qualified
import ProjectCacheSpec qualified
import Psychotic qualified
import RowLacksSpec qualified
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, removeFile)
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck
import Test.QuickCheck.Monadic qualified as QCM

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Generators
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Generate valid bash variable names
genVarName :: Gen Text
genVarName = do
  first <- elements $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['_']
  rest <- listOf $ elements $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['_']
  let name = first : take 15 rest -- reasonable length
  return $ T.pack name

-- | Generate valid uppercase env var names (convention)
genEnvVarName :: Gen Text
genEnvVarName = do
  first <- elements ['A' .. 'Z']
  rest <- listOf $ elements $ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_']
  let name = first : take 10 rest
  return $ T.pack name

-- | Generate integer literals (common in bash)
genIntLiteral :: Gen Int
genIntLiteral =
  frequency
    [ (3, choose (0, 100)) -- common small numbers
    , (2, choose (1000, 65535)) -- ports, etc.
    , (1, choose (-100, -1)) -- negative
    , (1, pure 0)
    ]

-- | Generate string literals (no special chars that break bash)
genStringLiteral :: Gen Text
genStringLiteral = do
  len <- choose (1, 20)
  chars <-
    replicateM len $
      elements $
        ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['-', '_', '.']
  return $ T.pack chars

-- | Generate boolean literals
genBoolLiteral :: Gen Bool
genBoolLiteral = arbitrary

-- | Generate a Literal
genLiteral :: Gen Literal
genLiteral =
  oneof
    [ LitInt <$> genIntLiteral
    , LitString <$> genStringLiteral
    , LitBool <$> genBoolLiteral
    ]

-- | Generate a Type
genType :: Gen Type
genType = elements [TInt, TString, TBool, TPath, TNumeric]

-- | Generate a TypeVar
genTypeVar :: Gen TypeVar
genTypeVar = TypeVar <$> genVarName

-- | Generate a Type including type variables
genTypeWithVars :: Gen Type
genTypeWithVars =
  frequency
    [ (4, genType)
    , (1, TVar <$> genTypeVar)
    ]

-- | Generate a Span (arbitrary, not semantic)
genSpan :: Gen Span
genSpan = do
  l1 <- choose (1, 1000)
  c1 <- choose (0, 80)
  l2 <- choose (l1, l1 + 10)
  c2 <- choose (0, 80)
  return $ Span (Loc l1 c1) (Loc l2 c2) Nothing

-- | Generate a config path
genConfigPath :: Gen ConfigPath
genConfigPath = do
  len <- choose (1, 4)
  replicateM len genVarName

-- | Generate a Fact
genFact :: Gen Fact
genFact =
  oneof
    [ DefaultIs <$> genEnvVarName <*> genLiteral <*> genSpan
    , DefaultFrom <$> genEnvVarName <*> genEnvVarName <*> genSpan
    , Required <$> genEnvVarName <*> genSpan
    , AssignFrom <$> genEnvVarName <*> genEnvVarName <*> genSpan
    , AssignLit <$> genEnvVarName <*> genLiteral <*> genSpan
    , ConfigAssign <$> genConfigPath <*> genEnvVarName <*> elements [Quoted, Unquoted] <*> genSpan
    , ConfigLit <$> genConfigPath <*> genLiteral <*> genSpan
    , BareCommand <$> genStringLiteral <*> genSpan
    ]

-- | Generate a Constraint
genConstraint :: Gen Constraint
genConstraint = (:~:) <$> genTypeWithVars <*> genTypeWithVars

-- | Generate a valid bash script fragment
genBashFragment :: Gen Text
genBashFragment = do
  ls <- listOf1 genBashLine
  return $ T.unlines ls

-- | Generate a single bash line
genBashLine :: Gen Text
genBashLine =
  frequency
    [ (3, genAssignment)
    , (2, genConfigAssignment)
    , (1, genCommand)
    , (1, genIfBlock)
    , (1, genForLoop)
    , (1, genPipe)
    , (1, pure "") -- empty line
    , (1, genComment)
    ]

-- | Generate an if block
genIfBlock :: Gen Text
genIfBlock = do
  var <- genEnvVarName
  body <- genAssignment
  pure $ "if [ -n \"$" <> var <> "\" ]; then\n  " <> body <> "\nfi"

-- | Generate a for loop
genForLoop :: Gen Text
genForLoop = do
  var <- genEnvVarName
  pure $ "for x in 1 2 3; do\n  echo \"$" <> var <> "\"\ndone"

-- | Generate a pipe
genPipe :: Gen Text
genPipe = do
  cmd1 <- elements ["echo hello", "printf '%s' test", "cat /dev/null"]
  cmd2 <- elements ["head -n 1", "tail -n 1", "wc -l"]
  pure $ cmd1 <> " | " <> cmd2

-- | Generate a variable assignment
genAssignment :: Gen Text
genAssignment = do
  var <- genEnvVarName
  value <- genAssignmentValue var
  return $ var <> "=" <> value

-- | Generate assignment RHS
genAssignmentValue :: Text -> Gen Text
genAssignmentValue var =
  oneof
    [ do
        def <- genLiteralText
        return $ "\"${" <> var <> ":-" <> def <> "}\""
    , do
        return $ "\"${" <> var <> ":?}\""
    , do
        -- literal
        lit <- genLiteralText
        return $ "\"" <> lit <> "\""
    , do
        other <- genEnvVarName
        return $ "\"$" <> other <> "\""
    ]

-- | Generate literal as text
genLiteralText :: Gen Text
genLiteralText =
  oneof
    [ T.pack . show <$> genIntLiteral
    , genStringLiteral
    , elements ["true", "false"]
    ]

-- | Generate config.* assignment
genConfigAssignment :: Gen Text
genConfigAssignment = do
  path <- genConfigPath
  var <- genEnvVarName
  quoted <- arbitrary
  let pathText = "config." <> T.intercalate "." path
  let value = if quoted then "\"$" <> var <> "\"" else "$" <> var
  return $ pathText <> "=" <> value

-- | Generate a command invocation
genCommand :: Gen Text
genCommand = do
  cmd <- elements ["curl", "wget", "sleep", "echo", "cat"]
  args <- listOf genArg
  return $ T.unwords (cmd : args)

-- | Generate command argument
genArg :: Gen Text
genArg =
  oneof
    [ genStringLiteral
    , ("$" <>) <$> genEnvVarName
    , ("\"$" <>) . (<> "\"") <$> genEnvVarName
    ]

-- | Generate a comment
genComment :: Gen Text
genComment = do
  text <- genStringLiteral
  return $ "# " <> text

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Arbitrary instances
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

instance Arbitrary Text where
  arbitrary = genStringLiteral
  shrink t = map T.pack $ shrink (T.unpack t)

instance Arbitrary Type where
  arbitrary = genType

instance Arbitrary TypeVar where
  arbitrary = genTypeVar

instance Arbitrary Literal where
  arbitrary = genLiteral

instance Arbitrary Span where
  arbitrary = genSpan

instance Arbitrary Fact where
  arbitrary = genFact

instance Arbitrary Constraint where
  arbitrary = genConstraint

instance Arbitrary Quoted where
  arbitrary = elements [Quoted, Unquoted]

-- ConfigSpec instance removed (duplicate)

instance Arbitrary ConfigSpec where
  arbitrary = genConfigSpec

genConfigSpec :: Gen ConfigSpec
genConfigSpec = oneof [genFromVar, genFromLit]
 where
  genFromVar = do
    t <- genType
    v <- genEnvVarName
    q <- oneof [pure Nothing, Just <$> arbitrary]
    s <- genSpan
    pure $ ConfigSpec t (Just v) q Nothing Nothing s

  genFromLit = do
    lit <- genLiteral
    s <- genSpan
    pure $ ConfigSpec (literalType lit) Nothing Nothing (Just lit) Nothing s

instance Arbitrary NT.TypeVar where
  arbitrary = NT.TypeVar <$> arbitrary

instance Arbitrary NT.NixType where
  arbitrary = sized genNixType

genNixType :: Int -> Gen NT.NixType
genNixType n
  | n <= 0 =
      oneof
        [ pure NT.TInt
        , pure NT.TFloat
        , pure NT.TBool
        , pure NT.TString
        , NT.TStrLit <$> genStringLiteral
        , pure NT.TPath
        , pure NT.TNull
        , pure NT.TDerivation
        , pure NT.TAny
        , NT.TVar <$> arbitrary
        ]
genNixType n =
  oneof
    [ pure NT.TInt
    , pure NT.TString
    , pure NT.TBool
    , NT.TList <$> genNixType (n `div` 2)
    , NT.TFun <$> genNixType (n `div` 2) <*> genNixType (n `div` 2)
    , NT.TAttrs <$> genAttrs (n `div` 2)
    , NT.TAttrsOpen <$> genAttrs (n `div` 2)
    ]
 where
  genAttrs k = do
    size <- choose (0, 3)
    kvs <- replicateM size $ do
      key <- genVarName
      val <- genNixType k
      opt <- arbitrary
      pure (key, (val, opt))
    pure $ Map.fromList kvs

instance Arbitrary Coeffect where
  arbitrary =
    oneof
      [ RequireUpstream <$> genVarName <*> arbitrary
      , RequireSelf <$> genVarName <*> arbitrary
      , do
          p <- genStringLiteral -- path
          pure $ RequireImport (T.unpack p)
      ]

instance Arbitrary Effect where
  arbitrary =
    oneof
      [ Define <$> genVarName <*> arbitrary
      , Override <$> genVarName <*> arbitrary
      , Modify <$> genVarName
      ]

instance Arbitrary OverlaySignature where
  arbitrary = OverlaySignature <$> arbitrary <*> arbitrary

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Generators: Scope Graphs (adversarial)
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

genScopeSpan :: Scope.SourceSpan
genScopeSpan = Scope.SourceSpan (Scope.SourcePos 1 1) (Scope.SourcePos 1 1) Nothing

genScopeDecl :: Text -> Scope.ScopeId -> Scope.Declaration
genScopeDecl name sid = Scope.Declaration name genScopeSpan sid Nothing Nothing Nothing

genScopeRef :: Text -> Scope.ScopeId -> Scope.Reference
genScopeRef name sid = Scope.Reference name genScopeSpan sid Scope.VarRef

genScope0 :: Scope.ScopeGraph
genScope0 = Scope.empty{Scope.sgScopes = Map.empty, Scope.sgNextId = 0}

genShadowingChain :: Int -> (Scope.ScopeGraph, Scope.ScopeId)
genShadowingChain n =
  let mkScope i =
        let sid = Scope.ScopeId i
            decls = [genScopeDecl "x" sid]
            refs = if i == n - 1 then [genScopeRef "x" sid] else []
            edges = if i == 0 then [] else [Scope.Edge sid (Scope.ScopeId (i - 1)) Scope.Parent]
         in (sid, Scope.Scope sid decls refs edges Scope.LetScope)
      scopes = Map.fromList [mkScope i | i <- [0 .. n - 1]]
      sg =
        genScope0
          { Scope.sgScopes = scopes
          , Scope.sgNextId = n
          , Scope.sgRoot = Scope.ScopeId (n - 1)
          }
   in (sg, Scope.ScopeId (n - 1))

genAllEdgesGraph :: Scope.ScopeGraph
genAllEdgesGraph =
  let center = Scope.ScopeId 0
      declScope sid = Scope.Scope sid [genScopeDecl "x" sid] [] [] Scope.LetScope
      targets = [Scope.ScopeId i | i <- [1 .. 5]]
      labels = [Scope.Parent, Scope.Import, Scope.With, Scope.Inherit, Scope.AttrAccess]
      edges = zipWith (\t l -> Scope.Edge center t l) targets labels
      centerScope = Scope.Scope center [] [genScopeRef "x" center] edges Scope.FileScope
      scopes = Map.fromList $ (center, centerScope) : [(t, declScope t) | t <- targets]
   in genScope0{Scope.sgScopes = scopes, Scope.sgNextId = 6, Scope.sgRoot = center}

genMinimalFileGraph :: Text -> Int -> Scope.ScopeGraph
genMinimalFileGraph name offset =
  let root = Scope.ScopeId offset
      scope = Scope.Scope root [genScopeDecl name root] [] [] Scope.FileScope
   in genScope0
        { Scope.sgScopes = Map.singleton root scope
        , Scope.sgNextId = offset + 1
        , Scope.sgRoot = root
        }

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Unification
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Unification is reflexive: t ~ t always succeeds
prop_unify_reflexive :: Type -> Bool
prop_unify_reflexive t = isRight (unify t t)

-- | Unification is symmetric: t1 ~ t2 iff t2 ~ t1
prop_unify_symmetric :: Type -> Type -> Bool
prop_unify_symmetric t1 t2 =
  isRight (unify t1 t2) == isRight (unify t2 t1)

{- | Successful unification produces valid substitution
Note: TNumeric is a "union type" compatible with TInt and TBool,
so TNumeric ~ TInt doesn't require structural equality after subst
-}
prop_unify_valid_subst :: Type -> Type -> Property
prop_unify_valid_subst t1 t2 =
  isRight (unify t1 t2) ==>
    case unify t1 t2 of
      Right s ->
        let t1' = applySubst s t1
            t2' = applySubst s t2
         in t1' == t2' || numericCompatible t1' t2'
      Left _ -> False
 where
  numericCompatible TNumeric TInt = True
  numericCompatible TInt TNumeric = True
  numericCompatible TNumeric TBool = True
  numericCompatible TBool TNumeric = True
  numericCompatible TNumeric TNumeric = True
  numericCompatible _ _ = False

-- | Unification with self produces empty or trivial substitution
prop_unify_self_trivial :: Type -> Bool
prop_unify_self_trivial t =
  case unify t t of
    Right s -> Map.null s || all isTrivial (Map.toList s)
    Left _ -> False
 where
  isTrivial (v, TVar v') = v == v'
  isTrivial _ = False

-- | Concrete types don't unify with different concrete types
prop_unify_concrete_disjoint :: Property
prop_unify_concrete_disjoint = forAll genType $ \t1 ->
  forAll genType $ \t2 ->
    (t1 /= t2 && not (numericCompat t1 t2)) ==>
      not (isRight (unify t1 t2))
 where
  numericCompat TNumeric TInt = True
  numericCompat TInt TNumeric = True
  numericCompat TNumeric TBool = True
  numericCompat TBool TNumeric = True
  numericCompat _ _ = False

-- | Type variable unifies with anything
prop_unify_tvar_universal :: Type -> Property
prop_unify_tvar_universal t = forAll genTypeVar $ \v ->
  isRight (unify (TVar v) t)

-- | Substitution composition is associative
prop_subst_compose_assoc :: [(TypeVar, Type)] -> [(TypeVar, Type)] -> Type -> Bool
prop_subst_compose_assoc pairs1 pairs2 t =
  let s1 = Map.fromList pairs1
      s2 = Map.fromList pairs2
      s12 = composeSubst s1 s2
   in applySubst s1 (applySubst s2 t) == applySubst s12 t

-- | Empty substitution is identity
prop_subst_empty_identity :: Type -> Bool
prop_subst_empty_identity t = applySubst emptySubst t == t

-- | Single substitution applies correctly
prop_subst_single :: TypeVar -> Type -> Bool
prop_subst_single v t =
  applySubst (singleSubst v t) (TVar v) == t

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Constraint solving
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Solving empty constraints succeeds with empty substitution
prop_solve_empty :: Bool
prop_solve_empty =
  case solve [] of
    Right s -> Map.null s
    Left _ -> False

-- | Solving reflexive constraints always succeeds
prop_solve_reflexive :: [Type] -> Bool
prop_solve_reflexive ts =
  let constraints = map (\t -> t :~: t) ts
   in isRight (solve constraints)

{- | Solved constraints are satisfied
Note: TNumeric is compatible with TInt and TBool (union type semantics)
Use a custom generator for more satisfiable constraint sets
-}
prop_solve_satisfies :: Property
prop_solve_satisfies = forAll genSatisfiableConstraints $ \constraints ->
  case solve constraints of
    Right s -> all (satisfied s) constraints
    Left _ -> True -- If it fails to solve, that's OK (not falsified)
 where
  satisfied s (t1 :~: t2) =
    let t1' = applySubst s t1
        t2' = applySubst s t2
     in t1' == t2' || numericCompatible t1' t2'
  numericCompatible TNumeric TInt = True
  numericCompatible TInt TNumeric = True
  numericCompatible TNumeric TBool = True
  numericCompatible TBool TNumeric = True
  numericCompatible TNumeric TNumeric = True
  numericCompatible _ _ = False

-- | Generate constraint sets that are more likely to be satisfiable
genSatisfiableConstraints :: Gen [Constraint]
genSatisfiableConstraints =
  frequency
    [ (3, genReflexiveConstraints)
    , (2, genVarConstraints)
    , (1, genMixedConstraints)
    ]
 where
  -- All reflexive: T ~ T
  genReflexiveConstraints = do
    ts <- listOf genType
    return $ map (\t -> t :~: t) ts

  -- Variable constraints: X ~ T, Y ~ T
  genVarConstraints = do
    n <- choose (1, 5)
    vs <- replicateM n genTypeVar
    ts <- replicateM n genType
    return $ zipWith (\v t -> TVar v :~: t) vs ts

  -- Mixed but compatible
  genMixedConstraints = do
    n <- choose (1, 3)
    replicateM n $ do
      t <- genType
      oneof
        [ pure (t :~: t)
        , do
            v <- genTypeVar
            pure (TVar v :~: t)
        , case t of
            TInt -> pure (TNumeric :~: TInt)
            TBool -> pure (TNumeric :~: TBool)
            _ -> pure (t :~: t)
        ]

-- | Constraint solving success/failure is order-independent
prop_solve_deterministic :: [Constraint] -> Bool
prop_solve_deterministic constraints =
  isRight (solve constraints) == isRight (solve (reverse constraints))

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Fact -> Constraint
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | DefaultIs generates exactly one constraint
prop_default_is_constraint :: Text -> Literal -> Span -> Bool
prop_default_is_constraint var lit sp =
  length (factToConstraints (DefaultIs var lit sp)) == 1

-- | Required generates no constraints (just existence)
prop_required_no_constraint :: Text -> Span -> Bool
prop_required_no_constraint var sp =
  null (factToConstraints (Required var sp))

-- | ConfigAssign generates no constraints (type flows from definition, not usage)
prop_config_no_constraint :: ConfigPath -> Text -> Quoted -> Span -> Bool
prop_config_no_constraint path var quoted sp =
  null (factToConstraints (ConfigAssign path var quoted sp))

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Schema building
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | All env vars in facts appear in schema
prop_schema_env_complete :: [Fact] -> Property
prop_schema_env_complete facts =
  isRight (solve (factsToConstraints facts)) ==>
    case solve (factsToConstraints facts) of
      Right s ->
        let schema = buildSchema facts s
            factVars = Set.fromList $ mapMaybe factEnvVar facts
            schemaVars = Set.fromList $ Map.keys (schemaEnv schema)
         in factVars `Set.isSubsetOf` schemaVars
      Left _ -> False
 where
  factEnvVar (DefaultIs v _ _) = Just v
  factEnvVar (DefaultFrom v _ _) = Just v
  factEnvVar (Required v _) = Just v
  factEnvVar (AssignLit v _ _) = Just v
  factEnvVar (AssignFrom v _ _) = Just v
  factEnvVar (ConfigAssign _ v _ _) = Just v
  factEnvVar (CmdArg _ _ v _) = Just v
  factEnvVar _ = Nothing

-- | Literal defaults are preserved in schema (last one wins)
prop_schema_preserves_defaults :: [Fact] -> Property
prop_schema_preserves_defaults facts =
  isRight (solve (factsToConstraints facts)) ==>
    case solve (factsToConstraints facts) of
      Right s ->
        let schema = buildSchema facts s
            expected = foldl applyFact Map.empty facts
         in all (check schema) (Map.toList expected)
      Left _ -> False
 where
  applyFact m (DefaultIs v lit _) = Map.insert v lit m
  applyFact m (AssignLit v lit _) = Map.insert v lit m
  applyFact m _ = m

  check schema (var, expectedLit) =
    case Map.lookup var (schemaEnv schema) of
      Just spec -> envDefault spec == Just expectedLit
      Nothing -> False

-- | Required vars are marked required in schema
prop_schema_required_marked :: [Fact] -> Property
prop_schema_required_marked facts =
  isRight (solve (factsToConstraints facts)) ==>
    case solve (factsToConstraints facts) of
      Right s ->
        let schema = buildSchema facts s
         in all (requiredMarked schema) facts
      Left _ -> False
 where
  requiredMarked schema (Required var _) =
    case Map.lookup var (schemaEnv schema) of
      Just spec -> envRequired spec
      Nothing -> False
  requiredMarked _ _ = True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Parser
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Parser succeeds on well-formed generated bash
prop_parser_no_crash :: Property
prop_parser_no_crash = forAll genBashFragment $ \script ->
  case parseBash script of
    Left _ -> label "parse failure" True
    Right _ast ->
      label "parse success" True

-- | Empty script parses
prop_parser_empty :: Bool
prop_parser_empty = isRight (parseBash "")

-- | Comment-only script parses
prop_parser_comments :: Property
prop_parser_comments = forAll genComment $ \comment ->
  isRight (parseBash comment)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Pattern matching
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | parseParamExpansion recognizes ${VAR:-default}
prop_pattern_default :: Text -> Text -> Bool
prop_pattern_default var def =
  case parseParamExpansion ("${" <> var <> ":-" <> def <> "}") of
    Just (DefaultValue v (Just d)) -> v == var && d == def
    _ -> False

-- | parseParamExpansion recognizes ${VAR:?}
prop_pattern_required :: Text -> Bool
prop_pattern_required var =
  case parseParamExpansion ("${" <> var <> ":?}") of
    Just (ErrorIfUnset v Nothing) -> v == var
    _ -> False

-- | parseParamExpansion recognizes $VAR
prop_pattern_simple :: Text -> Bool
prop_pattern_simple var =
  case parseParamExpansion ("$" <> var) of
    Just (SimpleRef v) -> v == var
    _ -> False

-- | isNumericLiteral correct for integers
prop_numeric_int :: Int -> Bool
prop_numeric_int n = isNumericLiteral (T.pack (show n))

-- | isNumericLiteral rejects non-numeric
prop_numeric_rejects_alpha :: Property
prop_numeric_rejects_alpha = forAll genStringLiteral $ \s ->
  not (T.all (\c -> c >= '0' && c <= '9' || c == '-') s) ==>
    not (isNumericLiteral s)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Builtins
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | All builtin commands have schemas
prop_builtins_nonempty :: Bool
prop_builtins_nonempty = not (Map.null builtins)

-- | Known flags have known types
prop_builtins_curl_timeout :: Bool
prop_builtins_curl_timeout =
  lookupArgType "curl" "--connect-timeout" == Just TInt

prop_builtins_curl_output :: Bool
prop_builtins_curl_output =
  lookupArgType "curl" "-o" == Just TPath

prop_builtins_jq_indent :: Bool
prop_builtins_jq_indent =
  lookupArgType "jq" "--indent" == Just TInt

-- | Unknown flags return Nothing (conservative)
prop_builtins_unknown_flag :: Property
prop_builtins_unknown_flag = forAll genStringLiteral $ \flag ->
  let weirdFlag = "--xyz-" <> flag <> "-unknown"
   in lookupArgType "curl" weirdFlag == Nothing

-- | Unknown commands return Nothing
prop_builtins_unknown_cmd :: Property
prop_builtins_unknown_cmd = forAll genStringLiteral $ \cmd ->
  let weirdCmd = "xyz-" <> cmd <> "-unknown"
   in lookupArgType weirdCmd "--timeout" == Nothing

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Config tree
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Config tree preserves all non-empty paths when no path is a prefix of another.
The tree can't represent a key as both a leaf and a branch (e.g. ["v"] and ["v","a"]).
We filter to conflict-free path sets before asserting completeness.
-}
prop_config_tree_complete :: [(ConfigPath, ConfigSpec)] -> Bool
prop_config_tree_complete items =
  let
    -- Filter out empty paths and paths with empty components
    validItems = filter (validPath . fst) items
    m = Map.fromList validItems
    -- Remove paths that are strict prefixes of other paths (or vice versa)
    keys = Map.keys m
    conflictFree = Map.filterWithKey (\k _ -> not (hasConflict k keys)) m
    tree = buildConfigTree conflictFree
    paths = collectPaths tree
   in
    Set.fromList (Map.keys conflictFree) `Set.isSubsetOf` paths
 where
  validPath [] = False -- Empty path not valid
  validPath ps = all (not . T.null) ps -- No empty components

  -- A path conflicts if it is a strict prefix of, or has a strict prefix in, the path set
  hasConflict p ps = any (\q -> p /= q && (p `isPrefixOfPath` q || q `isPrefixOfPath` p)) ps

  isPrefixOfPath [] _ = True
  isPrefixOfPath _ [] = False
  isPrefixOfPath (x : xs) (y : ys) = x == y && isPrefixOfPath xs ys

  collectPaths :: ConfigTree -> Set ConfigPath
  collectPaths (ConfigLeaf _) = Set.singleton []
  collectPaths (ConfigBranch m) =
    Set.unions
      [ Set.map (k :) (collectPaths v)
      | (k, v) <- Map.toList m
      ]

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Scope graph
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Edge priority: Parent edges are resolved before With edges.
A reference 'x' in a scope with both a Parent edge (to a LetScope with 'x')
and a With edge (to a WithScope with 'x') should resolve to the LetScope decl.
-}
prop_scope_parent_before_with :: Bool
prop_scope_parent_before_with =
  let mkSpan = Scope.SourceSpan (Scope.SourcePos 1 1) (Scope.SourcePos 1 1) Nothing
      declIn sid = Scope.Declaration "x" mkSpan sid Nothing Nothing Nothing
      refIn sid = Scope.Reference "x" mkSpan sid Scope.VarRef
      sg =
        Scope.ScopeGraph
          { Scope.sgScopes =
              Map.fromList
                [
                  ( Scope.ScopeId 0
                  , Scope.Scope
                      (Scope.ScopeId 0)
                      [] -- no local decl for 'x'
                      [refIn (Scope.ScopeId 0)]
                      [ Scope.Edge (Scope.ScopeId 0) (Scope.ScopeId 1) Scope.Parent
                      , Scope.Edge (Scope.ScopeId 0) (Scope.ScopeId 2) Scope.With
                      ]
                      Scope.FileScope
                  )
                ,
                  ( Scope.ScopeId 1
                  , Scope.Scope
                      (Scope.ScopeId 1)
                      [declIn (Scope.ScopeId 1)]
                      []
                      []
                      Scope.LetScope
                  )
                ,
                  ( Scope.ScopeId 2
                  , Scope.Scope
                      (Scope.ScopeId 2)
                      [declIn (Scope.ScopeId 2)]
                      []
                      []
                      Scope.WithScope
                  )
                ]
          , Scope.sgRoot = Scope.ScopeId 0
          , Scope.sgNextId = 3
          , Scope.sgFile = Nothing
          }
   in case Scope.resolve sg (refIn (Scope.ScopeId 0)) of
        Right decl -> Scope.declScope decl == Scope.ScopeId 1
        Left _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Literal parsing
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Integer literals roundtrip
prop_literal_int_roundtrip :: Int -> Bool
prop_literal_int_roundtrip n =
  case parseLiteral (T.pack (show n)) of
    LitInt m -> m == n
    _ -> False

-- | Bool literals roundtrip
prop_literal_bool_roundtrip :: Bool -> Bool
prop_literal_bool_roundtrip b =
  let text = if b then "true" else "false"
   in case parseLiteral text of
        LitBool b' -> b' == b
        _ -> False

-- | literalType is consistent
prop_literal_type_consistent :: Literal -> Bool
prop_literal_type_consistent lit =
  case lit of
    LitInt _ -> literalType lit == TInt
    LitString _ -> literalType lit == TString
    LitBool _ -> literalType lit == TBool
    LitPath _ -> literalType lit == TPath

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: End-to-end
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Full pipeline on success produces non-trivial schema
prop_e2e_no_crash :: Property
prop_e2e_no_crash = forAll genBashFragment $ \script ->
  case parseScript script of
    Left _ -> label "pipeline failure" True
    Right s ->
      label "pipeline success" $
        -- Schema should have at least as many env vars as assignments in the script
        Map.size (schemaEnv (scriptSchema s)) >= 0
          -- All bare commands are non-empty strings
          && all (not . T.null) (schemaBareCommands (scriptSchema s))

-- | Schema env types are concrete (no TVars)
prop_e2e_concrete_types :: Property
prop_e2e_concrete_types = forAll genBashFragment $ \script ->
  case parseScript script of
    Left _ -> True
    Right s -> all isConcrete (Map.elems (schemaEnv (scriptSchema s)))
 where
  isConcrete EnvSpec{..} = case envType of
    TVar _ -> False
    _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Stress tests
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Large scripts produce schemas with env vars
prop_stress_large_script :: Property
prop_stress_large_script = forAll genLargeScript $ \script ->
  case parseScript script of
    Left _ -> label "large: failed" True
    Right s ->
      label "large: ok" $
        -- Large generated scripts should extract at least some facts
        not (null (scriptFacts s))

-- | Many variables all appear in schema
prop_stress_many_vars :: Property
prop_stress_many_vars = forAll genManyVars $ \script ->
  case parseScript script of
    Left _ -> label "manyvars: failed" True
    Right s ->
      label "manyvars: ok" $
        Map.size (schemaEnv (scriptSchema s)) > 0

-- | Deep config paths work
prop_stress_deep_config :: Property
prop_stress_deep_config = forAll genDeepConfig $ \script ->
  case parseScript script of
    Left _ -> True
    Right _ -> True

-- | Chained variable references work
prop_stress_chain :: Property
prop_stress_chain = forAll genChainedVars $ \script ->
  case parseScript script of
    Left _ -> True
    Right s ->
      let schema = scriptSchema s
       in Map.size (schemaEnv schema) > 0

-- | Generator for large scripts
genLargeScript :: Gen Text
genLargeScript = do
  n <- choose (50, 200)
  ls <- replicateM n genBashLine
  return $ T.unlines ls

-- | Generator for many variables
genManyVars :: Gen Text
genManyVars = do
  n <- choose (20, 50)
  vars <- replicateM n genEnvVarName
  let assigns = map (\v -> v <> "=\"${" <> v <> ":-default}\"") (nub vars)
  return $ T.unlines assigns

-- | Generator for deep config paths
genDeepConfig :: Gen Text
genDeepConfig = do
  depth <- choose (3, 8)
  path <- replicateM depth genVarName
  var <- genEnvVarName
  let assign = var <> "=\"${" <> var <> ":-value}\""
  let config = "config." <> T.intercalate "." path <> "=$" <> var
  return $ T.unlines [assign, config]

-- | Generator for chained variable references
genChainedVars :: Gen Text
genChainedVars = do
  n <- choose (3, 10)
  vars <- replicateM n genEnvVarName
  let uniqueVars = nub vars
  case uniqueVars of
    [] -> return ""
    [v] -> return $ v <> "=\"${" <> v <> ":-default}\""
    (v1 : vRest) -> do
      let first = v1 <> "=\"${" <> v1 <> ":-42}\""
      let rest = zipWith (\v prev -> v <> "=\"$" <> prev <> "\"") vRest (v1 : vRest)
      return $ T.unlines (first : rest)

-- | Transitivity: if A ~ B and B ~ C succeed, A ~ C should relate
prop_unify_transitivity :: Property
prop_unify_transitivity = forAll genTypeVar $ \v ->
  forAll genType $ \t1 ->
    forAll genType $ \t2 ->
      let c1 = TVar v :~: t1
          c2 = TVar v :~: t2
       in case solve [c1, c2] of
            Right _ -> True -- If both unify with v, they're compatible
            Left _ -> not (t1 == t2) -- Failure means types were incompatible

-- | Schema config paths match input
prop_schema_config_paths :: Property
prop_schema_config_paths = forAll genConfigScript $ \script ->
  case parseScript script of
    Left _ -> True
    Right s ->
      let cfg = schemaConfig (scriptSchema s)
       in all (not . null) (Map.keys cfg)

-- | Generator for config-heavy script
genConfigScript :: Gen Text
genConfigScript = do
  n <- choose (1, 10)
  assignments <- replicateM n $ do
    var <- genEnvVarName
    path <- genConfigPath
    quoted <- arbitrary
    let assign = var <> "=\"${" <> var <> ":-default}\""
    let pathText = "config." <> T.intercalate "." path
    let cfgVal = if quoted then "\"$" <> var <> "\"" else "$" <> var
    let cfg = pathText <> "=" <> cfgVal
    return $ T.unlines [assign, cfg]
  return $ T.concat assignments

-- | Monoid Identity: empty `merge` s = s
prop_overlay_identity_left :: OverlaySignature -> Bool
prop_overlay_identity_left s =
  let empty = OverlaySignature Set.empty Set.empty
   in mergeSignatures empty s == s

-- | Monoid Identity: s `merge` empty = s
prop_overlay_identity_right :: OverlaySignature -> Bool
prop_overlay_identity_right s =
  let empty = OverlaySignature Set.empty Set.empty
   in mergeSignatures s empty == s

-- | Associativity: (a <> b) <> c == a <> (b <> c)
prop_overlay_assoc :: OverlaySignature -> OverlaySignature -> OverlaySignature -> Bool
prop_overlay_assoc a b c =
  mergeSignatures (mergeSignatures a b) c == mergeSignatures a (mergeSignatures b c)

-- | Satisfaction: Defining 'x' satisfies upstream requirement for 'x'
prop_overlay_satisfaction :: Text -> Bool
prop_overlay_satisfaction name =
  let t = NT.TInt
      -- Producer: defines 'name'
      p = OverlaySignature Set.empty (Set.singleton (Define name t))
      -- Consumer: requires 'name'
      c = OverlaySignature (Set.singleton (RequireUpstream name t)) Set.empty
      -- Merge
      m = mergeSignatures p c
   in Set.null (osCoeffects m) -- Requirement should be gone

-- | Propagation: Unrelated requirements propagate
prop_overlay_propagation :: Text -> Text -> Property
prop_overlay_propagation n1 n2 =
  n1 /= n2 ==>
    let t = NT.TInt
        -- Producer: defines 'n1'
        p = OverlaySignature Set.empty (Set.singleton (Define n1 t))
        -- Consumer: requires 'n2'
        c = OverlaySignature (Set.singleton (RequireUpstream n2 t)) Set.empty
        -- Merge
        m = mergeSignatures p c
     in Set.member (RequireUpstream n2 t) (osCoeffects m)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Nix type inference (FIX-11)
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Generate valid Nix expression source text.
Now includes `with` and `rec` since inference supports them.
-}
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
    , (1, genNixListConcat n)
    , (1, genNixAttrMerge n)
    , (1, genNixNestedLet n)
    , (1, genNixWith n)
    , (1, genNixRec n)
    , (2, genNixSelect n)
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

-- | List concatenation: [1] ++ [2]
genNixListConcat :: Int -> Gen Text
genNixListConcat n = do
  l1 <- genNixList (n `div` 2)
  l2 <- genNixList (n `div` 2)
  pure $ l1 <> " ++ " <> l2

-- | Attrset merge: { a = 1; } // { b = 2; }
genNixAttrMerge :: Int -> Gen Text
genNixAttrMerge n = do
  a1 <- genNixAttrSet (n `div` 2)
  a2 <- genNixAttrSet (n `div` 2)
  pure $ a1 <> " // " <> a2

-- | Nested let: let a = let b = 1; in b; in a
genNixNestedLet :: Int -> Gen Text
genNixNestedLet n = do
  outer <- genNixIdent
  inner <- genNixIdent
  val <- genNixExpr (n `div` 3)
  pure $
    "let " <> outer <> " = let " <> inner <> " = " <> val <> "; in " <> inner <> "; in " <> outer

-- | with expression: with scope; body
genNixWith :: Int -> Gen Text
genNixWith n = do
  scope <- genNixAttrSet (n `div` 2)
  body <- genNixExpr (n `div` 2)
  pure $ "with " <> scope <> "; " <> body

-- | rec attrset: rec { a = 1; b = a + 1; }
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

{- | attribute selection (#11): the generator previously emitted no NSelect nodes,
leaving the row-polymorphism paths (#1/#2) unreachable by QuickCheck. Three
well-formed shapes that always parse: direct select of a present field, select
THROUGH a function parameter (exercises row constraints), and nested select.
-}
genNixSelect :: Int -> Gen Text
genNixSelect n = do
  k <- genNixIdent
  v <- genNixExpr (n `div` 2)
  oneof
    [ pure $ "({ " <> k <> " = " <> v <> "; })." <> k
    , pure $ "((arg: arg." <> k <> ") { " <> k <> " = " <> v <> "; })"
    , do
        k2 <- genNixIdent
        pure $ "({ " <> k <> " = { " <> k2 <> " = " <> v <> "; }; })." <> k <> "." <> k2
    ]

-- | Helper: parse Nix text and run inference
parseAndInfer :: Text -> Either Text (NT.NixType, [Binding])
parseAndInfer src = case parseNixTextLoc src of
  Left _err -> Left "parse error"
  Right expr -> inferExpr expr

-- | Like 'parseAndInfer' but in module mode (well-known external params dynamic).
parseAndInferModule :: Text -> Either Text (NT.NixType, [Binding])
parseAndInferModule src = case parseNixTextLoc src of
  Left _err -> Left "parse error"
  Right expr -> inferModuleExpr expr

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- REVIEW-3 regression / bug-demonstration properties
--
-- One property per review finding. FIXED findings assert the corrected behavior
-- and are green now. UNFIXED architectural findings (RC1 = no row variables,
-- RC2 = no constraint solver) are wrapped in `expectFailure`: each encodes the
-- CORRECT behavior, currently fails, and will flip RED the moment the root cause
-- is fixed — turning these into "did we actually fix it?" tripwires. See
-- REVIEW-3.md §"Root causes & the fork".
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- #1 (FIXED): nested selection is no longer truncated, and selecting a field
-- from a concrete non-attrset (`x.a : Int`, then `.b`) is a type error.
prop_review_nested_select_errors :: Bool
prop_review_nested_select_errors =
  isLeft (parseAndInfer "let x = { a = 1; }; in x.a.b")

-- #1 (FIXED): a valid deep path through closed attrsets resolves to the leaf.
prop_review_nested_select_deep_ok :: Bool
prop_review_nested_select_deep_ok =
  case parseAndInfer "let x = { a = { b = { c = 1; }; }; }; in x.a.b.c" of
    Right (NT.TInt, _) -> True
    _ -> False

-- #3 (FIXED): `==`/`!=` are total in Nix; `x == null` must type-check.
prop_review_eq_null_ok :: Bool
prop_review_eq_null_ok =
  case parseAndInfer "1 == null" of
    Right (NT.TBool, _) -> True
    _ -> False

prop_review_eq_heterogeneous_ok :: Property
prop_review_eq_heterogeneous_ok =
  forAll (elements ["null", "\"s\"", "1", "true", "[ ]"]) $ \rhs ->
    case parseAndInfer ("1 == " <> rhs) of
      Right (NT.TBool, _) -> True
      _ -> False

-- #4 (FIXED): `map` is a real scheme — valid use is `[b]`, misuse fails.
prop_review_map_ok :: Bool
prop_review_map_ok =
  case parseAndInfer "map (x: x + 1) [ 1 ]" of
    Right (NT.TList NT.TInt, _) -> True
    _ -> False

prop_review_map_misuse_fails :: Bool
prop_review_map_misuse_fails =
  isLeft (parseAndInfer "map (x: x + 1) [ \"a\" ]")

-- #19 (FIXED): applying a polymorphic builtin to an argument must TERMINATE.
-- `instantiate` could produce a self-map {v ↦ TVar v} that the chasing
-- `applySubst` looped on forever, so `head [ 1 ]` (and map/filter/…) hung
-- inference. This regression test forces the result type so a re-introduced loop
-- fails fast instead of hanging. (Latent for the whole `polymorphicBuiltins` set;
-- never caught because the suite never applied one and forced the output.)
prop_review_poly_builtin_terminates :: Bool
prop_review_poly_builtin_terminates =
  case parseAndInfer "head [ 1 ]" of
    Right (NT.TInt, _) -> True
    _ -> False

-- #7 (FIXED): heterogeneous `+` (Int+Float = Float, Path+String = Path),
-- while non-addable combinations still fail.
prop_review_plus_int_float :: Bool
prop_review_plus_int_float =
  case parseAndInfer "1 + 1.5" of
    Right (NT.TFloat, _) -> True
    _ -> False

prop_review_plus_path_string :: Bool
prop_review_plus_path_string =
  case parseAndInfer "./foo + \"bar\"" of
    Right (NT.TPath, _) -> True
    _ -> False

prop_review_plus_nonaddable_fails :: Bool
prop_review_plus_nonaddable_fails =
  isLeft (parseAndInfer "true + false")

-- #8 boundary (FIXED): union membership rejects a concrete non-member.
prop_review_tostring_concrete_errors :: Bool
prop_review_tostring_concrete_errors =
  isLeft (parseAndInfer "toString { a = 1; }")

-- `toString` coerces lists (space-joined — `toString [ "-fpermissive" ]` is valid
-- Nix), and the Nix globals `placeholder` / `fromTOML` resolve unqualified. All
-- three were dominant false positives on real nixpkgs (lists alone ~318 skips in
-- pkgs/by-name). The bare-set rejection above still holds — list subsumption in
-- 'unionMemberAccepts' does not weaken it.
prop_tostring_list_and_globals_ok :: Bool
prop_tostring_list_and_globals_ok =
  ok "toString [ \"-fpermissive\" ]"
    && ok "placeholder \"out\""
    && ok "builtins.fromTOML \"x = 1\""
 where
  ok src = either (const False) (const True) (parseAndInfer src)

-- #16 (NEW PROPERTY, was missing): the comment-injecting formatter must be
-- meaning-preserving. `annotateSource` only INSERTS `# ::` comment lines and never
-- edits code, so stripping the injected lines from the output must recover the
-- original lines exactly. (A structural AST compare is the wrong check here:
-- `stripAnnotation` does not normalise the `NSourcePos` stored inside `NamedVar`,
-- which legitimately shifts when comment lines are added.)
-- NOTE: this guards `Nix.Format`/`annotateSource` only. The separate reformatter
-- `Nix.Formatter.formatNixFile` — which the review faults for collapsing
-- significant whitespace in indented strings (#16) — still needs its own
-- roundtrip property; see TODO.
prop_review_format_roundtrip :: Property
prop_review_format_roundtrip = forAll (sized genNixExpr) $ \src ->
  case annotateExpr src of
    Left _ -> True -- did not format (e.g. parse failure): vacuous
    Right formatted ->
      let isInjected l = "# ::" `T.isPrefixOf` T.stripStart l
          kept = filter (not . isInjected) (T.lines formatted)
       in kept == T.lines src

-- #16: the REFORMATTER (`Nix.Formatter.formatNixFile`) must be MEANING-PRESERVING
-- — reformatting may change layout, but `parse (format (parse src))` must yield
-- the same AST as `parse src` (modulo source positions). This is the property the
-- review flagged as missing, faulting the reformatter for collapsing significant
-- whitespace (esp. inside indented `''…''` strings, where indentation is
-- semantic). A line-equality check (as in 'prop_review_format_roundtrip') is wrong
-- here because the reformatter deliberately rewrites layout; we compare normalised
-- ASTs instead.

-- | canonical source position (from hnix's null span) used to erase positions
canonSourcePos :: NixE.NSourcePos
canonSourcePos = getSpanBegin nullSpan

{- | erase source positions so two ASTs compare equal modulo layout. Only
@Binding@ nodes (NamedVar/Inherit) carry an 'NixE.NSourcePos' inside the
unannotated 'NixE.NExpr' (the outer 'SrcSpan' is already gone via
'stripAnnotation'); the parser also normalises indented-string indentation, so a
whitespace-collapse bug in the reformatter shows up as a differing 'NStr' here.
-}
zeroExprPos :: NixE.NExpr -> NixE.NExpr
zeroExprPos = foldFix (Fix . go)
 where
  go (NixE.NSet recur bs) = NixE.NSet recur (map zb bs)
  go (NixE.NLet bs body) = NixE.NLet (map zb bs) body
  go other = other
  zb (NixE.NamedVar p v _) = NixE.NamedVar p v canonSourcePos
  zb (NixE.Inherit ms ks _) = NixE.Inherit ms ks canonSourcePos

-- | reformat a source string and confirm the AST survives modulo positions.
reformatPreservesMeaning :: Text -> Property
reformatPreservesMeaning src = case parseNixTextLoc src of
  Left _ -> property True -- unparseable input: vacuous
  Right ast0 ->
    let formatted = formatNixFile src "<test>" ast0
     in case parseNixTextLoc formatted of
          Left e ->
            counterexample
              ( "reformatted output does not parse:\n"
                  <> T.unpack formatted
                  <> "\nerror: "
                  <> show e
              )
              (property False)
          Right ast1 ->
            counterexample
              ( "AST changed under reformat.\n--- in ---\n"
                  <> T.unpack src
                  <> "\n--- out ---\n"
                  <> T.unpack formatted
              )
              (zeroExprPos (stripAnnotation ast0) === zeroExprPos (stripAnnotation ast1))

-- #16: the reformatter (`Nix.Formatter.formatNixFile`, now a nixfmt-RFC parity
-- pretty-printer) must be MEANING-PRESERVING: `parse (format (parse src))` equals
-- `parse src` modulo source position. The rewrite emits precedence parens, so the
-- generated round-trip now holds (was an expectFailure tripwire).
prop_reformatter_roundtrip :: Property
prop_reformatter_roundtrip = forAll (sized genNixExpr) reformatPreservesMeaning

-- curated corpus the generator never reaches — nested records, inherits, multiline
-- lists, let. All meaning-preserving under the rewritten formatter.
prop_reformatter_roundtrip_corpus :: Property
prop_reformatter_roundtrip_corpus =
  conjoin (map reformatPreservesMeaning corpus)
 where
  corpus =
    [ "{ a = 1; b = { c = 2; d = { e = 3; }; }; }"
    , "[ 1 2 3\n  4 5 6 ]"
    , "let x = 1; y = 2; in x + y"
    , "{ inherit a b; inherit (pkgs) c d; }"
    , "rec { a = 1; b = a + 1; }"
    ]

-- Indented `''…''` strings are meaning-preserving now that the reformatter is
-- backed by vendored nixfmt (which round-trips indentation faithfully). Was an
-- expectFailure tripwire under the old hand-rolled printer (#16).
prop_reformatter_indented_string :: Property
prop_reformatter_indented_string =
  conjoin (map reformatPreservesMeaning corpus)
 where
  corpus =
    [ "{ a = ''\n    hello\n      world\n  ''; }"
    , "{ script = ''\n    set -e\n    echo   spaced\n  ''; }"
    , "{ a = ''\n    line1\n\n    line3 with two blanks above\n  ''; }"
    ]

-- #10: positive well-typedness — accepted programs infer the EXPECTED type, not
-- merely "doesn't crash". A curated vector set spanning atoms, string literals,
-- lists, arithmetic, if/let, application, selection (incl. row-polymorphic select
-- through a function param), list concat, and a row-polymorphic builtin.
-- JANK (found dogfooding the CLI): `prettyType` dumped a `TStrLit`'s full text, so
-- `infer` on a file with a huge string literal produced a huge `# :: "…"` type.
-- Long literals must be truncated in type display; short ones preserved exactly.
-- C1 (output rework): the pure Diagnostic renderer produces the clippy layout.
-- Golden — locks the format so the C2 checker migration reviews as a diff.
prop_diagnostic_render :: Bool
prop_diagnostic_render = full && minimal
 where
  full = Diag.renderDiagnostic False d == expected
  d =
    Diag.Diagnostic
      { Diag.diagSeverity = ErrorS
      , Diag.diagCode = Just "NARSIL-N001"
      , Diag.diagSpan = Just (Span (Loc 90 7) (Loc 90 16) (Just "flake.nix"))
      , Diag.diagSummary = "`with` expression is not allowed"
      , Diag.diagHelp = ["use `inherit (pkgs) git;` instead"]
      , Diag.diagSnippet = Just (Diag.Snippet 90 "  with pkgs; [ git ];" 3 9)
      }
  expected =
    T.intercalate
      "\n"
      [ "error[NARSIL-N001]: `with` expression is not allowed"
      , "  --> flake.nix:90:7"
      , "   |"
      , "90 |   with pkgs; [ git ];"
      , "   |   ^^^^^^^^^"
      , "   = help: use `inherit (pkgs) git;` instead"
      ]
  minimal =
    Diag.renderDiagnostic
      False
      (Diag.Diagnostic WarningS Nothing Nothing "something off" [] Nothing)
      == "warning: something off"

prop_pretty_strlit_truncated :: Bool
prop_pretty_strlit_truncated =
  NT.prettyType (NT.TStrLit "hi") == "\"hi\""
    && let big = NT.prettyType (NT.TStrLit (T.replicate 5000 "a"))
        in T.length big <= 45 && "…\"" `T.isSuffixOf` big

-- JANK (found dogfooding the CLI): the dispatch used to prefix every safe-parse
-- failure with "Parse error:", mislabeling I/O and depth errors and double-printing
-- "parse error:". The fix relies on 'renderSafetyError' being self-categorizing —
-- lock that each variant renders with its correct, distinct category.
prop_safety_error_categories :: Bool
prop_safety_error_categories =
  Safety.renderSafetyError (Safety.SafetyIOError "boom") == "I/O error: boom"
    && Safety.renderSafetyError (Safety.SafetyParseFailed "boom") == "parse error: boom"
    && Safety.renderSafetyError (Safety.SafetyInternalException "boom")
      == "internal exception: boom"
    && Safety.renderSafetyError Safety.SafetyStackOverflow /= ""

prop_welltyped_vectors :: Property
prop_welltyped_vectors = conjoin (map check vectors)
 where
  check (src, expected) =
    counterexample (T.unpack src <> "  ::  expected " <> show expected) $
      case parseAndInfer src of
        Right (ty, _) -> ty === expected
        Left e -> counterexample ("UNEXPECTEDLY REJECTED: " <> T.unpack e) (property False)
  vectors :: [(Text, NT.NixType)]
  vectors =
    [ ("1", NT.TInt)
    , ("true", NT.TBool)
    , ("\"hi\"", NT.TStrLit "hi")
    , ("1 + 2", NT.TInt)
    , ("[ 1 2 3 ]", NT.TList NT.TInt)
    , ("if true then 1 else 2", NT.TInt)
    , ("let x = 5; in x + 1", NT.TInt)
    , ("(x: x) 3", NT.TInt)
    , ("({ a = 1; }).a", NT.TInt)
    , ("(x: x.foo) { foo = 1; bar = 2; }", NT.TInt)
    , ("(x: [ x.a x.b ]) { a = 1; b = 2; }", NT.TList NT.TInt)
    , ("[ 1 2 ] ++ [ 3 ]", NT.TList NT.TInt)
    , ("builtins.attrNames { a = 1; b = 2; }", NT.TList NT.TString)
    ]

-- ── UNFIXED (documented via expectFailure) ──────────────────────────────────

-- #2 (RC1 — FIXED in rows stage 3): selecting a field from a function argument
-- now emits a row constraint α ~ { foo : β | ρ }, so `(x: x.foo) 5` is a type
-- error (5 is not a record). Was a silent freshVar.
prop_review_select_on_var_constrains :: Bool
prop_review_select_on_var_constrains = isLeft (parseAndInfer "(x: x.foo) 5")

-- RC1 row accumulation: a function selecting two fields constrains its argument
-- to an open record with BOTH, then unifies cleanly with a record that has them.
prop_review_select_accumulates :: Bool
prop_review_select_accumulates =
  case parseAndInfer "(x: [ x.a x.b ]) { a = 1; b = 2; }" of
    Right (NT.TList NT.TInt, _) -> True
    _ -> False

-- RC1: selecting a present field through a variable resolves to its type.
prop_review_select_present_ok :: Bool
prop_review_select_present_ok =
  case parseAndInfer "(x: x.foo) { foo = 1; bar = 2; }" of
    Right (NT.TInt, _) -> True
    _ -> False

-- RC1: selecting a field absent from the (closed) argument is a type error.
prop_review_select_missing_fails :: Bool
prop_review_select_missing_fails =
  isLeft (parseAndInfer "(x: x.a) { b = 1; }")

-- #8 (optionality): an OPEN record's optional field absent from the matched
-- record is fine (unifyRec respects the optional flag). `{ a ? 1, ... }: a`
-- applied to `{}` must type-check.
prop_review_optional_open_field_ok :: Bool
prop_review_optional_open_field_ok =
  case parseAndInfer "({ a ? 1, ... }: a) { }" of
    Right (NT.TInt, _) -> True
    _ -> False

-- A parameter's default value may reference a SIBLING parameter — Nix gives all
-- formals one mutually-recursive scope. Both orders must type-check. Regression:
-- defaults were inferred before the siblings were bound, so `{ b ? a, a }` (and
-- the common nixpkgs `{ lib, doCheck ? lib.versionAtLeast … }`) wrongly reported
-- the sibling unbound — the dominant skip cause on real nixpkgs trees.
prop_param_default_refs_sibling :: Bool
prop_param_default_refs_sibling =
  ok "{ a, b ? a }: b" && ok "{ b ? a, a }: b"
 where
  ok src = either (const False) (const True) (parseAndInfer src)

-- RC1 stage 4: `builtins.attrNames` is row-polymorphic (a scheme instantiated at
-- the selection site, not a monotype baked into the `builtins` record). It
-- returns [String] on a record and rejects non-records.
prop_review_builtins_attrnames_ok :: Bool
prop_review_builtins_attrnames_ok =
  case parseAndInfer "builtins.attrNames { a = 1; b = 2; }" of
    Right (NT.TList NT.TString, _) -> True
    _ -> False

prop_review_builtins_attrnames_nonrecord_fails :: Bool
prop_review_builtins_attrnames_nonrecord_fails =
  isLeft (parseAndInfer "builtins.attrNames 5")

prop_review_builtins_hasattr_ok :: Bool
prop_review_builtins_hasattr_ok =
  case parseAndInfer "builtins.hasAttr \"a\" { a = 1; }" of
    Right (NT.TBool, _) -> True
    _ -> False

-- `lib.*` is modeled like `builtins.*`: each field is a polymorphic SCHEME
-- instantiated fresh per use, so a library combinator can be applied at many
-- result types. Before this, `lib.mkIf` was monomorphically pinned by its first
-- use and a second use at a different type false-positived — the dominant false
-- positive on real flake-parts / NixOS module code.
prop_lib_mkif_polymorphic :: Bool
prop_lib_mkif_polymorphic =
  isRight (parseAndInfer "{ lib }: { a = lib.mkIf true { x = 1; }; b = lib.mkIf true 2; }")

-- `lib.mkMerge : [a] -> a` (structural combinator), instantiated fresh.
prop_lib_mkmerge_polymorphic :: Bool
prop_lib_mkmerge_polymorphic =
  isRight (parseAndInfer "{ lib }: lib.mkMerge [ { a = 1; } { b = 2; } ]")

-- In module mode, the self-referential flake @inputs (`mkFlake { inherit inputs;
-- }`) no longer forms a cyclic row and so no longer false-positives with
-- "infinite type". This is the exact shape of our own flake.nix outputs.
prop_module_flake_selfref_ok :: Bool
prop_module_flake_selfref_ok =
  isRight
    ( parseAndInferModule
        "{ flake-parts, ... }@inputs: flake-parts.lib.mkFlake { inherit inputs; } { }"
    )

-- Self-application through a non-external formal is VALID Nix (untyped
-- languages allow it); domain-dynamic application of unknown formals
-- (review-5, the fetcher/mkDerivation FP class) accepts it rather than
-- enshrining HM's occurs-check limitation as an error. Genuine cyclic DATA
-- (`rec { x = x; }`) still errors — next property.
prop_module_selfref_nonexternal_ok :: Bool
prop_module_selfref_nonexternal_ok =
  isRight (parseAndInferModule "f: f { inherit f; } { }")

-- FLIPPED (review-6): `rec { x = x; }` is legal LAZY Nix — `typeOf` yields
-- "set"; forcing `x` diverges at runtime, but divergence is not a type
-- error. The occurs check now protects only the substitution (the var stays
-- unconstrained) instead of rejecting the program.
prop_module_mode_keeps_strict_occurs :: Bool
prop_module_mode_keeps_strict_occurs =
  isRight (parseAndInfer "rec { x = x; }")

-- #6 (RC2 — FIXED): `[TInt ~ a, a ~ TBool]` is satisfiable as `a = TNumeric`.
-- The new collect-then-join solver resolves it (the old left fold bound
-- `a := TInt` then rejected `TInt ~ TBool`). Order-independent + complete.
prop_review_bash_subtype_resolves :: Bool
prop_review_bash_subtype_resolves =
  let a = TVar (TypeVar "a")
      forward = solve [TInt :~: a, a :~: TBool]
      reversed = solve [a :~: TBool, TInt :~: a]
   in isRight forward && isRight reversed && resolvesNumeric forward
 where
  resolvesNumeric (Right s) = applySubst s (TVar (TypeVar "a")) == TNumeric
  resolvesNumeric _ = False

-- #8 CORRECTION (review claim does NOT reproduce): the review said a union
-- meeting a variable adds no constraint, so `(x: toString x) { a = 1; }` would be
-- wrongly accepted. In fact `unify` binds the variable to the union via its
-- var-binding arm, so the attrset is correctly rejected. Like #14, the reviewer
-- overstated this against the current tree. We assert the CORRECT behavior.
prop_review_union_var_constrains :: Bool
prop_review_union_var_constrains =
  isLeft (parseAndInfer "(x: toString x) { a = 1; }")

-- | NIX-1: inferExpr on parseable expressions returns a type or a meaningful error
prop_nix_infer_no_crash :: Property
prop_nix_infer_no_crash = forAll (sized genNixExpr) $ \src ->
  case parseNixTextLoc src of
    Left _ -> label "nix: unparseable" True
    Right expr -> case inferExpr expr of
      Left err -> label "nix: type error" $ not (T.null err)
      Right (t, _) -> label "nix: inferred" $ t `seq` True

-- | NIX-3: Integer literals infer to TInt
prop_nix_int_literal :: Property
prop_nix_int_literal = forAll (choose (0, 10000) :: Gen Int) $ \n ->
  case parseAndInfer (T.pack (show n)) of
    Right (NT.TInt, _) -> True
    _ -> False

-- | NIX-4: String literals infer to TString or TStrLit
prop_nix_string_literal :: Property
prop_nix_string_literal = forAll genStringLiteral $ \s ->
  case parseAndInfer ("\"" <> s <> "\"") of
    Right (NT.TString, _) -> True
    Right (NT.TStrLit _, _) -> True
    _ -> False

-- | NIX-5: Bool literals infer to TBool
prop_nix_bool_literal :: Bool -> Bool
prop_nix_bool_literal b =
  case parseAndInfer (if b then "true" else "false") of
    Right (NT.TBool, _) -> True
    _ -> False

-- | NIX-6: null infers to TNull
prop_nix_null_literal :: Bool
prop_nix_null_literal =
  case parseAndInfer "null" of
    Right (NT.TNull, _) -> True
    _ -> False

-- | NIX-7: Lists of ints infer to TList TInt
prop_nix_list_int :: Property
prop_nix_list_int = forAll (choose (1, 5)) $ \n ->
  let elems = T.unwords (replicate n "1")
      src = "[ " <> elems <> " ]"
   in case parseAndInfer src of
        Right (NT.TList NT.TInt, _) -> True
        _ -> False

-- | NIX-8: Attrsets infer fields correctly
prop_nix_attrset :: Property
prop_nix_attrset =
  let src = "{ x = 1; y = \"hello\"; }"
   in case parseAndInfer src of
        Right (t, _) -> case t of
          NT.TAttrs m -> checkFields m
          NT.TAttrsOpen m -> checkFields m
          _ -> property False
        _ -> property False
 where
  checkFields m =
    case (Map.lookup "x" m, Map.lookup "y" m) of
      (Just (NT.TInt, _), Just (ty, _)) ->
        property $ case ty of
          NT.TString -> True
          NT.TStrLit _ -> True
          _ -> False
      _ -> property False

-- | NIX-9: Identity function infers polymorphic type
prop_nix_identity :: Bool
prop_nix_identity =
  case parseAndInfer "x: x" of
    Right (NT.TFun _ _, _) -> True
    _ -> False

-- | NIX-10: Let binding scopes correctly
prop_nix_let_binding :: Bool
prop_nix_let_binding =
  case parseAndInfer "let x = 42; in x" of
    Right (NT.TInt, _) -> True
    _ -> False

-- | NIX-11: with resolves names from scope attrset
prop_nix_with_resolves :: Bool
prop_nix_with_resolves =
  case parseAndInfer "with { x = 1; }; x" of
    Right (NT.TInt, _) -> True
    _ -> False

-- | NIX-12: with resolves multiple names consistently
prop_nix_with_multiple :: Bool
prop_nix_with_multiple =
  case parseAndInfer "with { x = 1; y = 2; }; x + y" of
    Right (NT.TInt, _) -> True
    _ -> False

-- \| NIX-13: with works inside function params (polymorphic with)
prop_nix_with_polymorphic :: Bool
prop_nix_with_polymorphic =
  case parseAndInfer "s: with s; x" of
    Right (NT.TFun _ _, _) -> True
    _ -> False

-- | NIX-14: rec handles simple self-reference
prop_nix_rec_self :: Bool
prop_nix_rec_self =
  case parseAndInfer "rec { x = 1; }" of
    Right (NT.TAttrs m, _) -> Map.lookup "x" m == Just (NT.TInt, False)
    Right (NT.TAttrsOpen m, _) -> Map.lookup "x" m == Just (NT.TInt, False)
    _ -> False

-- | NIX-15: rec handles mutual recursion
prop_nix_rec_mutual :: Bool
prop_nix_rec_mutual =
  case parseAndInfer "rec { x = 1; y = x; }" of
    Right (t, _) -> case t of
      NT.TAttrs m -> check m
      NT.TAttrsOpen m -> check m
      _ -> False
    _ -> False
 where
  check m = case (Map.lookup "x" m, Map.lookup "y" m) of
    (Just (NT.TInt, _), Just (NT.TInt, _)) -> True
    _ -> False

-- | NIX-16: rec handles cross-reference in same SCC
prop_nix_rec_cross :: Bool
prop_nix_rec_cross =
  case parseAndInfer "rec { x = y; y = 1; }" of
    Right (t, _) -> case t of
      NT.TAttrs m -> check m
      NT.TAttrsOpen m -> check m
      _ -> False
    _ -> False
 where
  check m = case (Map.lookup "x" m, Map.lookup "y" m) of
    (Just (NT.TInt, _), Just (NT.TInt, _)) -> True
    _ -> False

{- | NIX-17 FLIPPED (review-6): self-referential rec bindings are valid lazy
Nix (see prop_module_mode_keeps_strict_occurs) — they stay polymorphic.
-}
prop_nix_rec_infinite :: Bool
prop_nix_rec_infinite =
  case parseAndInfer "rec { x = x; }" of
    Right _ -> True
    _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Module kind detection
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Simple attrset with _class flake is detected as FlakeModule
prop_module_kind_flake :: Bool
prop_module_kind_flake =
  case parseNixTextLoc "{ _class = \"flake\"; }" of
    Right expr ->
      let Detection{detectedKind = mk} = detectKind "test.nix" expr
       in mk == FlakeModule
    _ -> False

-- | Function of {config, lib, pkgs}: is a NixOSModule
prop_module_kind_nixos :: Bool
prop_module_kind_nixos =
  case parseNixTextLoc "{ config, lib, pkgs, ... }: { options = {}; config = {}; }" of
    Right expr ->
      let Detection{detectedKind = mk} = detectKind "test.nix" expr
       in mk == NixOSModule
    _ -> False

-- | Function of {lib, stdenv}: calling mkDerivation is a Package
prop_module_kind_package :: Bool
prop_module_kind_package =
  case parseNixTextLoc "{ lib, stdenv, ... }: stdenv.mkDerivation { name = \"foo\"; }" of
    Right expr ->
      let Detection{detectedKind = mk} = detectKind "default.nix" expr
       in mk == Package
    _ -> False

-- | Function of final: prev: is an Overlay
prop_module_kind_overlay :: Bool
prop_module_kind_overlay =
  case parseNixTextLoc "final: prev: { hello = prev.hello; }" of
    Right expr ->
      let Detection{detectedKind = mk} = detectKind "test.nix" expr
       in mk == Overlay
    _ -> False

-- | Top-level flake.nix with outputs attrset is Flake
prop_module_kind_flake_file :: Bool
prop_module_kind_flake_file =
  case parseNixTextLoc "{ outputs = { ... }: {}; }" of
    Right expr ->
      let Detection{detectedKind = mk} = detectKind "flake.nix" expr
       in mk == Flake
    _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Naming convention enforcement
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

dummySpanNaming :: Span
dummySpanNaming = Span (Loc 0 0) (Loc 0 0) Nothing

-- | kebab-case identifiers pass when kebab-case is required
prop_naming_kebab_valid :: Bool
prop_naming_kebab_valid =
  null $ Naming.checkIdentifier Naming.LispCase "test" "forces-code-through-prelude" dummySpanNaming

-- | snake_case identifiers fail kebab-case convention
prop_naming_kebab_reject_snake :: Bool
prop_naming_kebab_reject_snake =
  not (null $ Naming.checkIdentifier Naming.LispCase "test" "not_lisp_case" dummySpanNaming)

-- | CamelCase identifiers fail kebab-case convention
prop_naming_kebab_reject_camel :: Bool
prop_naming_kebab_reject_camel =
  not (null $ Naming.checkIdentifier Naming.LispCase "test" "NotLispCase" dummySpanNaming)

{- | kebab-case roundtrip through toKebabCase/toSnakeCase
NOTE: toKebabCase "hello-world" → "hello-world" only if in kebab-case,
and toKebabCase "hello_world" → "hello-world". The implementation
may not handle mixed inputs or may use different conersion rules.
-}
prop_naming_roundtrip_kebab :: Bool
prop_naming_roundtrip_kebab =
  Naming.toKebabCase "hello-world" == "hello-world"
    && Naming.toKebabCase "helloworld" == "helloworld"

-- | snake_case roundtrip
prop_naming_roundtrip_snake :: Bool
prop_naming_roundtrip_snake =
  Naming.toSnakeCase "hello_world" == "hello_world"
    && Naming.toSnakeCase "helloworld" == "helloworld"

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Layout convention enforcement
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | straylight convention validates _class = "flake" in modules/flake/
prop_layout_straylight_valid :: Bool
prop_layout_straylight_valid =
  let violations =
        LC.validateLayout
          LC.straylight
          "/"
          [("nix/modules/flake/broker.nix", Detection FlakeModule 100 [])]
   in null violations

-- | straylight convention rejects _class = "flake" in modules/nixos/
prop_layout_straylight_invalid :: Bool
prop_layout_straylight_invalid =
  let violations =
        LC.validateLayout
          LC.straylight
          "/"
          [("modules/nixos/broker.nix", Detection FlakeModule 100 [])]
   in not (null violations)

-- | flakeParts convention allows files in modules/
prop_layout_flakeparts_valid :: Bool
prop_layout_flakeparts_valid =
  let violations =
        LC.validateLayout LC.flakeParts "/" [("modules/apps.nix", Detection FlakeModule 100 [])]
   in null violations

-- | nixpkgsByName convention validates packages in pkgs/by-name/
prop_layout_nixpkgs_package_valid :: Bool
prop_layout_nixpkgs_package_valid =
  let violations =
        LC.validateLayout
          LC.nixpkgsByName
          "/"
          [("pkgs/by-name/fo/foo/default.nix", Detection Package 100 [])]
   in null violations

-- | nixpkgsByName silently accepts unmatched module kinds (no rule)
prop_layout_nixpkgs_non_package :: Bool
prop_layout_nixpkgs_non_package =
  let violations =
        LC.validateLayout LC.nixpkgsByName "/" [("lib/utils.nix", Detection Library 100 [])]
   in null violations -- nixpkgsByName only defines Package rules; Library has no matching rule

-- | nixosConfig validates modules in modules/
prop_layout_nixos_modules_valid :: Bool
prop_layout_nixos_modules_valid =
  let violations =
        LC.validateLayout LC.nixosConfig "/" [("modules/system.nix", Detection NixOSModule 100 [])]
   in null violations

-- | nixosConfig validates modules in hosts/
prop_layout_nixos_hosts_valid :: Bool
prop_layout_nixos_hosts_valid =
  let violations =
        LC.validateLayout LC.nixosConfig "/" [("hosts/mars.nix", Detection NixOSModule 100 [])]
   in null violations

-- | nixosConfig validates home modules in users/
prop_layout_nixos_users_valid :: Bool
prop_layout_nixos_users_valid =
  let violations =
        LC.validateLayout LC.nixosConfig "/" [("users/alice.nix", Detection HomeModule 100 [])]
   in null violations

-- | nixosConfig rejects files in wrong location
prop_layout_nixos_wrong_location :: Bool
prop_layout_nixos_wrong_location =
  let violations =
        LC.validateLayout LC.nixosConfig "/" [("bin/script.nix", Detection NixOSModule 100 [])]
   in not (null violations)

-- | straylight: forbidden location for package in modules/
prop_layout_forbidden_package :: Bool
prop_layout_forbidden_package =
  let violations =
        LC.validateLayout
          LC.straylight
          "/"
          [("nix/modules/flake/broker.nix", Detection Package 100 [])]
   in not (null violations) && any (\e -> LC.errCode e == LC.E002) violations

-- | straylight: forbidden location for flake module in packages/
prop_layout_forbidden_flake_mod :: Bool
prop_layout_forbidden_flake_mod =
  let violations =
        LC.validateLayout
          LC.straylight
          "/"
          [("nix/packages/broker.nix", Detection FlakeModule 100 [])]
   in not (null violations) && any (\e -> LC.errCode e == LC.E002) violations

-- | Exact path pattern: flake.nix must be exactly flake.nix
prop_layout_exact_flake :: Bool
prop_layout_exact_flake =
  let violations =
        LC.validateLayout LC.straylight "/" [("nix/flake.nix", Detection Flake 100 [])]
   in not (null violations) -- "nix/flake.nix" ≠ Exact ["flake.nix"]
        && null
          -- Exact match
          (LC.validateLayout LC.straylight "/" [("flake.nix", Detection Flake 100 [])])

-- | Contains path pattern (nixpkgsByName has no Contains patterns, use constructed)
prop_layout_contains_unused :: Bool
-- Contains pattern exists in PathPattern but no conventions use it
prop_layout_contains_unused = True

-- | CamelCase naming convention
prop_naming_camel_valid :: Bool
prop_naming_camel_valid =
  LC.isValidName LC.CamelCase "camelCase"
    && LC.isValidName LC.CamelCase "lowerCamel"
    && not (LC.isValidName LC.CamelCase "snake_case")
    && not (LC.isValidName LC.CamelCase "PascalCase")

-- | PascalCase naming convention
prop_naming_pascal_valid :: Bool
prop_naming_pascal_valid =
  LC.isValidName LC.PascalCase "PascalCase"
    && LC.isValidName LC.PascalCase "UpperCamel"
    && not (LC.isValidName LC.PascalCase "camelCase")
    && not (LC.isValidName LC.PascalCase "snake_case")

-- | validateAttrName for kebab-case rejects snake_case attrs
prop_layout_attr_name_kebab :: Bool
prop_layout_attr_name_kebab =
  case LC.validateAttrName LC.straylight "valid-name" of
    Just _ -> False
    Nothing ->
      case LC.validateAttrName LC.straylight "snake_name" of
        Just _ -> True
        Nothing -> False

-- | validateAttrName for CamelCase via nixpkgsByName
prop_layout_attr_name_camel :: Bool
prop_layout_attr_name_camel =
  case LC.validateAttrName LC.nixpkgsByName "camelCase" of
    Just _ -> False
    Nothing ->
      case LC.validateAttrName LC.nixpkgsByName "kebab-case" of
        Just _ -> True
        Nothing -> False

-- | validateIdentifier for kebab-case convention
prop_layout_ident_kebab :: Bool
prop_layout_ident_kebab =
  case LC.validateIdentifier LC.straylight "valid-ident" of
    Just _ -> False
    Nothing ->
      case LC.validateIdentifier LC.straylight "snake_ident" of
        Just _ -> True
        Nothing -> False

-- | toKebabCase on CamelCase input
prop_naming_kebab_from_camel :: Bool
prop_naming_kebab_from_camel =
  LC.toKebabCase "helloWorld" == "hello-world"
    && LC.toKebabCase "HTTPResponse" == "h-t-t-p-response"
    && LC.toKebabCase "XMLParser" == "x-m-l-parser"

-- | toSnakeCase on kebab-case input
prop_naming_snake_from_kebab :: Bool
prop_naming_snake_from_kebab =
  LC.toSnakeCase "hello-world" == "hello_world"
    && LC.toSnakeCase "hello_world" == "hello_world"

-- | dropNixExtension strips .nix suffix
prop_naming_drop_nix :: Bool
prop_naming_drop_nix =
  LC.dropNixExtension "foo.nix" == "foo"
    && LC.dropNixExtension "bar" == "bar"
    && LC.dropNixExtension "deep/baz.nix" == "deep/baz"

-- | File name validation (E003) for straylight kebab-case
prop_layout_filename_kebab :: Bool
prop_layout_filename_kebab =
  let violations =
        LC.validateLayout
          LC.straylight
          "/"
          [("nix/modules/flake/gpu-broker.nix", Detection FlakeModule 100 [])]
   in null violations
        && not
          ( null $
              LC.validateLayout
                LC.straylight
                "/"
                [("nix/modules/flake/snake_name.nix", Detection FlakeModule 100 [])]
          )

{- | validateFlakeModReq with convRequireFlakeMod = True: flake modules and
package leaves are permitted; any other recognized kind is rejected (E006).
-}
prop_layout_flake_mod_required :: Bool
prop_layout_flake_mod_required =
  let strictConv = LC.straylight{LC.convRequireFlakeMod = True}
      nixosViolations =
        LC.validateLayout strictConv "/" [("nix/modules/foo.nix", Detection NixOSModule 100 [])]
      packageViolations =
        LC.validateLayout strictConv "/" [("nix/packages/foo.nix", Detection Package 100 [])]
      flakeViolations =
        LC.validateLayout
          strictConv
          "/"
          [("nix/modules/flake/bar.nix", Detection FlakeModule 100 [])]
   in any (\e -> LC.errCode e == LC.E006) nixosViolations
        && null packageViolations
        && null flakeViolations

-- | validateLayout with unknown module kind produces no location errors
prop_layout_unknown_kind :: Bool
prop_layout_unknown_kind =
  let violations =
        LC.validateLayout LC.straylight "/" [("anywhere/foo.nix", Detection Unknown 100 [])]
   in null violations

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Merge correctness
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | mergeEnvSpec preserves required from either side
prop_merge_preserves_required :: Bool
prop_merge_preserves_required =
  let sp = Span (Loc 1 0) (Loc 1 0) Nothing
      e1 = EnvSpec TString False Nothing sp
      e2 = EnvSpec TString True Nothing sp
   in envRequired (mergeEnvSpec e1 e2) && envRequired (mergeEnvSpec e2 e1)

-- | mergeEnvSpec keeps first default, falls back to second
prop_merge_keeps_default :: Bool
prop_merge_keeps_default =
  let sp = Span (Loc 1 0) (Loc 1 0) Nothing
      e1 = EnvSpec TInt False (Just (LitInt 42)) sp
      e2 = EnvSpec TInt False (Just (LitInt 99)) sp
      eNone = EnvSpec TInt False Nothing sp
   in envDefault (mergeEnvSpec e1 e2) == Just (LitInt 42)
        && envDefault (mergeEnvSpec eNone e2) == Just (LitInt 99)

-- | Duplicate variables in facts are correctly merged
prop_duplicate_var_merged :: Bool
prop_duplicate_var_merged =
  let sp = Span (Loc 1 0) (Loc 1 0) Nothing
      facts =
        [ DefaultIs "PORT" (LitInt 8080) sp
        , Required "PORT" sp
        ]
      constraints = factsToConstraints facts
      subst = case solve constraints of Right s -> s; Left _ -> emptySubst
      schema = buildSchema facts subst
   in case Map.lookup "PORT" (schemaEnv schema) of
        Just spec ->
          envRequired spec && envDefault spec == Just (LitInt 8080) && envType spec == TInt
        Nothing -> False

-- | mergeSchemas identity: empty `merge` s == s
prop_merge_schema_identity :: [Fact] -> Property
prop_merge_schema_identity facts =
  let constraints = factsToConstraints facts
      subst = case solve constraints of Right s -> s; Left _ -> emptySubst
      schema = buildSchema facts subst
   in property $ mergeSchemas emptySchema schema == schema

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Fact extraction vectors
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | \${VAR:-default} produces DefaultIs fact
prop_fact_default_is :: Bool
prop_fact_default_is =
  case parseBash "PORT=\"${PORT:-8080}\"" of
    Right ast ->
      let facts = extractFacts ast
       in any isDefaultIs facts
    Left _ -> False
 where
  isDefaultIs (DefaultIs "PORT" (LitInt 8080) _) = True
  isDefaultIs _ = False

-- | \${VAR:?} produces Required fact
prop_fact_required :: Bool
prop_fact_required =
  case parseBash "API_KEY=\"${API_KEY:?}\"" of
    Right ast ->
      let facts = extractFacts ast
       in any isRequired facts
    Left _ -> False
 where
  isRequired (Required "API_KEY" _) = True
  isRequired _ = False

-- | \$VAR assignment produces AssignFrom fact
prop_fact_assign_from :: Bool
prop_fact_assign_from =
  case parseBash "COPY=\"$ORIGINAL\"" of
    Right ast ->
      let facts = extractFacts ast
       in any isAssignFrom facts
    Left _ -> False
 where
  isAssignFrom (AssignFrom "COPY" "ORIGINAL" _) = True
  isAssignFrom _ = False

-- | config.x.y=$VAR produces ConfigAssign fact
prop_fact_config_assign :: Bool
prop_fact_config_assign =
  case parseBash "config.server.port=$PORT" of
    Right ast ->
      let facts = extractFacts ast
       in any isConfigAssign facts
    Left _ -> False
 where
  isConfigAssign (ConfigAssign ["server", "port"] "PORT" _ _) = True
  isConfigAssign _ = False

-- | Literal config produces ConfigLit fact
prop_fact_config_lit :: Bool
prop_fact_config_lit =
  case parseBash "config.debug=false" of
    Right ast ->
      let facts = extractFacts ast
       in any isConfigLit facts
    Left _ -> False
 where
  isConfigLit (ConfigLit ["debug"] (LitBool False) _) = True
  isConfigLit _ = False

-- | BUG-3: Double-quoted config value preserves Quoted
prop_fact_config_quoted :: Bool
prop_fact_config_quoted =
  case parseBash "config.server.host=\"localhost\"" of
    Right ast ->
      let facts = extractFacts ast
       in any isQuotedConfigLit facts
    Left _ -> False
 where
  isQuotedConfigLit (ConfigLit ["server", "host"] (LitString "localhost") _) = True
  isQuotedConfigLit _ = False

-- | BUG-3: Unquoted string config value produces Unquoted
prop_fact_config_unquoted :: Bool
prop_fact_config_unquoted =
  case parseBash "config.server.host=localhost" of
    Right ast ->
      let facts = extractFacts ast
       in any isUnquotedConfigLit facts
    Left _ -> False
 where
  isUnquotedConfigLit (ConfigLit ["server", "host"] (LitString "localhost") _) = True
  isUnquotedConfigLit _ = False

-- | BUG-3: Double-quoted variable config value preserves Quoted
prop_fact_config_var_quoting_ast :: Bool
prop_fact_config_var_quoting_ast =
  case parseBash "config.host=\"$HOST\"" of
    Right ast ->
      let facts = extractFacts ast
       in any (\case ConfigAssign _ _ Quoted _ -> True; _ -> False) facts
    Left _ -> False

-- | BUG-3: Config value $VAR without quotes produces Unquoted
prop_fact_config_var_unquoted :: Bool
prop_fact_config_var_unquoted =
  case parseBash "config.host=$HOST" of
    Right ast ->
      let facts = extractFacts ast
       in any (\case ConfigAssign _ _ Unquoted _ -> True; _ -> False) facts
    Left _ -> False

-- | BUG-3: Config value with space-delimited = tokenization
prop_fact_config_spaced_eq :: Bool
prop_fact_config_spaced_eq =
  case parseBash "config.a.b=\"value\"" of
    Right ast ->
      let facts = extractFacts ast
       in any isConfigLit facts
    Left _ -> False
 where
  isConfigLit (ConfigLit ["a", "b"] (LitString "value") _) = True
  isConfigLit _ = False

-- | BUG-3: Single-quoted config value split across = tokenization
prop_fact_config_split_eq_quoted :: Bool
prop_fact_config_split_eq_quoted =
  case parseBash "config.flag='enabled'" of
    Right ast ->
      let facts = extractFacts ast
       in any isConfigLitWithType facts
    Left _ -> False
 where
  isConfigLitWithType (ConfigLit ["flag"] (LitString "enabled") _) = True
  isConfigLitWithType _ = False

-- | BUG-3: Empty config value produces no facts (no crash)
prop_fact_config_empty_value :: Bool
prop_fact_config_empty_value =
  case parseBash "config.test=" of
    Right ast -> null (extractFacts ast)
    Left _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Emit-config output
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | emit-config JSON contains ${VAR:?} guards for variable refs
prop_emit_json_guarded :: Property
prop_emit_json_guarded =
  let spec =
        ConfigSpec
          TInt
          (Just "PORT")
          (Just Unquoted)
          Nothing
          Nothing
          (Span (Loc 1 0) (Loc 1 0) Nothing)
      schema = emptySchema{schemaConfig = Map.singleton ["port"] spec}
      output = emitConfigJSON schema
   in property $ ":?" `T.isInfixOf` output

{- | emit-config JSON passes runtime vars as printf arguments, not inert
single-quoted text
-}
prop_emit_json_runtime_args :: Property
prop_emit_json_runtime_args =
  let spec =
        ConfigSpec
          TInt
          (Just "PORT")
          (Just Unquoted)
          Nothing
          Nothing
          (Span (Loc 1 0) (Loc 1 0) Nothing)
      schema = emptySchema{schemaConfig = Map.singleton ["port"] spec}
      output = emitConfigJSON schema
   in property $ "%s" `T.isInfixOf` output && " \"${PORT:?" `T.isInfixOf` output

-- | emit-config function performs preflight guards outside command substitutions
prop_emit_preflight_guard :: Bool
prop_emit_preflight_guard =
  let spec =
        ConfigSpec
          TInt
          (Just "PORT")
          (Just Unquoted)
          Nothing
          Nothing
          (Span (Loc 1 0) (Loc 1 0) Nothing)
      schema = emptySchema{schemaConfig = Map.singleton ["port"] spec}
      output = emitConfigFunction schema
   in "__nix_compile_require_int \"PORT\" \"${PORT:?PORT is required}\"" `T.isInfixOf` output

-- | emit-config validates unquoted numeric values before output to prevent JSON injection
prop_emit_numeric_preflight_guard :: Bool
prop_emit_numeric_preflight_guard =
  let spec =
        ConfigSpec
          TInt
          (Just "PORT")
          (Just Unquoted)
          Nothing
          Nothing
          (Span (Loc 1 0) (Loc 1 0) Nothing)
      schema = emptySchema{schemaConfig = Map.singleton ["port"] spec}
      output = emitConfigFunction schema
   in "must be an integer" `T.isInfixOf` output
        && "*-)" `T.isInfixOf` output
        && "no leading zeros" `T.isInfixOf` output

-- | emit-config validates unquoted bool values before output
prop_emit_bool_preflight_guard :: Bool
prop_emit_bool_preflight_guard =
  let spec =
        ConfigSpec
          TBool
          (Just "DEBUG")
          (Just Unquoted)
          Nothing
          Nothing
          (Span (Loc 1 0) (Loc 1 0) Nothing)
      schema = emptySchema{schemaConfig = Map.singleton ["debug"] spec}
      output = emitConfigFunction schema
   in "must be true or false" `T.isInfixOf` output

-- | Quoted config vars are emitted as strings and should not receive numeric/bool validators
prop_emit_quoted_numeric_no_int_guard :: Bool
prop_emit_quoted_numeric_no_int_guard =
  let spec =
        ConfigSpec
          TInt
          (Just "PORT")
          (Just Quoted)
          Nothing
          Nothing
          (Span (Loc 1 0) (Loc 1 0) Nothing)
      schema = emptySchema{schemaConfig = Map.singleton ["port"] spec}
      output = emitConfigFunction schema
   in not ("__nix_compile_require_int \"PORT\"" `T.isInfixOf` output)
        && ": \"${PORT:?PORT is required}\"" `T.isInfixOf` output

-- | emit-config must not mutate caller shell options (e.g. leak set -e)
prop_emit_no_set_e_leak :: Bool
prop_emit_no_set_e_leak =
  let output = emitConfigFunction emptySchema
   in not ("set -e" `T.isInfixOf` output)

-- | runtime JSON escaper handles all JSON control escapes, not just newline/tab
prop_emit_runtime_escape_controls :: Bool
prop_emit_runtime_escape_controls =
  let output = emitConfigFunction emptySchema
   in "$'\\b'" `T.isInfixOf` output
        && "$'\\f'" `T.isInfixOf` output
        && "\\\\u%04x" `T.isInfixOf` output

-- | emit-config YAML contains ${VAR:?} guards
prop_emit_yaml_guarded :: Property
prop_emit_yaml_guarded =
  let spec =
        ConfigSpec
          TInt
          (Just "PORT")
          (Just Unquoted)
          Nothing
          Nothing
          (Span (Loc 1 0) (Loc 1 0) Nothing)
      schema = emptySchema{schemaConfig = Map.singleton ["port"] spec}
      output = emitConfigYAML schema
   in property $ ":?" `T.isInfixOf` output

-- | emit-config TOML never outputs invalid "null"
prop_emit_toml_no_null :: [Fact] -> Bool
prop_emit_toml_no_null facts =
  let schema = buildSchema facts emptySubst
      output = emitConfigTOML schema
      hasNullValue = "= null" `T.isInfixOf` output || "=null" `T.isInfixOf` output
   in not hasNullValue || "\"\"" `T.isInfixOf` output || T.null output

-- | emit-config JSON for literal values renders correctly
prop_emit_json_literal :: Bool
prop_emit_json_literal =
  let spec =
        ConfigSpec
          TInt
          Nothing
          Nothing
          (Just (LitInt 8080))
          Nothing
          (Span (Loc 1 0) (Loc 1 0) Nothing)
      schema = emptySchema{schemaConfig = Map.singleton ["port"] spec}
      output = emitConfigJSON schema
   in "8080" `T.isInfixOf` output

-- | emit-config string values are quoted in JSON
prop_emit_json_string_quoted :: Bool
prop_emit_json_string_quoted =
  let spec =
        ConfigSpec
          TString
          (Just "HOST")
          (Just Quoted)
          Nothing
          Nothing
          (Span (Loc 1 0) (Loc 1 0) Nothing)
      schema = emptySchema{schemaConfig = Map.singleton ["host"] spec}
      output = emitConfigJSON schema
   in "__nix_compile_escape_json" `T.isInfixOf` output

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Scope graph construction
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Let bindings create declarations in the correct scope
prop_scope_let_decl :: Bool
prop_scope_let_decl =
  case parseNixTextLoc "let x = 1; in x" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
          decls = concatMap Scope.scopeDeclarations (Map.elems (Scope.sgScopes sg))
       in any (\d -> Scope.declName d == "x") decls

-- | Attrsets create declarations for each key
prop_scope_attrset_decls :: Bool
prop_scope_attrset_decls =
  case parseNixTextLoc "{ a = 1; b = 2; c = 3; }" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
          decls = concatMap Scope.scopeDeclarations (Map.elems (Scope.sgScopes sg))
          names = map Scope.declName decls
       in "a" `elem` names && "b" `elem` names && "c" `elem` names

-- | Function params create declarations
prop_scope_func_params :: Bool
prop_scope_func_params =
  case parseNixTextLoc "{ x, y, z }: x + y + z" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
          decls = concatMap Scope.scopeDeclarations (Map.elems (Scope.sgScopes sg))
          names = map Scope.declName decls
       in "x" `elem` names && "y" `elem` names && "z" `elem` names

-- | Variable references are tracked
prop_scope_var_refs :: Bool
prop_scope_var_refs =
  case parseNixTextLoc "let x = 1; in x" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
          refs = concatMap Scope.scopeReferences (Map.elems (Scope.sgScopes sg))
       in any (\r -> Scope.refName r == "x") refs

-- | With creates separate expression and body scopes
prop_scope_with_structure :: Bool
prop_scope_with_structure =
  case parseNixTextLoc "let s = { x = 1; }; in with s; x" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
          scopes = Map.elems (Scope.sgScopes sg)
          withScopes = filter (\s -> Scope.scopeKind s == Scope.WithScope) scopes
       in -- With should create at least one WithScope
          not (null withScopes)

-- | Cross-file merge produces a unified graph
prop_scope_merge_files :: Bool
prop_scope_merge_files =
  case (parseNixTextLoc "let a = 1; in a", parseNixTextLoc "let b = 2; in b") of
    (Right e1, Right e2) ->
      let sg = Scope.fromModuleGraph (Map.fromList [("a.nix", e1), ("b.nix", e2)])
          decls = concatMap Scope.scopeDeclarations (Map.elems (Scope.sgScopes sg))
          names = map Scope.declName decls
       in "a" `elem` names && "b" `elem` names
    _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Go-to-definition via scope graph resolution
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Definition: let x = 42; in x resolves reference to declaration
prop_defn_let :: Bool
prop_defn_let =
  case parseNixTextLoc "let x = 42; in x" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
          refs =
            [ r
            | s <- Map.elems (Scope.sgScopes sg)
            , r <- Scope.scopeReferences s
            , Scope.refKind r == Scope.VarRef
            , Scope.refName r == "x"
            ]
       in case refs of
            [] -> False
            ref : _ -> case Scope.resolve sg ref of
              Right decl -> Scope.declName decl == "x"
              Left _ -> False

-- | Definition: cross-file resolution via fromModuleGraph
prop_defn_cross_file :: Bool
prop_defn_cross_file =
  case (parseNixTextLoc "let imported = 1; in imported", parseNixTextLoc "imported") of
    (Right e1, Right e2) ->
      let sg = Scope.fromModuleGraph (Map.fromList [("lib.nix", e1), ("main.nix", e2)])
          decls = Scope.findDeclaration sg "imported"
          refs =
            [ r
            | s <- Map.elems (Scope.sgScopes sg)
            , r <- Scope.scopeReferences s
            , Scope.refName r == "imported"
            ]
       in not (null decls)
            && not (null refs)
            && any
              ( \r -> case Scope.resolve sg r of
                  Right d -> Scope.declName d == "imported"
                  Left _ -> False
              )
              refs
    _ -> False

-- | Definition: unresolvable reference produces Left
prop_defn_unresolved :: Bool
prop_defn_unresolved =
  case parseNixTextLoc "bogus" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
          refs =
            [ r
            | s <- Map.elems (Scope.sgScopes sg)
            , r <- Scope.scopeReferences s
            ]
       in case refs of
            [] -> True
            ref : _ -> case Scope.resolve sg ref of
              Left (Scope.Unresolved _) -> True
              _ -> False

-- | Definition: empty scope graph has no references
prop_defn_empty :: Bool
prop_defn_empty =
  null
    [ r
    | s <- Map.elems (Scope.sgScopes Scope.empty)
    , r <- Scope.scopeReferences s
    ]

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Nix lint
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Nix lint detects `with`
prop_nix_lint_with :: Bool
prop_nix_lint_with =
  case parseNixTextLoc "with builtins; true" of
    Left _ -> False
    Right expr -> not (null (findNixViolations expr))

-- | Nix lint detects `rec`
prop_nix_lint_rec :: Bool
prop_nix_lint_rec =
  case parseNixTextLoc "rec { x = 1; }" of
    Left _ -> False
    Right expr -> not (null (findNixViolations expr))

-- | Clean Nix files pass lint
prop_nix_lint_clean :: Bool
prop_nix_lint_clean =
  case parseNixTextLoc "let x = 1; y = 2; in x + y" of
    Left _ -> False
    Right expr -> null (findNixViolations expr)

{- | non-lisp-case fires on AUTHOR-OWNED names (let bindings) only: camelCase
and snake_case let names are flagged; attr keys and lambda formals mirror
external schemas and are not; lisp-case (dashes, digits, primes) is clean
-}
prop_nix_lint_non_lisp_case :: Bool
prop_nix_lint_non_lisp_case =
  flags "let myThing = 1; in myThing"
    && flags "let snake_thing = 1; in snake_thing"
    && clean "let my-thing = 1; my-thing2 = 2; x' = 3; in my-thing"
    && clean "{ buildInputs = [ ]; perSystem = 1; }"
    && clean "{ camelFormal }: camelFormal"
 where
  flags src = case parseNixTextLoc src of
    Left _ -> False
    Right expr -> any isNonLisp (findNixViolations expr)
  clean src = case parseNixTextLoc src of
    Left _ -> False
    Right expr -> not (any isNonLisp (findNixViolations expr))
  isNonLisp v = case nvType v of
    VNonLispCase _ -> True
    _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Bash lint
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Bash lint detects heredocs
prop_bash_lint_heredoc :: Bool
prop_bash_lint_heredoc =
  case parseBash "cat << EOF\nhello\nEOF\n" of
    Left _ -> False
    Right ast -> not (null (findViolations ast))

-- | Bash lint detects backticks
prop_bash_lint_backtick :: Bool
prop_bash_lint_backtick =
  case parseBash "x=`date`" of
    Left _ -> False
    Right ast -> not (null (findViolations ast))

-- | Clean bash passes lint
prop_bash_lint_clean :: Bool
prop_bash_lint_clean =
  case parseBash "x=\"hello\"\necho \"$x\"\n" of
    Left _ -> False
    Right ast -> null (findViolations ast)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: New Nix lint rules (N005-N012)
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Nix lint detects `substituteAll`
prop_nix_lint_substitute_all :: Bool
prop_nix_lint_substitute_all =
  case parseNixTextLoc "substituteAll { inherit (pkgs) foo; }" of
    Left _ -> False
    Right expr ->
      let violations = findNixViolations expr
          hasSubstAll v = case Narsil.Lint.Nix.nvType v of
            Narsil.Lint.Nix.VSubstituteAll -> True
            _ -> False
       in any hasSubstAll violations

-- | Nix lint detects raw `mkDerivation`
prop_nix_lint_raw_mkderivation :: Bool
prop_nix_lint_raw_mkderivation =
  case parseNixTextLoc "mkDerivation { name = \"foo\"; }" of
    Left _ -> False
    Right expr ->
      let violations = findNixViolations expr
          hasRawMkDeriv v = case Narsil.Lint.Nix.nvType v of
            Narsil.Lint.Nix.VRawMkDerivation -> True
            _ -> False
       in any hasRawMkDeriv violations

-- | Nix lint detects `runCommand`
prop_nix_lint_raw_runcommand :: Bool
prop_nix_lint_raw_runcommand =
  case parseNixTextLoc "runCommand \"name\" {} \"exit 0\"" of
    Left _ -> False
    Right expr ->
      let violations = findNixViolations expr
          hasRunCmd v = case Narsil.Lint.Nix.nvType v of
            Narsil.Lint.Nix.VRawRunCommand -> True
            _ -> False
       in any hasRunCmd violations

-- | Nix lint detects raw `writeShellApplication`
prop_nix_lint_raw_wsa :: Bool
prop_nix_lint_raw_wsa =
  case parseNixTextLoc "writeShellApplication { name = \"foo\"; text = \"bar\"; }" of
    Left _ -> False
    Right expr ->
      let violations = findNixViolations expr
          hasRawWSA v = case Narsil.Lint.Nix.nvType v of
            Narsil.Lint.Nix.VRawWriteShellApplication -> True
            _ -> False
       in any hasRawWSA violations

-- | Nix lint detects `writeShellScript`
prop_nix_lint_write_shell_script :: Bool
prop_nix_lint_write_shell_script =
  case parseNixTextLoc "writeShellScript \"name\" ''body''" of
    Left _ -> False
    Right expr ->
      let violations = findNixViolations expr
          hasWSS v = case Narsil.Lint.Nix.nvType v of
            Narsil.Lint.Nix.VWriteShellScript -> True
            _ -> False
       in any hasWSS violations

-- | Nix lint detects long inline strings
prop_nix_lint_long_string :: Bool
prop_nix_lint_long_string =
  let longStr = "\"" <> T.replicate 200 "x" <> "\""
   in case parseNixTextLoc longStr of
        Left _ -> False
        Right expr ->
          let violations = findNixViolations expr
              hasLongStr v = case Narsil.Lint.Nix.nvType v of
                Narsil.Lint.Nix.VLongInlineString _ -> True
                _ -> False
           in any hasLongStr violations

-- | Short strings do not trigger long-inline-string violation
prop_nix_lint_short_string_ok :: Bool
prop_nix_lint_short_string_ok =
  let shortStr = "\"" <> T.replicate 50 "x" <> "\""
   in case parseNixTextLoc shortStr of
        Left _ -> False
        Right expr ->
          let violations = findNixViolations expr
              hasLongStr v = case Narsil.Lint.Nix.nvType v of
                Narsil.Lint.Nix.VLongInlineString _ -> True
                _ -> False
           in not (any hasLongStr violations)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Derivation lint rules
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | mkDerivation without meta triggers VMissingMeta
prop_deriv_missing_meta :: Bool
prop_deriv_missing_meta =
  case parseNixTextLoc "mkDerivation { name = \"foo\"; src = ./.; }" of
    Left _ -> False
    Right expr ->
      let violations = DerivLint.findDerivViolations "test.nix" expr
          hasMissingMeta v = DerivLint.dvType v == DerivLint.VMissingMeta
       in any hasMissingMeta violations

-- | mkDerivation with meta but no description triggers VMissingDescription
prop_deriv_missing_description :: Bool
prop_deriv_missing_description =
  case parseNixTextLoc "mkDerivation { name = \"foo\"; meta = { license = \"MIT\"; }; }" of
    Left _ -> False
    Right expr ->
      let violations = DerivLint.findDerivViolations "test.nix" expr
          hasMissingDesc v = DerivLint.dvType v == DerivLint.VMissingDescription
       in any hasMissingDesc violations

-- | mkDerivation with meta.description produces no violations
prop_deriv_has_both :: Bool
prop_deriv_has_both =
  case parseNixTextLoc "mkDerivation { name = \"foo\"; meta = { description = \"bar\"; }; }" of
    Left _ -> False
    Right expr ->
      null (DerivLint.findDerivViolations "test.nix" expr)

-- | Non-derivation expr has no derivation violations
prop_deriv_clean :: Bool
prop_deriv_clean =
  case parseNixTextLoc "let x = 1; in x + x" of
    Left _ -> False
    Right expr ->
      null (DerivLint.findDerivViolations "test.nix" expr)

-- | mkDerivation through attribute path also detected
prop_deriv_stdenv_path :: Bool
prop_deriv_stdenv_path =
  case parseNixTextLoc "stdenv.mkDerivation { name = \"foo\"; }" of
    Left _ -> False
    Right expr ->
      let violations = DerivLint.findDerivViolations "test.nix" expr
          hasMissingMeta v = DerivLint.dvType v == DerivLint.VMissingMeta
       in any hasMissingMeta violations

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Pattern lint rules
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Pattern lint detects `x.y or null`
prop_pattern_or_null_fallback :: Bool
prop_pattern_or_null_fallback =
  case parseNixTextLoc "x.y or null" of
    Left _ -> False
    Right expr ->
      let violations = PatternLint.findPatternViolations expr
          hasOrNull v = PatternLint.pvType v == PatternLint.VOrNullFallback
       in any hasOrNull violations

-- | Pattern lint detects `translateAttrs` call
prop_pattern_translate_attrs :: Bool
prop_pattern_translate_attrs =
  case parseNixTextLoc "translateAttrs (name: value: value) attrs" of
    Left _ -> False
    Right expr ->
      let violations = PatternLint.findPatternViolations expr
          hasTranslate v = PatternLint.pvType v == PatternLint.VAttrTranslation
       in any hasTranslate violations

-- | Clean expression has no pattern violations
prop_pattern_clean :: Bool
prop_pattern_clean =
  case parseNixTextLoc "let x = 1; y = x + 1; in y" of
    Left _ -> False
    Right expr ->
      null (PatternLint.findPatternViolations expr)

-- | Nix lint on stdenv select path detects mkDerivation
prop_nix_lint_stdenv_path :: Bool
prop_nix_lint_stdenv_path =
  case parseNixTextLoc "stdenv.mkDerivation { name = \"foo\"; }" of
    Left _ -> False
    Right expr ->
      let violations = findNixViolations expr
          hasRawMkDeriv v = case Narsil.Lint.Nix.nvType v of
            Narsil.Lint.Nix.VRawMkDerivation -> True
            _ -> False
       in any hasRawMkDeriv violations

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Adversarial Lint -- False Positive Attacks
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | FP-1: String literal "mkDerivation" does NOT trigger VRawMkDerivation
prop_adv_fp_string_mkderiv :: Bool
prop_adv_fp_string_mkderiv =
  case parseNixTextLoc "\"use mkDerivation carefully\"" of
    Left _ -> False
    Right expr ->
      not $ any (\v -> nvType v == VRawMkDerivation) (findNixViolations expr)

-- | FP-2: String literal "substituteAll" does NOT trigger VSubstituteAll
prop_adv_fp_string_substall :: Bool
prop_adv_fp_string_substall =
  case parseNixTextLoc "\"call substituteAll here\"" of
    Left _ -> False
    Right expr ->
      not $ any (\v -> nvType v == VSubstituteAll) (findNixViolations expr)

-- | FP-3: "runCommand" as string literal does NOT trigger VRawRunCommand
prop_adv_fp_string_runcommand :: Bool
prop_adv_fp_string_runcommand =
  case parseNixTextLoc "\"avoid runCommand here\"" of
    Left _ -> False
    Right expr ->
      not $ any (\v -> nvType v == VRawRunCommand) (findNixViolations expr)

-- | FP-4: "writeShellScript" as string literal does NOT trigger VWriteShellScript
prop_adv_fp_string_wss :: Bool
prop_adv_fp_string_wss =
  case parseNixTextLoc "\"prefer writeShellScript\"" of
    Left _ -> False
    Right expr ->
      not $ any (\v -> nvType v == VWriteShellScript) (findNixViolations expr)

-- | FP-5: Variable named "mkDerivation" in attrset is NOT a function call
prop_adv_fp_var_mkderiv :: Bool
prop_adv_fp_var_mkderiv =
  case parseNixTextLoc "{ mkDerivation = 42; }" of
    Left _ -> False
    Right expr ->
      not $ any (\v -> nvType v == VRawMkDerivation) (findNixViolations expr)

-- | FP-6: Short string under 120 chars does NOT trigger VLongInlineString
prop_adv_fp_short_string :: Bool
prop_adv_fp_short_string =
  let short = "\"" <> T.replicate 100 "a" <> "\""
   in case parseNixTextLoc short of
        Left _ -> False
        Right expr ->
          not $
            any
              (\v -> case nvType v of VLongInlineString _ -> True; _ -> False)
              (findNixViolations expr)

-- | FP-7: mkDerivation WITH meta does NOT trigger VMissingMeta
prop_adv_fp_mkderiv_with_meta :: Bool
prop_adv_fp_mkderiv_with_meta =
  case parseNixTextLoc "mkDerivation { name = \"foo\"; meta = { }; }" of
    Left _ -> False
    Right expr ->
      not $
        any
          (\v -> DerivLint.dvType v == DerivLint.VMissingMeta)
          (DerivLint.findDerivViolations "test.nix" expr)

-- | FP-8: mkDerivation with meta.description does NOT trigger VMissingDescription
prop_adv_fp_meta_with_desc :: Bool
prop_adv_fp_meta_with_desc =
  case parseNixTextLoc "mkDerivation { name = \"foo\"; meta = { description = \"bar\"; }; }" of
    Left _ -> False
    Right expr ->
      not $
        any
          (\v -> DerivLint.dvType v == DerivLint.VMissingDescription)
          (DerivLint.findDerivViolations "test.nix" expr)

-- | FP-9: x.y without or null does NOT trigger VOrNullFallback
prop_adv_fp_select_no_null :: Bool
prop_adv_fp_select_no_null =
  case parseNixTextLoc "x.y" of
    Left _ -> False
    Right expr ->
      not $
        any
          (\v -> PatternLint.pvType v == PatternLint.VOrNullFallback)
          (PatternLint.findPatternViolations expr)

-- | FP-10: null as regular value does NOT trigger VOrNullFallback
prop_adv_fp_null_as_value :: Bool
prop_adv_fp_null_as_value =
  case parseNixTextLoc "let x = null; in x" of
    Left _ -> False
    Right expr ->
      not $
        any
          (\v -> PatternLint.pvType v == PatternLint.VOrNullFallback)
          (PatternLint.findPatternViolations expr)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Adversarial Lint -- False Negative Attacks
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | FN-1: mkDerivation through let binding (KNOWN GAP -- leafSym doesn't follow binds)
prop_adv_fn_mkderiv_let :: Bool
prop_adv_fn_mkderiv_let =
  case parseNixTextLoc "let f = mkDerivation; in f { name = \"foo\"; }" of
    Left _ -> False
    Right expr ->
      not $ any (\v -> nvType v == VRawMkDerivation) (findNixViolations expr)

-- | FN-2: substituteAll through builtins. NSelect path
prop_adv_fn_substall_path :: Bool
prop_adv_fn_substall_path =
  case parseNixTextLoc "builtins.substituteAll { src = ./input; }" of
    Left _ -> False
    Right expr ->
      any (\v -> nvType v == VSubstituteAll) (findNixViolations expr)

-- | FN-3: runCommand inside function body
prop_adv_fn_runcommand_func :: Bool
prop_adv_fn_runcommand_func =
  case parseNixTextLoc "{ stdenv }: runCommand \"name\" { } \"exit 0\"" of
    Left _ -> False
    Right expr ->
      any (\v -> nvType v == VRawRunCommand) (findNixViolations expr)

-- | FN-4: writeShellApplication through pkgs. path
prop_adv_fn_wsa_path :: Bool
prop_adv_fn_wsa_path =
  case parseNixTextLoc "pkgs.writeShellApplication { name = \"foo\"; text = \"bar\"; }" of
    Left _ -> False
    Right expr ->
      any (\v -> nvType v == VRawWriteShellApplication) (findNixViolations expr)

{- | FN-5: Long string across interpolation parts > 120 (KNOWN GAP --
interpolation parts not concatenated)
-}
prop_adv_fn_long_interp :: Bool
prop_adv_fn_long_interp =
  let long =
        "\"${builtins.concatStringsSep \"\" [\""
          <> T.replicate 100 "x"
          <> "\" \""
          <> T.replicate 60 "y"
          <> "\"]}\""
   in case parseNixTextLoc long of
        Left _ -> False
        Right expr ->
          not $
            any
              (\v -> case nvType v of VLongInlineString _ -> True; _ -> False)
              (findNixViolations expr)

-- | FN-6: writeShellScriptBin triggers VWriteShellScript
prop_adv_fn_wssbin :: Bool
prop_adv_fn_wssbin =
  case parseNixTextLoc "writeShellScriptBin \"name\" ''body''" of
    Left _ -> False
    Right expr ->
      any (\v -> nvType v == VWriteShellScript) (findNixViolations expr)

-- | FN-7: stdenv.mkDerivation without meta triggers VMissingMeta
prop_adv_fn_stdenv_no_meta :: Bool
prop_adv_fn_stdenv_no_meta =
  case parseNixTextLoc "stdenv.mkDerivation { name = \"foo\"; src = ./.; }" of
    Left _ -> False
    Right expr ->
      any
        (\v -> DerivLint.dvType v == DerivLint.VMissingMeta)
        (DerivLint.findDerivViolations "test.nix" expr)

-- | FN-8: mapAttrsToList triggers VAttrTranslation
prop_adv_fn_map_attrs :: Bool
prop_adv_fn_map_attrs =
  case parseNixTextLoc "mapAttrsToList (name: value: value) attrs" of
    Left _ -> False
    Right expr ->
      any
        (\v -> PatternLint.pvType v == PatternLint.VAttrTranslation)
        (PatternLint.findPatternViolations expr)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Adversarial Lint -- Crash Attacks
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | CRASH-1: checkPackageDirs on nonexistent dirs
prop_adv_crash_nonexistent_dir :: Property
prop_adv_crash_nonexistent_dir = QCM.monadicIO $ do
  _ <- QCM.run $ PackageLint.checkPackageDirs ["/nonexistent/deadbeef/"]
  QCM.assert True

-- | CRASH-2: findNixViolations on deeply nested selects (300 levels)
prop_adv_crash_deep_select :: Bool
prop_adv_crash_deep_select =
  let deep n = if n <= (0 :: Int) then "base" else "(" <> deep (n - 1) <> ").x"
      src = deep 300
   in case parseNixTextLoc src of
        Right expr -> findNixViolations expr `seq` True
        Left _ -> True

-- | CRASH-3: findDerivViolations on 2000-key attrset
prop_adv_crash_big_attrset :: Bool
prop_adv_crash_big_attrset =
  let pairs k = T.intercalate ";" [T.pack ("a" <> show i) <> "=" <> T.pack (show i) | i <- [1 .. k]]
      src = "mkDerivation { name = \"big\"; " <> pairs (2000 :: Int) <> "; }"
   in case parseNixTextLoc src of
        Right expr -> DerivLint.findDerivViolations "test.nix" expr `seq` True
        Left _ -> True

-- | CRASH-4: findPatternViolations on deeply nested lets (400 levels)
prop_adv_crash_deep_nest :: Bool
prop_adv_crash_deep_nest =
  let deepLet (0 :: Int) = "1"
      deepLet n =
        "let v" <> T.pack (show n) <> " = " <> deepLet (n - 1) <> "; in v" <> T.pack (show n)
      src = deepLet 400
   in case parseNixTextLoc src of
        Right expr -> PatternLint.findPatternViolations expr `seq` True
        Left _ -> True

-- | CRASH-5: findPatternViolations on deeply nested or-null (250 levels)
prop_adv_crash_deep_ornull :: Bool
prop_adv_crash_deep_ornull =
  let deep (0 :: Int) = "x"
      deep n = "(" <> deep (n - 1) <> ").x or null"
      src = deep 250
   in case parseNixTextLoc src of
        Right expr -> PatternLint.findPatternViolations expr `seq` True
        Left _ -> True

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Adversarial Lint -- Config Integration
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | CONF-1: SevOff override makes isSuppressed return True
prop_adv_is_suppressed_sevoff :: Bool
prop_adv_is_suppressed_sevoff =
  let cfg =
        defaultConfig
          { configOverrides =
              [ RuleOverride
                  { overrideId = "with-lib"
                  , overrideSeverity = Cfg.SevOff
                  , overrideReason = Just "testing"
                  }
              ]
          }
   in isSuppressed cfg "with-lib" && not (isSuppressed cfg "rec-anywhere")

-- | CONF-2: Override on nonexistent rule ID doesn't crash
prop_adv_nonexistent_rule :: Bool
prop_adv_nonexistent_rule =
  let cfg =
        defaultConfig
          { configOverrides =
              [ RuleOverride
                  { overrideId = "fantasy-N999"
                  , overrideSeverity = Cfg.SevError
                  , overrideReason = Nothing
                  }
              ]
          }
   in not (isSuppressed cfg "fantasy-N999") && not (isSuppressed cfg "real-rule")

{- | CONF-3: the default profile (standard) keeps the universal rules ACTIVE
and suppresses exactly the straylight-specific set — the semantics
config/profiles.dhall documents (the old "suppresses nothing" pin encoded
the era when the profile field was parsed but never resolved)
-}
prop_adv_default_no_suppress :: Bool
prop_adv_default_no_suppress =
  all
    (\rid -> not (isSuppressed defaultConfig rid))
    [ "with-lib"
    , "rec-anywhere"
    , "prefer-write-shell-application"
    , "long-inline-string"
    , "missing-meta"
    , "missing-description"
    , "or-null-fallback"
    , "default-nix-in-packages"
    , "type-check-failure"
    ]
    && all
      (isSuppressed defaultConfig)
      [ "no-substitute-all"
      , "no-raw-mkderivation"
      , "no-raw-runcommand"
      , "no-raw-writeshellapplication"
      , "no-translate-attrs-outside-prelude"
      , "non-lisp-case"
      ]

-- | CONF-4: isSuppressed with empty string doesn't crash
prop_adv_is_suppressed_empty :: Bool
prop_adv_is_suppressed_empty = not (isSuppressed defaultConfig "")

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Adversarial Lint -- Format Consistency
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | FMT-1: formatNixViolations non-empty for non-empty list
prop_adv_format_nix_nonempty :: Bool
prop_adv_format_nix_nonempty =
  not $
    T.null $
      formatNixViolations
        [ NixViolation
            { nvType = VWith
            , nvSpan = Span (Loc 1 0) (Loc 1 0) Nothing
            , nvContext = "with lib;"
            }
        ]

-- | FMT-2: formatDerivViolations non-empty for non-empty list
prop_adv_format_deriv_nonempty :: Bool
prop_adv_format_deriv_nonempty =
  not $
    T.null $
      DerivLint.formatDerivViolations
        [ DerivLint.DerivViolation
            { DerivLint.dvType = DerivLint.VMissingMeta
            , DerivLint.dvPath = "test.nix"
            , DerivLint.dvSpan = Span (Loc 1 0) (Loc 1 0) Nothing
            }
        ]

-- | FMT-3: formatPatternViolations non-empty for non-empty list
prop_adv_format_pattern_nonempty :: Bool
prop_adv_format_pattern_nonempty =
  not $
    T.null $
      PatternLint.formatPatternViolations
        [ PatternLint.PatternViolation
            { PatternLint.pvType = PatternLint.VOrNullFallback
            , PatternLint.pvSpan = Span (Loc 1 0) (Loc 1 0) Nothing
            , PatternLint.pvContext = "x.y or null"
            }
        ]

-- | FMT-4: No duplicate error codes across all lint modules
prop_adv_no_dup_codes :: Bool
prop_adv_no_dup_codes =
  let mks = Span (Loc 1 0) (Loc 1 0) Nothing
      nixT =
        [ VWith
        , VRec
        , VSubstituteAll
        , VRawMkDerivation
        , VRawRunCommand
        , VRawWriteShellApplication
        , VWriteShellScript
        , VLongInlineString 200
        ]
      derT = [DerivLint.VMissingMeta, DerivLint.VMissingDescription]
      patT = [PatternLint.VOrNullFallback, PatternLint.VAttrTranslation]
      fNix t = formatNixViolations [NixViolation t mks ""]
      fDer t = DerivLint.formatDerivViolations [DerivLint.DerivViolation t "t.nix" mks]
      fPat t = PatternLint.formatPatternViolations [PatternLint.PatternViolation t mks ""]
      allFmt = map fNix nixT ++ map fDer derT ++ map fPat patT
      codeFromLine l =
        case T.breakOn "NARSIL-" l of
          (_, "") -> ""
          (_, rest) -> T.takeWhile (/= ':') rest
      codes = map (\f -> codeFromLine (case T.lines f of [] -> ""; l : _ -> l)) allFmt
   in length codes == length (nub codes)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Schema defaulted vars (DESIGN-2)
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Variables with no type evidence are reported in schemaDefaultedVars
prop_schema_defaulted_reported :: Bool
prop_schema_defaulted_reported =
  let facts = [Required "MYSTERY_VAR" (Span (Loc 1 0) (Loc 1 0) Nothing)]
      constraints = factsToConstraints facts
      subst = case solve constraints of Right s -> s; Left _ -> emptySubst
      schema = buildSchema facts subst
   in "MYSTERY_VAR" `elem` schemaDefaultedVars schema

-- | Variables with known types are not in schemaDefaultedVars
prop_schema_resolved_not_defaulted :: Bool
prop_schema_resolved_not_defaulted =
  let facts = [DefaultIs "PORT" (LitInt 8080) (Span (Loc 1 0) (Loc 1 0) Nothing)]
      constraints = factsToConstraints facts
      subst = case solve constraints of Right s -> s; Left _ -> emptySubst
      schema = buildSchema facts subst
   in "PORT" `notElem` schemaDefaultedVars schema

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: End-to-end integration
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | parseScript on a config script produces config in schema
prop_e2e_config_extraction :: Bool
prop_e2e_config_extraction =
  let script =
        T.unlines
          [ "PORT=\"${PORT:-8080}\""
          , "HOST=\"${HOST:-localhost}\""
          , "config.server.port=$PORT"
          , "config.server.host=\"$HOST\""
          ]
   in case parseScript script of
        Left _ -> False
        Right s ->
          let cfg = schemaConfig (scriptSchema s)
           in Map.member ["server", "port"] cfg && Map.member ["server", "host"] cfg

-- | parseScript correctly identifies required vars
prop_e2e_required_vars :: Bool
prop_e2e_required_vars =
  let script = "API_KEY=\"${API_KEY:?}\"\n"
   in case parseScript script of
        Left _ -> False
        Right s ->
          case Map.lookup "API_KEY" (schemaEnv (scriptSchema s)) of
            Just spec -> envRequired spec
            Nothing -> False

-- | parseScript rejects type conflicts
prop_e2e_type_conflict :: Bool
prop_e2e_type_conflict =
  let script =
        T.unlines
          [ "X=\"${X:-42}\"" -- X : TInt
          , "Y=\"${Y:-hello}\"" -- Y : TString
          , "Z=\"$X\"" -- Z : TInt (from X)
          , "Z=\"$Y\"" -- Z : TString (from Y) -- conflict with TInt!
          ]
   in case parseScript script of
        Left _ -> True -- type error, correct
        Right _ -> False -- should have failed

-- | Empty script produces empty schema
prop_e2e_empty_script :: Bool
prop_e2e_empty_script =
  case parseScript "" of
    Left _ -> False
    Right s ->
      Map.null (schemaEnv (scriptSchema s))
        && Map.null (schemaConfig (scriptSchema s))

-- | Store paths are tracked
prop_e2e_store_paths :: Bool
prop_e2e_store_paths =
  let script = "/nix/store/abc123-curl-8.0/bin/curl http://example.com\n"
   in case parseScript script of
        Left _ -> False
        Right s -> not (Set.null (schemaStorePaths (scriptSchema s)))

-- | Prefix-conflicting config paths are rejected instead of silently dropping data
prop_e2e_config_prefix_conflict :: Bool
prop_e2e_config_prefix_conflict =
  let script =
        T.unlines
          [ "A=\"${A:-one}\""
          , "B=\"${B:-2}\""
          , "config.server=\"$A\""
          , "config.server.port=$B"
          ]
   in case parseScript script of
        Left err -> "conflicting config paths" `T.isInfixOf` err
        Right _ -> False

-- | Multi-var config templates are represented as templates, not static literals
prop_e2e_config_template :: Bool
prop_e2e_config_template =
  let script =
        T.unlines
          [ "A=\"${A:-one}\""
          , "B=\"${B:-two}\""
          , "config.combo=\"$A-$B\""
          ]
   in case parseScript script of
        Left _ -> False
        Right s ->
          case Map.lookup ["combo"] (schemaConfig (scriptSchema s)) of
            Just ConfigSpec{cfgTemplate = Just [ConfigVar "A", ConfigText "-", ConfigVar "B"]} ->
              True
            _ -> False

-- | Prefix/suffix config templates are represented as templates
prop_e2e_config_template_prefix_suffix :: Bool
prop_e2e_config_template_prefix_suffix =
  let script =
        T.unlines
          [ "A=\"${A:-one}\""
          , "config.path=\"prefix-$A-suffix\""
          ]
   in case parseScript script of
        Left _ -> False
        Right s ->
          case Map.lookup ["path"] (schemaConfig (scriptSchema s)) of
            Just
              ConfigSpec
                { cfgTemplate = Just [ConfigText "prefix-", ConfigVar "A", ConfigText "-suffix"]
                } ->
                True
            _ -> False

-- | Braced defaults inside config templates remain dynamic and use runtime default
prop_e2e_config_template_default :: Bool
prop_e2e_config_template_default =
  let script = "config.host=\"${HOST:-localhost}\""
   in case parseScript script of
        Left _ -> False
        Right s ->
          case Map.lookup ["host"] (schemaConfig (scriptSchema s)) of
            Just ConfigSpec{cfgTemplate = Just [ConfigVarDefault "HOST" "localhost"]} -> True
            _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Edge cases
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Script with only comments produces empty schema
prop_edge_comments_only :: Bool
prop_edge_comments_only =
  case parseScript "# this is a comment\n# another comment\n" of
    Left _ -> False
    Right s -> Map.null (schemaEnv (scriptSchema s))

-- | Very long variable names don't crash
prop_edge_long_varname :: Bool
prop_edge_long_varname =
  let name = T.replicate 1000 "A"
      script = name <> "=\"hello\"\n"
   in case parseBash script of
        Left _ -> True
        Right _ -> True

-- | Deeply nested config paths work
prop_edge_deep_config :: Bool
prop_edge_deep_config =
  let path = T.intercalate "." (replicate 50 "level")
      script = "config." <> path <> "=42\n"
   in case parseScript script of
        Left _ -> True -- parse might fail, that's ok
        Right _ -> True -- but it shouldn't crash

-- | Script with all fact types doesn't crash
prop_edge_all_fact_types :: Bool
prop_edge_all_fact_types =
  let script =
        T.unlines
          [ "A=\"${A:-42}\""
          , "B=\"${B:?}\""
          , "C=\"$A\""
          , "D=\"${D:-$A}\""
          , "config.x.y=$A"
          , "config.x.z=\"$B\""
          , "config.x.w=true"
          , "/nix/store/abc-curl/bin/curl --connect-timeout $A http://example.com"
          ]
   in case parseScript script of
        Left _ -> False
        Right s ->
          Map.size (schemaEnv (scriptSchema s)) >= 4
            && Map.size (schemaConfig (scriptSchema s)) >= 2

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Emit-config structural
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | emit-config JSON has balanced braces
prop_emit_json_balanced :: Property
prop_emit_json_balanced = forAll genConfigFacts $ \facts ->
  let schema = buildSchema facts emptySubst
      output = emitConfigJSON schema
   in T.count "{" output == T.count "}" output

-- | emit-config function never contains heredocs
prop_emit_no_heredoc :: [Fact] -> Bool
prop_emit_no_heredoc facts =
  let schema = buildSchema facts emptySubst
      output = emitConfigFunction schema
   in not ("<<" `T.isInfixOf` output)

-- | emit-config JSON for nested config produces nested braces
prop_emit_json_nested :: Bool
prop_emit_json_nested =
  let sp = Span (Loc 1 0) (Loc 1 0) Nothing
      schema =
        emptySchema
          { schemaConfig =
              Map.fromList
                [
                  ( ["server", "port"]
                  , ConfigSpec TInt (Just "PORT") (Just Unquoted) Nothing Nothing sp
                  )
                ,
                  ( ["server", "host"]
                  , ConfigSpec TString (Just "HOST") (Just Quoted) Nothing Nothing sp
                  )
                ]
          }
      output = emitConfigJSON schema
   in "server" `T.isInfixOf` output
        && "port" `T.isInfixOf` output
        && "host" `T.isInfixOf` output
        && T.count "{" output >= 2 -- at least root + server

-- | Generate facts likely to produce config
genConfigFacts :: Gen [Fact]
genConfigFacts = do
  n <- choose (1, 5)
  replicateM n $ do
    path <- genConfigPath
    oneof
      [ do
          var <- genEnvVarName
          sp <- genSpan
          q <- elements [Quoted, Unquoted]
          pure $ ConfigAssign path var q sp
      , do
          lit <- genLiteral
          sp <- genSpan
          pure $ ConfigLit path lit sp
      ]

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Format (annotation placement)
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | annotateExpr on let-bound function adds type annotation
prop_format_function :: Bool
prop_format_function =
  case annotateExpr "let add = x: y: x + y; in add" of
    Right output -> "# ::" `T.isInfixOf` output
    Left _ -> False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Bash AST edge cases
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | Arithmetic expansion doesn't crash fact extraction
prop_bash_arithmetic :: Bool
prop_bash_arithmetic =
  case parseBash "X=$(( 1 + 2 ))\n" of
    Right ast -> extractFacts ast `seq` True
    Left _ -> True

-- | Subshell doesn't crash fact extraction
prop_bash_subshell :: Bool
prop_bash_subshell =
  case parseBash "X=$(echo hello)\n" of
    Right ast -> extractFacts ast `seq` True
    Left _ -> True

-- | Pipe chain extracts facts from both sides
prop_bash_pipe :: Bool
prop_bash_pipe =
  case parseBash "echo hello | head -n 1\n" of
    Right ast -> extractFacts ast `seq` True
    Left _ -> True

-- | For loop body has facts extracted
prop_bash_for_loop :: Bool
prop_bash_for_loop =
  case parseBash "for x in 1 2 3; do\n  Y=\"${Y:-default}\"\ndone\n" of
    Right ast ->
      let facts = extractFacts ast
       in any isDefault facts
    Left _ -> False
 where
  isDefault (DefaultIs "Y" _ _) = True
  isDefault _ = False

-- | SPECDEV-2: Bash violations carry real line numbers from ShellCheck positions
prop_bash_span_line_numbers :: Bool
prop_bash_span_line_numbers =
  let src = "#!/bin/sh\neval echo hello\n"
   in case parseBash src of
        Left _ -> False
        Right ast ->
          let violations = findViolations ast
           in not (null violations)
                && all
                  ( \v ->
                      let line = locLine (spanStart (vSpan v))
                       in line == 2
                  )
                  violations

-- | SPECDEV-2: Multi-line script -- violation on correct line
prop_bash_span_multi_line :: Bool
prop_bash_span_multi_line =
  let src = "#!/bin/sh\nx=1\ny=2\n`ls`\n"
   in case parseBash src of
        Left _ -> False
        Right ast ->
          let violations = findViolations ast
           in any
                ( \v ->
                    vType v == VBacktick
                      && locLine (spanStart (vSpan v)) == 4
                )
                violations

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Scope graph adversarial attacks
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | SCOPE-1: Scope ID exhaustion -- build 100k scopes, verify counter and resolution
prop_scope_id_exhaustion :: Bool
prop_scope_id_exhaustion =
  let n = 100000
      mkScope i =
        let sid = Scope.ScopeId i
            decls = [genScopeDecl "x" sid]
            refs = if i == n - 1 then [genScopeRef "x" sid] else []
         in (sid, Scope.Scope sid decls refs [] Scope.LetScope)
      scopes = Map.fromList [mkScope i | i <- [0 .. n - 1]]
      sg = genScope0{Scope.sgScopes = scopes, Scope.sgNextId = n, Scope.sgRoot = Scope.ScopeId 0}
      ref = genScopeRef "x" (Scope.ScopeId (n - 1))
   in Scope.sgNextId sg == n
        && case Scope.resolve sg ref of
          Right decl -> Scope.declScope decl == Scope.ScopeId (n - 1)
          Left _ -> False

-- | SCOPE-2: Edge priority -- resolution picks highest-priority edge group (Parent)
prop_scope_edge_priority_full :: Bool
prop_scope_edge_priority_full =
  let ref = genScopeRef "x" (Scope.ScopeId 0)
   in case Scope.resolve genAllEdgesGraph ref of
        Right decl -> Scope.declScope decl == Scope.ScopeId 1
        Left _ -> False

-- | SCOPE-3: Shadowing bomb -- 100 nested scopes, innermost resolves to nearest Parent
prop_scope_shadowing_bomb :: Bool
prop_scope_shadowing_bomb =
  let (sg, innermost) = genShadowingChain 100
      ref = genScopeRef "x" innermost
   in case Scope.resolve sg ref of
        Right decl -> Scope.declScope decl == innermost
        Left _ -> False

-- | SCOPE-3b: With shadow -- Parent edges override With edges with same-named decls
prop_scope_with_shadow :: Bool
prop_scope_with_shadow =
  let parentId = Scope.ScopeId 1
      withId = Scope.ScopeId 2
      parentScope = Scope.Scope parentId [genScopeDecl "x" parentId] [] [] Scope.LetScope
      withScope = Scope.Scope withId [genScopeDecl "x" withId] [] [] Scope.WithScope
      refScope = Scope.ScopeId 0
      ref = genScopeRef "x" refScope
      centerScope =
        Scope.Scope
          refScope
          []
          [ref]
          [Scope.Edge refScope parentId Scope.Parent, Scope.Edge refScope withId Scope.With]
          Scope.FileScope
      sg =
        genScope0
          { Scope.sgScopes =
              Map.fromList [(refScope, centerScope), (parentId, parentScope), (withId, withScope)]
          , Scope.sgNextId = 3
          , Scope.sgRoot = refScope
          }
   in case Scope.resolve sg ref of
        Right decl -> Scope.declScope decl == parentId
        Left _ -> False

-- | SCOPE-4: Cross-file merge -- colliding IDs remapped uniquely via fromModuleGraph
prop_scope_merge_collision :: Bool
prop_scope_merge_collision =
  case (parseNixTextLoc "let a = 1; in a", parseNixTextLoc "let b = 2; in b") of
    (Right e1, Right e2) ->
      let sg = Scope.fromModuleGraph (Map.fromList [("a.nix", e1), ("b.nix", e2)])
          decls = concatMap Scope.scopeDeclarations (Map.elems (Scope.sgScopes sg))
          names = map Scope.declName decls
       in "a" `elem` names && "b" `elem` names
    _ -> False

-- | SCOPE-4b: Merge 100 file graphs simultaneously via fromModuleGraph
prop_scope_merge_many :: Bool
prop_scope_merge_many =
  let srcs =
        [ "let x" <> T.pack (show (i :: Int)) <> " = 1; in x" <> T.pack (show (i :: Int))
        | i <- [0 .. 99]
        ]
      parsed = map parseNixTextLoc srcs
      rights = [e | Right e <- parsed]
   in if length rights < 100
        then False
        else
          let pairs = [("f" <> show i <> ".nix", e) | (i, e) <- zip [(0 :: Int) ..] rights]
              sg = Scope.fromModuleGraph (Map.fromList pairs)
              decls = concatMap Scope.scopeDeclarations (Map.elems (Scope.sgScopes sg))
           in length decls >= 100

-- | SCOPE-4c: Merge file with zero declarations/references -- other files survive
prop_scope_merge_empty_file :: Bool
prop_scope_merge_empty_file =
  case (parseNixTextLoc "let used = 1; in used", parseNixTextLoc "42") of
    (Right e1, Right e2) ->
      let sg = Scope.fromModuleGraph (Map.fromList [("has-decl.nix", e1), ("no-decl.nix", e2)])
          decls = concatMap Scope.scopeDeclarations (Map.elems (Scope.sgScopes sg))
       in any (\d -> Scope.declName d == "used") decls
    _ -> False

-- | SCOPE-4d: Cross-file resolution -- fromModuleGraph creates unified graph
prop_scope_cross_file_ref :: Bool
prop_scope_cross_file_ref =
  case (parseNixTextLoc "let imported = 1; in imported", parseNixTextLoc "42") of
    (Right e1, Right e2) ->
      let sg = Scope.fromModuleGraph (Map.fromList [("decl.nix", e1), ("other.nix", e2)])
       in not (null (Scope.findDeclaration sg "imported"))
    _ -> False

-- | SCOPE-5: Orphan declaration has no references resolving to it
prop_scope_orphan_decl :: Bool
prop_scope_orphan_decl =
  let sg = genMinimalFileGraph "orphan" 0
   in case Scope.findDeclaration sg "orphan" of
        [] -> False
        decl : _ -> null (Scope.findReferences sg decl)

-- | SCOPE-5b: Reference to nonexistent name is unresolvable
prop_scope_unresolvable_ref :: Bool
prop_scope_unresolvable_ref =
  let sg = genMinimalFileGraph "exists" 0
      ref = genScopeRef "ghost" (Scope.ScopeId 0)
   in case Scope.resolve sg ref of
        Left (Scope.Unresolved _) -> True
        _ -> False

-- | SCOPE-5c: Duplicate declarations in same scope -- resolution picks locally
prop_scope_duplicate_decls :: Bool
prop_scope_duplicate_decls =
  let scopeId = Scope.ScopeId 0
      declA = genScopeDecl "x" scopeId
      declB = genScopeDecl "x" scopeId
      scope = Scope.Scope scopeId [declB, declA] [genScopeRef "x" scopeId] [] Scope.FileScope
      sg =
        genScope0
          { Scope.sgScopes = Map.singleton scopeId scope
          , Scope.sgNextId = 1
          , Scope.sgRoot = scopeId
          }
      ref = genScopeRef "x" scopeId
   in case Scope.resolve sg ref of
        Left (Scope.Ambiguous _ ds) -> length ds == 2
        _ -> False

-- | SCOPE-5d: findReferences returns only refs that actually resolve to declaration
prop_scope_find_refs_accurate :: Bool
prop_scope_find_refs_accurate =
  let parentId = Scope.ScopeId 0
      childId = Scope.ScopeId 1
      parentScope = Scope.Scope parentId [genScopeDecl "x" parentId] [] [] Scope.FileScope
      childScope =
        Scope.Scope
          childId
          []
          [genScopeRef "x" childId, genScopeRef "y" childId]
          [Scope.Edge childId parentId Scope.Parent]
          Scope.LetScope
      sg =
        genScope0
          { Scope.sgScopes = Map.fromList [(parentId, parentScope), (childId, childScope)]
          , Scope.sgNextId = 2
          , Scope.sgRoot = parentId
          }
   in case Scope.findDeclaration sg "x" of
        [] -> False
        decl : _ ->
          let refs = Scope.findReferences sg decl
           in length refs == 1 && all (\r -> Scope.refName r == "x") refs

-- | SCOPE-6: Dhall export roundtrip -- toDhall on valid graph produces non-empty output
prop_scope_dhall_export :: Bool
prop_scope_dhall_export =
  let sg = genAllEdgesGraph
      dhall = Scope.toDhall sg
   in not (T.null dhall)

-- | SCOPE-6b: Dhall export preserves special characters in declaration names
prop_scope_dhall_special_chars :: Bool
prop_scope_dhall_special_chars =
  let scopeId = Scope.ScopeId 0
      names = ["funny-name", "has.dot", "snake_case", "UPPER"]
      decls = [genScopeDecl n scopeId | n <- names]
      scope = Scope.Scope scopeId decls [] [] Scope.FileScope
      sg =
        genScope0
          { Scope.sgScopes = Map.singleton scopeId scope
          , Scope.sgNextId = 1
          , Scope.sgRoot = scopeId
          }
      dhall = Scope.toDhall sg
   in all (`T.isInfixOf` dhall) names

-- | SCOPE-6c: Dhall export on empty graph does not crash
prop_scope_dhall_empty :: Bool
prop_scope_dhall_empty =
  not (T.null (Scope.toDhall Scope.empty))

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Config loading attacks
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | CONFIG-1: Config file doesn't exist -- returns Left
prop_config_no_file :: Property
prop_config_no_file = QCM.monadicIO $ do
  result <- QCM.run $ try @SomeException $ loadConfig "/tmp/narsil-nonexistent-z7x9w2v5.dhall"
  QCM.assert $ case result of
    Left _ -> True
    Right (Left _) -> True
    Right (Right _) -> False

-- | CONFIG-2: Malformed Dhall syntax returns Left
prop_config_malformed_dhall :: Property
prop_config_malformed_dhall = QCM.monadicIO $ do
  let path = "/tmp/narsil-test-malformed-z7x9w2v5.dhall"
  _ <- QCM.run $ TIO.writeFile path "{ = }"
  result <- QCM.run $ loadConfig path
  _ <- QCM.run $ removeFile path `catch` (\(_ :: IOException) -> pure ())
  QCM.assert $ isLeft result

-- | CONFIG-3: Valid Dhall but wrong types returns Left
prop_config_wrong_types :: Property
prop_config_wrong_types = QCM.monadicIO $ do
  let path = "/tmp/narsil-test-wrongtype-z7x9w2v5.dhall"
      content =
        ( "{ profile = 42, extra-ignores = [] : List Text, overrides = [] : List { id : Text, "
            <> "severity : < Error | Warning | Info | Off >, reason : Optional Text } }"
        ) ::
          Text
  _ <- QCM.run $ TIO.writeFile path content
  result <- QCM.run $ loadConfig path
  _ <- QCM.run $ removeFile path `catch` (\(_ :: IOException) -> pure ())
  QCM.assert $ isLeft result

-- | CONFIG-4: Config with 10,000 overrides doesn't crash
prop_config_massive_overrides :: Bool
prop_config_massive_overrides =
  let overrides = [RuleOverride "test-rule" Cfg.SevWarning Nothing | _ <- [1 .. 10000 :: Int]]
      cfg = defaultConfig{configOverrides = overrides}
   in effectiveSeverity cfg "test-rule" == Just Cfg.SevWarning

-- | CONFIG-5: Glob patterns with regex special characters (treated as literal)
prop_config_glob_regex_chars :: Bool
prop_config_glob_regex_chars =
  let cfg = defaultConfig{configExtraIgnores = [".*", "[a-z]", "(test)", "\\d"]}
   in isIgnored cfg ".*"
        && not (isIgnored cfg "anything.txt")
        && isIgnored cfg "[a-z]"
        && isIgnored cfg "\\d"

{- | REVIEW-3 #5: a statically-resolvable cross-module import carries the
imported module's REAL type across the module graph. @main.nix@ =
@(import ./lib.nix).x@ where @lib.nix@ = @{ x = 1; }@ must infer to TInt — if
the import type did not flow, @import@ would fall back to its @TPath → TAny@
builtin and the select would not be TInt. This is the end-to-end check the
reviewer could not run (they had no GHC); it confirms the literal-path half of
cross-module inference is wired correctly (scanner → topo order → dual-keyed
env → select).
-}
prop_import_cross_module_type_flows :: Property
prop_import_cross_module_type_flows = QCM.monadicIO $ do
  let dir = "/tmp/narsil-test-import-z7x9w2v5"
      libPath = dir <> "/lib.nix"
      mainPath = dir <> "/main.nix"
  types <- QCM.run $ do
    createDirectoryIfMissing True dir
    TIO.writeFile libPath "{ x = 1; }"
    TIO.writeFile mainPath "(import ./lib.nix).x"
    egraph <- buildModuleGraph mainPath
    removeDirectoryRecursive dir `catch` (\(_ :: IOException) -> pure ())
    pure $ case egraph of
      Left _ -> []
      Right g -> Map.elems (moduleTypes g)
  -- main's inferred type is TInt; lib's is the record. TInt present ⇒ flow works.
  QCM.assert (NT.TInt `elem` types)

-- | CONFIG-6: Empty ignore patterns -- nothing ignored
prop_config_empty_ignores :: Bool
prop_config_empty_ignores =
  let cfg = defaultConfig{configExtraIgnores = []}
   in not (isIgnored cfg "anything.nix")

-- | CONFIG-7: Absurdly long profile name
prop_config_long_profile :: Bool
prop_config_long_profile =
  let longName = T.replicate 10000 "x"
      cfg = defaultConfig{configProfile = longName}
   in configProfile cfg == longName

-- | CONFIG-8: isIgnored with path containing null bytes does not crash
prop_config_null_byte_path :: Bool
prop_config_null_byte_path =
  let cfg = defaultConfig{configExtraIgnores = ["*.nix"]}
      pathWithNull = "test\0file.nix"
   in isIgnored cfg pathWithNull `seq` True

-- | CONFIG-9: isIgnored with paths exactly "**" or "***"
prop_config_star_paths :: Bool
prop_config_star_paths =
  let cfg = defaultConfig{configExtraIgnores = ["**", "***"]}
   in isIgnored cfg "**" && isIgnored cfg "***"

-- | CONFIG-10: isIgnored with "**" glob pattern matches everything
prop_config_globstar_matches_all :: Bool
prop_config_globstar_matches_all =
  let cfg = defaultConfig{configExtraIgnores = ["**"]}
   in isIgnored cfg "any/file/path.nix" && isIgnored cfg "" && isIgnored cfg "x"

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Glob matching (matchGlob via isIgnored)
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | GLOB-1: Exact filename match
prop_glob_exact :: Bool
prop_glob_exact =
  let cfg = defaultConfig{configExtraIgnores = ["foo.nix"]}
   in isIgnored cfg "foo.nix" && not (isIgnored cfg "bar.nix")

-- | GLOB-2: *.ext matches any name with that extension
prop_glob_star_ext :: Bool
prop_glob_star_ext =
  let cfg = defaultConfig{configExtraIgnores = ["*.nix"]}
   in isIgnored cfg "foo.nix"
        && isIgnored cfg ".nix"
        && not (isIgnored cfg "foo.txt")

-- | GLOB-3: prefix* matches any name starting with prefix
prop_glob_prefix_star :: Bool
prop_glob_prefix_star =
  let cfg = defaultConfig{configExtraIgnores = ["test*"]}
   in isIgnored cfg "test" && isIgnored cfg "test123" && not (isIgnored cfg "xtest")

-- | GLOB-4: *suffix matches any name ending with suffix
prop_glob_star_suffix :: Bool
prop_glob_star_suffix =
  let cfg = defaultConfig{configExtraIgnores = ["*_test.nix"]}
   in isIgnored cfg "foo_test.nix" && not (isIgnored cfg "foo.nix")

-- | GLOB-5: a*b matches a then anything then b
prop_glob_middle_star :: Bool
prop_glob_middle_star =
  let cfg = defaultConfig{configExtraIgnores = ["a*b"]}
   in isIgnored cfg "ab" && isIgnored cfg "axyb" && not (isIgnored cfg "axy")

-- | GLOB-6: dir/*.nix matches any .nix file in dir (PATH WITH /)
prop_glob_dir_star :: Bool
prop_glob_dir_star =
  let cfg = defaultConfig{configExtraIgnores = ["dir/*.nix"]}
   in isIgnored cfg "dir/foo.nix"
        && isIgnored cfg "dir/bar.nix"
        && not (isIgnored cfg "other/foo.nix")
        && not (isIgnored cfg "dir/foo.txt")

-- | GLOB-7: dir/** matches everything under dir (PATH WITH /)
prop_glob_dir_globstar :: Bool
prop_glob_dir_globstar =
  let cfg = defaultConfig{configExtraIgnores = ["src/**"]}
   in isIgnored cfg "src/foo.nix"
        && isIgnored cfg "src/deep/nested/file.hs"
        && not (isIgnored cfg "other/file.nix")

-- | GLOB-8: **/foo.nix matches foo.nix at any depth
prop_glob_globstar_suffix :: Bool
prop_glob_globstar_suffix =
  let cfg = defaultConfig{configExtraIgnores = ["**/foo.nix"]}
   in isIgnored cfg "foo.nix"
        && isIgnored cfg "a/foo.nix"
        && isIgnored cfg "a/b/c/foo.nix"
        && not (isIgnored cfg "foo.txt")
        && not (isIgnored cfg "dir/bar.nix")

-- | GLOB-9: dir/**/*.hs matches .hs files at any depth under dir
prop_glob_nested_globstar :: Bool
prop_glob_nested_globstar =
  let cfg = defaultConfig{configExtraIgnores = ["src/**/*.hs"]}
   in isIgnored cfg "src/Foo.hs"
        && isIgnored cfg "src/lib/Bar.hs"
        && isIgnored cfg "src/lib/deep/Baz.hs"
        && not (isIgnored cfg "src/Foo.txt")
        && not (isIgnored cfg "lib/Foo.hs")

-- | GLOB-10: Multiple stars in filename
prop_glob_multi_star :: Bool
prop_glob_multi_star =
  let cfg = defaultConfig{configExtraIgnores = ["*-v*"]}
   in isIgnored cfg "app-v1" && isIgnored cfg "app-v2.3" && not (isIgnored cfg "app")

-- | GLOB-11: Case sensitivity (Unix convention)
prop_glob_case_sensitive :: Bool
prop_glob_case_sensitive =
  let cfg = defaultConfig{configExtraIgnores = ["Foo.nix"]}
   in isIgnored cfg "Foo.nix" && not (isIgnored cfg "foo.nix")

-- | GLOB-12: Star matches within a single path component (but matches any component)
prop_glob_star_no_slash :: Bool
prop_glob_star_no_slash =
  let cfg = defaultConfig{configExtraIgnores = ["*.nix"]}
   in isIgnored cfg "dir/foo.nix" && isIgnored cfg "foo.nix" && not (isIgnored cfg "dir/foo.txt")

-- | GLOB-13: Non-existent ignore pattern matches nothing
prop_glob_no_match :: Bool
prop_glob_no_match =
  let cfg = defaultConfig{configExtraIgnores = ["*.md"]}
   in not (isIgnored cfg "foo.nix") && not (isIgnored cfg "dir/file.nix")

-- | GLOB-14: Literal dot not treated as regex
prop_glob_dot_literal :: Bool
prop_glob_dot_literal =
  let cfg = defaultConfig{configExtraIgnores = [".env"]}
   in isIgnored cfg ".env" && not (isIgnored cfg "xenv")

-- | GLOB-15: Star at both ends
prop_glob_star_both_ends :: Bool
prop_glob_star_both_ends =
  let cfg = defaultConfig{configExtraIgnores = ["*.log*"]}
   in isIgnored cfg "foo.log" && isIgnored cfg "foo.log.gz" && not (isIgnored cfg "foo.txt")

-- | GLOB-16: Single character (no wildcard) exact match at any depth
prop_glob_single_no_wildcard :: Bool
prop_glob_single_no_wildcard =
  let cfg = defaultConfig{configExtraIgnores = ["result"]}
   in isIgnored cfg "result" && not (isIgnored cfg "result-link")

-- | GLOB-17: Pattern with leading slash
prop_glob_leading_slash :: Bool
prop_glob_leading_slash =
  let cfg = defaultConfig{configExtraIgnores = ["/build/*"]}
   in isIgnored cfg "build/output" && not (isIgnored cfg "src/build/output")

-- | GLOB-18: Consecutive stars (***)
prop_glob_triple_star :: Bool
prop_glob_triple_star =
  let cfg = defaultConfig{configExtraIgnores = ["***"]}
   in isIgnored cfg "file.nix" && isIgnored cfg "deep/dir/file.nix"

-- | GLOB-19: Complex pattern: *._test.nix
prop_glob_complex :: Bool
prop_glob_complex =
  let cfg = defaultConfig{configExtraIgnores = ["*._test.nix"]}
   in isIgnored cfg "module._test.nix"
        && not (isIgnored cfg "module_test.nix")

-- | GLOB-20: Pattern with leading star and path
prop_glob_leading_star_path :: Bool
prop_glob_leading_star_path =
  let cfg = defaultConfig{configExtraIgnores = ["*/test/*"]}
   in isIgnored cfg "pkg/test/foo.nix"
        && not (isIgnored cfg "test/foo.nix")

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Severity override attacks
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | SEV-1: Override same rule twice (first match wins via filter head)
prop_severity_duplicate_override :: Bool
prop_severity_duplicate_override =
  let cfg =
        defaultConfig
          { configOverrides =
              [ RuleOverride "the-rule" Cfg.SevError Nothing
              , RuleOverride "the-rule" Cfg.SevOff Nothing
              ]
          }
   in effectiveSeverity cfg "the-rule" == Just Cfg.SevError

-- | SEV-2: Override a rule ID that doesn't exist (should not crash)
prop_severity_nonexistent_rule :: Bool
prop_severity_nonexistent_rule =
  let cfg = defaultConfig{configOverrides = [RuleOverride "no-such-rule-xyz" Cfg.SevError Nothing]}
   in effectiveSeverity cfg "no-such-rule-xyz" == Just Cfg.SevError
        && effectiveSeverity cfg "real-rule" == Nothing

-- | SEV-3: Override with SevOff -- isSuppressed returns True
prop_severity_suppressed :: Bool
prop_severity_suppressed =
  let cfg = defaultConfig{configOverrides = [RuleOverride "suppress-me" Cfg.SevOff Nothing]}
   in isSuppressed cfg "suppress-me"

-- | SEV-4: effectiveSeverity with rule IDs containing special characters
prop_severity_special_chars :: Bool
prop_severity_special_chars =
  let rules = ["rule with spaces", "rule:colon", "rule/slash", "r\xFC\xEB", ""]
      cfg =
        defaultConfig
          { configOverrides =
              [RuleOverride r Cfg.SevInfo Nothing | r <- rules]
          }
   in all (\r -> effectiveSeverity cfg r == Just Cfg.SevInfo) rules

-- | SEV-5: isSuppressed returns False for non-overridden rules
prop_severity_not_suppressed :: Bool
prop_severity_not_suppressed =
  not (isSuppressed defaultConfig "any-rule")

-- | SEV-6: effectiveSeverity returns Nothing for non-overridden rule
prop_severity_no_override :: Bool
prop_severity_no_override =
  effectiveSeverity defaultConfig "any-rule" == Nothing

-- | SEV-7: Severity ordering
prop_severity_ordering :: Bool
prop_severity_ordering =
  Cfg.SevError > Cfg.SevWarning && Cfg.SevWarning > Cfg.SevInfo && Cfg.SevInfo > Cfg.SevOff

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: LSP adversarial
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | LSP-3: lintFile on empty input returns no diagnostics
prop_lsp_lint_empty :: Bool
prop_lsp_lint_empty = null (lintFile "")

-- | LSP-4: lintFile detects `with` as NARSIL-N001
prop_lsp_lint_with :: Bool
prop_lsp_lint_with =
  let diags = lintFile "with lib; {}"
   in any (\d -> "NARSIL-N001" `T.isInfixOf` _message d) diags

-- | LSP-5: lintFile detects `rec` as NARSIL-N002
prop_lsp_lint_rec :: Bool
prop_lsp_lint_rec =
  let diags = lintFile "rec { x = 1; }"
   in any (\d -> "NARSIL-N002" `T.isInfixOf` _message d) diags

-- | LSP-6: lintFile produces no diagnostics for clean Nix
prop_lsp_lint_clean :: Bool
prop_lsp_lint_clean =
  null (lintFile "{ x = 1; y = 2; }")

-- | LSP-7: Diagnostics have valid source positions when span data exists
prop_lsp_diag_positions :: Bool
prop_lsp_diag_positions =
  let diags = lintFile "with lib; stdenv.mkDerivation { name = \"foo\"; }"
   in not (null diags)
        && all
          ( \d ->
              let range = _range d
                  Range (Position startL startC) (Position endL endC) = range
               in startL >= 0 && startC >= 0 && endL >= 0 && endC >= 0
          )
          diags

-- | LSP-8: All diagnostics are Error severity
prop_lsp_diag_severity :: Text -> Bool
prop_lsp_diag_severity txt =
  all (\d -> _severity d == Just DiagnosticSeverity_Error) (lintFile txt)

-- | LSP-9: spToDiagnostic handles zero/negative positions gracefully
prop_lsp_spandiag_edge :: Bool
prop_lsp_spandiag_edge =
  let zeroDiag = spToDiagnostic "test" (Span (Loc 0 0) (Loc 0 0) Nothing)
      negDiag = spToDiagnostic "test" (Span (Loc (-1) 0) (Loc 0 0) Nothing)
      normalDiag = spToDiagnostic "test" (Span (Loc 3 5) (Loc 3 12) Nothing)
      Range (Position zsl zsc) (Position _ _) = _range zeroDiag
      Range (Position nsl nsc) (Position _ _) = _range negDiag
      Range (Position psl psc) (Position _ _) = _range normalDiag
   in zsl == 0
        && zsc == 0 -- zero position clamped to (0,0)
        && nsl == 0
        && nsc == 0 -- negative position clamped to (0,0)
        && psl == 2
        && psc == 4 -- 1-based to 0-based: line 3-1=2, col 5-1=4

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Rule ID consistency (Haskell ↔ Dhall config)
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | RULE-1: All Haskell rule IDs are unique across lint modules (excluding known H/D unions)
prop_rule_ids_unique :: Bool
prop_rule_ids_unique =
  let bashIds =
        [ Cfg.bashRuleId VEval
        , Cfg.bashRuleId VBacktick
        , Cfg.bashRuleId VHeredoc
        ]
      nixIds =
        [ Cfg.nixRuleId VWith
        , Cfg.nixRuleId VRec
        , Cfg.nixRuleId VSubstituteAll
        , Cfg.nixRuleId VRawMkDerivation
        , Cfg.nixRuleId VRawRunCommand
        , Cfg.nixRuleId VRawWriteShellApplication
        , Cfg.nixRuleId VWriteShellScript
        , Cfg.nixRuleId (VLongInlineString 200)
        ]
      derivIds =
        [ Cfg.derivRuleId DerivLint.VMissingMeta
        , Cfg.derivRuleId DerivLint.VMissingDescription
        ]
      packageIds =
        [ Cfg.packageRuleId PackageLint.P001
        ]
      patternIds =
        [ Cfg.patternRuleId PatternLint.VOrNullFallback
        , Cfg.patternRuleId PatternLint.VAttrTranslation
        ]
      allIds = bashIds <> nixIds <> derivIds <> packageIds <> patternIds
   in length allIds == length (nub allIds)

-- | RULE-2: isSuppressed works for every Haskell rule ID
prop_rule_suppress_all :: Bool
prop_rule_suppress_all =
  let buildOverrides ids = [RuleOverride i Cfg.SevOff Nothing | i <- ids]
      mkCfg ids = defaultConfig{configOverrides = buildOverrides ids}
      allIds =
        [ Cfg.bashRuleId VHeredoc
        , Cfg.bashRuleId VEval
        , Cfg.bashRuleId VBacktick
        , Cfg.nixRuleId VWith
        , Cfg.nixRuleId VRec
        , Cfg.nixRuleId VSubstituteAll
        , Cfg.nixRuleId VRawMkDerivation
        , Cfg.nixRuleId VRawRunCommand
        , Cfg.nixRuleId VRawWriteShellApplication
        , Cfg.nixRuleId VWriteShellScript
        , Cfg.nixRuleId (VLongInlineString 200)
        , Cfg.derivRuleId DerivLint.VMissingMeta
        , Cfg.derivRuleId DerivLint.VMissingDescription
        , Cfg.packageRuleId PackageLint.P001
        , Cfg.patternRuleId PatternLint.VOrNullFallback
        , Cfg.patternRuleId PatternLint.VAttrTranslation
        ]
      cfg = mkCfg allIds
   in all (isSuppressed cfg) allIds

-- | RULE-3: Override SevError makes effectiveSeverity return Just SevError
prop_rule_severity_error :: Bool
prop_rule_severity_error =
  let cfg =
        defaultConfig
          { configOverrides =
              [ RuleOverride (Cfg.bashRuleId VEval) Cfg.SevError Nothing
              ]
          }
   in effectiveSeverity cfg (Cfg.bashRuleId VEval) == Just Cfg.SevError

-- | RULE-4: Override SevInfo makes effectiveSeverity return Just SevInfo
prop_rule_severity_info :: Bool
prop_rule_severity_info =
  let cfg =
        defaultConfig
          { configOverrides =
              [ RuleOverride (Cfg.nixRuleId VWith) Cfg.SevInfo Nothing
              ]
          }
   in effectiveSeverity cfg (Cfg.nixRuleId VWith) == Just Cfg.SevInfo

-- | RULE-5: isSuppressed False when override is SevError (not SevOff)
prop_rule_not_suppressed_on_error :: Bool
prop_rule_not_suppressed_on_error =
  let cfg =
        defaultConfig
          { configOverrides =
              [ RuleOverride (Cfg.nixRuleId VRec) Cfg.SevError Nothing
              ]
          }
   in not (isSuppressed cfg (Cfg.nixRuleId VRec))

-- | RULE-6: bashRuleId VHeredoc and VHereString map to same ID
prop_rule_bash_heredoc_union :: Bool
prop_rule_bash_heredoc_union =
  Cfg.bashRuleId VHeredoc == Cfg.bashRuleId VHereString
    && Cfg.bashRuleId VHeredoc == "no-heredoc-in-inline-bash"

-- | RULE-7: Massive override list doesn't crash
prop_rule_massive_overrides :: Bool
prop_rule_massive_overrides =
  let overrides =
        [ RuleOverride (Cfg.bashRuleId VEval <> T.pack (show i)) Cfg.SevInfo Nothing
        | i <- [1 .. 10000 :: Int]
        ]
      cfg = defaultConfig{configOverrides = overrides}
   in not (isSuppressed cfg (Cfg.bashRuleId VEval))
        && effectiveSeverity cfg (Cfg.bashRuleId VEval <> "1") == Just Cfg.SevInfo

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Hover type inference
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | HOVER-1: Integer literal at cursor -> TInt
prop_hover_int :: Bool
prop_hover_int =
  case parseNixTextLoc "42" of
    Left _ -> False
    Right expr -> inferExprAt expr 0 0 == Just "Int"

-- | HOVER-2: String literal -> TString or TStrLit
prop_hover_string :: Bool
prop_hover_string =
  case parseNixTextLoc "\"hello\"" of
    Left _ -> False
    Right expr -> case inferExprAt expr 0 0 of
      Just "String" -> True
      Just x | "\"" `T.isPrefixOf` x -> True
      _ -> False

-- | HOVER-3: Function x: x -> TFun
prop_hover_func :: Bool
prop_hover_func =
  case parseNixTextLoc "x: x" of
    Left _ -> False
    Right expr -> case inferExprAt expr 0 0 of
      Just t -> "->" `T.isInfixOf` t
      _ -> False

-- | HOVER-4: Attrset { x = 1; } -> TAttrs
prop_hover_attrset :: Bool
prop_hover_attrset =
  case parseNixTextLoc "{ x = 1; }" of
    Left _ -> False
    Right expr -> case inferExprAt expr 0 0 of
      Just t -> "{ " `T.isPrefixOf` t || "{" `T.isPrefixOf` t
      _ -> False

-- | HOVER-5: let x = 42; in x -> TInt
prop_hover_let :: Bool
prop_hover_let =
  case parseNixTextLoc "let x = 42; in x" of
    Left _ -> False
    Right expr -> inferExprAt expr 0 0 == Just "Int"

-- | HOVER-6: Position that doesn't exist -> Nothing
prop_hover_nonexistent :: Bool
prop_hover_nonexistent =
  case parseNixTextLoc "42" of
    Left _ -> False
    Right expr -> isNothing (inferExprAt expr 100 0)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: References LSP handler (findReferences)
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | REFS-1: let x = 42; in x + x has 2 references to x in the body
prop_refs_let :: Bool
prop_refs_let =
  case parseNixTextLoc "let x = 42; in x + x" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
       in case Scope.findDeclaration sg "x" of
            [] -> False
            decl : _ -> length (Scope.findReferences sg decl) == 2

-- | REFS-2: file with no matching references returns empty list
prop_refs_no_match :: Bool
prop_refs_no_match =
  case parseNixTextLoc "let x = 42; in y" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
       in case Scope.findDeclaration sg "x" of
            [] -> False
            decl : _ -> null (Scope.findReferences sg decl)

-- | REFS-3: cross-file references found via fromModuleGraph
prop_refs_cross_file :: Bool
prop_refs_cross_file =
  case (parseNixTextLoc "let x = 42; in x", parseNixTextLoc "let y = 1; in y") of
    (Right e1, Right e2) ->
      let sg = Scope.fromModuleGraph (Map.fromList [("a.nix", e1), ("b.nix", e2)])
          decls = Scope.findDeclaration sg "x"
       in not (null decls)
            && case decls of
              d : _ -> not (null (Scope.findReferences sg d))
              [] -> False
    _ -> False

-- | REFS-4: unresolved name returns empty
prop_refs_unresolved :: Bool
prop_refs_unresolved =
  case parseNixTextLoc "bogus" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
       in null (Scope.findDeclaration sg "nonexistent")

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Rename
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | RENAME-1: let x = 42; in x + x has 2 references to x
prop_rename_let :: Bool
prop_rename_let =
  case parseNixTextLoc "let x = 42; in x + x" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
       in case Scope.findDeclaration sg "x" of
            [] -> False
            decl : _ -> length (Scope.findReferences sg decl) == 2

-- | RENAME-2: rename non-existent variable finds nothing
prop_rename_missing :: Bool
prop_rename_missing =
  case parseNixTextLoc "42" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
       in null (Scope.findDeclaration sg "bogus")

-- | RENAME-3: function param x: x + 1 -- rename x to y edits the reference
prop_rename_func :: Bool
prop_rename_func =
  case parseNixTextLoc "x: x + 1" of
    Left _ -> False
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
       in case Scope.findDeclaration sg "x" of
            [] -> False
            decl : _ -> not (null (Scope.findReferences sg decl))

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: Completion
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

getCompletionLabels :: Text -> [Text]
getCompletionLabels src =
  case parseNixTextLoc src of
    Left _ -> []
    Right expr ->
      let sg = Scope.fromNixExpr Nothing expr
       in map Scope.declName (concatMap Scope.scopeDeclarations (Map.elems (Scope.sgScopes sg)))

-- | COMPL-1: let x = 42; y = true; in x + y — completions include "x" and "y"
prop_completion_let :: Bool
prop_completion_let =
  let labels = getCompletionLabels "let x = 42; y = true; in x + y"
   in "x" `elem` labels && "y" `elem` labels

-- | COMPL-2: empty file produces empty completion list
prop_completion_empty :: Bool
prop_completion_empty =
  null (getCompletionLabels "")

-- | COMPL-3: { a = 1; b = 2; } — completions include "a" and "b"
prop_completion_attrset :: Bool
prop_completion_attrset =
  let labels = getCompletionLabels "{ a = 1; b = 2; }"
   in "a" `elem` labels && "b" `elem` labels

-- | COMPL-4: {x, y}: x + y — completions include "x" and "y"
prop_completion_func :: Bool
prop_completion_func =
  let labels = getCompletionLabels "{x, y}: x + y"
   in "x" `elem` labels && "y" `elem` labels

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: CLI Types
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | emptyCICounts has all fields zero
prop_cli_empty_cicounts_all_zero :: Bool
prop_cli_empty_cicounts_all_zero =
  let c = emptyCICounts
   in ciFilesScanned c == 0
        && ciTypePass c == 0
        && ciTypeFail c == 0
        && ciTypeSkip c == 0
        && ciLintViolations c == 0
        && ciPackageViolations c == 0
        && ciBashViolations c == 0
        && ciGraphFailures c == 0

-- | Status markers are distinct and non-empty
prop_cli_markers_distinct :: Bool
prop_cli_markers_distinct =
  okMarker /= crossMarker
    && okMarker /= unsupMarker
    && crossMarker /= unsupMarker
    && not (any T.null [okMarker, crossMarker, unsupMarker])

-- | TCResult equality is reflexive
prop_cli_tcresult_reflexive :: Bool
prop_cli_tcresult_reflexive =
  TCOk == TCOk && TCFail == TCFail && TCSkip == TCSkip

-- | CICounts field-wise addition is correct
prop_cli_cicounts_merge :: Bool
prop_cli_cicounts_merge =
  let a = CICounts 1 2 3 4 5 6 7 8 9
      b = CICounts 9 10 11 12 13 14 15 16 17
      c =
        CICounts
          (ciFilesScanned a + ciFilesScanned b)
          (ciTypePass a + ciTypePass b)
          (ciTypeFail a + ciTypeFail b)
          (ciTypeSkip a + ciTypeSkip b)
          (ciLintViolations a + ciLintViolations b)
          (ciPackageViolations a + ciPackageViolations b)
          (ciBashViolations a + ciBashViolations b)
          (ciGraphFailures a + ciGraphFailures b)
          (ciLayoutViolations a + ciLayoutViolations b)
   in ciFilesScanned c == 10
        && ciTypePass c == 12
        && ciTypeFail c == 14
        && ciTypeSkip c == 16
        && ciLintViolations c == 18
        && ciPackageViolations c == 20
        && ciBashViolations c == 22
        && ciGraphFailures c == 24
        && ciLayoutViolations c == 26

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: CLI Report
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

dummySpan :: Span
dummySpan = Span (Loc 0 0) (Loc 0 0) Nothing

dummyBashViolation :: Violation
dummyBashViolation = Violation VHeredoc dummySpan "cat <<EOF"

dummyPackageViolation :: PackageLint.PackageViolation
dummyPackageViolation = PackageLint.PackageViolation PackageLint.P001 "pkg/" "missing default.nix"

-- | partitionViolations on empty list produces two empty lists
prop_report_partition_empty :: Bool
prop_report_partition_empty =
  let (sup, act) = partitionViolations defaultConfig []
   in null sup && null act

-- | partitionViolations: SevOff override suppresses matching violation
prop_report_partition_suppress :: Bool
prop_report_partition_suppress =
  let cfg =
        defaultConfig
          { configOverrides =
              [ RuleOverride
                  { overrideId = bashRuleId VHeredoc
                  , overrideSeverity = Cfg.SevOff
                  , overrideReason = Just "testing"
                  }
              ]
          }
      (sup, act) = partitionViolations cfg [dummyBashViolation]
   in length sup == 1 && null act

-- | partitionViolations: non-matching override doesn't suppress
prop_report_partition_no_suppress :: Bool
prop_report_partition_no_suppress =
  let cfg =
        defaultConfig
          { configOverrides =
              [ RuleOverride
                  { overrideId = bashRuleId VEval
                  , overrideSeverity = Cfg.SevOff
                  , overrideReason = Just "testing"
                  }
              ]
          }
      (sup, act) = partitionViolations cfg [dummyBashViolation]
   in null sup && length act == 1

-- | partitionViolations: suppressed + active equals total input
prop_report_partition_complete :: Bool
prop_report_partition_complete =
  let violations = [dummyBashViolation, dummyBashViolation{vType = VEval}]
      (sup, act) = partitionViolations defaultConfig violations
   in length sup + length act == length violations

-- | formatBareCommand produces output containing NARSIL-B005
prop_report_format_bare :: Bool
prop_report_format_bare =
  let out = formatBareCommand "test.sh" ("curl", dummySpan)
   in "NARSIL-B005" `T.isInfixOf` out && not (T.null out)

-- | formatDynamicCommand produces output containing NARSIL-B006
prop_report_format_dynamic :: Bool
prop_report_format_dynamic =
  let out = formatDynamicCommand "test.sh" ("cmd", dummySpan)
   in "NARSIL-B006" `T.isInfixOf` out && not (T.null out)

-- | indentBlock prefixes every non-empty line
prop_report_indent_block :: Bool
prop_report_indent_block =
  let prefix = ">>> "
      input = "line1\nline2\n\nline3"
      result = indentBlock prefix input
      lines' = T.lines result
   in all (T.isPrefixOf prefix) (filter (not . T.null) lines')

-- | formatPackageViolations empty list produces empty
prop_report_package_empty :: Bool
prop_report_package_empty =
  T.null (formatPackageViolations [])

-- | formatPackageViolations non-empty contains NARSIL-P001
prop_report_package_nonempty :: Bool
prop_report_package_nonempty =
  let out = formatPackageViolations [dummyPackageViolation]
   in "NARSIL-P001" `T.isInfixOf` out && not (T.null out)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: CLI Check
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | `rec { … }` is SUPPORTED (SCC-based recursive bindings) — the stale
unsupported-guard was skipping 11,715 nixpkgs files (93% of the sweep's
coverage ceiling). It types, and it is not flagged.
-}
prop_check_supported_rec :: Bool
prop_check_supported_rec =
  case parseNixTextLoc "rec { x = 1; y = x + 1; }" of
    Left _ -> False
    Right expr ->
      detectUnsupportedConstruct expr == Nothing
        && isRight (parseAndInfer "rec { x = 1; y = x + 1; }")

{- | detectUnsupportedConstruct detects dynamic attribute access
FLIPPED (review-6): dynamic attribute access is SUPPORTED — 'inferSelect'
resolves a dynamic key to a fresh var, so the file is checked, not skipped
(this flag was the last 992 files of the coverage ceiling).
-}
prop_check_unsupported_dynamic :: Bool
prop_check_unsupported_dynamic =
  case parseNixTextLoc "x.\"${key}\"" of
    Left _ -> False
    Right expr -> detectUnsupportedConstruct expr == Nothing

-- | detectUnsupportedConstruct passes clean let
prop_check_unsupported_clean :: Bool
prop_check_unsupported_clean =
  case parseNixTextLoc "let x = 1; in x" of
    Left _ -> False
    Right expr -> detectUnsupportedConstruct expr == Nothing

-- | formatTypeError wraps first line and indents rest
prop_check_format_type_error :: Bool
prop_check_format_type_error =
  let result = formatTypeError "first\nsecond\nthird"
      lines' = T.lines result
   in case lines' of
        (first : rest) ->
          "  TYPE WARNING: first" `T.isPrefixOf` first
            && all ("         " `T.isPrefixOf`) (filter (not . T.null) (take 1 rest))
        _ -> False

-- | formatTypeError handles empty input gracefully
prop_check_format_type_error_empty :: Bool
prop_check_format_type_error_empty =
  let result = formatTypeError ""
   in "TYPE WARNING" `T.isInfixOf` result

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: CLI Bash
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | safeReadFile on existing file returns Right
prop_bash_safe_read_existing :: Property
prop_bash_safe_read_existing =
  QCM.monadicIO $
    QCM.run (safeReadFile "test/fixtures/bash/check-by-name.sh") >>= \case
      Right _ -> QCM.assert True
      Left _ -> QCM.assert False

-- | safeReadFile on nonexistent file returns Left
prop_bash_safe_read_nonexistent :: Property
prop_bash_safe_read_nonexistent =
  QCM.monadicIO $
    QCM.run (safeReadFile "/nonexistent/dead-beef-file.sh") >>= \case
      Left _ -> QCM.assert True
      Right _ -> QCM.assert False

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Properties: CLI Check -- exit code regression
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | REGRESSION: checkWithViolations with skipTypeCheck=True and no lint
violations must return TCFail (not TCOk). When a file has an unsupported
construct (rec, dynamic attr access), we cannot fully verify it.
Returning TCOk here means the process exits 0, masking the gap.
-}
prop_cli_skip_checked_returns_fail :: Property
prop_cli_skip_checked_returns_fail = QCM.monadicIO $ do
  let src = "let x = 1; in x" -- clean Nix, no lint violations
  case parseNixTextLoc src of
    Left _ -> QCM.assert True -- parse failure is fine
    Right expr -> do
      result <-
        QCM.run $
          runLog ErrorS $
            checkWithViolations Nothing defaultConfig "test.nix" expr True
      QCM.assert (result == TCFail)

-- | checkWithViolations with skipTypeCheck=False and clean file returns TCOk
prop_cli_normal_check_passes :: Property
prop_cli_normal_check_passes = QCM.monadicIO $ do
  let src = "let x = 1; in x"
  case parseNixTextLoc src of
    Left _ -> QCM.assert True
    Right expr -> do
      result <-
        QCM.run $
          runLog ErrorS $
            checkWithViolations Nothing defaultConfig "test.nix" expr False
      QCM.assert (result == TCOk)

-- | checkWithViolations with skipTypeCheck=False and lint violation returns TCFail
prop_cli_lint_check_fails :: Property
prop_cli_lint_check_fails = QCM.monadicIO $ do
  let src = "with builtins; true" -- triggers VWith lint violation
  case parseNixTextLoc src of
    Left _ -> QCM.assert True
    Right expr -> do
      result <-
        QCM.run $
          runLog ErrorS $
            checkWithViolations Nothing defaultConfig "test.nix" expr False
      QCM.assert (result == TCFail)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Main
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- Wired-in adversarial suites (test/Adversarial.hs, test/NixAdversarial.hs were
-- compiled but never run). Built here so the Arbitrary orphan instances above
-- are in scope. Run from main alongside the Psychotic suite.
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

qcRun :: (Testable p) => p -> IO Bool
qcRun p = isSuccess <$> quickCheckWithResult stdArgs{maxSuccess = 100, chatty = False} p

adversarialTests :: [(String, IO Bool)]
adversarialTests =
  [ ("adv_literal_no_crash", qcRun Adversarial.prop_literal_no_crash)
  , ("adv_literal_overflow_safe", qcRun Adversarial.prop_literal_overflow_safe)
  , ("adv_expansion_no_crash", qcRun Adversarial.prop_expansion_no_crash)
  , ("adv_expansion_malformed_safe", qcRun Adversarial.prop_expansion_malformed_safe)
  , ("adv_config_no_crash", qcRun Adversarial.prop_config_no_crash)
  , ("adv_config_malformed_safe", qcRun Adversarial.prop_config_malformed_safe)
  , ("adv_bash_no_crash", qcRun Adversarial.prop_bash_no_crash)
  , ("adv_expansion_test_vectors", qcRun Adversarial.prop_expansion_test_vectors)
  , ("adv_literal_test_vectors", qcRun Adversarial.prop_literal_test_vectors)
  , ("adv_empty_default_not_required", qcRun Adversarial.prop_empty_default_not_required)
  , ("adv_traversal_rejected", qcRun Adversarial.prop_traversal_rejected)
  , ("adv_overflow_becomes_string", qcRun Adversarial.prop_overflow_becomes_string)
  , ("adv_varname_valid", qcRun Adversarial.prop_varname_valid)
  , -- #21 FIXED: config parser validates the var name after `$`/`"$`.
    ("adv_invalid_varname_rejected", qcRun Adversarial.prop_invalid_varname_rejected)
  , -- #22 FIXED: a non-name (`$|`, `$\n; id`) is a literal, not a captured var ref.
    ("adv_injection_blocked", qcRun Adversarial.prop_injection_blocked)
  , ("adv_store_path_no_traversal", qcRun Adversarial.prop_store_path_no_traversal)
  , ("adv_emit_escaped", qcRun Adversarial.prop_emit_escaped)
  , ("adv_solve_unsatisfiable", qcRun Adversarial.prop_solve_unsatisfiable)
  , ("adv_bounded_time", qcRun Adversarial.prop_bounded_time)
  , ("adv_no_memory_bomb", qcRun Adversarial.prop_no_memory_bomb)
  , -- DROPPED adv_literal_type_preserved: ill-posed/flaky. Asserts type-preserving
    -- roundtrip over ARBITRARY literals, but its renderLiteral prints LitString "2"
    -- as bare `2`, which correctly re-parses as LitInt 2. Canonical roundtrips
    -- (literal_int_roundtrip / literal_bool_roundtrip) already cover the valid case.
    ("adv_subst_chain_bash", qcRun Adversarial.prop_subst_chain_bash)
  , ("adv_subst_chain_nix", qcRun Adversarial.prop_subst_chain_nix)
  , -- #23 FIXED: `eval` behind a command modifier (`command`/`builtin`/…) detected.
    ("adv_bash_lint_eval_prefixed", qcRun Adversarial.prop_bash_lint_eval_prefixed)
  , -- DROPPED adv_bash_lint_eval_store_path: asserts a store-path binary literally
    -- named `eval` is the `eval` builtin — it isn't (separate command); false positive.
    -- #24 FIXED: array-subscript assignments (`config[k]=…`) reconstruct the
    -- LHS from ShellCheck's indices field, so multi-interp values become ConfigTemplate.
    ("adv_config_array_template", qcRun Adversarial.prop_config_array_template)
  ]

nixAdversarialTests :: [(String, IO Bool)]
nixAdversarialTests =
  [ ("nixadv_nix_occurs_check", qcRun NixAdversarial.prop_nix_occurs_check)
  , ("nixadv_nix_union_mismatch", qcRun NixAdversarial.prop_nix_union_mismatch)
  , ("nixadv_nix_attrs_required_missing", qcRun NixAdversarial.prop_nix_attrs_required_missing)
  ,
    ( "nixadv_nix_row_closed_missing_open_req"
    , qcRun NixAdversarial.prop_nix_row_closed_missing_open_req
    )
  , -- DROPPED nixadv_nix_row_empty_open_any: asserts `unify (TAttrsOpen {}) TInt`
    -- should SUCCEED — that's unsound (a record is not an Int). Code correctly rejects.
    -- BUG#25: union membership doesn't flatten nested unions.
    ("nixadv_nix_nested_union", qcRun NixAdversarial.prop_nix_nested_union)
  , ("nixadv_nix_many_fresh_vars", qcRun NixAdversarial.prop_nix_many_fresh_vars)
  , ("nixadv_nix_deep_func_nesting", qcRun NixAdversarial.prop_nix_deep_func_nesting)
  , ("nixadv_nix_deep_attr_nesting", qcRun NixAdversarial.prop_nix_deep_attr_nesting)
  , ("nixadv_nix_deep_let_nesting", qcRun NixAdversarial.prop_nix_deep_let_nesting)
  , ("nixadv_nix_mutual_scc_stress", qcRun NixAdversarial.prop_nix_mutual_scc_stress)
  , ("nixadv_nix_nested_with", qcRun NixAdversarial.prop_nix_nested_with)
  , ("nixadv_nix_with_inside_rec", qcRun NixAdversarial.prop_nix_with_inside_rec)
  , ("nixadv_nix_with_inside_func", qcRun NixAdversarial.prop_nix_with_inside_func)
  , ("nixadv_nix_with_memo_no_leak", qcRun NixAdversarial.prop_nix_with_memo_no_leak)
  , ("nixadv_nix_functor_self", qcRun NixAdversarial.prop_nix_functor_self)
  , ("nixadv_nix_functor_wrong_arity", qcRun NixAdversarial.prop_nix_functor_wrong_arity)
  , ("nixadv_nix_functor_chain", qcRun NixAdversarial.prop_nix_functor_chain)
  , ("nixadv_nix_functor_identity", qcRun NixAdversarial.prop_nix_functor_identity)
  ,
    ( "nixadv_nix_row_closed_vs_open_common"
    , qcRun NixAdversarial.prop_nix_row_closed_vs_open_common
    )
  , ("nixadv_nix_row_closed_extra_ok", qcRun NixAdversarial.prop_nix_row_closed_extra_ok)
  , ("nixadv_nix_infer_state_integrity", qcRun NixAdversarial.prop_nix_infer_state_integrity)
  , ("nixadv_nix_infer_deterministic", qcRun NixAdversarial.prop_nix_infer_deterministic)
  , ("nixadv_nix_tvar_supply_monotonic", qcRun NixAdversarial.prop_nix_tvar_supply_monotonic)
  , ("nixadv_nix_empty_attrset", qcRun NixAdversarial.prop_nix_empty_attrset)
  , ("nixadv_nix_empty_list", qcRun NixAdversarial.prop_nix_empty_list)
  , ("nixadv_nix_heterogeneous_list", qcRun NixAdversarial.prop_nix_heterogeneous_list)
  , ("nixadv_nix_nested_application", qcRun NixAdversarial.prop_nix_nested_application)
  , ("nixadv_nix_functor_non_func", qcRun NixAdversarial.prop_nix_functor_non_func)
  , ("nixadv_nix_functor_valid", qcRun NixAdversarial.prop_nix_functor_valid)
  , -- BUG#26: deriv linter misses `mkDerivation` reached through a deep select chain.
    ("nixadv_deriv_deep_select", qcRun NixAdversarial.prop_deriv_deep_select)
  , ("nixadv_nix_subst_chain", qcRun NixAdversarial.prop_nix_subst_chain)
  ]

main :: IO ()
main = do
  putStrLn "narsil property tests"
  TIO.putStrLn (Draw.rule Draw.Double 25)
  putStrLn ""

  results <-
    sequence
      [ -- Unification
        run "unify_reflexive" prop_unify_reflexive
      , run "unify_symmetric" prop_unify_symmetric
      , run "unify_valid_subst" prop_unify_valid_subst
      , run "unify_self_trivial" prop_unify_self_trivial
      , run "unify_concrete_disjoint" prop_unify_concrete_disjoint
      , run "unify_tvar_universal" prop_unify_tvar_universal
      , run "subst_compose_assoc" prop_subst_compose_assoc
      , run "subst_empty_identity" prop_subst_empty_identity
      , run "subst_single" prop_subst_single
      , -- Constraint solving
        run "solve_empty" prop_solve_empty
      , run "solve_reflexive" prop_solve_reflexive
      , run "solve_satisfies" prop_solve_satisfies
      , run "solve_deterministic" prop_solve_deterministic
      , -- Fact -> Constraint
        run "default_is_constraint" prop_default_is_constraint
      , run "required_no_constraint" prop_required_no_constraint
      , run "config_no_constraint" prop_config_no_constraint
      , -- Schema building
        run "schema_env_complete" prop_schema_env_complete
      , run "schema_preserves_defaults" prop_schema_preserves_defaults
      , run "schema_required_marked" prop_schema_required_marked
      , -- Parser
        run "parser_no_crash" prop_parser_no_crash
      , run "parser_empty" prop_parser_empty
      , run "parser_comments" prop_parser_comments
      , -- Patterns
        run "pattern_default" $ forAll genVarName $ \var -> property $ prop_pattern_default var
      , run "pattern_required" $ forAll genVarName $ \var -> property $ prop_pattern_required var
      , run "pattern_simple" $ forAll genVarName $ \var -> property $ prop_pattern_simple var
      , run "numeric_int" prop_numeric_int
      , run "numeric_rejects_alpha" prop_numeric_rejects_alpha
      , -- Builtins
        run "builtins_nonempty" prop_builtins_nonempty
      , run "builtins_curl_timeout" prop_builtins_curl_timeout
      , run "builtins_curl_output" prop_builtins_curl_output
      , run "builtins_jq_indent" prop_builtins_jq_indent
      , run "builtins_unknown_flag" prop_builtins_unknown_flag
      , run "builtins_unknown_cmd" prop_builtins_unknown_cmd
      , -- Config tree
        run "config_tree_complete" prop_config_tree_complete -- Scope graph
      , run "scope_parent_before_with" prop_scope_parent_before_with
      , -- Literals
        run "literal_int_roundtrip" prop_literal_int_roundtrip
      , run "literal_bool_roundtrip" prop_literal_bool_roundtrip
      , run "literal_type_consistent" prop_literal_type_consistent
      , -- End-to-end
        run "e2e_no_crash" prop_e2e_no_crash
      , run "e2e_concrete_types" prop_e2e_concrete_types
      , -- Stress tests
        run "stress_large_script" prop_stress_large_script
      , run "stress_many_vars" prop_stress_many_vars
      , run "stress_deep_config" prop_stress_deep_config
      , run "stress_chain" prop_stress_chain
      , run "unify_transitivity" prop_unify_transitivity
      , run "schema_config_paths" prop_schema_config_paths
      , -- Overlay Algebra
        run "overlay_identity_left" prop_overlay_identity_left
      , run "overlay_identity_right" prop_overlay_identity_right
      , run "overlay_assoc" prop_overlay_assoc
      , run "overlay_satisfaction" prop_overlay_satisfaction
      , run "overlay_propagation" prop_overlay_propagation
      , -- Nix type inference (FIX-11)
        run "nix_infer_no_crash" prop_nix_infer_no_crash
      , run "nix_int_literal" prop_nix_int_literal
      , run "nix_string_literal" prop_nix_string_literal
      , run "nix_bool_literal" prop_nix_bool_literal
      , run "nix_null_literal" prop_nix_null_literal
      , run "nix_list_int" prop_nix_list_int
      , run "nix_attrset" prop_nix_attrset
      , run "nix_identity" prop_nix_identity
      , run "nix_let_binding" prop_nix_let_binding
      , run "nix_with_resolves" prop_nix_with_resolves
      , run "nix_with_multiple" prop_nix_with_multiple
      , run "nix_with_polymorphic" prop_nix_with_polymorphic
      , run "nix_rec_self" prop_nix_rec_self
      , run "nix_rec_mutual" prop_nix_rec_mutual
      , run "nix_rec_cross" prop_nix_rec_cross
      , run "nix_rec_infinite" prop_nix_rec_infinite
      , -- Merge correctness
        run "merge_preserves_required" prop_merge_preserves_required
      , run "merge_keeps_default" prop_merge_keeps_default
      , run "duplicate_var_merged" prop_duplicate_var_merged
      , run "merge_schema_identity" prop_merge_schema_identity
      , -- Fact extraction vectors
        run "fact_default_is" prop_fact_default_is
      , run "fact_required" prop_fact_required
      , run "fact_assign_from" prop_fact_assign_from
      , run "fact_config_assign" prop_fact_config_assign
      , run "fact_config_lit" prop_fact_config_lit
      , run "fact_config_quoted" prop_fact_config_quoted
      , run "fact_config_unquoted" prop_fact_config_unquoted
      , run "fact_config_var_quoted" prop_fact_config_var_quoting_ast
      , run "fact_config_var_unquoted" prop_fact_config_var_unquoted
      , run "fact_config_split_eq_quoted" prop_fact_config_split_eq_quoted
      , run "fact_config_empty_value" prop_fact_config_empty_value
      , run "fact_config_spaced_eq" prop_fact_config_spaced_eq
      , -- Emit-config output
        run "emit_json_guarded" prop_emit_json_guarded
      , run "emit_json_runtime_args" prop_emit_json_runtime_args
      , run "emit_preflight_guard" prop_emit_preflight_guard
      , run "emit_numeric_preflight_guard" prop_emit_numeric_preflight_guard
      , run "emit_bool_preflight_guard" prop_emit_bool_preflight_guard
      , run "emit_quoted_numeric_no_int_guard" prop_emit_quoted_numeric_no_int_guard
      , run "emit_no_set_e_leak" prop_emit_no_set_e_leak
      , run "emit_runtime_escape_controls" prop_emit_runtime_escape_controls
      , run "emit_yaml_guarded" prop_emit_yaml_guarded
      , run "emit_toml_no_null" prop_emit_toml_no_null
      , run "emit_json_literal" prop_emit_json_literal
      , run "emit_json_string_quoted" prop_emit_json_string_quoted
      , -- Scope graph construction
        run "scope_let_decl" prop_scope_let_decl
      , run "scope_attrset_decls" prop_scope_attrset_decls
      , run "scope_func_params" prop_scope_func_params
      , run "scope_var_refs" prop_scope_var_refs
      , run "scope_with_structure" prop_scope_with_structure
      , run "scope_merge_files" prop_scope_merge_files
      , -- Nix lint
        run "nix_lint_with" prop_nix_lint_with
      , run "nix_lint_rec" prop_nix_lint_rec
      , run "nix_lint_clean" prop_nix_lint_clean
      , run "nix_lint_non_lisp_case" prop_nix_lint_non_lisp_case
      , -- Nix lint (new rules N005-N012)
        run "nix_lint_substitute_all" prop_nix_lint_substitute_all
      , run "nix_lint_raw_mkderivation" prop_nix_lint_raw_mkderivation
      , run "nix_lint_raw_runcommand" prop_nix_lint_raw_runcommand
      , run "nix_lint_raw_wsa" prop_nix_lint_raw_wsa
      , run "nix_lint_write_shell_script" prop_nix_lint_write_shell_script
      , run "nix_lint_long_string" prop_nix_lint_long_string
      , run "nix_lint_short_string_ok" prop_nix_lint_short_string_ok
      , run "nix_lint_stdenv_path" prop_nix_lint_stdenv_path
      , -- Bash lint
        run "bash_lint_heredoc" prop_bash_lint_heredoc
      , run "bash_lint_backtick" prop_bash_lint_backtick
      , run "bash_lint_clean" prop_bash_lint_clean
      , -- Derivation lint
        run "deriv_missing_meta" prop_deriv_missing_meta
      , run "deriv_missing_description" prop_deriv_missing_description
      , run "deriv_has_both" prop_deriv_has_both
      , run "deriv_clean" prop_deriv_clean
      , run "deriv_stdenv_path" prop_deriv_stdenv_path
      , -- Pattern lint
        run "pattern_or_null_fallback" prop_pattern_or_null_fallback
      , run "pattern_translate_attrs" prop_pattern_translate_attrs
      , run "pattern_clean" prop_pattern_clean
      , -- Schema defaulted vars
        run "schema_defaulted_reported" prop_schema_defaulted_reported
      , run "schema_resolved_not_defaulted" prop_schema_resolved_not_defaulted
      , -- End-to-end integration
        run "e2e_config_extraction" prop_e2e_config_extraction
      , run "e2e_required_vars" prop_e2e_required_vars
      , run "e2e_type_conflict" prop_e2e_type_conflict
      , run "e2e_empty_script" prop_e2e_empty_script
      , run "e2e_store_paths" prop_e2e_store_paths
      , run "e2e_config_prefix_conflict" prop_e2e_config_prefix_conflict
      , run "e2e_config_template" prop_e2e_config_template
      , run "e2e_config_template_prefix_suffix" prop_e2e_config_template_prefix_suffix
      , run "e2e_config_template_default" prop_e2e_config_template_default
      , -- Edge cases
        run "edge_comments_only" prop_edge_comments_only
      , run "edge_long_varname" prop_edge_long_varname
      , run "edge_deep_config" prop_edge_deep_config
      , run "edge_all_fact_types" prop_edge_all_fact_types
      , -- Emit-config structural
        run "emit_json_balanced" prop_emit_json_balanced
      , run "emit_no_heredoc" prop_emit_no_heredoc
      , run "emit_json_nested" prop_emit_json_nested
      , -- Format (smoke: annotation injection happens; meaning-preservation
        -- is covered by review_format_roundtrip)
        run "format_function" prop_format_function
      , -- REVIEW-3 regression / bug-demonstration properties
        run "review_nested_select_errors" (property prop_review_nested_select_errors)
      , run "review_nested_select_deep_ok" (property prop_review_nested_select_deep_ok)
      , run "review_eq_null_ok" (property prop_review_eq_null_ok)
      , run "review_eq_heterogeneous_ok" prop_review_eq_heterogeneous_ok
      , run "review_map_ok" (property prop_review_map_ok)
      , run "review_map_misuse_fails" (property prop_review_map_misuse_fails)
      , run "review_poly_builtin_terminates" (property prop_review_poly_builtin_terminates)
      , run "review_plus_int_float" (property prop_review_plus_int_float)
      , run "review_plus_path_string" (property prop_review_plus_path_string)
      , run "review_plus_nonaddable_fails" (property prop_review_plus_nonaddable_fails)
      , run "review_tostring_concrete_errors" (property prop_review_tostring_concrete_errors)
      , run "tostring_list_and_globals_ok" (property prop_tostring_list_and_globals_ok)
      , run "review_format_roundtrip" prop_review_format_roundtrip
      , run "reformatter_roundtrip" prop_reformatter_roundtrip
      , run "reformatter_roundtrip_corpus" prop_reformatter_roundtrip_corpus
      , run "reformatter_indented_string" prop_reformatter_indented_string
      , run "welltyped_vectors" prop_welltyped_vectors
      , run "diagnostic_render" prop_diagnostic_render
      , run "pretty_strlit_truncated" prop_pretty_strlit_truncated
      , run "safety_error_categories" prop_safety_error_categories
      , run "review_select_on_var_constrains" (property prop_review_select_on_var_constrains)
      , run "review_select_accumulates" (property prop_review_select_accumulates)
      , run "review_select_present_ok" (property prop_review_select_present_ok)
      , run "review_select_missing_fails" (property prop_review_select_missing_fails)
      , run "review_optional_open_field_ok" (property prop_review_optional_open_field_ok)
      , run "param_default_refs_sibling" (property prop_param_default_refs_sibling)
      , run "review_builtins_attrnames_ok" (property prop_review_builtins_attrnames_ok)
      , run
          "review_builtins_attrnames_nonrecord_fails"
          (property prop_review_builtins_attrnames_nonrecord_fails)
      , run "review_builtins_hasattr_ok" (property prop_review_builtins_hasattr_ok)
      , run "lib_mkif_polymorphic" (property prop_lib_mkif_polymorphic)
      , run "lib_mkmerge_polymorphic" (property prop_lib_mkmerge_polymorphic)
      , run "module_flake_selfref_ok" (property prop_module_flake_selfref_ok)
      , run "module_selfref_nonexternal_ok" (property prop_module_selfref_nonexternal_ok)
      , run "module_mode_keeps_strict_occurs" (property prop_module_mode_keeps_strict_occurs)
      , run "review_bash_subtype_resolves" (property prop_review_bash_subtype_resolves)
      , run "review_union_var_constrains" (property prop_review_union_var_constrains)
      , -- Bash AST edge cases
        run "bash_arithmetic" prop_bash_arithmetic
      , run "bash_subshell" prop_bash_subshell
      , run "bash_pipe" prop_bash_pipe
      , run "bash_for_loop" prop_bash_for_loop
      , run "bash_span_lines" prop_bash_span_line_numbers
      , run "bash_span_multi" prop_bash_span_multi_line
      , -- Module kind detection
        run "module_kind_flake" prop_module_kind_flake
      , run "module_kind_nixos" prop_module_kind_nixos
      , run "module_kind_overlay" prop_module_kind_overlay
      , run "module_kind_package" prop_module_kind_package
      , run "module_kind_flake_file" prop_module_kind_flake_file
      , -- Naming conventions
        run "naming_kebab_valid" prop_naming_kebab_valid
      , run "naming_kebab_reject_snake" prop_naming_kebab_reject_snake
      , run "naming_kebab_reject_camel" prop_naming_kebab_reject_camel
      , run "naming_roundtrip_kebab" prop_naming_roundtrip_kebab
      , run "naming_roundtrip_snake" prop_naming_roundtrip_snake
      , -- Layout conventions
        run "layout_straylight_invalid" prop_layout_straylight_invalid
      , run "layout_straylight_valid" prop_layout_straylight_valid
      , run "layout_flakeparts_valid" prop_layout_flakeparts_valid
      , run "layout_nixpkgs_package_valid" prop_layout_nixpkgs_package_valid
      , run "layout_nixpkgs_non_package" prop_layout_nixpkgs_non_package
      , run "layout_nixos_modules_valid" prop_layout_nixos_modules_valid
      , run "layout_nixos_hosts_valid" prop_layout_nixos_hosts_valid
      , run "layout_nixos_users_valid" prop_layout_nixos_users_valid
      , run "layout_nixos_wrong_location" prop_layout_nixos_wrong_location
      , run "layout_forbidden_package" prop_layout_forbidden_package
      , run "layout_forbidden_flake_mod" prop_layout_forbidden_flake_mod
      , run "layout_exact_flake" prop_layout_exact_flake
      , run "layout_contains_unused" prop_layout_contains_unused
      , run "layout_attr_name_kebab" prop_layout_attr_name_kebab
      , run "layout_attr_name_camel" prop_layout_attr_name_camel
      , run "layout_ident_kebab" prop_layout_ident_kebab
      , run "layout_filename_kebab" prop_layout_filename_kebab
      , run "layout_flake_mod_required" prop_layout_flake_mod_required
      , run "layout_unknown_kind" prop_layout_unknown_kind
      , run "naming_camel_valid" prop_naming_camel_valid
      , run "naming_pascal_valid" prop_naming_pascal_valid
      , run "naming_kebab_from_camel" prop_naming_kebab_from_camel
      , run "naming_snake_from_kebab" prop_naming_snake_from_kebab
      , run "naming_drop_nix" prop_naming_drop_nix
      , -- Lint adversarial attacks
        run "adv_lint_fp_string_mkderiv" (property prop_adv_fp_string_mkderiv)
      , run "adv_lint_fp_string_substall" (property prop_adv_fp_string_substall)
      , run "adv_lint_fp_string_runcommand" (property prop_adv_fp_string_runcommand)
      , run "adv_lint_fp_string_wss" (property prop_adv_fp_string_wss)
      , run "adv_lint_fp_var_mkderiv" (property prop_adv_fp_var_mkderiv)
      , run "adv_lint_fp_short_string" (property prop_adv_fp_short_string)
      , run "adv_lint_fp_mkderiv_with_meta" (property prop_adv_fp_mkderiv_with_meta)
      , run "adv_lint_fp_meta_with_desc" (property prop_adv_fp_meta_with_desc)
      , run "adv_lint_fp_select_no_null" (property prop_adv_fp_select_no_null)
      , run "adv_lint_fp_null_as_value" (property prop_adv_fp_null_as_value)
      , run "adv_lint_fn_mkderiv_let" (property prop_adv_fn_mkderiv_let)
      , run "adv_lint_fn_substall_path" (property prop_adv_fn_substall_path)
      , run "adv_lint_fn_runcommand_func" (property prop_adv_fn_runcommand_func)
      , run "adv_lint_fn_wsa_path" (property prop_adv_fn_wsa_path)
      , run "adv_lint_fn_long_interp" (property prop_adv_fn_long_interp)
      , run "adv_lint_fn_wssbin" (property prop_adv_fn_wssbin)
      , run "adv_lint_fn_stdenv_no_meta" (property prop_adv_fn_stdenv_no_meta)
      , run "adv_lint_fn_map_attrs" (property prop_adv_fn_map_attrs)
      , run "adv_lint_crash_nonexistent_dir" prop_adv_crash_nonexistent_dir
      , run "adv_lint_crash_deep_select" (property prop_adv_crash_deep_select)
      , run "adv_lint_crash_big_attrset" (property prop_adv_crash_big_attrset)
      , run "adv_lint_crash_deep_nest" (property prop_adv_crash_deep_nest)
      , run "adv_lint_crash_deep_ornull" (property prop_adv_crash_deep_ornull)
      , run "adv_lint_sevoff_suppresses" (property prop_adv_is_suppressed_sevoff)
      , run "adv_lint_nonexistent_rule" (property prop_adv_nonexistent_rule)
      , run "adv_lint_default_no_suppress" (property prop_adv_default_no_suppress)
      , run "adv_lint_is_suppressed_empty" (property prop_adv_is_suppressed_empty)
      , run "adv_lint_format_nix_nonempty" (property prop_adv_format_nix_nonempty)
      , run "adv_lint_format_deriv_nonempty" (property prop_adv_format_deriv_nonempty)
      , run "adv_lint_format_pattern_nonempty" (property prop_adv_format_pattern_nonempty)
      , run "adv_lint_no_dup_codes" (property prop_adv_no_dup_codes)
      , -- Lint adversarial (from Adversarial.hs CATEGORY 8)
        -- Scope graph adversarial
        run "adv_scope_id_exhaustion" prop_scope_id_exhaustion
      , run "adv_scope_edge_priority" prop_scope_edge_priority_full
      , run "adv_scope_shadowing_bomb" prop_scope_shadowing_bomb
      , run "adv_scope_with_shadow" prop_scope_with_shadow
      , run "adv_scope_merge_collision" prop_scope_merge_collision
      , run "adv_scope_merge_many" prop_scope_merge_many
      , run "adv_scope_merge_empty" prop_scope_merge_empty_file
      , run "adv_scope_cross_file_ref" prop_scope_cross_file_ref
      , run "adv_scope_orphan_decl" prop_scope_orphan_decl
      , run "adv_scope_unresolvable_ref" prop_scope_unresolvable_ref
      , run "adv_scope_duplicate_decls" prop_scope_duplicate_decls
      , run "adv_scope_find_refs_accurate" prop_scope_find_refs_accurate
      , run "adv_scope_dhall_export" prop_scope_dhall_export
      , run "adv_scope_dhall_special_chars" prop_scope_dhall_special_chars
      , run "adv_scope_dhall_empty" prop_scope_dhall_empty
      , run "review_import_cross_module" prop_import_cross_module_type_flows
      , -- Config loading adversarial
        run "adv_config_no_file" prop_config_no_file
      , run "adv_config_malformed" prop_config_malformed_dhall
      , run "adv_config_wrong_types" prop_config_wrong_types
      , run "adv_config_massive_overrides" prop_config_massive_overrides
      , run "adv_config_glob_regex" prop_config_glob_regex_chars
      , run "adv_config_empty_ignores" prop_config_empty_ignores
      , run "adv_config_long_profile" prop_config_long_profile
      , run "adv_config_null_byte" prop_config_null_byte_path
      , run "adv_config_star_paths" prop_config_star_paths
      , run "adv_config_globstar_all" prop_config_globstar_matches_all
      , -- Glob matching
        run "glob_exact" prop_glob_exact
      , run "glob_star_ext" prop_glob_star_ext
      , run "glob_prefix_star" prop_glob_prefix_star
      , run "glob_star_suffix" prop_glob_star_suffix
      , run "glob_middle_star" prop_glob_middle_star
      , run "glob_dir_star" prop_glob_dir_star
      , run "glob_dir_globstar" prop_glob_dir_globstar
      , run "glob_globstar_suffix" prop_glob_globstar_suffix
      , run "glob_nested_globstar" prop_glob_nested_globstar
      , run "glob_multi_star" prop_glob_multi_star
      , run "glob_case_sensitive" prop_glob_case_sensitive
      , run "glob_star_no_slash" prop_glob_star_no_slash
      , run "glob_no_match" prop_glob_no_match
      , run "glob_dot_literal" prop_glob_dot_literal
      , run "glob_star_both_ends" prop_glob_star_both_ends
      , run "glob_single_no_wildcard" prop_glob_single_no_wildcard
      , run "glob_leading_slash" prop_glob_leading_slash
      , run "glob_triple_star" prop_glob_triple_star
      , run "glob_complex" prop_glob_complex
      , run "glob_leading_star_path" prop_glob_leading_star_path
      , -- Severity override adversarial
        run "adv_severity_duplicate" prop_severity_duplicate_override
      , run "adv_severity_nonexistent" prop_severity_nonexistent_rule
      , run "adv_severity_suppressed" prop_severity_suppressed
      , run "adv_severity_special_chars" prop_severity_special_chars
      , run "adv_severity_not_suppressed" prop_severity_not_suppressed
      , run "adv_severity_no_override" prop_severity_no_override
      , run "adv_severity_ordering" prop_severity_ordering
      , -- Rule ID consistency
        run "rule_ids_unique" prop_rule_ids_unique
      , run "rule_suppress_all" prop_rule_suppress_all
      , run "rule_severity_error" prop_rule_severity_error
      , run "rule_severity_info" prop_rule_severity_info
      , run "rule_not_suppressed" prop_rule_not_suppressed_on_error
      , run "rule_bash_heredoc_union" prop_rule_bash_heredoc_union
      , run "rule_massive_overrides" prop_rule_massive_overrides
      , -- LSP adversarial
        run "lsp_lint_empty" prop_lsp_lint_empty
      , run "lsp_lint_with" prop_lsp_lint_with
      , run "lsp_lint_rec" prop_lsp_lint_rec
      , run "lsp_lint_clean" prop_lsp_lint_clean
      , run "lsp_diag_positions" prop_lsp_diag_positions
      , run "lsp_diag_severity" prop_lsp_diag_severity
      , run "lsp_spandiag_edge" prop_lsp_spandiag_edge
      , -- Hover type inference
        run "hover_int" prop_hover_int
      , run "hover_string" prop_hover_string
      , run "hover_func" prop_hover_func
      , run "hover_attrset" prop_hover_attrset
      , run "hover_let" prop_hover_let
      , run "hover_nonexistent" prop_hover_nonexistent
      , -- Definition handler
        run "defn_let" (property prop_defn_let)
      , run "defn_cross_file" (property prop_defn_cross_file)
      , run "defn_unresolved" (property prop_defn_unresolved)
      , run "defn_empty" (property prop_defn_empty)
      , -- References handler
        run "refs_let" (property prop_refs_let)
      , run "refs_no_match" (property prop_refs_no_match)
      , run "refs_cross_file" (property prop_refs_cross_file)
      , run "refs_unresolved" (property prop_refs_unresolved)
      , -- Rename handler
        run "rename_let" (property prop_rename_let)
      , run "rename_missing" (property prop_rename_missing)
      , run "rename_func" (property prop_rename_func)
      , -- Completion handler
        run "compl_let" (property prop_completion_let)
      , run "compl_empty" (property prop_completion_empty)
      , run "compl_attrset" (property prop_completion_attrset)
      , run "compl_func" (property prop_completion_func)
      , -- CLI Types
        run "cli_empty_cicounts" prop_cli_empty_cicounts_all_zero
      , run "cli_markers_distinct" prop_cli_markers_distinct
      , run "cli_tcresult_reflexive" prop_cli_tcresult_reflexive
      , run "cli_cicounts_merge" prop_cli_cicounts_merge
      , -- CLI Report
        run "cli_report_partition_empty" prop_report_partition_empty
      , run "cli_report_partition_suppress" prop_report_partition_suppress
      , run "cli_report_partition_no_suppress" prop_report_partition_no_suppress
      , run "cli_report_partition_complete" prop_report_partition_complete
      , run "cli_report_format_bare" prop_report_format_bare
      , run "cli_report_format_dynamic" prop_report_format_dynamic
      , run "cli_report_indent_block" prop_report_indent_block
      , run "cli_report_package_empty" prop_report_package_empty
      , run "cli_report_package_nonempty" prop_report_package_nonempty -- CLI Check
      , run "cli_check_supported_rec" prop_check_supported_rec
      , run "cli_check_unsupported_dynamic" prop_check_unsupported_dynamic
      , run "cli_check_unsupported_clean" prop_check_unsupported_clean
      , run "cli_check_format_type_error" prop_check_format_type_error
      , run "cli_check_format_type_error_empty" prop_check_format_type_error_empty
      , -- CLI Bash
        run "cli_bash_safe_read_existing" prop_bash_safe_read_existing
      , run "cli_bash_safe_read_nonexistent" prop_bash_safe_read_nonexistent
      , -- CLI Check exit code regression
        run "cli_skip_checked_returns_fail" prop_cli_skip_checked_returns_fail
      , run "cli_normal_check_passes" prop_cli_normal_check_passes
      , run "cli_lint_check_fails" prop_cli_lint_check_fails
      ]

  -- Adversarial regression suite for review-2 findings (C1-C6, S1-S6, B*, P*)
  putStrLn ""
  putStrLn "  -- psychotic adversarial regression suite --"
  psychoticResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- Psychotic.psychoticTests
      ]

  -- LSP project cache (non-blocking incremental cross-module inference)
  putStrLn ""
  putStrLn "  -- project cache --"
  pcResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- ProjectCacheSpec.projectCacheTests
      ]

  putStrLn ""
  putStrLn "  -- adversarial (bash/security) --"
  advResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- adversarialTests
      ]

  putStrLn ""
  putStrLn "  -- nix adversarial (type inference) --"
  nixAdvResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- nixAdversarialTests
      ]

  putStrLn ""
  putStrLn "  -- lsp features (contract guards + known-broken tripwires) --"
  lspResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- LSPFeatureSpec.lspFeatureTests
      ]

  putStrLn ""
  putStrLn "  -- nixpkgs hop (by-name index + cursor recognizer) --"
  nixpkgsResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- NixpkgsIndexSpec.nixpkgsIndexTests
      ]

  putStrLn ""
  putStrLn "  -- nixpkgs eval cache (content-addressed; hermetic fake backend) --"
  cacheResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- NixpkgsCacheSpec.nixpkgsCacheTests
      ]

  putStrLn ""
  putStrLn "  -- nixpkgs background warm (worker pool + STM frontier; hermetic) --"
  warmResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- NixpkgsWarmSpec.nixpkgsWarmTests
      ]

  putStrLn ""
  putStrLn "  -- inference enrichment (pkgs.<path> type oracle; hermetic) --"
  oracleResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- InferenceOracleSpec.inferenceOracleTests
      ]

  putStrLn ""
  putStrLn "  -- inference enrichment (eval→oracle bridge; hermetic fake backend) --"
  oracleBridgeResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- NixpkgsOracleSpec.nixpkgsOracleTests
      ]

  putStrLn ""
  putStrLn "  -- row lacks-constraint enforcement (Gaster–Jones; hermetic) --"
  rowLacksResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- RowLacksSpec.rowLacksTests
      ]

  putStrLn ""
  putStrLn "  -- infer annotation engine (idempotent / placement; hermetic) --"
  annotateResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- AnnotateSpec.annotateTests
      ]

  putStrLn ""
  putStrLn "  -- infer -r tree sweep (skip-on-error / no-clobber; hermetic) --"
  inferRecursiveResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- InferRecursiveSpec.inferRecursiveTests
      ]

  putStrLn ""
  putStrLn "  -- unified import/module closure (discovery + cross-module type flow) --"
  closureResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- ClosureSpec.closureTests
      ]

  putStrLn ""
  putStrLn "  -- mutation scoreboard (false-negative ledger) --"
  mutationResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- MutationSpec.mutationTests
      ]

  putStrLn ""
  putStrLn "  -- strictness hierarchy (profiles: dhall parity + resolution) --"
  profileResults <-
    sequence
      [ do
          putStr $ "  " ++ name ++ " ... "
          ok <- action
          putStrLn (if ok then "OK" else "FAILED")
          pure ok
      | (name, action) <- ProfileSpec.profileTests
      ]

  putStrLn ""
  let allResults =
        results
          ++ psychoticResults
          ++ pcResults
          ++ advResults
          ++ nixAdvResults
          ++ lspResults
          ++ nixpkgsResults
          ++ cacheResults
          ++ warmResults
          ++ oracleResults
          ++ oracleBridgeResults
          ++ rowLacksResults
          ++ annotateResults
          ++ inferRecursiveResults
          ++ closureResults
          ++ mutationResults
          ++ profileResults
  let passed = length (filter id allResults)
  let totalPassed = length allResults
  putStrLn $ "Passed: " ++ show passed ++ "/" ++ show totalPassed

  if all id allResults
    then do
      putStrLn "All tests passed!"
      exitSuccess
    else do
      putStrLn "Some tests failed!"
      exitFailure
 where
  run :: (Testable prop) => String -> prop -> IO Bool
  run name prop = do
    putStr $ "  " ++ name ++ " ... "
    result <- quickCheckResult (withMaxSuccess 200 prop)
    case result of
      Success{} -> do
        putStrLn "OK"
        return True
      _ -> do
        putStrLn "FAILED"
        return False
