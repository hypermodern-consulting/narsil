{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Narsil.CLI.Check (
  checkFile,
  checkFileShared,
  checkWithViolations,
  performTypeCheck,
  formatTypeError,
  detectUnsupportedConstruct,
  detectUnsupportedBinding,

  -- * Re-exports
  Safety.maxRecursionDepth,
)
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (fromRight)
import Data.Text qualified as T
import Nix.Expr.Types
import Nix.Expr.Types.Annotated (NExprLoc)

import Narsil.CLI.Report
import Narsil.CLI.Types
import Narsil.Core.Config qualified as Config
import Narsil.Core.Diagnostic qualified as Diag
import Narsil.Core.Log
import Narsil.Core.Profiles qualified as Profiles
import Narsil.Core.Safety qualified as Safety
import Narsil.Inference.Nix (TypeEnv (..), builtinEnv, inferExprWithEnv)
import Narsil.Inference.Nix.Type qualified
import Narsil.Layout.Closure qualified as Closure
import Narsil.Layout.ModuleKind (ModuleKind (..), detectKind, detectedKind)
import Narsil.Lint.Combined qualified as Combined
import Narsil.Lint.Derivation qualified as Derivation
import Narsil.Lint.Nix qualified as Lint
import Narsil.Lint.Patterns qualified as Patterns
import Narsil.Syntax.Annotation (pattern Layer)
import Narsil.Syntax.Parse qualified as Nix

{- | Parse, depth-guard, and check one .nix file: emits lint and type
diagnostics and returns the overall 'TCResult' (a parse/depth failure is
'TCFail'; unsupported constructs skip the type check).
-}
checkFile :: Config.Config -> FilePath -> AppM TCResult
checkFile = checkFileShared Nothing

{- | 'checkFile' with an optional sweep-shared closure cache: the CI type-check
phase passes one so N files over shared deps pay one closure per file reached,
not one full rebuild per file swept.
-}
checkFileShared :: Maybe Closure.ClosureCache -> Config.Config -> FilePath -> AppM TCResult
checkFileShared mCache config file = do
  parseResult <- liftIO $ Nix.parseNixFile file
  either onParseError afterParse parseResult
 where
  onParseError parseError = do
    $(logTM) ErrorS $
      logStr $
        T.unlines
          [ ""
          , "━━━ " <> crossMarker <> " " <> T.pack file <> " ━━━"
          , ""
          , "  " <> parseError
          , ""
          ]
    return TCFail

  -- past parse: enforce the depth guard, then check (unless an unsupported
  -- construct means we skip the type-check phase)
  afterParse expression =
    either
      (onDepthExceeded expression)
      (const (afterDepth expression))
      (Safety.analyzeDepth expression)

  onDepthExceeded _ de = do
    $(logTM) ErrorS $
      logStr $
        crossMarker
          <> " "
          <> T.pack file
          <> " (depth limit exceeded: "
          <> Safety.renderSafetyError (Safety.SafetyDepthExceeded de)
          <> ")"
    return TCFail

  afterDepth expression =
    maybe
      (checkWithViolations mCache config file expression False)
      (skipTypeCheck expression)
      (detectUnsupportedConstruct expression)

  skipTypeCheck expression reason = do
    $(logTM) DebugS $
      logStr $
        unsupMarker <> " " <> T.pack file <> " (skipping type check: " <> reason <> ")"
    checkWithViolations mCache config file expression True

{- | Run the combined lint suite and (unless @skipTypeCheck@) the type check on
an already-parsed expression, emitting diagnostics and folding both into a
single 'TCResult'.
-}
checkWithViolations ::
  Maybe Closure.ClosureCache -> Config.Config -> FilePath -> NExprLoc -> Bool -> AppM TCResult
checkWithViolations mCache config file expression skipTypeCheck = do
  let bundle = Combined.combinedLint file expression
  let (_, activeNixViolations) = partitionNixViolations config (Combined.lbNix bundle)
  let (_, activeDerivViolations) = partitionDerivViolations config (Combined.lbDeriv bundle)
  let (_, activePatternViolations) = partitionPatternViolations config (Combined.lbPattern bundle)

  -- read the source once so lint diagnostics can show the offending line + caret
  srcResult <- liftIO (Safety.safeReadFile file)
  let src = fromRight "" srcResult
      emitAll toDiag = mapM_ (emitDiagnostic . attachSnippet src . toDiag)
  emitAll Lint.nixViolationDiagnostic activeNixViolations
  emitAll Derivation.derivViolationDiagnostic activeDerivViolations
  emitAll Patterns.patternViolationDiagnostic activePatternViolations

  typeCheckResult <- performTypeCheck mCache config file expression skipTypeCheck
  let allClean =
        null activeNixViolations
          && null activeDerivViolations
          && null activePatternViolations
      report TCFail = return TCFail
      report TCOk
        | skipTypeCheck = do
            $(logTM) DebugS $
              logStr $
                crossMarker <> " " <> T.pack file <> " (unsupported construct — type check skipped)"
            return TCFail
        | allClean = do
            $(logTM) DebugS $ logStr $ okMarker <> " " <> T.pack file
            return TCOk
      report _ = do
        $(logTM) DebugS $ logStr $ crossMarker <> " " <> T.pack file <> " (lint violations)"
        return TCFail
  report typeCheckResult

{- | Infer the expression's type (module-mode for flakes/modules, strict env
otherwise), emitting a TYPE diagnostic on error at the rule's configured
severity. Returns 'TCOk' when @skipTypeCheck@, clean, or the rule is off.
-}
performTypeCheck ::
  Maybe Closure.ClosureCache -> Config.Config -> FilePath -> NExprLoc -> Bool -> AppM TCResult
performTypeCheck mCache config file expression skipTypeCheck
  | skipTypeCheck = return TCOk
  | otherwise = do
      -- Flakes and module-system files take their top-level parameters
      -- (self, inputs, config, pkgs, …) from the flake / module system, so we
      -- infer them in module mode (those params are dynamic). Everything else
      -- uses the strict builtin env. Either way, seed the cross-module
      -- import/callPackage closure (synchronous, eval-free — see
      -- 'Narsil.Layout.Closure') so an `import ./dep.nix` /
      -- `callPackage ./pkg.nix` resolves to its real type instead of a dynamic.
      let kind = detectedKind (detectKind file expression)
          moduleMode = kind `elem` [Flake, FlakeModule, NixOSModule, HomeModule, DarwinModule]
      crossEnv <- liftIO (maybe Closure.closureEnv Closure.closureEnvShared mCache builtinEnv file)
      let env = if moduleMode then crossEnv{envModuleParams = True} else crossEnv
      -- n.b. `either` forces inference to WHNF inside the `try`, so an exception
      -- from (pure but partial) inference is caught here; `prettyType` itself
      -- stays a thunk, exactly as the old `case` left it.
      result <-
        liftIO $
          try $
            either
              (pure . Left)
              (pure . Right . Narsil.Inference.Nix.Type.prettyType . fst)
              (inferExprWithEnv env expression)
      handleResult result
 where
  handleResult (Left exception) = do
    emitDiagnostic $
      Diag.Diagnostic
        { Diag.diagSeverity = ErrorS
        , Diag.diagCode = Just "INTERNAL"
        , Diag.diagSpan = Nothing
        , Diag.diagSummary =
            "internal error (this is a bug in narsil): "
              <> T.pack (show (exception :: SomeException))
        , Diag.diagHelp = []
        , Diag.diagSnippet = Nothing
        }
    return TCFail
  handleResult (Right (Left typeError)) =
    bySeverity (Profiles.effectiveSeverity config Config.typeCheckRuleId)
   where
    bySeverity (Just Config.SevOff) = return TCOk
    bySeverity (Just Config.SevWarning) = emitType WarningS typeError >> return TCOk
    bySeverity _ = emitType ErrorS typeError >> return TCFail
  handleResult (Right (Right _)) = return TCOk

  -- build a TYPE diagnostic and attach the source line/caret from the file
  emitType sev typeError = do
    srcResult <- liftIO (Safety.safeReadFile file)
    let base = typeDiagnostic sev file typeError
    emitDiagnostic (either (const base) (`attachSnippet` base) srcResult)

-- | Format a multi-line type-error string as an indented @TYPE WARNING:@ block.
formatTypeError :: T.Text -> T.Text
formatTypeError errorText = format (T.lines errorText)
 where
  format (firstLine : remainingLines) =
    T.unlines $
      ("  TYPE WARNING: " <> firstLine) : map ("         " <>) remainingLines
  format [] = "  TYPE WARNING: unknown error"

{- | Detect AST shapes that are syntactically valid but semantically unsupported.
n.b. depth checking now lives in 'Narsil.Core.Safety.analyzeDepth' and runs BEFORE
this; we only flag rec/dynamic-key here, never depth.
-}
detectUnsupportedConstruct :: NExprLoc -> Maybe T.Text
detectUnsupportedConstruct = go
 where
  -- n.b. `rec { … }` is NOT unsupported: inference has SCC-based recursive
  -- bindings ('inferRecursiveBindings'), and this stale guard was skipping
  -- 11,715 nixpkgs files (93% of the coverage ceiling, per the oracle sweep).
  -- Dynamic attribute ACCESS (`x.${e}`) and TESTS (`x ? ${e}`) are not
  -- unsupported either: 'inferSelect' resolves a dynamic key to a fresh var
  -- and 'inferHasAttr' type-checks the antiquotation — that stale flag was
  -- the remaining coverage ceiling (992 files). Dynamic key BINDINGS
  -- (`{ ${k} = v; }`) are handled by typing the set as an OPEN record
  -- ('inferAttrSet'), so nothing here needs flagging.
  go (Layer (NAbs _ body)) = go body
  go (Layer (NLet bindings body)) = foldl (<|>) (go body) (map detectUnsupportedBinding bindings)
  go (Layer (NSet _ bindings)) = foldl (<|>) Nothing (map detectUnsupportedBinding bindings)
  go (Layer (NList elements)) = foldl (<|>) Nothing (map go elements)
  go (Layer (NBinary _ left right)) = go left <|> go right
  go (Layer (NUnary _ arg)) = go arg
  go (Layer (NSelect _ base _)) = go base
  go (Layer (NHasAttr base _)) = go base
  go (Layer (NApp function arg)) = go function <|> go arg
  go (Layer (NIf cond thenBranch elseBranch)) = go cond <|> go thenBranch <|> go elseBranch
  go (Layer (NAssert cond body)) = go cond <|> go body
  go (Layer (NWith scope body)) = go scope <|> go body
  go (Layer (NStr (DoubleQuoted parts))) = foldl (<|>) Nothing (map goAnti parts)
  go (Layer (NStr (Indented _ parts))) = foldl (<|>) Nothing (map goAnti parts)
  go _ = Nothing

  goAnti (Antiquoted e) = go e
  goAnti _ = Nothing

{- | Detect an unsupported construct inside a single let/attrset binding,
recursing into its value (and into the source of an @inherit (e) …@).
-}
detectUnsupportedBinding :: Binding NExprLoc -> Maybe T.Text
detectUnsupportedBinding (NamedVar _ e _) = detectUnsupportedConstruct e
detectUnsupportedBinding (Inherit (Just s) _ _) = detectUnsupportedConstruct s
detectUnsupportedBinding (Inherit Nothing _ _) = Nothing
