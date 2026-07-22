{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                 // lsp // handlers // diagnostics
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Information sickness. He'd read about it, the price of too much knowing."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The lint → LSP 'Diagnostic' layer: run every checker (nix lint, derivation
--   lint, pattern lint, embedded-bash lint) over a parsed expression and render
--   each finding as an editor diagnostic. Pure (no parsing, no IO) — the
--   handler module owns the parse and hands us the 'NExprLoc'.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.LSP.Handlers.Diagnostics (
  -- * Whole-expression diagnostics
  diagnosticsForExpr,
  diagnosticsForExprWith,
  moduleModeEnv,

  -- * Single-finding rendering (used across handlers / tests)
  toNixDiag,
  nixCode,
  spToDiagnostic,

  -- * Re-exported nix-lint vocabulary
  NixViolation (..),
  ViolationType (..),
)
where

import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types
import Nix.Expr.Types.Annotated (NExprLoc)
import Text.Read (readMaybe)

import Narsil.Bash.Parse (parseBash)
import Narsil.Core.Config qualified as Config
import Narsil.Core.Profiles qualified as Profiles
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Inference.Nix (TypeEnv (..), builtinEnv, inferExprWithEnv)
import Narsil.Layout.ModuleKind (ModuleKind (..), detectKind, detectedKind)
import Narsil.Lint.Derivation qualified as Deriv
import Narsil.Lint.Forbidden qualified as Forbidden
import Narsil.Lint.Nix (NixViolation (..), ViolationType (..), findNixViolations)
import Narsil.Lint.Patterns qualified as Patterns
import Narsil.Syntax.Parse qualified as NixParse

{- | Every diagnostic for a parsed expression: TYPE ERRORS (the product) +
nix lint + derivation lint + pattern lint + embedded-bash lint, in one list,
FILTERED by the project config's suppression rules (profile chain + explicit
overrides) — the same 'Profiles.isSuppressed' \/ severity judgment the CLI
applies, so the editor and the command line disagree about nothing. @path@
labels derivation findings and drives module-kind detection (a flake\/module
file infers in module mode, exactly as `narsil check` would).

This baseline variant infers against 'builtinEnv'; the handlers pass the
cache-backed cross-module env through 'diagnosticsForExprWith' instead.
-}
diagnosticsForExpr :: Config.Config -> FilePath -> NExprLoc -> [Diagnostic]
diagnosticsForExpr config = diagnosticsForExprWith config builtinEnv

-- | 'diagnosticsForExpr' against a caller-supplied inference environment.
diagnosticsForExprWith :: Config.Config -> TypeEnv -> FilePath -> NExprLoc -> [Diagnostic]
diagnosticsForExprWith config env path expr =
  concat
    [ typeDiags config env path expr
    , nixVios' config expr
    , derivVios' config path expr
    , patternVios' config expr
    , embeddedBashDiags config expr
    ]

{- | The inference verdict as editor diagnostics, at the severity the config
gives @type-check-failure@ (default Error; the `nixpkgs` profile remaps to
Warning; 'SevOff' silences). Module-shaped files infer with module params
dynamic — the same 'detectKind' dispatch as the CLI check path.
-}
typeDiags :: Config.Config -> TypeEnv -> FilePath -> NExprLoc -> [Diagnostic]
typeDiags config env path expr =
  maybe [] (pure . toDiag) (verdict (Profiles.effectiveSeverity config Config.typeCheckRuleId))
 where
  verdict (Just Config.SevOff) = Nothing
  verdict msev =
    either (\err -> Just (sevOf msev, err)) (const Nothing) (inferExprWithEnv env' expr)
  sevOf (Just Config.SevWarning) = DiagnosticSeverity_Warning
  sevOf (Just Config.SevInfo) = DiagnosticSeverity_Information
  sevOf _ = DiagnosticSeverity_Error
  env' = moduleModeEnv env path expr
  toDiag (sev, err) =
    let (sp, summary) = typeErrLoc (T.breakOn ": " err)
     in (spToDiagnostic ("type: " <> summary) sp)
          { _severity = Just sev
          , _code = Just (InR "type-check-failure")
          }
  -- inference errors carry a "line:col: " prefix when a span was in scope
  typeErrLoc (loc, rest)
    | not (T.null rest)
    , [lt, ct] <- T.splitOn ":" loc
    , Just l <- readMaybe (T.unpack lt)
    , Just c <- readMaybe (T.unpack ct) =
        (Span (Loc l c) (Loc l c) Nothing, T.drop 2 rest)
  typeErrLoc (raw, _) = (Span (Loc 1 1) (Loc 1 1) Nothing, raw)

live :: Config.Config -> (a -> Text) -> [a] -> [a]
live config ruleIdOf = filter (not . Profiles.isSuppressed config . ruleIdOf)

nixVios' :: Config.Config -> NExprLoc -> [Diagnostic]
nixVios' config expr =
  map toNixDiag (live config (Config.nixRuleId . nvType) (findNixViolations expr))

derivVios' :: Config.Config -> FilePath -> NExprLoc -> [Diagnostic]
derivVios' config path expr =
  map
    toDerivDiag
    (live config (Config.derivRuleId . Deriv.dvType) (Deriv.findDerivViolations path expr))

toDerivDiag :: Deriv.DerivViolation -> Diagnostic
toDerivDiag dv =
  spToDiagnostic
    (Deriv.derivRuleId (Deriv.dvType dv) <> ": " <> derivMsg (Deriv.dvType dv))
    (Deriv.dvSpan dv)
 where
  derivMsg Deriv.VMissingMeta = "mkDerivation call without meta attribute"
  derivMsg Deriv.VMissingDescription = "meta = { ... } without description key"

patternVios' :: Config.Config -> NExprLoc -> [Diagnostic]
patternVios' config expr =
  map
    toPatternDiag
    (live config (Config.patternRuleId . Patterns.pvType) (Patterns.findPatternViolations expr))

toPatternDiag :: Patterns.PatternViolation -> Diagnostic
toPatternDiag pv =
  spToDiagnostic
    (patternRuleId (Patterns.pvType pv) <> ": " <> Patterns.pvContext pv)
    (Patterns.pvSpan pv)
 where
  patternRuleId Patterns.VOrNullFallback = "or-null-fallback"
  patternRuleId Patterns.VAttrTranslation = "no-translate-attrs-outside-prelude"

embeddedBashDiags :: Config.Config -> NExprLoc -> [Diagnostic]
embeddedBashDiags config expr =
  concatMap (bashDiagFromCall config) (NixParse.findShellScriptCalls expr)

bashDiagFromCall :: Config.Config -> NixParse.ShellScriptCall -> [Diagnostic]
bashDiagFromCall config ssc = maybe [] withContent (NixParse.extractString (NixParse.sscBody ssc))
 where
  withContent (content, _, _) = either (const []) withAST (parseBash content)
  withAST ast =
    map
      (toBashDiag (NixParse.sscName ssc))
      (live config (Config.bashRuleId . Forbidden.vType) (Forbidden.findViolations ast))

toBashDiag :: Text -> Forbidden.Violation -> Diagnostic
toBashDiag scriptName v =
  spToDiagnostic
    ( bashErrorCode (Forbidden.vType v)
        <> ": "
        <> bashLabel (Forbidden.vType v)
        <> " in embedded script '"
        <> scriptName
        <> "'"
    )
    (Forbidden.vSpan v)
 where
  bashLabel Forbidden.VHeredoc = "heredoc (<<) not allowed"
  bashLabel Forbidden.VHereString = "here-string (<<<) not allowed"
  bashLabel Forbidden.VEval = "eval not allowed"
  bashLabel Forbidden.VBacktick = "backticks (`...`) not allowed"

bashErrorCode :: Forbidden.ViolationType -> Text
bashErrorCode Forbidden.VHeredoc = "NARSIL-B001"
bashErrorCode Forbidden.VHereString = "NARSIL-B002"
bashErrorCode Forbidden.VEval = "NARSIL-B003"
bashErrorCode Forbidden.VBacktick = "NARSIL-B004"

{- | The env adjusted for the FILE KIND: flake\/module-shaped files infer with
module params dynamic and the declared option spine bound — the same
'detectKind' dispatch as `narsil check`. Hover\/inlay\/diagnostics must all
use this or the editor's features disagree with each other about the same
buffer.
-}
moduleModeEnv :: TypeEnv -> FilePath -> NExprLoc -> TypeEnv
moduleModeEnv env path expr
  | moduleMode = env{envModuleParams = True}
  | otherwise = env
 where
  kind = detectedKind (detectKind path expr)
  moduleMode = kind `elem` [Flake, FlakeModule, NixOSModule, HomeModule, DarwinModule]

-- | Render one nix-lint 'NixViolation' as an LSP 'Diagnostic' (code + context).
toNixDiag :: NixViolation -> Diagnostic
toNixDiag NixViolation{nvType = vt, nvSpan = sp, nvContext = ctx} =
  spToDiagnostic (nixCode vt <> ": " <> ctx) sp

-- | The @NARSIL-N*@ rule code string for a nix-lint 'ViolationType'.
nixCode :: ViolationType -> Text
nixCode VWith = "NARSIL-N001"
nixCode VRec = "NARSIL-N002"
nixCode VSubstituteAll = "NARSIL-N005"
nixCode VRawMkDerivation = "NARSIL-N006"
nixCode VRawRunCommand = "NARSIL-N007"
nixCode VRawWriteShellApplication = "NARSIL-N008"
nixCode VWriteShellScript = "NARSIL-N011"
nixCode (VLongInlineString n) = "NARSIL-N012 (" <> T.pack (show n) <> " chars)"
nixCode (VNonLispCase name) = "NARSIL-N015 (`" <> name <> "`)"

{- | Build an error-severity LSP 'Diagnostic' from a message and a 1-based
  source 'Span', converting to 0-based LSP positions (clamped at zero).
-}
spToDiagnostic :: Text -> Span -> Diagnostic
spToDiagnostic msg (Span (Loc line col) (Loc endL endC) _) =
  -- n.b. ShellCheck positions are 0-based; megaparsec positions are 1-based.
  -- Clamp to zero rather than wrap unsigned underflow (B4 from review-2).
  Diagnostic
    { _range =
        Range
          (Position (clampU32 (line - 1)) (clampU32 (col - 1)))
          (Position (clampU32 (endL - 1)) (clampU32 (endC - 1)))
    , _severity = Just DiagnosticSeverity_Error
    , _code = Nothing
    , _codeDescription = Nothing
    , _source = Just "narsil"
    , _message = msg
    , _tags = Nothing
    , _relatedInformation = Nothing
    , _data_ = Nothing
    }
 where
  clampU32 :: Int -> UInt
  clampU32 n
    | n < 0 = 0
    | otherwise = fromIntegral n
