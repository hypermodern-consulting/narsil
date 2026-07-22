{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Narsil.CLI.Report (
  partitionViolations,
  partitionNixViolations,
  partitionDerivViolations,
  partitionPackageViolations,
  partitionPatternViolations,
  formatBareCommand,
  formatDynamicCommand,
  indentBlock,
  formatPackageViolations,
  bareDiagnostic,
  dynamicDiagnostic,
  printCheckResult,
  emitDiagnostic,
  typeDiagnostic,
  attachSnippet,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, stderr)
import Text.Read (readMaybe)

import Narsil.Core.Config qualified as Config
import Narsil.Core.Diagnostic qualified as Diag
import Narsil.Core.Log
import Narsil.Core.Profiles qualified as Profiles
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Lint.Derivation qualified as Derivation
import Narsil.Lint.Forbidden (Violation (..))
import Narsil.Lint.Nix qualified as Lint
import Narsil.Lint.Packages qualified as LintPackages
import Narsil.Lint.Patterns qualified as LintPatterns

{- | Render a diagnostic in the unified clippy layout and log it at its own
severity (to stderr, per the stdout/stderr contract). Colour is enabled when
stderr is a terminal; a trailing newline separates consecutive diagnostics.
-}
emitDiagnostic :: Diag.Diagnostic -> AppM ()
emitDiagnostic d = do
  color <- liftIO (hIsTerminalDevice stderr)
  $(logTM) (Diag.diagSeverity d) $ logStr (Diag.renderDiagnostic color d <> "\n")

{- | Fill in a diagnostic's source snippet (line text + caret range) from the
file's text, when it has a span but no snippet yet.
-}
attachSnippet :: Text -> Diag.Diagnostic -> Diag.Diagnostic
attachSnippet src d
  | Just sp <- Diag.diagSpan d
  , isNothing (Diag.diagSnippet d)
  , (l : _) <- drop (locLine (spanStart sp) - 1) (T.lines src) =
      d
        { Diag.diagSnippet =
            Just
              Diag.Snippet
                { Diag.snLine = locLine (spanStart sp)
                , Diag.snText = l
                , Diag.snCol = locCol (spanStart sp)
                , Diag.snWidth = max 1 (locCol (spanEnd sp) - locCol (spanStart sp))
                }
        }
  | otherwise = d

{- | Build a TYPE diagnostic from an engine error string, parsing a leading
@"line:col: "@ prefix into a span when present.
-}
typeDiagnostic :: Severity -> FilePath -> Text -> Diag.Diagnostic
typeDiagnostic sev file raw =
  Diag.Diagnostic
    { Diag.diagSeverity = sev
    , Diag.diagCode = Just "TYPE"
    , Diag.diagSpan = mspan
    , Diag.diagSummary = summary
    , Diag.diagHelp = []
    , Diag.diagSnippet = Nothing
    }
 where
  (mspan, summary) = parseLoc (T.breakOn ": " raw)
  parseLoc (loc, rest)
    | not (T.null rest)
    , [lt, ct] <- T.splitOn ":" loc
    , Just l <- readMaybe (T.unpack lt)
    , Just c <- readMaybe (T.unpack ct) =
        (Just (Span (Loc l c) (Loc l c) (Just file)), T.drop 2 rest)
  parseLoc _ = (Nothing, raw)

{- | Split bash 'Violation's into @(suppressed, active)@ by the config's
suppression rules.
-}
partitionViolations :: Config.Config -> [Violation] -> ([Violation], [Violation])
partitionViolations config = foldr go ([], [])
 where
  go v (suppressed, active)
    | Profiles.isSuppressed config (Config.bashRuleId (vType v)) = (v : suppressed, active)
    | otherwise = (suppressed, v : active)

-- | Split Nix lint violations into @(suppressed, active)@ by the config's rules.
partitionNixViolations ::
  Config.Config -> [Lint.NixViolation] -> ([Lint.NixViolation], [Lint.NixViolation])
partitionNixViolations config = foldr go ([], [])
 where
  go v (suppressed, active)
    | Profiles.isSuppressed config (Config.nixRuleId (Lint.nvType v)) = (v : suppressed, active)
    | otherwise = (suppressed, v : active)

-- | Split derivation lint violations into @(suppressed, active)@ by config rules.
partitionDerivViolations ::
  Config.Config ->
  [Derivation.DerivViolation] ->
  ([Derivation.DerivViolation], [Derivation.DerivViolation])
partitionDerivViolations config = foldr go ([], [])
 where
  go v (suppressed, active)
    | Profiles.isSuppressed config (Config.derivRuleId (Derivation.dvType v)) =
        (v : suppressed, active)
    | otherwise = (suppressed, v : active)

-- | Split package-directory violations into @(suppressed, active)@ by config rules.
partitionPackageViolations ::
  Config.Config ->
  [LintPackages.PackageViolation] ->
  ([LintPackages.PackageViolation], [LintPackages.PackageViolation])
partitionPackageViolations config = foldr go ([], [])
 where
  go v (suppressed, active)
    | Profiles.isSuppressed config (Config.packageRuleId (LintPackages.pvCode v)) =
        (v : suppressed, active)
    | otherwise = (suppressed, v : active)

-- | Split pattern lint violations into @(suppressed, active)@ by config rules.
partitionPatternViolations ::
  Config.Config ->
  [LintPatterns.PatternViolation] ->
  ([LintPatterns.PatternViolation], [LintPatterns.PatternViolation])
partitionPatternViolations config = foldr go ([], [])
 where
  go v (suppressed, active)
    | Profiles.isSuppressed config (Config.patternRuleId (LintPatterns.pvType v)) =
        (v : suppressed, active)
    | otherwise = (suppressed, v : active)

{- | Render a bare-command finding (ALEPH-B005) as a clippy-style text block,
given the source path and the command's name + span.
-}
formatBareCommand :: Text -> (Text, Span) -> Text
formatBareCommand src (cmd, sourceSpan) =
  let tok = locLine (spanStart sourceSpan)
   in T.unlines
        [ "error[ALEPH-B005]: bare command not allowed: " <> cmd
        , "  --> " <> src <> ":" <> T.pack (show tok)
        , ""
        , "  Use an explicit store path for external commands:"
        , "    /nix/store/...-pkg/bin/" <> cmd
        ]

{- | Render a dynamic-command finding (ALEPH-B006) as a clippy-style text block,
given the source path and the variable's name + span.
-}
formatDynamicCommand :: Text -> (Text, Span) -> Text
formatDynamicCommand src (var, sourceSpan) =
  let tok = locLine (spanStart sourceSpan)
   in T.unlines
        [ "error[ALEPH-B006]: dynamic command not allowed: $" <> var
        , "  --> " <> src <> ":" <> T.pack (show tok)
        , ""
        , "  Dynamic command selection is not statically analyzable."
        , "  Use a known store path or a case statement over a small allowlist."
        ]

-- | Prefix every line of a multi-line text block with the given prefix.
indentBlock :: Text -> Text -> Text
indentBlock prefix block =
  T.unlines [prefix <> line | line <- T.lines block]

{- | Render package-directory violations (ALEPH-P001) as a text block listing the
offending paths; @""@ when there are none.
-}
formatPackageViolations :: [LintPackages.PackageViolation] -> Text
formatPackageViolations [] = ""
formatPackageViolations violations =
  T.unlines
    [ "ALEPH-P001: Package directories must contain a `default.nix` file:"
    , ""
    ]
    <> T.unlines (map (\violation -> "  " <> T.pack (LintPackages.pvPath violation)) violations)

-- | A bare-command fact as a unified 'Diagnostic'.
bareDiagnostic :: (Text, Span) -> Diag.Diagnostic
bareDiagnostic (cmd, sp) =
  Diag.Diagnostic
    { Diag.diagSeverity = ErrorS
    , Diag.diagCode = Just "ALEPH-B005"
    , Diag.diagSpan = Just sp
    , Diag.diagSummary = "bare command not allowed: " <> cmd
    , Diag.diagHelp = ["use an explicit store path for external commands"]
    , Diag.diagSnippet = Nothing
    }

-- | A dynamic-command fact as a unified 'Diagnostic'.
dynamicDiagnostic :: (Text, Span) -> Diag.Diagnostic
dynamicDiagnostic (var, sp) =
  Diag.Diagnostic
    { Diag.diagSeverity = ErrorS
    , Diag.diagCode = Just "ALEPH-B006"
    , Diag.diagSpan = Just sp
    , Diag.diagSummary = "dynamic command not allowed: $" <> var
    , Diag.diagHelp = ["use a known store path or a case statement over a small allowlist"]
    , Diag.diagSnippet = Nothing
    }

{- | Exit a single-file check by its total error count: 'exitSuccess' when zero,
'exitFailure' otherwise (diagnostics were already emitted).
-}
printCheckResult :: FilePath -> Int -> AppM ()
printCheckResult _file totalErrors
  | totalErrors > 0 = liftIO exitFailure
  | otherwise = liftIO exitSuccess
