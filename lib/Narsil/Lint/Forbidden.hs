{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // lint // forbidden
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "They set a slamhound on Turner's trail in New Delhi, slotted it to
--    his pheromones and the color of his hair."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // lint // detection
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Lint.Forbidden (
  -- * Types
  Violation (..),
  ViolationType (..),

  -- * Detection
  findViolations,

  -- * Formatting
  formatViolation,
  formatViolations,
  formatViolationAt,
  formatViolationsAt,
  violationDiagnostic,
)
where

import Control.Monad.Reader (Reader, ask, runReader)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Katip (Severity (ErrorS))
import Narsil.Bash.Parse (BashAST (..))
import Narsil.Core.Diagnostic (Diagnostic (..))
import Narsil.Core.Span (Loc (..), Span (..))
import ShellCheck.AST qualified as SA
import ShellCheck.Interface (Position (..))

-- | the kind of forbidden bash construct: heredoc, here-string, @eval@, or backticks.
data ViolationType
  = VHeredoc
  | VHereString
  | VEval
  | VBacktick
  deriving (Eq, Show)

-- | one detected forbidden construct: its kind, source span, and a short context label.
data Violation = Violation
  { vType :: !ViolationType
  , vSpan :: !Span
  , vContext :: !Text
  }
  deriving (Eq, Show)

-- | walk a parsed bash AST and collect every forbidden-construct violation.
findViolations :: BashAST -> [Violation]
findViolations (BashAST root posMap) = runReader (go root) posMap

go :: SA.Token -> Reader (Map SA.Id (Position, Position)) [Violation]
go (SA.OuterToken shellCheckId inner) = do
  local <- localViolations shellCheckId inner
  nested <- mapM go (toList inner)
  pure (local ++ concat nested)

localViolations ::
  SA.Id -> SA.InnerToken SA.Token -> Reader (Map SA.Id (Position, Position)) [Violation]
localViolations shellCheckId inner = do
  violationSpan <- mkSpan shellCheckId
  dispatch violationSpan inner
 where
  dispatch violationSpan SA.Inner_T_HereDoc{} =
    pure [Violation VHeredoc violationSpan "heredoc (<<)"]
  dispatch violationSpan SA.Inner_T_HereString{} =
    pure [Violation VHereString violationSpan "here-string (<<<)"]
  dispatch violationSpan SA.Inner_T_Backticked{} =
    pure [Violation VBacktick violationSpan "backticks (`...`)"]
  dispatch _ (SA.Inner_T_SimpleCommand _ commandWords) = checkForEval shellCheckId commandWords
  dispatch _ _ = pure []

checkForEval :: SA.Id -> [SA.Token] -> Reader (Map SA.Id (Position, Position)) [Violation]
checkForEval tokenId commandWords
  | isEvalInvocation commandWords = do
      violationSpan <- mkSpan tokenId
      pure [Violation VEval violationSpan "eval"]
  | otherwise = pure []

{- | Detect an `eval` invocation, including `eval` hidden behind a command
modifier (`command eval …`, `builtin eval …`, `time`/`nice`/`sudo`/…).
`echo eval` is NOT flagged (eval is just an argument there). (REVIEW-3 #23)
-}
isEvalInvocation :: [SA.Token] -> Bool
isEvalInvocation = scan . map tokenToText
 where
  scan (w : _) | w == "eval" = True
  scan (w : rest) | w `elem` evalModifiers = scan rest
  scan _ = False
  -- words that run their remaining arguments as a command
  evalModifiers :: [Text]
  evalModifiers =
    [ "command"
    , "builtin"
    , "exec"
    , "env"
    , "time"
    , "nice"
    , "ionice"
    , "sudo"
    , "nohup"
    , "setsid"
    , "stdbuf"
    ]

tokenToText :: SA.Token -> Text
tokenToText (SA.OuterToken _ inner) = innerToText inner

innerToText :: SA.InnerToken SA.Token -> Text
innerToText (SA.Inner_T_Literal content) = T.pack content
innerToText (SA.Inner_T_SingleQuoted content) = T.pack content
innerToText (SA.Inner_T_NormalWord parts) = T.concat (map tokenToText parts)
innerToText (SA.Inner_T_DoubleQuoted parts) = T.concat (map tokenToText parts)
innerToText _ = ""

mkSpan :: SA.Id -> Reader (Map SA.Id (Position, Position)) Span
mkSpan tokenId = do
  positionMap <- ask
  pure $ lookupPosition tokenId positionMap

lookupPosition :: SA.Id -> Map SA.Id (Position, Position) -> Span
lookupPosition tokenId positionMap
  | Just (startPosition, endPosition) <- Map.lookup tokenId positionMap =
      -- n.b. ShellCheck positions are 1-based (matching megaparsec); the prior
      -- REVIEW erroneously claimed they were 0-based. No adjustment needed.
      Span
        (Loc (fromIntegral $ posLine startPosition) (fromIntegral $ posColumn startPosition))
        (Loc (fromIntegral $ posLine endPosition) (fromIntegral $ posColumn endPosition))
        (Just (posFile startPosition))
  | otherwise =
      Span (Loc 0 0) (Loc 0 0) Nothing

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // output formatting
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | A forbidden-construct violation as a unified 'Diagnostic'.
violationDiagnostic :: Violation -> Diagnostic
violationDiagnostic Violation{..} =
  Diagnostic
    { diagSeverity = ErrorS
    , diagCode = Just (forbiddenErrorCode vType)
    , diagSpan = Just vSpan
    , diagSummary = forbiddenTypeLabel vType <> " not allowed"
    , diagHelp = take 1 (filter (not . T.null) (map T.strip (T.lines (forbiddenSuggestion vType))))
    , diagSnippet = Nothing
    }

-- | render one violation as a rustc-style @error[CODE]@ block, with @src@ as the file name.
formatViolationAt :: Text -> Violation -> Text
formatViolationAt src Violation{..} =
  T.unlines
    [ "error[" <> forbiddenErrorCode vType <> "]: " <> forbiddenTypeLabel vType <> " not allowed"
    , "  --> " <> src <> ":" <> T.pack (show line)
    , ""
    , forbiddenSuggestion vType
    ]
 where
  line = locLine (spanStart vSpan)

-- ── violation type labels ──────────────────────────────────────────
-- Short, human-readable classification strings for each violation type.

forbiddenTypeLabel :: ViolationType -> Text
forbiddenTypeLabel VHeredoc = "heredoc"
forbiddenTypeLabel VHereString = "here-string"
forbiddenTypeLabel VEval = "eval"
forbiddenTypeLabel VBacktick = "backtick"

-- ── error codes ────────────────────────────────────────────────────
-- Stable ALEPH-Bxxx codes. B-prefix denotes bash/shell violations.

forbiddenErrorCode :: ViolationType -> Text
forbiddenErrorCode VHeredoc = "ALEPH-B001"
forbiddenErrorCode VHereString = "ALEPH-B002"
forbiddenErrorCode VEval = "ALEPH-B003"
forbiddenErrorCode VBacktick = "ALEPH-B004"

-- ── remediation suggestions ────────────────────────────────────────
-- Each forbidden bash construct has a suggested replacement. The text
-- includes concrete code examples because the target audience is
-- developers who may not know the idiomatic narsil alternatives.
-- n.b. heredoc replacements reference pkgs.writeText, which requires
-- a Nix context — the user must plumb the path through their build.

forbiddenSuggestion :: ViolationType -> Text
forbiddenSuggestion VHeredoc =
  T.unlines
    [ "  Prefer narsil's generated emitter for structured config:"
    , "    emit-config json   # or: yaml | toml"
    , ""
    , "  Or printf for simple strings:"
    , "    printf 'Hello, %s\\n' \"$NAME\""
    , ""
    , "  Or generate content in Nix, reference in bash:"
    , "    cat ${pkgs.writeText \"msg\" ''...''}"
    ]
forbiddenSuggestion VHereString =
  T.unlines
    [ "  Use echo with pipe:"
    , "    echo \"string\" | command"
    , ""
    , "  Or printf:"
    , "    printf '%s' \"string\" | command"
    ]
forbiddenSuggestion VEval =
  T.unlines
    [ "  eval is forbidden. Refactor to avoid dynamic code execution."
    , ""
    , "  If you need to set variables dynamically:"
    , "    declare \"$name=$value\""
    , ""
    , "  If you need to choose between commands:"
    , "    case \"$mode\" in"
    , "      a) /nix/store/...-tool/bin/tool ... ;;"
    , "      b) /nix/store/...-other/bin/other ... ;;"
    , "    esac"
    ]
forbiddenSuggestion VBacktick =
  T.unlines
    [ "  Use $() instead of backticks:"
    , "    result=$(command)"
    , ""
    , "  Not:"
    , "    result=`command`"
    ]

-- | render one violation with the placeholder file name @\<input\>@.
formatViolation :: Violation -> Text
formatViolation = formatViolationAt "<input>"

-- | render a list of violations (blank-line separated) with @src@ as the file name.
formatViolationsAt :: Text -> [Violation] -> Text
formatViolationsAt _ [] = ""
formatViolationsAt src violations = T.intercalate "\n" (map (formatViolationAt src) violations)

-- | render a list of violations with the placeholder file name @\<input\>@.
formatViolations :: [Violation] -> Text
formatViolations = formatViolationsAt "<input>"
