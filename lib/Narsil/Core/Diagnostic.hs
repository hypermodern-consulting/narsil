{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // nix // diagnostic
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   One diagnostic model for every checker (nix lint, bash lint, type, layout,
--   package, parse) and one pure renderer. See doc/design/output-rework.md.
--
--   The renderer is deliberately a pure 'Diagnostic -> Text' so the visual style
--   (currently rustc/clippy carets-and-gutter) is cheap to change and easy to
--   golden-test. Colour and stream routing live in the katip layer, not here.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Core.Diagnostic (
  Diagnostic (..),
  Snippet (..),
  severityWord,
  renderDiagnostic,
)
where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Katip (Severity (..))
import Narsil.Core.Span (Loc (..), Span (..))

-- | A single source line plus the caret range to underline within it.
data Snippet = Snippet
  { snLine :: !Int
  -- ^ 1-based source line number
  , snText :: !Text
  -- ^ the source line (no trailing newline)
  , snCol :: !Int
  -- ^ 1-based column where the underline starts
  , snWidth :: !Int
  -- ^ underline width in columns (rendered as at least one caret)
  }
  deriving (Eq, Show)

{- | A finding from any checker. @diagSpan@ drives the @file:line:col@ location
line; @diagSnippet@ (optional) adds the source line + caret block.
-}
data Diagnostic = Diagnostic
  { diagSeverity :: !Severity
  , diagCode :: !(Maybe Text)
  , diagSpan :: !(Maybe Span)
  , diagSummary :: !Text
  , diagHelp :: ![Text]
  , diagSnippet :: !(Maybe Snippet)
  }
  deriving (Eq, Show)

-- | the lowercase word shown for a severity in the header (@error@, @warning@, @note@, @debug@).
severityWord :: Severity -> Text
severityWord DebugS = "debug"
severityWord InfoS = "note"
severityWord WarningS = "warning"
severityWord ErrorS = "error"
severityWord _ = "note"

tshow :: Int -> Text
tshow = T.pack . show

{- | Render a diagnostic in the rustc/clippy idiom. With @color@ on, the severity
tag is bold-coloured, the gutter/arrow/@=@ are blue, and the carets take the
severity colour (selective styling, like rustc — not a single flat colour). With
@color@ off the output is plain (and byte-identical to the golden test), e.g.

@
error[NARSIL-N001]: `with` expression is not allowed
  --> flake.nix:90:7
   |
90 |   with pkgs; [ git ];
   |   ^^^^^^^^^
   = help: use `inherit (pkgs) git;` instead
@
-}
renderDiagnostic :: Bool -> Diagnostic -> Text
renderDiagnostic color d =
  T.intercalate "\n" (header : locLines <> snippetBlock <> helpLines)
 where
  sty :: Text -> Text -> Text
  sty codes t
    | color = "\ESC[" <> codes <> "m" <> t <> "\ESC[0m"
    | otherwise = t
  sevCodes = codesFor (diagSeverity d)
  codesFor ErrorS = "1;31" -- bold red
  codesFor WarningS = "1;33" -- bold yellow
  codesFor DebugS = "1;36" -- bold cyan
  codesFor _ = "1;36"
  sev = sty sevCodes
  bold = sty "1"
  blue = sty "1;34" -- gutter / arrow / `=`
  header = sev (severityWord (diagSeverity d) <> codePart) <> bold (": " <> diagSummary d)
  codePart = maybe "" (\c -> "[" <> c <> "]") (diagCode d)

  gutterW =
    maybe
      (maybe 1 (T.length . tshow . locLine . spanStart) (diagSpan d))
      (T.length . tshow . snLine)
      (diagSnippet d)
  pad n = T.replicate (max 0 n) " "
  bar = blue (pad gutterW <> " |")

  locLines = maybe [] locFor (diagSpan d)
  locFor sp =
    [ pad gutterW
        <> blue "--> "
        <> T.pack (stripDot (fromMaybe "<input>" (spanFile sp)))
        <> ":"
        <> tshow (locLine (spanStart sp))
        <> ":"
        <> tshow (locCol (spanStart sp))
    ]
  stripDot p = fromMaybe p (stripPrefix "./" p)

  snippetBlock = maybe [] snippetFor (diagSnippet d)
  snippetFor s =
    [ bar
    , blue (T.justifyRight gutterW ' ' (tshow (snLine s)) <> " |") <> " " <> snText s
    , bar <> " " <> pad (snCol s - 1) <> sev (T.replicate (max 1 (snWidth s)) "^")
    ]

  helpLines = map (\h -> blue (pad gutterW <> " =") <> " " <> bold "help:" <> " " <> h) (diagHelp d)
