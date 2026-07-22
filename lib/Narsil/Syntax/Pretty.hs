{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                       // nix // compile // pretty
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The box was a universe, a poem, frozen on the boundaries of human
--    experience."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                            // output // rendering
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Syntax.Pretty (
  -- * Re-exports
  module Prettyprinter,
  module Prettyprinter.Render.Terminal,

  -- * Standard Styles
  styleType,
  styleVar,
  styleKeyword,
  styleString,
  stylePath,
  styleError,
  styleWarning,
  styleInfo,
  styleSuccess,
  styleMuted,

  -- * Helpers
  renderStdOut,
  renderStdErr,
  toText,

  -- * Layout Helpers
  block,
  property,
)
where

import System.IO (stderr, stdout)

import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.Terminal

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Styles
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | annotate a doc as a type name (cyan).
styleType :: Doc AnsiStyle -> Doc AnsiStyle
styleType = annotate (color Cyan)

-- | annotate a doc as a variable (blue).
styleVar :: Doc AnsiStyle -> Doc AnsiStyle
styleVar = annotate (color Blue)

-- | annotate a doc as a keyword (bold magenta).
styleKeyword :: Doc AnsiStyle -> Doc AnsiStyle
styleKeyword = annotate (color Magenta <> bold)

-- | annotate a doc as a string literal (green).
styleString :: Doc AnsiStyle -> Doc AnsiStyle
styleString = annotate (color Green)

-- | annotate a doc as a path (yellow).
stylePath :: Doc AnsiStyle -> Doc AnsiStyle
stylePath = annotate (color Yellow)

-- | annotate a doc as an error (bold red).
styleError :: Doc AnsiStyle -> Doc AnsiStyle
styleError = annotate (color Red <> bold)

-- | annotate a doc as a warning (bold yellow).
styleWarning :: Doc AnsiStyle -> Doc AnsiStyle
styleWarning = annotate (color Yellow <> bold)

-- | annotate a doc as an informational note (bold blue).
styleInfo :: Doc AnsiStyle -> Doc AnsiStyle
styleInfo = annotate (color Blue <> bold)

-- | annotate a doc as a success message (bold green).
styleSuccess :: Doc AnsiStyle -> Doc AnsiStyle
styleSuccess = annotate (color Green <> bold)

-- | annotate a doc as muted/de-emphasized text (bold black, i.e. bright grey).
styleMuted :: Doc AnsiStyle -> Doc AnsiStyle
styleMuted = annotate (color Black <> bold)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Helpers
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | smart-layout and render a styled doc to stdout, preserving ANSI colour.
renderStdOut :: Doc AnsiStyle -> IO ()
renderStdOut = renderIO stdout . layoutSmart defaultLayoutOptions

-- | smart-layout and render a styled doc to stderr, tinting the whole thing red.
renderStdErr :: Doc AnsiStyle -> IO ()
renderStdErr = renderIO stderr . layoutSmart defaultLayoutOptions . annotate (color Red)

-- | smart-layout and render a styled doc to plain 'Text', discarding ANSI styling.
toText :: Doc AnsiStyle -> Text
toText = renderStrict . layoutSmart defaultLayoutOptions

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Layout Helpers
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | lay out a brace-delimited block: @header {@, the body indented two spaces, then @}@.
block :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
block header body =
  vsep
    [ header <+> lbrace
    , indent 2 body
    , rbrace
    ]

-- | lay out a single @key = value;@ assignment line.
property :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
property key value = key <+> equals <+> value <> semi
