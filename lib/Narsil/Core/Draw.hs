{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                   // core // draw
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Lines of light ranged in the nonspace of the mind, clusters and
--    constellations of data."
--
--                                                                                     — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Drawing primitives — the one home for the box-drawing vocabulary of
--   doc/TYPOGRAPHY.md. Runtime code that prints a heading or a divider asks for
--   a 'Weight' and gets the right Unicode glyph; it never spells out a '━' (or,
--   worse, an ASCII '='). Centralised so the next ergonomics change is one edit
--   here, not a sweep across every call site. (Source-file banners are comments
--   pinned to column 100 by tooling and live outside this module.)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Core.Draw (
  Weight (..),
  glyph,
  rule,
  framed,
)
where

import Data.Text (Text)
import Data.Text qualified as T

{- | Horizontal-rule weights, per doc/TYPOGRAPHY.md: 'Heavy' frames a file or
module, 'Double' a major section, 'Light' a subsection.
-}
data Weight = Heavy | Double | Light
  deriving (Eq, Show)

{- | The box-drawing glyph for a weight — the single place these characters
live. 'Heavy' is @━@, 'Double' is @═@, 'Light' is @─@.
-}
glyph :: Weight -> Char
glyph Heavy = '━'
glyph Double = '═'
glyph Light = '─'

-- | A horizontal rule: @n@ columns of the weight's glyph (clamped at zero).
rule :: Weight -> Int -> Text
rule weight n = T.replicate (max 0 n) (T.singleton (glyph weight))

{- | An inline framed heading for runtime output — @═══ title ═══@, three
glyphs each side. For headings printed to a terminal; source banners are
comments and do not go through here.
-}
framed :: Weight -> Text -> Text
framed weight title = bar <> " " <> title <> " " <> bar
 where
  bar = rule weight 3
