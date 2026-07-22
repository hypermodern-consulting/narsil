{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                // tests // inference // annotate
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He wrote the truth of the thing right onto its face, where the next
--    one to look would read it."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The @infer@ annotation engine: inject @# :: <type>@ comments. The properties
--   that make it safe to run (and re-run, in place):
--
--     * IDEMPOTENT — re-annotating already-annotated source reproduces it exactly
--       (prior @# ::@ lines are stripped before re-rendering);
--     * 'stripAnnotations' removes only our marker, leaving ordinary comments;
--     * ONE annotation per source line — inline / nested bindings sharing a line
--       collapse to the leftmost, instead of stacking;
--     * a binding on its own line still gets its annotation.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module AnnotateSpec (annotateTests) where

import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Inference.Nix.Annotate (annotateExpr, stripAnnotations)

-- ── helpers ────────────────────────────────────────────────────────

-- | How many @# ::@ annotation lines a rendering contains.
annCount :: Text -> Int
annCount = length . filter (("# ::" `T.isPrefixOf`) . T.stripStart) . T.lines

-- ── tests ──────────────────────────────────────────────────────────

-- | Re-annotating already-annotated source reproduces it exactly (idempotent).
testIdempotent :: IO Bool
testIdempotent =
  pure $ case annotateExpr "let\n  a = 1;\n  b = \"x\";\nin a" of
    Right once -> annotateExpr once == Right once
    Left _ -> False

-- | 'stripAnnotations' drops our marker lines but keeps ordinary comments + code.
testStrip :: IO Bool
testStrip =
  pure (stripAnnotations "  # :: Int\n  x = 1;\n  # a note\n" == "  x = 1;\n  # a note\n")

-- | Multiple bindings on one line collapse to a single annotation (the leftmost).
testOnePerLine :: IO Bool
testOnePerLine =
  let src = "let\n  p = { x = 1; y = 2; };\nin p"
   in pure $ either (const False) ((== 1) . annCount) (annotateExpr src)

-- | Bindings on their own lines each get exactly one annotation.
testEachOwnLine :: IO Bool
testEachOwnLine =
  pure $ either (const False) ((== 2) . annCount) (annotateExpr "let\n  a = 1;\n  b = 2;\nin a")

-- ── runner ──────────────────────────────────────────────────────────

-- | The @infer@ annotation-engine tests (hermetic; pure, no eval).
annotateTests :: [(String, IO Bool)]
annotateTests =
  [ ("annotate_idempotent", testIdempotent)
  , ("annotate_strip_keeps_comments", testStrip)
  , ("annotate_one_per_line", testOnePerLine)
  , ("annotate_each_own_line", testEachOwnLine)
  ]
