{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                   // tests // infer // recursive
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He told the machine to walk the whole house and touch nothing it
--    could not name."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   @infer -r@ over a tree is only trustworthy if each file is handled
--   independently and never clobbered. Driven against a real on-disk temp tree
--   with the identity enricher (no nixpkgs, hermetic), these pin the properties
--   that make "point it at a whole repo" safe:
--
--     * a well-typed file is annotated in place ('Wrote') and gains the marker;
--     * a second pass is idempotent — 'AlreadyOk', byte-identical, no rewrite;
--     * a parse error is 'InferSkipped' and the file's bytes are left untouched.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module InferRecursiveSpec (inferRecursiveTests) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Narsil.CLI.Dispatch (InferOutcome (..), inferOneFile)
import Narsil.Layout.Closure qualified as Closure
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

-- ── helpers ────────────────────────────────────────────────────────

-- | The identity enricher: no nixpkgs oracle, so the whole probe is hermetic.
idEnrich :: a -> b -> IO b
idEnrich _ env = pure env

-- | Write @src@ to @name@ inside a fresh temp dir and run the action on its path.
withFile :: FilePath -> Text -> (FilePath -> IO a) -> IO a
withFile name src act =
  withSystemTempDirectory "infer-r" $ \dir -> do
    let path = dir </> name
    TIO.writeFile path src
    act path

hasMarker :: Text -> Bool
hasMarker = T.isInfixOf "# ::"

-- | 'inferOneFile' with a fresh single-use closure cache (each test is one file).
inferOne1 :: FilePath -> IO InferOutcome
inferOne1 path = do
  cache <- Closure.newClosureCache
  inferOneFile cache idEnrich path

-- ── tests ──────────────────────────────────────────────────────────

-- | A well-typed file is rewritten in place and gains the @# ::@ marker.
testAnnotates :: IO Bool
testAnnotates =
  withFile "good.nix" "let\n  a = 1;\n  b = \"x\";\nin a\n" $ \path -> do
    outcome <- inferOne1 path
    after <- TIO.readFile path
    pure (outcome == Wrote && hasMarker after)

-- | A second pass is a no-op: 'AlreadyOk', and the bytes are byte-identical.
testIdempotent :: IO Bool
testIdempotent =
  withFile "good.nix" "let\n  a = 1;\n  b = \"x\";\nin a\n" $ \path -> do
    _ <- inferOne1 path
    once <- TIO.readFile path
    outcome <- inferOne1 path
    twice <- TIO.readFile path
    pure (outcome == AlreadyOk && once == twice)

-- | A parse error is skipped and the file's bytes are left exactly as written.
testSkipsParseError :: IO Bool
testSkipsParseError =
  let src = "let a = ( in a\n"
   in withFile "broken.nix" src $ \path -> do
        outcome <- inferOne1 path
        after <- TIO.readFile path
        pure (isSkip outcome && after == src)
 where
  isSkip (InferSkipped _) = True
  isSkip _ = False

-- ── runner ──────────────────────────────────────────────────────────

-- | The @infer -r@ tree-sweep tests (hermetic; identity enricher, temp tree).
inferRecursiveTests :: [(String, IO Bool)]
inferRecursiveTests =
  [ ("infer_r_annotates_in_place", testAnnotates)
  , ("infer_r_idempotent_no_rewrite", testIdempotent)
  , ("infer_r_skips_parse_error_untouched", testSkipsParseError)
  ]
