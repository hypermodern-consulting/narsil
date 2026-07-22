{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                        // tests // emit // config
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "She unfolded the paper and found herself holding a new Braun
--    holoprojector and a flat envelope of clear plastic. The envelope
--    contained seven numbered tabs of holofiche. The box she'd seen in
--    Virek's simulation of the Güell Park blossomed above the Braun, glowing
--    with the crystal resolution of the finest museum-grade holograms. Bone
--    and circuit-gold, dead lace, and a dull white marble rolled from clay.
--    Marly shook her head. How could anyone have arranged these bits, this
--    garbage, in such a way that it caught at the heart, snagged in the soul
--    like a fishhook?"
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                        // emit // config // tests
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Narsil.Bash.Types
import qualified Narsil.Core.Draw as Draw
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Emit.Config
import Narsil.Syntax.Pretty (toText)
import System.Exit (exitFailure, exitSuccess)

-- | Empty span for tests
testSpan :: Span
testSpan = Span (Loc 1 1) (Loc 1 1) Nothing

-- | Test schema with a string and int config value
testSchema :: Schema
testSchema =
  emptySchema
    { schemaConfig =
        Map.fromList
          [
            ( ["server", "host"]
            , ConfigSpec
                { cfgType = TString
                , cfgFrom = Just "HOST"
                , cfgLit = Nothing
                , cfgQuoted = Just Quoted
                , cfgSpan = testSpan
                }
            )
          ,
            ( ["server", "port"]
            , ConfigSpec
                { cfgType = TInt
                , cfgFrom = Just "PORT"
                , cfgLit = Nothing
                , cfgQuoted = Nothing
                , cfgSpan = testSpan
                }
            )
          ,
            ( ["debug"]
            , ConfigSpec
                { cfgType = TBool
                , cfgFrom = Nothing
                , cfgLit = Just (LitBool True)
                , cfgQuoted = Nothing
                , cfgSpan = testSpan
                }
            )
          ]
    }

-- | Test that JSON output has correct structure
testJSONOutput :: Bool
testJSONOutput =
  let json = emitConfigJSON testSchema
   in -- JSON should contain printf, proper quoting
      T.isInfixOf "printf" json
        && T.isInfixOf "server" json
        && T.isInfixOf "host" json
        && T.isInfixOf "port" json
        && T.isInfixOf "$HOST" json -- String var should be expanded
        && T.isInfixOf "$PORT" json -- Int var should be expanded
        && T.isInfixOf "true" json -- Boolean literal

-- | Test that YAML output has correct structure
testYAMLOutput :: Bool
testYAMLOutput =
  let yaml = emitConfigYAML testSchema
   in T.isInfixOf "printf" yaml
        && T.isInfixOf "server:" yaml
        && T.isInfixOf "  host:" yaml -- Proper indentation
        && T.isInfixOf "  port:" yaml

-- | Test that TOML output has correct structure
testTOMLOutput :: Bool
testTOMLOutput =
  let toml = emitConfigTOML testSchema
   in T.isInfixOf "printf" toml
        && T.isInfixOf "[server]" toml
        && T.isInfixOf "host = " toml
        && T.isInfixOf "port = " toml

-- | Test config key validation
testKeyValidation :: Bool
testKeyValidation =
  isValidConfigKey "validKey"
    && isValidConfigKey "valid_key"
    && isValidConfigKey "valid-key"
    && isValidConfigKey "_private"
    && not (isValidConfigKey "")
    && not (isValidConfigKey "123start")
    && not (isValidConfigKey "has space")
    && not (isValidConfigKey "has.dot")

-- | Test that invalid keys are detected
testInvalidKeyDetection :: Bool
testInvalidKeyDetection =
  let badSchema = Map.singleton ["bad.key"] (ConfigSpec TString Nothing Nothing Nothing testSpan)
   in not (null (validateConfigKeys badSchema))

{- | Test JSON escape function correctness
The escape function must produce valid JSON when run
-}
testJSONEscapeInOutput :: Bool
testJSONEscapeInOutput =
  let func = toText (emitConfigFunction testSchema)
   in -- The generated bash should have the correct escape pattern
      -- s=${s//\"/\\\"} which replaces " with \"
      -- In Haskell literal: s=${s//\\\"/\\\\\\\"} (pattern: \" replacement: \\\")
      T.isInfixOf "s=${s//\\\"" func -- Has the quote pattern
        && T.isInfixOf "__nix_compile_escape_json" func -- Has the function

-- | Run all tests
main :: IO ()
main = do
  TIO.putStrLn (Draw.framed Draw.Double "EmitConfig Generator Tests")
  results <-
    sequence
      [ check "JSON output structure" testJSONOutput
      , check "YAML output structure" testYAMLOutput
      , check "TOML output structure" testTOMLOutput
      , check "Config key validation" testKeyValidation
      , check "Invalid key detection" testInvalidKeyDetection
      , check "JSON escape function" testJSONEscapeInOutput
      ]
  if and results
    then do
      TIO.putStrLn (Draw.framed Draw.Double "All tests passed")
      exitSuccess
    else do
      TIO.putStrLn (Draw.framed Draw.Double "Some tests failed")
      exitFailure
 where
  check name True = do
    putStrLn $ "  ✓ " ++ name
    return True
  check name False = do
    putStrLn $ "  ✗ " ++ name ++ " FAILED"
    return False
