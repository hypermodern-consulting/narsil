-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                          // nix // compile // api
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "As her fingers closed around the cool brass knob, it seemed to squirm,
--    sliding along a touch spectrum of texture and temperature in the first
--    second of contact. Then it became metal again, green-painted iron,
--    sweeping out and down, along a line of perspective, an old railing she
--    grasped now in wonder. A few drops of rain blew into her face."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                               // top-level // api
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil (
  -- * Parsing
  parseScript,
  parseScriptFile,

  -- * Schema
  Schema (..),
  EnvSpec (..),
  ConfigSpec (..),
  CommandSpec (..),

  -- * Types
  Type (..),
  Literal (..),
  StorePath (..),

  -- * Errors
  TypeError (..),

  -- * Config
  Config.Config (..),
  Config.RuleOverride (..),
  Config.loadConfig,
  Config.defaultConfig,
  Profiles.effectiveSeverity,
  Config.configIgnores,
  Profiles.isIgnored,
  Profiles.isSuppressed,
  Config.bashRuleId,
  Config.nixRuleId,
  Config.derivRuleId,

  -- * Re-exports
  module Narsil.Bash.Types,
  module Narsil.Core.Span,
)
where

import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Narsil.Bash.Facts (extractFacts)
import Narsil.Bash.Parse (parseBash, parseBashWithFilename)
import Narsil.Bash.Types
import Narsil.Core.Config qualified as Config
import Narsil.Core.Profiles qualified as Profiles
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Inference.Bash.Constraint (factsToConstraints)
import Narsil.Inference.Bash.Schema (buildSchema, validateConfigPaths)
import Narsil.Inference.Bash.Unify (solve)

{- | Parse a bash script and extract its schema.

This variant has no filename context, so spans in the returned 'Script'
will have 'spanFile = Nothing'.
-}
parseScript :: Text -> Either Text Script
parseScript = parseScriptWithFile Nothing

{- | Parse a bash script file.

When parsing from a file, we propagate the file path into 'Span's
(best-effort; bash spans are still "token id" based).
-}
parseScriptFile :: FilePath -> IO (Either Text Script)
parseScriptFile path = do
  result <- try (TIO.readFile path)
  either
    (\(ex :: IOException) -> return $ Left $ T.pack $ show ex)
    (return . parseScriptWithFile (Just path))
    result

-- | Internal worker that allows attaching a file path to spans.
parseScriptWithFile :: Maybe FilePath -> Text -> Either Text Script
parseScriptWithFile mFile src = do
  ast <- maybe (parseBash src) (`parseBashWithFilename` src) mFile
  let facts0 = extractFacts ast
      facts = attachFileToFacts mFile facts0
      constraints = factsToConstraints facts
  validateConfigPaths facts
  subst <- either (Left . T.pack . show) Right (solve constraints)
  let schema = buildSchema facts subst
  Right
    Script
      { scriptSource = src
      , scriptFacts = facts
      , scriptSchema = schema
      }

-- | Propagate a file path into all spans, for more useful diagnostics.
attachFileToFacts :: Maybe FilePath -> [Fact] -> [Fact]
attachFileToFacts mFile = map (attachFileToFact mFile)

attachFileToFact :: Maybe FilePath -> Fact -> Fact
attachFileToFact mFile (DefaultIs v lit sp) = DefaultIs v lit (attachFileToSpan mFile sp)
attachFileToFact mFile (DefaultFrom v o sp) = DefaultFrom v o (attachFileToSpan mFile sp)
attachFileToFact mFile (Required v sp) = Required v (attachFileToSpan mFile sp)
attachFileToFact mFile (AssignFrom v o sp) = AssignFrom v o (attachFileToSpan mFile sp)
attachFileToFact mFile (AssignLit v lit sp) = AssignLit v lit (attachFileToSpan mFile sp)
attachFileToFact mFile (ConfigAssign p v q sp) = ConfigAssign p v q (attachFileToSpan mFile sp)
attachFileToFact mFile (ConfigLit p lit sp) = ConfigLit p lit (attachFileToSpan mFile sp)
attachFileToFact mFile (ConfigTemplate p parts q sp) =
  ConfigTemplate p parts q (attachFileToSpan mFile sp)
attachFileToFact mFile (CmdArg c a v sp) = CmdArg c a v (attachFileToSpan mFile sp)
attachFileToFact mFile (UsesStorePath p sp) = UsesStorePath p (attachFileToSpan mFile sp)
attachFileToFact mFile (BareCommand c sp) = BareCommand c (attachFileToSpan mFile sp)
attachFileToFact mFile (DynamicCommand v sp) = DynamicCommand v (attachFileToSpan mFile sp)

attachFileToSpan :: Maybe FilePath -> Span -> Span
attachFileToSpan Nothing sp = sp
attachFileToSpan (Just file) sp = sp{spanFile = Just file}
