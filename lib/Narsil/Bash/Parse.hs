{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                // bash // parsing
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "When Beauvoir or I talk to you about the loa and their horses, as we
--    call those few the loa choose to ride, you should pretend that we are
--    talking two languages at once. One of them, you already understand.
--    That's the language of street tech, as you call it. We may be using
--    different words, but we're talking tech."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                           // shellcheck // bridge
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Bash.Parse (
  parseBash,
  parseBashWithFilename,
  parseBashFile,
  BashAST (..),
)
where

import Control.Monad.Identity (Identity, runIdentity)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Safety qualified as Safety
import ShellCheck.AST qualified as SA
import ShellCheck.Interface (
  ParseResult (..),
  ParseSpec (..),
  Position (..),
  SystemInterface (..),
  newParseSpec,
  newSystemInterface,
 )
import ShellCheck.Parser (parseScript)

-- | The AST from ShellCheck with source positions
data BashAST = BashAST
  { astRoot :: SA.Token
  , astPositions :: Map.Map SA.Id (Position, Position)
  }
  deriving (Show, Eq)

-- | Parse bash source text
parseBash :: Text -> Either Text BashAST
parseBash = parseBashWithFilename "<input>"

{- | Parse bash source text with an associated filename.

ShellCheck includes the filename in diagnostics; we also propagate it into
'Span's at higher layers.
-}
parseBashWithFilename :: FilePath -> Text -> Either Text BashAST
parseBashWithFilename filename sourceText =
  let parseSpec =
        newParseSpec
          { psFilename = filename
          , psScript = T.unpack sourceText
          }
      result = runIdentity $ parseScript sysInterface parseSpec
   in maybe
        (Left $ T.pack $ "Parse errors: " ++ show (length (prComments result)))
        (\astRoot -> Right $ BashAST astRoot (prTokenPositions result))
        (prRoot result)
 where
  sysInterface :: SystemInterface Identity
  sysInterface =
    newSystemInterface
      { siReadFile = \_ _ -> return (Left "no file access")
      }

{- | Parse a bash file.
n.b. routes through Safety to catch StackOverflow and other async exceptions
that try @IOException misses.
-}
parseBashFile :: FilePath -> IO (Either Text BashAST)
parseBashFile path = do
  readResult <- Safety.safeReadFile path
  either (pure . Left . Safety.renderSafetyError) fromContent readResult
 where
  fromContent content = do
    parseAttempt <- Safety.safeIO (pure (parseBashWithFilename path content))
    either (pure . Left . Safety.renderSafetyError) pure parseAttempt
