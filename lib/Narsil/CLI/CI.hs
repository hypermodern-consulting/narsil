{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Narsil.CLI.CI (
  cmdCI,
  runCIPhases,
  runTypeCheckPhase,
  runGraphPhase,
  runNixPhase,
  runPackagePhase,
  reportCISummary,
  collectFiles,
  wrapCheckFile,
)
where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSemN (newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import Control.Monad (foldM, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (isPrefixOf)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Conc (getNumCapabilities)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (makeRelative, pathSeparator, takeDirectory, takeExtension, (</>))

import Narsil.CLI.Bash
import Narsil.CLI.Check
import Narsil.CLI.Report
import Narsil.CLI.Types
import Narsil.Core.Config qualified as Config
import Narsil.Core.Diagnostic qualified as Diag
import Narsil.Core.Log
import Narsil.Core.Profiles qualified as Profiles
import Narsil.Core.Safety qualified as Safety
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Layout.Closure qualified as Closure
import Narsil.Layout.Convention qualified as Layout
import Narsil.Layout.Graph qualified as Mod
import Narsil.Lint.Packages qualified as LintPackages

{- | @check <dir>@: run all CI phases over a directory, then print the aggregate
summary and exit (0 if clean, non-zero on any failure/violation).
-}
cmdCI :: Config.Config -> FilePath -> AppM ()
cmdCI config dir = do
  counts <- runCIPhases config dir
  reportCISummary counts

{- | Run the five CI phases (type-check, graph, bash, packages, layout) over a
directory and accumulate their findings into a single 'CICounts'. Graph/bash
phases run only when a @flake.nix@ is present.
-}
runCIPhases :: Config.Config -> FilePath -> AppM CICounts
runCIPhases config dir = do
  let flakePath = dir </> "flake.nix"
  hasFlake <- liftIO $ doesFileExist flakePath

  $(logTM) DebugS $ logStr "Phase 1/5: type-check"

  typeCounts <- runTypeCheckPhase config dir

  $(logTM) DebugS $ logStr $ "Phase 2/5: graph  (hasFlake=" <> T.pack (show hasFlake) <> ")"

  graphCounts <-
    if hasFlake
      then runGraphPhase config flakePath
      else pure emptyCICounts

  $(logTM) DebugS $ logStr "Phase 3/5: bash"

  bashCounts <-
    if hasFlake
      then runNixPhase config flakePath
      else pure emptyCICounts

  $(logTM) DebugS $ logStr "Phase 4/5: packages"

  files <- liftIO $ collectFiles config dir
  pkgCounts <- runPackagePhase config files

  $(logTM) DebugS $ logStr "Phase 5/5: layout"

  layoutCount <- runLayoutPhase config dir

  pure $
    CICounts
      { ciFilesScanned = ciFilesScanned typeCounts
      , ciTypePass = ciTypePass typeCounts
      , ciTypeFail = ciTypeFail typeCounts
      , ciTypeSkip = ciTypeSkip typeCounts
      , ciLintViolations = ciLintViolations graphCounts
      , ciPackageViolations = pkgCounts
      , ciBashViolations = ciBashViolations bashCounts
      , ciGraphFailures = ciGraphFailures graphCounts
      , ciLayoutViolations = layoutCount
      }

{- | Type-check phase: collect every .nix file under the dir and check each
concurrently (bounded by the capability count), tallying ok/skip/fail counts.
-}
runTypeCheckPhase :: Config.Config -> FilePath -> AppM CICounts
runTypeCheckPhase config dir = do
  files <- liftIO $ collectFiles config dir

  loggingEnv <- getLogEnv
  loggingCtx <- getKatipContext
  loggingNamespace <- getKatipNamespace

  numCapabilities <- liftIO getNumCapabilities
  let maxConcurrency = numCapabilities
  concurrencySemaphore <- liftIO $ newQSemN maxConcurrency
  -- one shared closure cache for the whole phase: files over shared deps reuse
  -- each other's cross-module types instead of rebuilding the closure per file
  closureCache <- liftIO Closure.newClosureCache
  results <- liftIO $ forConcurrently files $ \file ->
    bracket_ (waitQSemN concurrencySemaphore 1) (signalQSemN concurrencySemaphore 1) $
      wrapCheckFile closureCache config (loggingEnv, loggingCtx, loggingNamespace) file

  let okCount = length [() | result <- results, result == TCOk]
  let skipCount = length [() | result <- results, result == TCSkip]
  let failCount = length [() | result <- results, result == TCFail]

  pure $
    CICounts
      { ciFilesScanned = length files
      , ciTypePass = okCount
      , ciTypeFail = failCount
      , ciTypeSkip = skipCount
      , ciLintViolations = 0
      , ciPackageViolations = 0
      , ciBashViolations = 0
      , ciGraphFailures = 0
      , ciLayoutViolations = 0
      }

{- | Graph phase: build the import-reachable module graph from the flake and
tally its lint violations and build failures (emits only the aggregate count).
-}
runGraphPhase :: Config.Config -> FilePath -> AppM CICounts
runGraphPhase _config flakePath = do
  $(logTM) DebugS $ logStr $ "  building module graph from " <> T.pack flakePath
  graphResult <- liftIO $ Mod.buildModuleGraphFromFlake (takeDirectory flakePath)
  $(logTM) DebugS $ logStr "  graph build complete"
  either onGraphError onGraph graphResult
 where
  onGraphError err = do
    $(logTM) ErrorS $ logStr $ "Graph error: " <> err
    pure $ emptyCICounts{ciGraphFailures = 1}

  -- n.b. layout is enforced by runLayoutPhase (a tree walk rooted at the project
  -- dir, so relative paths and orphan files are handled correctly); the graph
  -- phase covers only the import-reachable lint findings.
  onGraph graph = do
    -- Never hide partial coverage: the closure walk truncates at its node
    -- bound on gigantic graphs, and a truncated graph phase must say so.
    when (Mod.mgTruncated graph) $
      $(logTM) WarningS $
        logStr
          ( "Graph walk truncated at the closure node bound — counts cover a partial graph" ::
              T.Text
          )
    let lintCount = sum (map (length . Mod.lfViolations) (Mod.mgLintFailures graph))
     in if lintCount > 0 || not (null (Mod.mgFailures graph))
          then do
            -- n.b. the per-file type-check phase already prints the detailed
            -- lint violations for every on-disk file, so the graph phase only
            -- emits the aggregate count line here — re-dumping each violation
            -- double-printed everything the flake graph shares with the
            -- type-check walk.
            $(logTM) ErrorS $
              logStr $
                "\nGraph violations: "
                  <> T.pack (show lintCount)
                  <> " lint across "
                  <> T.pack (show (length (Mod.mgLintFailures graph)))
                  <> " files"
            pure $
              emptyCICounts
                { ciLintViolations = lintCount
                , ciGraphFailures = length (Mod.mgFailures graph)
                }
          else pure emptyCICounts

{- | Bash phase: extract embedded shell scripts from the flake's .nix files and
check each, returning the total bash-violation count.
-}
runNixPhase :: Config.Config -> FilePath -> AppM CICounts
runNixPhase config flakePath = do
  scripts <- parseNixFiles flakePath
  totalErrors <- sum <$> mapM (checkScript config flakePath) scripts
  pure $
    emptyCICounts
      { ciBashViolations = totalErrors
      }

{- | Package phase: check that package directories carry a @default.nix@; emit
the (non-suppressed) violations and return their count.
-}
runPackagePhase :: Config.Config -> [FilePath] -> AppM Int
runPackagePhase config files = do
  packageViolations <- liftIO $ LintPackages.checkPackageDirs files
  let (_, active) = partitionPackageViolations config packageViolations
  unless (null active) $ do
    $(logTM) ErrorS $
      logStr $
        T.unlines
          [ ""
          , "Package directory violations:"
          ]
    $(logTM) ErrorS $ logStr $ formatPackageViolations active
  pure $ length active

{- | Enforce the directory-layout convention across the whole project tree.

n.b. this walks every on-disk .nix file (via 'collectFiles', honoring the
configured ignores) and validates each against @effectiveLayout@ using the
PROJECT ROOT — so a file's path relative to the root is what the convention's
location rules see, and stray/orphan files are caught too. This is distinct from
the import-following module graph, which only reaches files wired via @import@.
-}
runLayoutPhase :: Config.Config -> FilePath -> AppM Int
runLayoutPhase config dir = do
  let conv = Config.effectiveLayout config
  files <- liftIO $ collectFiles config dir
  sum <$> mapM (checkFileLayout conv dir) files

-- | Validate a single file's placement/shape; emit a diagnostic per violation.
checkFileLayout :: Layout.Convention -> FilePath -> FilePath -> AppM Int
checkFileLayout conv root path = do
  parsed <- liftIO $ Safety.safeParseNixFile path
  -- parse failures are already surfaced by the type-check phase; don't
  -- double-report them here (Left -> 0).
  either (const (pure 0)) onParsed parsed
 where
  onParsed expr = do
    let errs = Layout.validateFileFromExpr conv root path expr
    mapM_ (emitDiagnostic . layoutDiagnostic) errs
    pure (length errs)

{- | A 'Layout.LayoutError' as a unified clippy 'Diagnostic'. Layout findings are
file-level (placement/shape), so they carry the file path but no caret span.
-}
layoutDiagnostic :: Layout.LayoutError -> Diag.Diagnostic
layoutDiagnostic e =
  Diag.Diagnostic
    { Diag.diagSeverity = ErrorS
    , Diag.diagCode = Just (T.pack (show (Layout.errCode e)))
    , Diag.diagSpan = Just (Span (Loc 1 1) (Loc 1 1) (Just (Layout.errPath e)))
    , Diag.diagSummary = Layout.errMessage e
    , Diag.diagHelp = maybe [] (\x -> ["expected: " <> x]) (Layout.errExpected e)
    , Diag.diagSnippet = Nothing
    }

{- | Print the one-line CI summary ("checked N files: … ok, … failed, …
violations") at 'InfoS' and 'exitSuccess' when clean, else at 'ErrorS' and
'exitFailure'.
-}
reportCISummary :: CICounts -> AppM ()
reportCISummary counts = do
  let totalFailures =
        ciTypeFail counts
          + ciLintViolations counts
          + ciPackageViolations counts
          + ciBashViolations counts
          + ciGraphFailures counts
          + ciLayoutViolations counts
  let n = T.pack . show
      plural one count = n count <> " " <> one <> (if count == 1 then "" else "s")
      violations =
        ciLintViolations counts
          + ciPackageViolations counts
          + ciBashViolations counts
          + ciGraphFailures counts
          + ciLayoutViolations counts
      summary =
        "checked "
          <> plural "file" (ciFilesScanned counts)
          <> ": "
          <> n (ciTypePass counts)
          <> " ok"
          <> (if ciTypeSkip counts > 0 then ", " <> n (ciTypeSkip counts) <> " skipped" else "")
          <> (if ciTypeFail counts > 0 then ", " <> n (ciTypeFail counts) <> " failed" else "")
          <> (if violations > 0 then ", " <> plural "violation" violations else "")
  if totalFailures == 0
    then do
      $(logTM) InfoS $ logStr summary
      liftIO exitSuccess
    else do
      $(logTM) ErrorS $ logStr summary
      liftIO exitFailure

{- | Gather the .nix files to check: recurse a directory (honoring configured
ignores) or yield the single path (unless ignored).
-}
collectFiles :: Config.Config -> FilePath -> IO [FilePath]
collectFiles config path = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then collectNixFilesRecursive config path
    else pure [path | not (Profiles.isIgnored config path)]

collectNixFilesRecursive :: Config.Config -> FilePath -> IO [FilePath]
collectNixFilesRecursive config root = do
  canonicalRoot <- canonicalizePath root
  let ignoredDirs =
        Set.fromList
          [".git", ".direnv", "node_modules", ".cache", ".lake", "result", "result-lib", "target"]
  allFiles <- walkDirectory canonicalRoot ignoredDirs [] Set.empty [root]
  pure $ filter (not . Profiles.isIgnored config . makeRelative canonicalRoot) allFiles

walkDirectory ::
  FilePath -> Set.Set FilePath -> [FilePath] -> Set.Set FilePath -> [FilePath] -> IO [FilePath]
walkDirectory _canonicalRoot _ignoredDirs accumulatedFiles _visited [] = pure accumulatedFiles
walkDirectory canonicalRoot ignoredDirs accumulatedFiles visited (directory : worklist) = do
  canonical <- canonicalizePath directory
  -- n.b. boundary check must include the path separator (C5 from review-2). Without it,
  -- `/home/u/proj` is considered a prefix of `/home/u/proj-evil`, walking the sibling.
  let rootBoundary = canonicalRoot ++ [pathSeparator]
      insideRoot = canonical == canonicalRoot || rootBoundary `isPrefixOf` canonical
  if canonical `Set.member` visited || not insideRoot
    then walkDirectory canonicalRoot ignoredDirs accumulatedFiles visited worklist
    else do
      entries <- listDirectory directory
      (nixFiles, subDirectories) <- classifyEntries directory ignoredDirs entries
      walkDirectory
        canonicalRoot
        ignoredDirs
        (nixFiles ++ accumulatedFiles)
        (Set.insert canonical visited)
        (subDirectories ++ worklist)

classifyEntries :: FilePath -> Set.Set FilePath -> [FilePath] -> IO ([FilePath], [FilePath])
classifyEntries basePath ignoredDirs =
  foldM
    ( \(nixFiles, subDirectories) entry -> do
        let fullPath = basePath </> entry
        -- n.b. use Set.member instead of Set.toList .. elem (P2 from review-2).
        if Set.member entry ignoredDirs
          then pure (nixFiles, subDirectories)
          else do
            isDirectory <- doesDirectoryExist fullPath
            if isDirectory
              then pure (nixFiles, fullPath : subDirectories)
              else
                pure
                  ( if takeExtension fullPath == ".nix" then fullPath : nixFiles else nixFiles
                  , subDirectories
                  )
    )
    ([], [])

{- | Run 'checkFile' in plain 'IO' by re-establishing the captured katip
logging environment/context/namespace — lets the type-check phase fan files
out via 'forConcurrently' outside 'AppM'.
-}
wrapCheckFile ::
  Closure.ClosureCache ->
  Config.Config ->
  (LogEnv, LogContexts, Namespace) ->
  FilePath ->
  IO TCResult
wrapCheckFile closureCache config (loggingEnv, loggingContext, loggingNamespace) file =
  runKatipContextT loggingEnv loggingContext loggingNamespace $
    checkFileShared (Just closureCache) config file
