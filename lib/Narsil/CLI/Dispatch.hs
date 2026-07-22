{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Narsil.CLI.Dispatch (
  cmdCheck,
  cmdFmt,
  cmdInfer,
  cmdInferInPlace,
  cmdInferRecursive,
  InferOutcome (..),
  inferOneFile,
  cmdEmit,
  cmdScope,
  cmdScopeJSON,
  cmdScopeDhall,
  cmdLSP,
)
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSemN (newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, doesFileExist, renameFile)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeExtension)

import Nix.Expr.Types.Annotated (NExprLoc)

import Narsil (parseScriptFile, scriptSchema)
import Narsil.CLI.Bash
import Narsil.CLI.CI
import Narsil.Core.Config qualified as Config
import Narsil.Core.Draw qualified as Draw
import Narsil.Core.Log
import Narsil.Core.Profiles qualified as Profiles
import Narsil.Core.Safety qualified as Safety
import Narsil.Emit.Config (emitConfigFunction)
import Narsil.Inference.Nix (TypeEnv, builtinEnv)
import Narsil.Inference.Nix.Annotate qualified as Annotate
import Narsil.LSP.Handlers qualified as Handlers
import Narsil.LSP.Server qualified as LSP
import Narsil.Layout.Closure qualified as Closure
import Narsil.Layout.Scope qualified as Scope
import Narsil.Syntax.Format qualified as Formatter
import Narsil.Syntax.Parse qualified as Nix

{- | @check <path>@: dispatch on the path — a directory runs the full CI sweep
('cmdCI'), a @.nix@ file is type-checked, any other file is checked as bash.
A missing path fails via 'failSafety'.
-}
cmdCheck :: Config.Config -> FilePath -> AppM ()
cmdCheck config path = do
  isDir <- liftIO $ doesDirectoryExist path
  if isDir
    then cmdCI config path
    else do
      exists <- liftIO $ doesFileExist path
      if not exists
        then failSafety (T.pack path <> ": no such file or directory")
        else
          -- an explicitly named file still honors the config's ignore globs
          -- (including a profile's — `off` ignores the world); say so
          -- instead of silently checking past them
          if Profiles.isIgnored config path
            then liftIO $ putStrLn (path <> ": skipped (ignored by config)")
            else
              if takeExtension path == ".nix"
                then checkNixFile config path
                else checkBashFile config path

{- | Run an analysis pass after enforcing the depth guard.
n.b. every command except `check` flowed through 'inferExpr'/'buildExpr' with no
depth guard; this helper funnels them all through 'Safety.analyzeDepth' first.
-}
withSafeNix :: FilePath -> (NExprLoc -> AppM ()) -> AppM ()
withSafeNix file act = do
  parseResult <- liftIO $ Nix.parseNixFile file
  either failSafety guardDepth parseResult
 where
  guardDepth expr = either depthFailed (const (act expr)) (Safety.analyzeDepth expr)
  depthFailed de = failSafety (Safety.renderSafetyError (Safety.SafetyDepthExceeded de))

{- | @fmt <file>@: format a Nix file and write the result to stdout (after the
depth guard). I/O failures go to stderr via 'failSafety'.
-}
cmdFmt :: FilePath -> AppM ()
cmdFmt file = withSafeNix file $ \expr -> do
  srcResult <- liftIO $ safeReadFile file
  either
    (\err -> failSafety ("I/O error: " <> err))
    (\src -> liftIO $ TIO.putStr $ Formatter.formatNixFile src file expr)
    srcResult

-- | @infer <file>@: print the Nix file annotated with inferred types to stdout.
cmdInfer :: FilePath -> AppM ()
cmdInfer = runInfer (liftIO . TIO.putStr)

{- | @infer -i\/--in-place@: rewrite the file with its annotations instead of
printing to stdout. Idempotent (prior annotations are stripped first) and written
atomically (temp + rename), so a re-run replaces cleanly and a failure never
clobbers the source — and on a parse/type error nothing is written at all.
-}
cmdInferInPlace :: FilePath -> AppM ()
cmdInferInPlace file = runInfer (liftIO . atomicWriteFile file) file

-- | Shared @infer@ core: enrich, annotate, and hand the result to a sink.
runInfer :: (Text -> AppM ()) -> FilePath -> AppM ()
runInfer sink file = withSafeNix file $ \expr -> do
  -- Seed cross-module types: build the import/module closure rooted at this file
  -- (synchronous, eval-free) so an `import ./dep.nix` annotates as `./dep.nix`'s
  -- actual type instead of an opaque dynamic. Then layer the nixpkgs pkgs oracle
  -- on top (best-effort, time-boxed) for `pkgs.<…>` references. The whole
  -- pipeline runs under 'Safety.safeIO' with the result forced: the closure
  -- runs pure-but-partial inference over DEPENDENCY files, and a crash there
  -- must degrade to a diagnostic, not kill the command (there is no other
  -- handler anywhere on the infer path).
  attempt <- liftIO $ Safety.safeIO $ do
    crossEnv <- Closure.closureEnv builtinEnv file
    env <- Handlers.enrichInferEnv file expr crossEnv
    forceEither =<< Annotate.annotateFileWithEnv env file
  either (failSafety . Safety.renderSafetyError) (either failSafety sink) attempt

{- | Force both payloads of an annotation result so any latent inference bomb (a
thunk inside a seeded dependency type) detonates inside 'Safety.safeIO', not
later in a sink with no handler.
-}
forceEither :: Either Text Text -> IO (Either Text Text)
forceEither r = either T.length T.length r `seq` pure r

-- | Write a file atomically: a sibling temp then 'renameFile' (atomic on POSIX).
atomicWriteFile :: FilePath -> Text -> IO ()
atomicWriteFile path txt = do
  let tmp = path <> ".narsil.tmp"
  TIO.writeFile tmp txt
  renameFile tmp path

-- | What @infer -r@ did with one file.
data InferOutcome
  = -- | annotations changed; the file was rewritten
    Wrote
  | -- | annotations already correct; nothing written
    AlreadyOk
  | -- | a parse / depth / type error; the file was left untouched
    InferSkipped !Text
  deriving (Eq, Show)

{- | @infer -r\/--recursive <path>@: annotate every @.nix@ file under a directory
(or the single file, if @path@ is one) in place. Each file is handled independently
and NON-fatally — a parse, depth, or type error skips just that file and leaves it
untouched (never a clobber), while the rest proceed. File discovery honours the
configured ignores (the same 'collectFiles' the CI sweep uses), and the nixpkgs
index is built once for the whole run (see 'Handlers.enrichInferEnvBatch') rather
than per file. A summary is printed at the end; the command always exits success —
a file that doesn't type-check is expected, not a failure of @infer@.
-}
cmdInferRecursive :: Config.Config -> FilePath -> AppM ()
cmdInferRecursive config root = do
  files <- liftIO $ collectFiles config root
  enrich <- liftIO $ Handlers.enrichInferEnvBatch root
  -- One shared closure cache and capability-bounded fan-out, mirroring the CI
  -- type-check phase: each file's work is independent (distinct files, atomic
  -- writes), and shared deps are parsed + inferred once for the sweep instead
  -- of once per importer.
  closureCache <- liftIO Closure.newClosureCache
  outcomes <- liftIO $ do
    sem <- newQSemN =<< getNumCapabilities
    forConcurrently files $ \f ->
      bracket_ (waitQSemN sem 1) (signalQSemN sem 1) $
        (,) f <$> inferOneFile closureCache enrich f
  reportInferSweep outcomes

{- | Annotate one file in place, non-fatally, reusing a batch pkgs enricher. Any
failure becomes an 'InferSkipped' rather than aborting the sweep, and the file is
only rewritten when the annotation actually changes it (so a clean tree is a no-op).
-}
inferOneFile ::
  Closure.ClosureCache -> (NExprLoc -> TypeEnv -> IO TypeEnv) -> FilePath -> IO InferOutcome
inferOneFile closureCache enrich file = do
  parsed <- Nix.parseNixFile file
  either (pure . InferSkipped) viaExpr parsed
 where
  viaExpr expr =
    either (pure . InferSkipped . depthMsg) (const (annotateOne expr)) (Safety.analyzeDepth expr)
  depthMsg de = Safety.renderSafetyError (Safety.SafetyDepthExceeded de)
  annotateOne expr = do
    -- Under 'Safety.safeIO' (forced): a crash while the closure infers a
    -- DEPENDENCY must become this file's 'InferSkipped', honouring the
    -- non-fatal-per-file contract above.
    attempt <- Safety.safeIO $ do
      crossEnv <- Closure.closureEnvShared closureCache builtinEnv file
      env <- enrich expr crossEnv
      forceEither =<< Annotate.annotateFileWithEnv env file
    case attempt of -- CASE-OK: shape dispatch
      Left serr -> pure (InferSkipped (Safety.renderSafetyError serr))
      Right result -> either (pure . InferSkipped) (writeIfChanged file) result

-- | Rewrite the file only when the annotation differs from what is already on disk.
writeIfChanged :: FilePath -> Text -> IO InferOutcome
writeIfChanged file txt = do
  existing <- TIO.readFile file
  if txt == existing
    then pure AlreadyOk
    else atomicWriteFile file txt >> pure Wrote

-- | Print the per-file skips and a one-line tally for an @infer -r@ sweep.
reportInferSweep :: [(FilePath, InferOutcome)] -> AppM ()
reportInferSweep outcomes = liftIO $ do
  forM_ skips $ \(file, reason) ->
    putStrLn $ "  ⚠ " <> file <> " — " <> T.unpack (firstLine reason)
  putStrLn $
    "infer -r: "
      <> show (length outcomes)
      <> " files · "
      <> show (length [() | (_, Wrote) <- outcomes])
      <> " annotated · "
      <> show (length [() | (_, AlreadyOk) <- outcomes])
      <> " unchanged · "
      <> show (length skips)
      <> " skipped"
 where
  skips = [(file, reason) | (file, InferSkipped reason) <- outcomes]
  firstLine = T.takeWhile (/= '\n')

{- | @emit <file>@: from a script's inferred schema, emit the generated
@emit-config@ bash function to stdout.
-}
cmdEmit :: FilePath -> AppM ()
cmdEmit file = do
  result <- liftIO $ parseScriptFile file
  either failSafety (liftIO . TIO.putStr . emitConfigFunction . scriptSchema) result

{- | @lsp@: run the language server over stdio until the client disconnects,
then exit cleanly.
-}
cmdLSP :: AppM ()
cmdLSP = liftIO LSP.run >> liftIO exitSuccess

{- | @scope <file>@: build the scope graph and print it as a human-readable
framed report (scopes, declarations, references, edges, resolution) to stdout.
-}
cmdScope :: FilePath -> AppM ()
cmdScope file = withSafeNix file $ \expr -> do
  let scopeGraph = Scope.fromNixFile file expr
  liftIO $ printScopeGraph scopeGraph

-- | @scope --json <file>@: build the scope graph and print it as JSON to stdout.
cmdScopeJSON :: FilePath -> AppM ()
cmdScopeJSON file = withSafeNix file $ \expr -> do
  let scopeGraph = Scope.fromNixFile file expr
  liftIO $ BL.putStrLn $ encode scopeGraph

-- | @scope --dhall <file>@: build the scope graph and print it as Dhall to stdout.
cmdScopeDhall :: FilePath -> AppM ()
cmdScopeDhall file = withSafeNix file $ \expr -> do
  let scopeGraph = Scope.fromNixFile file expr
  liftIO $ TIO.putStrLn $ Scope.toDhall scopeGraph

{- | The uniform CLI failure path: log a message at 'ErrorS' and exit non-zero.
Messages are emitted as-is — callers pass already-categorized text (e.g.
'Safety.renderSafetyError' yields "parse error: …" / "I/O error: …" / "depth
limit exceeded …"), so no prefix is added here (a blanket "Parse error:" would
mislabel I/O and depth failures and double-print for real parse failures).
-}
failSafety :: Text -> AppM a
failSafety err = do
  $(logTM) ErrorS $ logStr err
  liftIO exitFailure

printScopeGraph :: Scope.ScopeGraph -> IO ()
printScopeGraph scopeGraph = do
  TIO.putStrLn (Draw.framed Draw.Double "Scope Graph")
  putStrLn $ "File: " ++ fromMaybe "(none)" (Scope.sgFile scopeGraph)
  putStrLn $ "Scopes: " ++ show (Map.size (Scope.sgScopes scopeGraph))
  putStrLn ""

  forM_ (Map.elems (Scope.sgScopes scopeGraph)) $ \scope -> do
    putStrLn $
      "Scope "
        ++ show (Scope.unScopeId (Scope.scopeId scope))
        ++ " ("
        ++ show (Scope.scopeKind scope)
        ++ "):"

    let decls = Scope.scopeDeclarations scope
    unless (null decls) $ do
      putStrLn "  Declarations:"
      forM_ decls $ \declaration -> do
        TIO.putStrLn $
          "    "
            <> Scope.declName declaration
            <> maybe "" (" : " <>) (Scope.declType declaration)

    let refs = Scope.scopeReferences scope
    unless (null refs) $ do
      putStrLn "  References:"
      forM_ refs $ \reference -> do
        TIO.putStrLn $
          "    "
            <> Scope.refName reference
            <> " ("
            <> T.pack (show (Scope.refKind reference))
            <> ")"

    let edges = Scope.scopeEdges scope
    unless (null edges) $ do
      putStrLn "  Edges:"
      forM_ edges $ \edge -> do
        putStrLn $
          "    -> "
            ++ show (Scope.unScopeId (Scope.edgeTarget edge))
            ++ " ("
            ++ show (Scope.edgeLabel edge)
            ++ ")"

    putStrLn ""

  either reportUnresolved reportResolved (Scope.resolveAll scopeGraph)
 where
  reportUnresolved errors = do
    TIO.putStrLn $
      Draw.framed Draw.Double ("Unresolved References (" <> T.pack (show (length errors)) <> ")")
    forM_ errors printUnresolved
  reportResolved resolved =
    TIO.putStrLn $
      Draw.framed Draw.Double ("All " <> T.pack (show (length resolved)) <> " references resolved")
  printUnresolved (Scope.Unresolved ref) = TIO.putStrLn $ "  " <> Scope.refName ref
  printUnresolved (Scope.Ambiguous ref _) =
    TIO.putStrLn $ "  " <> Scope.refName ref <> " (ambiguous)"
