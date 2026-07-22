{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                  // tests // oracle sweep (all of nixpkgs)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- The L-size differential oracle. Where test/Oracle.hs asserts kinds against
-- `nix-instantiate` on small corpora, this asserts the brutal corpus-level
-- property: STOCK NIXPKGS IS WELL-TYPED. nixpkgs is the most battle-tested Nix
-- in existence — every file in it parses, evaluates, and builds somewhere — so
-- every diagnostic we emit against it is presumptively OUR bug (a false
-- positive, the adoption killer) until classified otherwise.
--
-- The sweep runs the real check pipeline (parse → depth guard → unsupported
-- detection → module-kind → shared cross-module closure → inference) over every
-- .nix file under the corpus root, buckets outcomes, and normalizes error
-- messages into SIGNATURES so a new false-positive class surfaces as a named
-- line with an example file — the automated version of the by-name expedition
-- that mined the toString coercion gaps by hand.
--
-- Against a committed BASELINE (test/oracle/nixpkgs-baseline.json):
--   * crashes and hangs fail ABSOLUTELY (the safeIO contract);
--   * every "bad" bucket (parse-skip, depth-skip, unsupported, type-err) may
--     only ratchet DOWN — an increase fails, an improvement suggests
--     --write-baseline;
--   * a corpus fingerprint mismatch (different nixpkgs pin) downgrades count
--     ratchets to a warning — crashes still fail.
--
-- Sizes:   --sample N     deterministic evenly-spaced subset (quick runs)
--          (no flag)      all of nixpkgs
-- Corpus:  --nixpkgs PATH, else $NIX_COMPILE_NIXPKGS, else the flake.lock pin
--          (the same resolution the LSP uses).
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSemN (newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_, evaluate, fromException, try)
import Control.Monad (unless, when)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.ByteString.Base16 qualified as B16
import Data.Char (isDigit)
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Language.LSP.Protocol.Types (filePathToUri)
import Narsil.CLI.Check (detectUnsupportedConstruct)
import Narsil.Core.Safety qualified as Safety
import Narsil.Inference.Nix (TypeEnv (..), builtinEnv, inferExprWithEnv)
import Narsil.Inference.Nix.Type (prettyType)
import Narsil.LSP.Handlers.Project (resolveNixpkgsRoot)
import Narsil.Layout.Closure qualified as Closure
import Narsil.Layout.ModuleKind (ModuleKind (..), detectKind, detectedKind)
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
  listDirectory,
  pathIsSymbolicLink,
 )
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (makeRelative, takeExtension, (</>))
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Timeout (Timeout, timeout)

-- | Per-file budget: hub files truncate at the closure node bound, so 30s is generous.
perFileMicros :: Int
perFileMicros = 30 * 1000000

-- ── outcomes ────────────────────────────────────────────────────────

data Outcome
  = TypeOk
  | TypeErr !Text
  | ParseSkip !Text
  | DepthSkip
  | Unsupported !Text
  | Crash !Text
  | TimedOut
  | Hang
  deriving (Show)

bucketOf :: Outcome -> String
bucketOf = \case
  TypeOk -> "type-ok"
  TypeErr _ -> "type-err"
  ParseSkip _ -> "parse-skip"
  DepthSkip -> "depth-skip"
  Unsupported _ -> "unsupported"
  Crash _ -> "crash"
  TimedOut -> "timeout"
  Hang -> "hang"

-- | Buckets where an increase against the baseline is a regression.
ratchetBuckets :: [String]
ratchetBuckets = ["parse-skip", "depth-skip", "unsupported", "type-err", "timeout"]

{- | The per-file check pipeline — the same stages `narsil check` runs,
sharing one closure cache across the sweep. The whole attempt lives under
try+timeout: at this size, anything that escapes IS the finding.
-}
checkOne :: Closure.ClosureCache -> FilePath -> IO Outcome
checkOne cache file = do
  outcome <- timeout perFileMicros (try attempt)
  pure $ case outcome of
    Nothing -> Hang
    -- the per-file budget: 'timeout' throws into the thread and the inner
    -- 'try' catches it first, so budget-exceeded arrives HERE, not as
    -- Nothing. A genuinely huge generated file (hackage-packages.nix is
    -- 17MB) exceeds any budget — that is a ratcheted bucket, not a crash.
    Just (Left e)
      | Just (_ :: Timeout) <- fromException e -> TimedOut
      | otherwise -> Crash (T.pack (show e))
    Just (Right o) -> o
 where
  attempt = do
    parsed <- Safety.safeParseNixFile file
    case parsed of
      Left err -> pure (ParseSkip (Safety.renderSafetyError err))
      Right expr -> case Safety.analyzeDepth expr of
        Left _ -> pure DepthSkip
        Right () -> case detectUnsupportedConstruct expr of
          Just reason -> pure (Unsupported reason)
          Nothing -> do
            let kind = detectedKind (detectKind file expr)
                moduleMode = kind `elem` [Flake, FlakeModule, NixOSModule, HomeModule, DarwinModule]
            crossEnv <- Closure.closureEnvShared cache builtinEnv file
            let env = if moduleMode then crossEnv{envModuleParams = True} else crossEnv
            evaluate $ case inferExprWithEnv env expr of
              Left err -> TypeErr err
              Right (t, _) -> T.length (prettyType t) `seq` TypeOk

-- ── error signatures ────────────────────────────────────────────────

{- | Normalize an error message into a stable signature so the same
false-positive CLASS groups under one line regardless of file-specific names:
first line only, digit runs collapsed, quoted/backticked literals elided,
capped length.
-}
signatureOf :: Text -> Text
signatureOf msg =
  T.take 160
    . collapseDigits
    . elideQuoted '"'
    . elideQuoted '`'
    . T.strip
    . T.takeWhile (/= '\n')
    $ msg
 where
  collapseDigits = T.pack . go . T.unpack
   where
    go [] = []
    go (c : cs)
      | isDigit c = '#' : go (dropWhile isDigit cs)
      | otherwise = c : go cs
  elideQuoted q t = case T.splitOn (T.singleton q) t of
    (h : _ : _ : rest) -> h <> "…" <> elideQuoted q (T.intercalate (T.singleton q) (drop' rest))
    _ -> t
   where
    drop' xs = if null xs then [""] else xs

-- ── baseline ────────────────────────────────────────────────────────

data Baseline = Baseline
  { blFingerprint :: String
  , blFileCount :: Int
  , blCounts :: Map String Int
  , blSignatures :: Map Text Int
  , blUnsupported :: Map Text Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Cheap corpus identity: SHA-256 over the sorted relative paths.
fingerprintOf :: FilePath -> [FilePath] -> String
fingerprintOf root files =
  T.unpack . TE.decodeUtf8 . B16.encode . SHA256.hash . TE.encodeUtf8 . T.pack $
    unlines (sort (map (makeRelative root) files))

-- ── corpus collection ───────────────────────────────────────────────

-- | Every .nix file under the root; prunes .git and never follows directory symlinks.
collectNixFiles :: FilePath -> IO [FilePath]
collectNixFiles root = go root
 where
  go dir = do
    entries <- listDirectory dir
    fmap concat . mapM (visit . (dir </>)) $ filter (/= ".git") entries
  visit path = do
    isLink <- pathIsSymbolicLink path
    isDir <- doesDirectoryExist path
    if isDir && not isLink
      then go path
      else pure [path | takeExtension path == ".nix", not isLink]

-- | Deterministic evenly-spaced subset of a sorted corpus.
sampleFiles :: Int -> [FilePath] -> [FilePath]
sampleFiles n files
  | n <= 0 || n >= length files = files
  | otherwise =
      let step = max 1 (length files `div` n)
       in take n [f | (i, f) <- zip [(0 :: Int) ..] files, i `mod` step == 0]

-- ── driver ──────────────────────────────────────────────────────────

data Opts = Opts
  { optNixpkgs :: Maybe FilePath
  , optBaseline :: FilePath
  , optWriteBaseline :: Bool
  , optSample :: Maybe Int
  , optDumpErrors :: Maybe FilePath
  }

defaultOpts :: Opts
defaultOpts =
  Opts
    { optNixpkgs = Nothing
    , optBaseline = "test/oracle/nixpkgs-baseline.json"
    , optWriteBaseline = False
    , optSample = Nothing
    , optDumpErrors = Nothing
    }

parseArgs :: [String] -> Opts -> Opts
parseArgs [] o = o
parseArgs ("--nixpkgs" : p : rest) o = parseArgs rest o{optNixpkgs = Just p}
parseArgs ("--baseline" : p : rest) o = parseArgs rest o{optBaseline = p}
parseArgs ("--write-baseline" : rest) o = parseArgs rest o{optWriteBaseline = True}
parseArgs ("--sample" : n : rest) o = parseArgs rest o{optSample = Just (read n)}
parseArgs ("--dump-errors" : p : rest) o = parseArgs rest o{optDumpErrors = Just p}
parseArgs (a : rest) o = error ("oracle-sweep: unknown argument " ++ a) `seq` parseArgs rest o

-- | The corpus root: explicit flag, env var, or the flake.lock pin (as the LSP resolves it).
resolveCorpus :: Opts -> IO FilePath
resolveCorpus opts = do
  fromFlag <- pure (optNixpkgs opts)
  fromEnv <- lookupEnv "NIX_COMPILE_NIXPKGS"
  fromLock <- do
    cwd <- getCurrentDirectory
    resolveNixpkgsRoot (filePathToUri (cwd </> "flake.nix"))
  case listToMaybe (mapMaybe id [fromFlag, fromEnv, fromLock]) of
    Just root -> do
      ok <- doesDirectoryExist root
      unless ok (error ("oracle-sweep: nixpkgs root does not exist: " ++ root))
      pure root
    Nothing ->
      error
        "oracle-sweep: no corpus — pass --nixpkgs PATH, set NIX_COMPILE_NIXPKGS, \
        \or run from a project whose flake.lock pins nixpkgs"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <- flip parseArgs defaultOpts <$> getArgs
  root <- resolveCorpus opts
  putStrLn ("oracle-sweep: corpus root " ++ root)

  allFiles <- sort <$> collectNixFiles root
  let files = maybe allFiles (`sampleFiles` allFiles) (optSample opts)
      fingerprint = fingerprintOf root files
  putStrLn
    ("oracle-sweep: " ++ show (length files) ++ " files (of " ++ show (length allFiles) ++ ")")

  cache <- Closure.newClosureCache
  sem <- newQSemN =<< getNumCapabilities
  results <- forConcurrently files $ \f ->
    bracket_ (waitQSemN sem 1) (signalQSemN sem 1) $ do
      o <- checkOne cache f
      pure (f, o)

  let counts = foldl' (\m (_, o) -> Map.insertWith (+) (bucketOf o) 1 m) Map.empty results
      sigOf (f, TypeErr e) = Just (signatureOf e, f)
      sigOf _ = Nothing
      sigPairs = mapMaybe sigOf results
      sigCounts = foldl' (\m (s, _) -> Map.insertWith (+) s 1 m) Map.empty sigPairs
      exampleFor s = listToMaybe [f | (s', f) <- sigPairs, s' == s]
      unsupOf (_, Unsupported r) = Just (signatureOf r)
      unsupOf _ = Nothing
      unsupCounts = foldl' (\m r -> Map.insertWith (+) r 1 m) Map.empty (mapMaybe unsupOf results)
      count b = Map.findWithDefault 0 b counts
      hardFailures =
        [(f, o) | (f, o) <- results, case o of Crash _ -> True; Hang -> True; _ -> False]

  case optDumpErrors opts of
    Nothing -> pure ()
    Just p -> do
      let rows =
            [ makeRelative root f ++ "\t" ++ T.unpack (T.replace "\n" " " e)
            | (f, TypeErr e) <- results
            ]
      writeFile p (unlines rows)
      putStrLn ("oracle-sweep: " ++ show (length rows) ++ " type-errors dumped to " ++ p)

  putStrLn ""
  putStrLn "── outcomes ──────────────────────────────────────────────"
  mapM_ (\(b, n) -> putStrLn ("  " ++ pad 14 b ++ show n)) (Map.toList counts)

  unless (Map.null sigCounts) $ do
    putStrLn ""
    putStrLn "── top type-error signatures (each is a suspected false-positive class) ──"
    mapM_
      ( \(s, n) ->
          putStrLn
            ( "  "
                ++ pad 6 (show n)
                ++ T.unpack s
                ++ maybe "" (\f -> "\n        e.g. " ++ makeRelative root f) (exampleFor s)
            )
      )
      (take 20 (sortOn (Down . snd) (Map.toList sigCounts)))

  unless (Map.null unsupCounts) $ do
    putStrLn ""
    putStrLn "── unsupported-construct reasons (the coverage ceiling) ──"
    mapM_
      (\(r, n) -> putStrLn ("  " ++ pad 6 (show n) ++ T.unpack r))
      (take 15 (sortOn (Down . snd) (Map.toList unsupCounts)))

  mapM_
    (\(f, o) -> putStrLn ("  HARD-FAIL " ++ makeRelative root f ++ ": " ++ show o))
    (take 20 hardFailures)

  -- name the timeout files: the bucket is ratcheted, but WHICH files sit in
  -- it is the lead for any inference-performance round
  mapM_
    (\f -> putStrLn ("  TIMEOUT " ++ makeRelative root f))
    (take 20 [f | (f, TimedOut) <- results])

  let baseline =
        Baseline
          { blFingerprint = fingerprint
          , blFileCount = length files
          , blCounts = counts
          , blSignatures = sigCounts
          , blUnsupported = unsupCounts
          }

  if optWriteBaseline opts
    then do
      encodeFile (optBaseline opts) baseline
      putStrLn ("oracle-sweep: baseline written to " ++ optBaseline opts)
      if null hardFailures then exitSuccess else exitFailure
    else do
      prior <- do
        exists <- doesFileExist (optBaseline opts)
        if exists
          then either (const Nothing) Just <$> eitherDecodeFileStrict (optBaseline opts)
          else pure Nothing
      verdictAgainst root prior baseline hardFailures count

{- | Judge the run: crashes/hangs always fail; against a matching baseline the
ratchet buckets may only shrink; against a different corpus (or no baseline)
counts are informational.
-}
verdictAgainst ::
  FilePath -> Maybe Baseline -> Baseline -> [(FilePath, Outcome)] -> (String -> Int) -> IO ()
verdictAgainst _root prior current hardFailures count = do
  putStrLn ""
  let crashOrHang = length hardFailures
  regressions <- case prior of
    Nothing -> do
      putStrLn
        "oracle-sweep: no baseline (run with --write-baseline to set one); counts informational"
      pure []
    Just b
      | blFingerprint b /= blFingerprint current -> do
          putStrLn
            ( "oracle-sweep: WARNING corpus fingerprint differs from baseline "
                ++ "(different nixpkgs pin or sample?)"
            )
          putStrLn "              count ratchets skipped; crashes/hangs still enforced"
          pure []
      | otherwise -> do
          let regs =
                [ (bucket, was, now)
                | bucket <- ratchetBuckets
                , let was = Map.findWithDefault 0 bucket (blCounts b)
                , let now = count bucket
                , now > was
                ]
              improvements =
                [ (bucket, was, now)
                | bucket <- ratchetBuckets
                , let was = Map.findWithDefault 0 bucket (blCounts b)
                , let now = count bucket
                , now < was
                ]
              newSigs =
                [ s
                | s <- Map.keys (blSignatures current)
                , not (Map.member s (blSignatures b))
                ]
          mapM_
            ( \(bkt, was, now) ->
                putStrLn ("REGRESSION " ++ bkt ++ ": " ++ show was ++ " -> " ++ show now)
            )
            regs
          unless (null newSigs) $ do
            putStrLn "NEW error signatures (not in baseline):"
            mapM_ (putStrLn . ("  " ++) . T.unpack) (take 10 newSigs)
          unless (null improvements) $
            putStrLn
              ( "improved: "
                  ++ show [(bkt, was, now) | (bkt, was, now) <- improvements]
                  ++ " — consider --write-baseline"
              )
          pure regs
  when (crashOrHang > 0) $
    putStrLn
      ("oracle-sweep: FAILED — " ++ show crashOrHang ++ " crash/hang outcomes (always fatal)")
  if crashOrHang == 0 && null regressions
    then putStrLn "oracle-sweep: OK" >> exitSuccess
    else exitFailure

pad :: Int -> String -> String
pad n s = take (max n (length s)) (s ++ repeat ' ')
