{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                               // bench // nixpkgs
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Just don't ask me what it cost the boys to bring it in."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Integration benchmark against ~/src/nixpkgs (or any large nix corpus).
--   Measures:
--     1. Raw parse throughput on a representative sample
--     2. Full safety pipeline (parse + analyzeDepth) on a sample
--     3. ProjectCache fill rate at corpus scale (workers actually engaged)
--     4. Steady-state memory + cache statistics
--
--   Usage:
--     cabal run nix-compile-nixpkgs-bench -- ~/src/nixpkgs [sample-size]
--   Default sample size: 500. Use 0 to skip the sample phases and only
--   exercise the ProjectCache fill phase.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Conc (getNumCapabilities)
import Narsil.Core.Safety qualified as Safety
import Narsil.LSP.ProjectCache qualified as PC
import System.Directory (
  doesDirectoryExist,
  getHomeDirectory,
 )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO qualified
import System.Process (readProcess)
import Text.Printf (printf)

main :: IO ()
main = do
  System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
  args <- getArgs
  home <- getHomeDirectory
  let (rootArg, sampleArg) = case args of
        [r, s] -> (r, read s :: Int)
        [r] -> (r, 500)
        [] -> (home </> "src" </> "nixpkgs", 500)
        _ -> error "usage: nixpkgs-bench [path] [sample-size]"

  rootExists <- doesDirectoryExist rootArg
  when (not rootExists) $ do
    hPutStrLn stderr $ "corpus not found: " <> rootArg
    exitFailure

  putStrLn $ "═══ nix-compile nixpkgs bench ═══"
  putStrLn $ "corpus: " <> rootArg
  n <- getNumCapabilities
  putStrLn $ "capabilities: " <> show n

  putStrLn ""
  putStrLn "── enumerating .nix files ──"
  enumStart <- getCurrentTime
  files <- enumerateNixFiles rootArg
  enumEnd <- getCurrentTime
  let total = length files
  printf "  %d files in %.2fs\n" total (realToFrac (diffUTCTime enumEnd enumStart) :: Double)

  when (sampleArg > 0) $ do
    let sampleSize = min sampleArg total
    let sample = take sampleSize files
    putStrLn ""
    putStrLn $ "── parse-only sample (" <> show sampleSize <> " files) ──"
    parseStats <- parseSample sample
    reportParseStats parseStats

    putStrLn ""
    putStrLn $ "── safety pipeline (" <> show sampleSize <> " files) ──"
    pipelineStats <- pipelineSample sample
    reportPipelineStats pipelineStats

  -- ── LSP-realistic: warm one file + its transitive imports ──
  let candidateOpen =
        [ p
        | p <- files
        , any
            (`elem` (words "lib pkgs nixos modules" :: [String]))
            (words (map (\c -> if c == '/' then ' ' else c) p))
        , "default.nix" `endsWith` p
        ]
      target = case (candidateOpen, files) of
        (p : _, _) -> p
        ([], p : _) -> p
        ([], []) -> error "empty corpus"
  putStrLn ""
  putStrLn $ "── LSP-realistic single-file warm (target: " <> target <> ") ──"
  warmStats <- warmSingleFile target
  reportWarmStats warmStats

  putStrLn ""
  putStrLn $ "── ProjectCache fill (full corpus, " <> show total <> " files) ──"
  fillStats <- fillProjectCache files
  reportFillStats fillStats total
 where
  endsWith suf s = drop (length s - length suf) s == suf

-- ───────────────────────── enumeration ─────────────────────────────

{- | Enumerate .nix files under root via /usr/bin/find. ~10x faster than
crawling with doesDirectoryExist/doesFileExist for large trees because find
uses readdir's d_type instead of separate stat syscalls.
-}
enumerateNixFiles :: FilePath -> IO [FilePath]
enumerateNixFiles root = do
  out <-
    readProcess
      "find"
      [ root
      , "("
      , "-name"
      , ".git"
      , "-o"
      , "-name"
      , "result"
      , "-o"
      , "-name"
      , "result-*"
      , "-o"
      , "-name"
      , "node_modules"
      , "-o"
      , "-name"
      , ".direnv"
      , "-o"
      , "-name"
      , "_build"
      , "-o"
      , "-name"
      , "target"
      , ")"
      , "-prune"
      , "-o"
      , "-name"
      , "*.nix"
      , "-type"
      , "f"
      , "-print"
      ]
      ""
  pure (sort (lines out))

-- ───────────────────────── parse sample ────────────────────────────

data ParseStats = ParseStats
  { psSamples :: !Int
  , psSuccesses :: !Int
  , psFailures :: !Int
  , psTotalMicros :: !Double
  , psMinMicros :: !Double
  , psMaxMicros :: !Double
  , psP50Micros :: !Double
  , psP95Micros :: !Double
  , psP99Micros :: !Double
  , psTotalBytes :: !Int
  }

parseSample :: [FilePath] -> IO ParseStats
parseSample files = do
  timings <- forM files $ \fp -> do
    t0 <- getCurrentTime
    result <- Safety.safeParseNixFile fp
    t1 <- getCurrentTime
    let micros = realToFrac (diffUTCTime t1 t0) * 1_000_000 :: Double
    case result of
      Left _ -> pure (micros, False, 0 :: Int)
      Right _ -> pure (micros, True, 0)
  let micros = [m | (m, _, _) <- timings]
      sortedMicros = sort micros
      n = length micros
      successes = length [() | (_, True, _) <- timings]
      failures = length [() | (_, False, _) <- timings]
      bytes = sum [b | (_, _, b) <- timings]
  pure
    ParseStats
      { psSamples = n
      , psSuccesses = successes
      , psFailures = failures
      , psTotalMicros = sum micros
      , psMinMicros = if null micros then 0 else minimum micros
      , psMaxMicros = if null micros then 0 else maximum micros
      , psP50Micros = percentile 50 sortedMicros
      , psP95Micros = percentile 95 sortedMicros
      , psP99Micros = percentile 99 sortedMicros
      , psTotalBytes = bytes
      }

reportParseStats :: ParseStats -> IO ()
reportParseStats s = do
  printf "  files:        %d (%d ok, %d failed)\n" (psSamples s) (psSuccesses s) (psFailures s)
  printf "  total:        %.2fs\n" (psTotalMicros s / 1_000_000)
  printf
    "  throughput:   %.0f files/sec\n"
    (fromIntegral (psSamples s) / (psTotalMicros s / 1_000_000) :: Double)
  printf
    "  per-file:     min=%.0fμs  p50=%.0fμs  p95=%.0fμs  p99=%.0fμs  max=%.0fμs\n"
    (psMinMicros s)
    (psP50Micros s)
    (psP95Micros s)
    (psP99Micros s)
    (psMaxMicros s)

-- ───────────────────────── pipeline sample ─────────────────────────

data PipelineStats = PipelineStats
  { plsParseMicros :: !Double
  , plsAnalyzeMicros :: !Double
  , plsTotal :: !Int
  , plsParseFailed :: !Int
  , plsDepthExceeded :: !Int
  }

pipelineSample :: [FilePath] -> IO PipelineStats
pipelineSample files = do
  totalRef <- newIORef (0 :: Int)
  parseFailedRef <- newIORef (0 :: Int)
  depthRef <- newIORef (0 :: Int)
  parseTimeRef <- newIORef (0 :: Double)
  analyzeTimeRef <- newIORef (0 :: Double)

  forM_ files $ \fp -> do
    atomicModifyIORef' totalRef (\n -> (n + 1, ()))
    t0 <- getCurrentTime
    parseRes <- Safety.safeParseNixFile fp
    t1 <- getCurrentTime
    atomicModifyIORef' parseTimeRef (\acc -> (acc + diffMicros t0 t1, ()))
    case parseRes of
      Left _ -> atomicModifyIORef' parseFailedRef (\n -> (n + 1, ()))
      Right expr -> do
        t2 <- getCurrentTime
        let depthRes = Safety.analyzeDepth expr
        t3 <- getCurrentTime
        atomicModifyIORef' analyzeTimeRef (\acc -> (acc + diffMicros t2 t3, ()))
        case depthRes of
          Left _ -> atomicModifyIORef' depthRef (\n -> (n + 1, ()))
          Right () -> pure ()

  PipelineStats
    <$> readIORef parseTimeRef
    <*> readIORef analyzeTimeRef
    <*> readIORef totalRef
    <*> readIORef parseFailedRef
    <*> readIORef depthRef

reportPipelineStats :: PipelineStats -> IO ()
reportPipelineStats s = do
  printf "  total:        %d\n" (plsTotal s)
  printf
    "  parse:        %.2fs  (%.0f files/sec)\n"
    (plsParseMicros s / 1_000_000)
    (fromIntegral (plsTotal s) / (plsParseMicros s / 1_000_000) :: Double)
  printf
    "  analyzeDepth: %.2fs  (%.0f files/sec)\n"
    (plsAnalyzeMicros s / 1_000_000)
    (fromIntegral (plsTotal s) / (plsAnalyzeMicros s / 1_000_000) :: Double)
  printf "  parse-failed: %d\n" (plsParseFailed s)
  printf "  depth-rejected: %d\n" (plsDepthExceeded s)

-- ───────────────────────── single-file warm ───────────────────────

{- | The LSP-realistic measurement: enqueue ONE file (the "just-opened" file)
and time until that file becomes Fresh, then time until its imports
become Fresh, then time until the BFS frontier stops expanding.
-}
data WarmStats = WarmStats
  { wsTimeToFirstFresh :: !Double -- seconds until the target file is Fresh
  , wsTimeToImportsFresh :: !Double -- + until immediate imports
  , wsFilesReached :: !Int -- final cache size after BFS settles
  , wsSettleTime :: !Double -- seconds until BFS stops growing
  }

warmSingleFile :: FilePath -> IO WarmStats
warmSingleFile target = do
  pc <- PC.newProjectCache
  PC.startWorkers pc
  t0 <- getCurrentTime
  PC.enqueueFile pc target

  -- Phase 1: target file becomes Fresh. Bounded at 10s — if the file fails
  -- to parse or hits depth limit, the cache never gets an entry, so we
  -- can't wait forever.
  let waitForTarget deadline = do
        now <- getCurrentTime
        if realToFrac (diffUTCTime now t0) > (deadline :: Double)
          then pure now
          else do
            entry <- PC.lookupFile pc target
            case entry of
              Just _ -> getCurrentTime
              Nothing -> threadDelay 5_000 >> waitForTarget deadline
  t1 <- waitForTarget 10.0
  let firstFresh = realToFrac (diffUTCTime t1 t0) :: Double

  -- Phase 2: target's immediate imports become Fresh.
  snap <- PC.snapshotFiles pc
  let imports = case Map.elems snap of
        (e : _) -> PC.feImports e
        [] -> []

  let waitForImports deadline = do
        now <- getCurrentTime
        if realToFrac (diffUTCTime now t0) > (deadline :: Double)
          then pure now
          else do
            currentSnap <- PC.snapshotFiles pc
            let allFresh =
                  all
                    ( \i -> case Map.lookup i currentSnap of
                        Just e -> PC.feStatus e == PC.Fresh
                        Nothing -> False
                    )
                    imports
            if allFresh || null imports
              then getCurrentTime
              else threadDelay 5_000 >> waitForImports deadline
  t2 <- waitForImports 30.0
  let importsFresh = realToFrac (diffUTCTime t2 t0) :: Double

  -- Phase 3: wait for BFS to settle (no growth for 2 s).
  lastSizeRef <- newIORef =<< (PC.pcsFiles <$> PC.statsOf pc)
  lastGrowthRef <- newIORef =<< getCurrentTime
  let settleLoop = do
        threadDelay 100_000
        stats <- PC.statsOf pc
        now <- getCurrentTime
        lastSize <- readIORef lastSizeRef
        when (PC.pcsFiles stats > lastSize) $ do
          writeIORef lastSizeRef (PC.pcsFiles stats)
          writeIORef lastGrowthRef now
        lastGrowth <- readIORef lastGrowthRef
        let idle = realToFrac (diffUTCTime now lastGrowth) :: Double
            elapsed = realToFrac (diffUTCTime now t0) :: Double
        if idle > 2.0 || elapsed > 60
          then pure ()
          else settleLoop
  settleLoop
  t3 <- getCurrentTime
  final <- PC.statsOf pc
  PC.stopWorkers pc
  pure
    WarmStats
      { wsTimeToFirstFresh = firstFresh
      , wsTimeToImportsFresh = importsFresh
      , wsFilesReached = PC.pcsFresh final
      , wsSettleTime = realToFrac (diffUTCTime t3 t0) :: Double
      }

reportWarmStats :: WarmStats -> IO ()
reportWarmStats s = do
  printf "  time-to-first-fresh: %.3fs (target file)\n" (wsTimeToFirstFresh s)
  printf "  time-to-imports-fresh: %.3fs\n" (wsTimeToImportsFresh s)
  printf "  BFS reached: %d files in %.2fs\n" (wsFilesReached s) (wsSettleTime s)

-- ───────────────────────── ProjectCache fill ───────────────────────

data FillStats = FillStats
  { fsEnqueueTime :: !Double
  , fsFillTime :: !Double
  , fsFinal :: !PC.ProjectCacheStats
  , fsCheckpoints :: ![(Double, Int)]
  -- ^ (seconds-elapsed, fresh-count) samples
  }

fillProjectCache :: [FilePath] -> IO FillStats
fillProjectCache files = do
  putStrLn $ "  starting workers..."
  pc <- PC.newProjectCache
  PC.startWorkers pc

  -- Enqueue everything as fast as possible. This itself takes a moment for
  -- 43k files because each canonicalises the path.
  putStrLn $ "  enqueueing " <> show (length files) <> " files..."
  enqStart <- getCurrentTime
  forM_ files (PC.enqueueFile pc)
  enqEnd <- getCurrentTime
  let enqueueSecs = realToFrac (diffUTCTime enqEnd enqStart) :: Double
  printf "  enqueue took %.2fs\n" enqueueSecs

  -- Sample the cache periodically until it stops growing for 5 seconds.
  putStrLn $ "  draining (sample every 1s):"
  fillStart <- getCurrentTime
  checkpointsRef <- newIORef []
  lastGrowthRef <- newIORef =<< getCurrentTime
  lastFreshRef <- newIORef (0 :: Int)

  let loop = do
        threadDelay 1_000_000
        stats <- PC.statsOf pc
        now <- getCurrentTime
        let elapsed = realToFrac (diffUTCTime now fillStart) :: Double
        printf
          "    t=%.1fs  files=%d  fresh=%d  inflight=%d\n"
          elapsed
          (PC.pcsFiles stats)
          (PC.pcsFresh stats)
          (PC.pcsInflight stats)
        hFlush stdout

        lastFresh <- readIORef lastFreshRef
        when (PC.pcsFresh stats > lastFresh) $ do
          writeIORef lastFreshRef (PC.pcsFresh stats)
          writeIORef lastGrowthRef now
        atomicModifyIORef' checkpointsRef (\xs -> ((elapsed, PC.pcsFresh stats) : xs, ()))

        lastGrowth <- readIORef lastGrowthRef
        let idleSecs = realToFrac (diffUTCTime now lastGrowth) :: Double

        -- Stop conditions: 5s idle, or 5 minutes total, or all files done.
        let allDone = PC.pcsFresh stats >= length files
        if idleSecs > 5.0 || elapsed > 300 || allDone
          then pure ()
          else loop

  loop

  fillEnd <- getCurrentTime
  PC.stopWorkers pc
  final <- PC.statsOf pc
  checkpoints <- readIORef checkpointsRef
  pure
    FillStats
      { fsEnqueueTime = enqueueSecs
      , fsFillTime = realToFrac (diffUTCTime fillEnd fillStart)
      , fsFinal = final
      , fsCheckpoints = reverse checkpoints
      }

reportFillStats :: FillStats -> Int -> IO ()
reportFillStats fs corpusSize = do
  let s = fsFinal fs
  putStrLn ""
  printf "  enqueue:    %.2fs\n" (fsEnqueueTime fs)
  printf "  drain:      %.2fs\n" (fsFillTime fs)
  printf "  corpus:     %d files\n" corpusSize
  printf
    "  cached:     %d (fresh=%d, stale=%d)\n"
    (PC.pcsFiles s)
    (PC.pcsFresh s)
    (PC.pcsStale s)
  printf
    "  coverage:   %.1f%%\n"
    (100 * fromIntegral (PC.pcsFresh s) / fromIntegral corpusSize :: Double)
  when (fsFillTime fs > 0) $
    printf
      "  throughput: %.0f files/sec (workers=%d)\n"
      (fromIntegral (PC.pcsFresh s) / fsFillTime fs :: Double)
      (PC.pcsWorkers s)

-- ───────────────────────── helpers ─────────────────────────────────

diffMicros :: UTCTime -> UTCTime -> Double
diffMicros a b = realToFrac (diffUTCTime b a) * 1_000_000

percentile :: Double -> [Double] -> Double
percentile _ [] = 0
percentile p xs =
  let n = length xs
      idx = max 0 (min (n - 1) (floor (p / 100 * fromIntegral n)))
   in xs !! idx
