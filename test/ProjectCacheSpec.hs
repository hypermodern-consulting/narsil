{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                         // tests // project cache
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "There is no there, there. They taught that, in seminars on the metric
--    cosmos."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module ProjectCacheSpec (projectCacheTests) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Narsil.LSP.ProjectCache qualified as PC
import System.Directory (canonicalizePath)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

-- ── helpers ────────────────────────────────────────────────────────

{- | Block until either the predicate holds or the timeout (μs) elapses.
Returns True on success, False on timeout. Polling is the right pattern for
"wait for an async worker to finish" — we deliberately don't expose a
"join the workers" API on ProjectCache because real LSP code can't block.
-}
waitFor :: Int -> IO Bool -> IO Bool
waitFor totalMicros predicate = go 0
 where
  pollIntervalMicros :: Int
  pollIntervalMicros = 10_000 -- 10 ms
  go elapsed
    | elapsed >= totalMicros = pure False
    | otherwise = do
        ok <- predicate
        if ok
          then pure True
          else do
            threadDelay pollIntervalMicros
            go (elapsed + pollIntervalMicros)

-- | Wait up to 5 s for the cache to contain N fresh entries.
waitForFreshCount :: PC.ProjectCache -> Int -> IO Bool
waitForFreshCount pc n =
  waitFor 5_000_000 $ do
    stats <- PC.statsOf pc
    pure (PC.pcsFresh stats >= n)

-- ── tests ──────────────────────────────────────────────────────────

{- | Smoke: a fresh cache reports empty stats, lookupFile returns Nothing for
any path, and no workers are pegged unless we start them.
-}
testEmptyCache :: IO Bool
testEmptyCache = do
  pc <- PC.newProjectCache
  stats <- PC.statsOf pc
  let !ok = PC.pcsFiles stats == 0 && PC.pcsFresh stats == 0 && PC.pcsWorkers stats == 0
  nope <- PC.lookupFile pc "/nonexistent/file.nix"
  case nope of
    Nothing -> pure ok
    Just _ -> pure False

-- | A single enqueue → after worker drains, lookupFile returns Fresh entry.
testSingleFileEnqueueAndLookup :: IO Bool
testSingleFileEnqueueAndLookup =
  withSystemTempDirectory "pc-single" $ \tmp -> do
    let path = tmp </> "a.nix"
    TIO.writeFile path "{ a = 1; b = 2; }"
    pc <- PC.newProjectCache
    PC.startWorkers pc
    PC.enqueueFile pc path
    drained <- waitForFreshCount pc 1
    if not drained
      then pure False
      else do
        lookup' <- PC.lookupFile pc path
        PC.stopWorkers pc
        pure (case lookup' of Just _ -> True; Nothing -> False)

{- | Editing a file (re-enqueuing with new content) replaces the cached entry
and changes the hash; content-identical re-enqueues are no-ops at the
worker level (hash short-circuits the recompute).
| Two-shot property:
  (a) re-enqueuing an unchanged Fresh file is a no-op (hash stable).
  (b) after content changes, 'invalidateFile' triggers a recompute
      and the new hash is different.
This is the real LSP idiom: didSave calls invalidateFile, not bare enqueue.
-}
testContentHashShortCircuit :: IO Bool
testContentHashShortCircuit =
  withSystemTempDirectory "pc-hash" $ \tmp -> do
    let path = tmp </> "a.nix"
    TIO.writeFile path "{ a = 1; }"
    pc <- PC.newProjectCache
    PC.startWorkers pc
    PC.enqueueFile pc path
    _ <- waitForFreshCount pc 1
    firstLookup <- PC.lookupFile pc path
    let firstHash = fmap PC.feHash firstLookup

    -- (a) Same content, re-enqueue: short-circuited at the enqueue gate
    -- (already-Fresh). Hash identical.
    PC.enqueueFile pc path
    threadDelay 50_000
    secondLookup <- PC.lookupFile pc path
    let secondHash = fmap PC.feHash secondLookup

    -- (b) Change the file's contents on disk, then invalidate (the LSP
    -- idiom). Worker recomputes; hash changes.
    TIO.writeFile path "{ a = 2; b = 3; }"
    PC.invalidateFile pc path
    _ <- waitFor 1_000_000 $ do
      now <- PC.lookupFile pc path
      pure
        ( case now of
            Just e -> PC.feHash e /= maybe mempty PC.feHash firstLookup
            Nothing -> False
        )
    thirdLookup <- PC.lookupFile pc path
    let thirdHash = fmap PC.feHash thirdLookup

    PC.stopWorkers pc
    pure (firstHash == secondHash && thirdHash /= firstHash && thirdHash /= Nothing)

{- | An import edge A → B makes A appear in reverseDeps[B]; marking B stale
cascades the Stale status onto A.
-}
testReverseDepCascade :: IO Bool
testReverseDepCascade =
  withSystemTempDirectory "pc-rev" $ \tmp -> do
    let bPath = tmp </> "b.nix"
        aPath = tmp </> "a.nix"
    canonBPath <- canonicalizePath =<< (TIO.writeFile bPath "{ y = 99; }" >> pure bPath)
    _ <-
      canonicalizePath
        =<< (TIO.writeFile aPath ("let x = import " <> T.pack bPath <> "; in x.y") >> pure aPath)

    pc <- PC.newProjectCache
    PC.startWorkers pc
    PC.enqueueFile pc aPath
    PC.enqueueFile pc bPath
    _ <- waitForFreshCount pc 2

    -- Now mark B stale; A (which imports B) should also become Stale.
    PC.markStale pc canonBPath
    threadDelay 20_000

    snap <- PC.snapshotFiles pc
    canonA <- canonicalizePath aPath
    canonB <- canonicalizePath bPath
    let aStale = case Map.lookup canonA snap of
          Just e -> PC.feStatus e == PC.Stale
          Nothing -> False
        bStale = case Map.lookup canonB snap of
          Just e -> PC.feStatus e == PC.Stale
          Nothing -> False

    PC.stopWorkers pc
    pure (aStale && bStale)

{- | Concurrent enqueues of the same file don't double-process: even with
many enqueues, the worker only writes one entry. (Smoke test for the
'pcInflight' tracking.)
-}
testConcurrentEnqueueDedup :: IO Bool
testConcurrentEnqueueDedup =
  withSystemTempDirectory "pc-dedup" $ \tmp -> do
    let path = tmp </> "a.nix"
    TIO.writeFile path "{ a = 1; }"
    pc <- PC.newProjectCache
    PC.startWorkers pc
    -- Enqueue 50 times in rapid succession.
    replicateM_ 50 (PC.enqueueFile pc path)
    _ <- waitForFreshCount pc 1
    threadDelay 50_000
    stats <- PC.statsOf pc
    PC.stopWorkers pc
    -- Even with 50 enqueues, we end up with exactly one entry.
    pure (PC.pcsFiles stats == 1)

{- | Looking up a stale entry returns Nothing (consumers fall back to
single-file inference).
-}
testLookupOfStaleReturnsNothing :: IO Bool
testLookupOfStaleReturnsNothing =
  withSystemTempDirectory "pc-stale" $ \tmp -> do
    let path = tmp </> "a.nix"
    TIO.writeFile path "{ a = 1; }"
    pc <- PC.newProjectCache
    PC.startWorkers pc
    PC.enqueueFile pc path
    _ <- waitForFreshCount pc 1
    PC.stopWorkers pc -- stop workers so markStale's effect is observable
    canon <- canonicalizePath path
    PC.markStale pc canon
    result <- PC.lookupFile pc path
    pure (case result of Nothing -> True; Just _ -> False)

-- | A nonexistent file enqueued is harmlessly skipped.
testNonexistentFileEnqueue :: IO Bool
testNonexistentFileEnqueue = do
  pc <- PC.newProjectCache
  PC.startWorkers pc
  PC.enqueueFile pc "/this/path/does/not/exist.nix"
  threadDelay 100_000
  stats <- PC.statsOf pc
  PC.stopWorkers pc
  pure (PC.pcsFiles stats == 0)

-- ── runner ─────────────────────────────────────────────────────────

projectCacheTests :: [(String, IO Bool)]
projectCacheTests =
  [ ("pc_empty_cache", testEmptyCache)
  , ("pc_enqueue_and_lookup", testSingleFileEnqueueAndLookup)
  , ("pc_content_hash_short_circuit", testContentHashShortCircuit)
  , ("pc_reverse_dep_cascade", testReverseDepCascade)
  , ("pc_concurrent_dedup", testConcurrentEnqueueDedup)
  , ("pc_stale_returns_nothing", testLookupOfStaleReturnsNothing)
  , ("pc_nonexistent_file", testNonexistentFileEnqueue)
  ]
