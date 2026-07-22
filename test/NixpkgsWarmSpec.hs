{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                       // tests // nixpkgs // warm
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He set the little machines to their task and watched them work the dark."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The background-warm pool, driven by a FAKE warm action (a recorded set plus a
--   per-path gate) so every property is hermetic — no nix, no eval:
--
--     * drain & dedup — a batch with duplicates warms each path once;
--     * retry-park — an idle pool makes no progress, then wakes on the next enqueue;
--     * the SWAP TRIO — across a focus change: a previously-warmed entry survives,
--       an in-flight eval still lands (never cancelled), and workers pick up the
--       new frontier;
--     * one-hop BFS — a demand node expands to its children once, and no further
--       (the depth budget halts at the second hop).
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module NixpkgsWarmSpec (nixpkgsWarmTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Narsil.Nixpkgs.Eval (EvalError)
import Narsil.Nixpkgs.Warm (
  WarmPath,
  enqueueDemand,
  inflightCount,
  newWarmPool,
  swapFocus,
 )

-- ── helpers ────────────────────────────────────────────────────────

{- | A fake warm action: record the call, optionally block on a per-path gate (to
hold an eval in flight), then mark the path warmed and return its canned children.
-}
mkWarm ::
  TVar (Set WarmPath) ->
  TVar (Map WarmPath Int) ->
  Map WarmPath (MVar ()) ->
  Map WarmPath [Text] ->
  (WarmPath -> IO (Either EvalError [Text]))
mkWarm warmed calls gates childMap path = do
  atomically (modifyTVar' calls (Map.insertWith (+) path 1))
  maybe (pure ()) takeMVar (Map.lookup path gates)
  atomically (modifyTVar' warmed (Set.insert path))
  pure (Right (Map.findWithDefault [] path childMap))

-- | No speculation: warming a node enqueues nothing.
noExpand :: WarmPath -> [Text] -> [WarmPath]
noExpand _ _ = []

-- | Append each child name to the parent — the canonical one-hop expansion.
childExpand :: WarmPath -> [Text] -> [WarmPath]
childExpand parent = map (\n -> parent <> [n])

-- | Poll a condition up to ~2s (400 × 5ms); 'True' if it held in time.
waitFor :: IO Bool -> IO Bool
waitFor = go (400 :: Int)
 where
  go 0 _ = pure False
  go n chk = chk >>= \ok -> if ok then pure True else threadDelay 5000 >> go (n - 1) chk

-- ── tests ──────────────────────────────────────────────────────────

-- | A batch with duplicates warms each distinct path exactly once.
testDrainDedup :: IO Bool
testDrainDedup = do
  warmed <- newTVarIO Set.empty
  calls <- newTVarIO Map.empty
  pool <- newWarmPool 2 (mkWarm warmed calls Map.empty Map.empty) noExpand
  enqueueDemand pool [["a"], ["b"], ["c"], ["a"], ["a"]]
  done <- waitFor ((== Set.fromList [["a"], ["b"], ["c"]]) <$> readTVarIO warmed)
  cs <- readTVarIO calls
  pure (done && Map.lookup ["a"] cs == Just 1 && Map.size cs == 3)

-- | An idle pool makes no progress, then wakes on the next enqueue (retry-park).
testRetryPark :: IO Bool
testRetryPark = do
  warmed <- newTVarIO Set.empty
  calls <- newTVarIO Map.empty
  pool <- newWarmPool 1 (mkWarm warmed calls Map.empty Map.empty) noExpand
  threadDelay 30000 -- empty frontier: a parked worker must not progress
  idle <- Set.null <$> readTVarIO warmed
  enqueueDemand pool [["x"]]
  woke <- waitFor (Set.member ["x"] <$> readTVarIO warmed)
  pure (idle && woke)

{- | The swap trio: across a focus change, a previously-warmed entry survives, an
in-flight eval still lands (never cancelled), and workers pick up the new frontier.
-}
testSwapTrio :: IO Bool
testSwapTrio = do
  warmed <- newTVarIO Set.empty
  calls <- newTVarIO Map.empty
  gateX <- newEmptyMVar
  pool <- newWarmPool 2 (mkWarm warmed calls (Map.singleton ["x"] gateX) Map.empty) noExpand
  -- pre-warm z (cached before the swap)
  enqueueDemand pool [["z"]]
  zPre <- waitFor (Set.member ["z"] <$> readTVarIO warmed)
  -- x is popped, goes in-flight, and blocks on its gate
  enqueueDemand pool [["x"]]
  xInflight <- waitFor ((>= 1) <$> inflightCount pool)
  -- swap the frontier to y (no longer contains x or z)
  swapFocus pool [["y"]]
  -- a free worker warms y from the new frontier while x is still blocked
  yWarmed <- waitFor (Set.member ["y"] <$> readTVarIO warmed)
  -- release x: the in-flight eval must still land despite the swap
  putMVar gateX ()
  xLanded <- waitFor (Set.member ["x"] <$> readTVarIO warmed)
  zSurvived <- Set.member ["z"] <$> readTVarIO warmed
  pure (zPre && xInflight && yWarmed && xLanded && zSurvived)

{- | One-hop BFS: a demand node expands to its children once; the children, out of
budget, do not expand further.
-}
testOneHopBounded :: IO Bool
testOneHopBounded = do
  warmed <- newTVarIO Set.empty
  calls <- newTVarIO Map.empty
  let childMap = Map.fromList [(["ns"], ["a", "b"]), (["ns", "a"], ["deep"])]
  pool <- newWarmPool 2 (mkWarm warmed calls Map.empty childMap) childExpand
  enqueueDemand pool [["ns"]]
  hop <- waitFor (allWarmed warmed [["ns"], ["ns", "a"], ["ns", "b"]])
  threadDelay 30000 -- settle: a second hop, if it happened, would land here
  deepAbsent <- Set.notMember ["ns", "a", "deep"] <$> readTVarIO warmed
  pure (hop && deepAbsent)
 where
  allWarmed warmed want = (\w -> all (`Set.member` w) want) <$> readTVarIO warmed

-- ── runner ─────────────────────────────────────────────────────────

-- | The background-warm orchestration tests (hermetic; fake warm action only).
nixpkgsWarmTests :: [(String, IO Bool)]
nixpkgsWarmTests =
  [ ("warm_drain_dedup", testDrainDedup)
  , ("warm_retry_park", testRetryPark)
  , ("warm_swap_trio", testSwapTrio)
  , ("warm_one_hop_bounded", testOneHopBounded)
  ]
