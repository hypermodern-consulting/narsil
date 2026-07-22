{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                               // nixpkgs // warm
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The work went on without him, in the dark, the small machines turning
--    over the question of the next thing even as he slept."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The speculation layer over the eval cache: a fixed pool of workers
--   draining a priority FRONTIER of attribute paths to warm. Three lifetimes:
--   the cache is permanent; the frontier is disposable; the workers are fixed.
--
--   The worker loop parks on STM 'retry' over an empty frontier — zero CPU until
--   something is enqueued OR the frontier is swapped, the SAME wakeup covering
--   both. A focus change is therefore not a restart: it's one 'writeTVar' of a
--   fresh frontier, observed live on the next pop. In-flight evals are NEVER
--   cancelled — at most poolSize of them finish-and-cache after a swap, a bounded
--   stale tail that still lands useful work.
--
--   Demand (a completion/hover request) enqueues its target at top priority — the
--   floor, correct with no speculation at all. One-hop BFS layers on top: warming
--   a node may enqueue its children (via 'wpExpand') one priority tier down, so
--   the next request under it is a cache hit. Bounded to ONE hop by a per-item
--   depth budget.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Nixpkgs.Warm (
  -- * The pool
  WarmPool,
  newWarmPool,
  enqueueDemand,
  swapFocus,

  -- * Introspection
  pendingCount,
  inflightCount,

  -- * Frontier internals (exposed for testing)
  WarmPath,
  Frontier,
  emptyFrontier,
  insertMany,
  popHighestF,
  frontierSize,
)
where

import Control.Concurrent.Async (async)
import Control.Concurrent.STM (
  STM,
  TVar,
  atomically,
  modifyTVar',
  newTVarIO,
  readTVar,
  readTVarIO,
  retry,
  writeTVar,
 )
import Control.Exception (SomeException, try)
import Control.Monad (forM_, forever, void, when)
import Data.List (maximumBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import Narsil.Nixpkgs.Eval (EvalError (..))

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- the frontier (pure)
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

-- | An attribute path to warm, from the package-set root, e.g. @["python3Packages","requests"]@.
type WarmPath = [Text]

-- | Higher wins. Demand sits above speculation so a request always preempts pre-warming.
type Priority = Int

{- | The "what to warm next" set: each path mapped to its priority and remaining
one-hop depth BUDGET (how many further hops it may still expand into). Keying by
path makes membership and dedup O(log n) and collapses a path enqueued twice to
its strongest claim.
-}
newtype Frontier = Frontier (Map WarmPath (Priority, Int))

-- | The empty frontier (workers park on it).
emptyFrontier :: Frontier
emptyFrontier = Frontier Map.empty

-- | How many paths are queued.
frontierSize :: Frontier -> Int
frontierSize (Frontier m) = Map.size m

{- | Add many paths at one priority and depth budget. A path already present keeps
whichever claim has the higher priority (ties keep the larger remaining budget, so
the more eager intent wins).
-}
insertMany :: Priority -> Int -> [WarmPath] -> Frontier -> Frontier
insertMany prio budget paths (Frontier m) =
  Frontier (foldr (\p -> Map.insertWith strongest p (prio, budget)) m paths)
 where
  strongest new@(p1, b1) old@(p2, b2)
    | p1 /= p2 = if p1 > p2 then new else old
    | otherwise = (p1, max b1 b2)

{- | Remove and return the highest-priority path with its depth budget, or 'Nothing'
if empty. Ties resolve arbitrarily (speculation is best-effort). O(n) — negligible
beside the ~62ms eval each pop precedes.
-}
popHighestF :: Frontier -> Maybe ((WarmPath, Int), Frontier)
popHighestF (Frontier m)
  | Map.null m = Nothing
  | otherwise =
      let (path, (_, budget)) = maximumBy (comparing (fst . snd)) (Map.toList m)
       in Just ((path, budget), Frontier (Map.delete path m))

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- the pool
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{- | A warm pool: the shared frontier, the in-flight set (paths a worker is
currently evaluating), the warm action (eval-and-cache, returning the node's
attribute names for expansion), and the one-hop expansion policy.
-}
data WarmPool = WarmPool
  { wpFrontier :: !(TVar Frontier)
  , wpInflight :: !(TVar (Set WarmPath))
  , wpWarm :: !(WarmPath -> IO (Either EvalError [Text]))
  , wpExpand :: !(WarmPath -> [Text] -> [WarmPath])
  }

-- | Demand outranks speculation.
demandPriority, specPriority :: Priority
demandPriority = 1
specPriority = 0

{- | A demand item may expand one hop; the children it spawns may not. So a demand
enters with budget 1 and its children with budget 0.
-}
demandBudget :: Int
demandBudget = 1

{- | Create a pool and spawn its workers (at least one). Each worker drains the
frontier forever, parking on 'retry' when it is empty. @warm@ evaluates a path and
caches it, returning its attribute names; @expand@ turns a warmed node and its
names into the children to pre-warm one hop ahead (use @\\_ _ -> []@ for pure
demand-driven, no speculation).
-}
newWarmPool ::
  Int ->
  (WarmPath -> IO (Either EvalError [Text])) ->
  (WarmPath -> [Text] -> [WarmPath]) ->
  IO WarmPool
newWarmPool workers warm expand = do
  pool <-
    WarmPool
      <$> newTVarIO emptyFrontier
      <*> newTVarIO Set.empty
      <*> pure warm
      <*> pure expand
  forM_ [1 .. max 1 workers] (\_ -> void (async (workerLoop pool)))
  pure pool

-- | Enqueue demand targets at top priority (one-hop budget). The floor signal.
enqueueDemand :: WarmPool -> [WarmPath] -> IO ()
enqueueDemand pool paths =
  atomically (modifyTVar' (wpFrontier pool) (insertMany demandPriority demandBudget paths))

{- | Replace the whole frontier with a fresh seed (a focus change). Paths currently
in flight are dropped from the seed — they are already being warmed, and the cache
is permanent, so re-queuing them would be redundant. Nothing is cancelled; the
drain simply observes the new frontier on its next pop.
-}
swapFocus :: WarmPool -> [WarmPath] -> IO ()
swapFocus pool paths = atomically $ do
  inflight <- readTVar (wpInflight pool)
  let fresh = filter (`Set.notMember` inflight) paths
  writeTVar (wpFrontier pool) (insertMany demandPriority demandBudget fresh emptyFrontier)

-- | Paths queued in the frontier (introspection / tests).
pendingCount :: WarmPool -> IO Int
pendingCount pool = frontierSize <$> readTVarIO (wpFrontier pool)

-- | Paths a worker is currently evaluating (introspection / tests).
inflightCount :: WarmPool -> IO Int
inflightCount pool = Set.size <$> readTVarIO (wpInflight pool)

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- the worker
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

-- | Drain the frontier forever; park at zero CPU when it is empty.
workerLoop :: WarmPool -> IO ()
workerLoop pool = forever (atomically (takeNext pool) >>= warmOne pool)

{- | Claim the highest-priority path: mark it in-flight and remove it from the
frontier in one transaction, or 'retry' (park) until the frontier is written.
-}
takeNext :: WarmPool -> STM (WarmPath, Int)
takeNext pool = do
  fr <- readTVar (wpFrontier pool)
  maybe retry claim (popHighestF fr)
 where
  claim ((path, budget), fr') = do
    writeTVar (wpFrontier pool) fr'
    modifyTVar' (wpInflight pool) (Set.insert path)
    pure (path, budget)

{- | Warm one path: evaluate-and-cache (swallowing any exception so a worker never
dies), clear it from in-flight, then — if it still has depth budget — enqueue its
children one priority tier down with the budget spent by one.
-}
warmOne :: WarmPool -> (WarmPath, Int) -> IO ()
warmOne pool (path, budget) = do
  outcome <- try (wpWarm pool path) :: IO (Either SomeException (Either EvalError [Text]))
  atomically (modifyTVar' (wpInflight pool) (Set.delete path))
  let res = either (Left . EvalFailed . const "warm: exception") id outcome
      children = if budget > 0 then either (const []) (wpExpand pool path) res else []
  when (not (null children)) $
    atomically (modifyTVar' (wpFrontier pool) (insertMany specPriority (budget - 1) children))
