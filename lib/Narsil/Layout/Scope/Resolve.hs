{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                     // layout // scope // resolve
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Names, and the things the names reached for, threaded back along the
--    edges until they found their declarations or didn't."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Name resolution and queries over a built scope graph: walk a reference's
--   scope up through edge chains (Parent, then Import, then With, …) to the
--   declaration it binds to, report unresolved/ambiguous, and answer the
--   editor's go-to-definition / find-references / outline questions. Pure
--   reads — never mutates the graph.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Scope.Resolve (
  -- * Resolution
  resolve,
  resolveAll,
  ResolutionError (..),

  -- * Queries
  declarationsInScope,
  referencesInScope,
  findDeclaration,
  findReferences,
)
where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Narsil.Layout.Scope.Types

-- ── name resolution: find which declaration a reference points to ──

-- | why a reference failed to resolve: no binding found, or more than one.
data ResolutionError
  = Unresolved Reference
  | Ambiguous Reference [Declaration]
  deriving stock (Eq, Show, Generic)

-- | resolve a single reference to its declaration
resolve :: ScopeGraph -> Reference -> Either ResolutionError Declaration
resolve scopeGraph ref = decide (findPaths scopeGraph (refScope ref) (refName ref))
 where
  decide [] = Left (Unresolved ref)
  decide [d] = Right d
  decide ds = Left (Ambiguous ref ds)

-- | resolve all references in a graph, collecting errors
resolveAll :: ScopeGraph -> Either [ResolutionError] [(Reference, Declaration)]
resolveAll scopeGraph =
  let refs = concatMap scopeReferences (Map.elems (sgScopes scopeGraph))
      results = map (\ref -> (ref, resolve scopeGraph ref)) refs
      errors = [err | (_, Left err) <- results]
      successes = [(ref, decl) | (ref, Right decl) <- results]
   in if null errors
        then Right successes
        else Left errors

{- | search for a declaration by name, walking up through edge chains
edges are grouped by label and tried in priority order (Parent, Import, With, ...)
-}
findPaths :: ScopeGraph -> ScopeId -> Text -> [Declaration]
findPaths scopeGraph startScope targetName = searchScope Set.empty startScope
 where
  searchScope :: Set ScopeId -> ScopeId -> [Declaration]
  searchScope visited scopeId
    | Set.member scopeId visited = [] -- cycle guard
    | otherwise = maybe [] inScope (Map.lookup scopeId (sgScopes scopeGraph))
   where
    inScope scope =
      let updatedVisited = Set.insert scopeId visited
          localDeclarations = filter (\d -> declName d == targetName) (scopeDeclarations scope)
          -- try each edge label group in order; stop at the first group that yields results
          fromEdges =
            firstNonEmptyGroup
              [ concatMap (searchScope updatedVisited . edgeTarget) group
              | group <- groupEdgesByLabel (scopeEdges scope)
              ]
       in if not (null localDeclarations) then localDeclarations else fromEdges

-- | group edges by their label, maintaining priority order within each group
groupEdgesByLabel :: [Edge] -> [[Edge]]
groupEdgesByLabel = groupByEdgeLabel . sortOn edgeLabel

groupByEdgeLabel :: [Edge] -> [[Edge]]
groupByEdgeLabel [] = []
groupByEdgeLabel (edge : rest) =
  let (sameLabel, different) = Prelude.span (\e -> edgeLabel e == edgeLabel edge) rest
   in (edge : sameLabel) : groupByEdgeLabel different

-- | return the first non-empty group, or [] if all are empty
firstNonEmptyGroup :: [[a]] -> [a]
firstNonEmptyGroup [] = []
firstNonEmptyGroup (group : rest)
  | null group = firstNonEmptyGroup rest
  | otherwise = group

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                        // queries
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- ── queries against the scope graph ──────────────────────────────

-- | all declarations reachable from a scope (walking edges transitively)
declarationsInScope :: ScopeGraph -> ScopeId -> [Declaration]
declarationsInScope scopeGraph = go Set.empty
 where
  go visited currentScopeId
    | Set.member currentScopeId visited = []
    | otherwise = maybe [] inScope (Map.lookup currentScopeId (sgScopes scopeGraph))
   where
    inScope scope =
      let visited' = Set.insert currentScopeId visited
       in scopeDeclarations scope ++ concatMap (go visited' . edgeTarget) (scopeEdges scope)

-- | all references in a specific scope
referencesInScope :: ScopeGraph -> ScopeId -> [Reference]
referencesInScope scopeGraph scopeId =
  maybe [] scopeReferences (Map.lookup scopeId (sgScopes scopeGraph))

-- | find all declarations with a given name across the whole graph
findDeclaration :: ScopeGraph -> Text -> [Declaration]
findDeclaration scopeGraph name =
  [ d
  | scope <- Map.elems (sgScopes scopeGraph)
  , d <- scopeDeclarations scope
  , declName d == name
  ]

-- | find all references that resolve to a specific declaration
findReferences :: ScopeGraph -> Declaration -> [Reference]
findReferences scopeGraph decl =
  [ ref
  | scope <- Map.elems (sgScopes scopeGraph)
  , ref <- scopeReferences scope
  , refName ref == declName decl
  , resolvesToDecl ref
  ]
 where
  resolvesToDecl ref =
    either
      (const False)
      (\d -> declScope d == declScope decl && declSpan d == declSpan decl)
      (resolve scopeGraph ref)
