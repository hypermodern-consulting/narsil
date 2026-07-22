{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                       // layout // scope // build
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He flipped the construct on and watched the dataspace assemble itself,
--    scope by scope, around the seed of the parse."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Scope-graph construction: a small State monad that walks an annotated Nix
--   AST and emits scopes, declarations, references, and edges. Each
--   scope-opening form (let / set / rec set / lambda / with) creates a child
--   node linked to its parent; multi-file builds merge per-file graphs by
--   offsetting IDs and join their roots under a synthetic Import root.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Scope.Build (
  -- * Construction
  empty,
  fromNixExpr,
  fromNixFile,
  fromModuleGraph,
  mergeGraphs,
)
where

import Control.Monad (forM_)
import Control.Monad.State.Strict
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Narsil.Layout.Scope.Types
import Narsil.Syntax.Annotation (pattern Layer, pattern LayerAnn)
import Nix.Expr.Types hiding (Binding, SourcePos)
import Nix.Expr.Types qualified as Nix
import Nix.Expr.Types.Annotated
import Nix.Utils (Path (..))

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                          // construction // state
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- ── build state: graph under construction + current insertion point ─

data BuildState = BuildState
  { bsGraph :: ScopeGraph
  , bsCurrentScope :: ScopeId
  }

type Build a = State BuildState a

-- | allocate a new scope node with the given kind, insert into graph
freshScope :: ScopeKind -> Build ScopeId
freshScope kind = do
  st <- get
  let newId = ScopeId (sgNextId (bsGraph st))
  let scope = Scope newId [] [] [] kind
  put
    st
      { bsGraph =
          (bsGraph st)
            { sgScopes = Map.insert newId scope (sgScopes (bsGraph st))
            , sgNextId = sgNextId (bsGraph st) + 1
            }
      }
  pure newId

-- | append a declaration to the current scope's declaration list
addDecl :: Declaration -> Build ()
addDecl decl = do
  st <- get
  let sid = declScope decl
  let update s = s{scopeDeclarations = decl : scopeDeclarations s}
  put
    st
      { bsGraph =
          (bsGraph st)
            { sgScopes = Map.adjust update sid (sgScopes (bsGraph st))
            }
      }

-- | record a reference in the current scope
addRef :: Reference -> Build ()
addRef ref = do
  st <- get
  let sid = refScope ref
  let update s = s{scopeReferences = ref : scopeReferences s}
  put
    st
      { bsGraph =
          (bsGraph st)
            { sgScopes = Map.adjust update sid (sgScopes (bsGraph st))
            }
      }

-- | add an edge between two scopes (parent, import, with, inherit, attr-access)
addEdge :: Edge -> Build ()
addEdge edge = do
  st <- get
  let sid = edgeSource edge
  let update s = s{scopeEdges = edge : scopeEdges s}
  put
    st
      { bsGraph =
          (bsGraph st)
            { sgScopes = Map.adjust update sid (sgScopes (bsGraph st))
            }
      }

-- | read the current scope (insertion point)
currentScope :: Build ScopeId
currentScope = gets bsCurrentScope

-- | run an action under a given scope, restoring the previous one after
withScope :: ScopeId -> Build a -> Build a
withScope sid action = do
  old <- gets bsCurrentScope
  modify $ \st -> st{bsCurrentScope = sid}
  result <- action
  modify $ \st -> st{bsCurrentScope = old}
  pure result

-- | create a child scope, link it to parent via Parent edge, run action
withChildScope :: ScopeKind -> (ScopeId -> Build ()) -> Build ()
withChildScope kind action = do
  parent <- currentScope
  childScope <- freshScope kind
  addEdge (Edge childScope parent Parent)
  withScope childScope (action childScope)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                    // construction // from // nix
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- ── graph construction ───────────────────────────────────────────

-- | empty scope graph: one root FileScope node
empty :: ScopeGraph
empty =
  ScopeGraph
    { sgScopes = Map.singleton (ScopeId 0) (Scope (ScopeId 0) [] [] [] FileScope)
    , sgRoot = ScopeId 0
    , sgNextId = 1
    , sgFile = Nothing
    }

-- | build a scope graph from a single Nix expression (may be file-backed)
fromNixExpr :: Maybe FilePath -> NExprLoc -> ScopeGraph
fromNixExpr maybeFilePath expr =
  let initState =
        BuildState
          { bsGraph = empty{sgFile = maybeFilePath}
          , bsCurrentScope = ScopeId 0
          }
      finalState = execState (buildExpr expr) initState
   in bsGraph finalState

-- | build scope graph, recording the file path
fromNixFile :: FilePath -> NExprLoc -> ScopeGraph
fromNixFile = fromNixExpr . Just

-- | build a combined scope graph from multiple files, linking imports
fromModuleGraph :: Map FilePath NExprLoc -> ScopeGraph
fromModuleGraph modules
  | Map.null modules = empty
  | otherwise =
      let fileGraphs = Map.mapWithKey fromNixFile modules
          (merged, fileRoots) = mergeGraphs (Map.elems fileGraphs)
          withImports = addImportEdges merged fileRoots
       in withImports

-- | merge multiple scope graphs into one by offsetting scope IDs
mergeGraphs :: [ScopeGraph] -> (ScopeGraph, [ScopeId])
mergeGraphs [] = (empty, [])
mergeGraphs graphs =
  let (finalGraph, _, roots) = foldl mergeOneGraph (empty, sgNextId empty, []) graphs
   in (finalGraph, reverse roots)

{- | merge a single graph into the accumulator, remapping all IDs by an offset
n.b. the offset ensures no ID collisions across files
-}
mergeOneGraph :: (ScopeGraph, Int, [ScopeId]) -> ScopeGraph -> (ScopeGraph, Int, [ScopeId])
mergeOneGraph (accumulator, nextId, roots) scopeGraph =
  let offset = nextId - unScopeId (sgRoot scopeGraph)
      remappedScopes =
        Map.fromList
          [ (remapScopeId offset scopeId, remapEntireScope offset scope)
          | (scopeId, scope) <- Map.toList (sgScopes scopeGraph)
          ]
      newRoot = remapScopeId offset (sgRoot scopeGraph)
      maxId =
        if Map.null remappedScopes
          then nextId
          else maximum (map (unScopeId . fst) (Map.toList remappedScopes)) + 1
      updatedAccumulator =
        accumulator
          { sgScopes = Map.union (sgScopes accumulator) remappedScopes
          , sgNextId = maxId
          , sgFile = chooseFile (sgFile accumulator) (sgFile scopeGraph)
          }
   in (updatedAccumulator, maxId, newRoot : roots)

-- | shift a ScopeId by a constant offset (to avoid collisions during merge)
remapScopeId :: Int -> ScopeId -> ScopeId
remapScopeId offset scopeId = ScopeId (unScopeId scopeId + offset)

-- | remap all IDs inside a scope (declarations, refs, edges)
remapEntireScope :: Int -> Scope -> Scope
remapEntireScope offset scope =
  scope
    { scopeId = remapScopeId offset (scopeId scope)
    , scopeDeclarations = map (remapDeclaration offset) (scopeDeclarations scope)
    , scopeReferences = map (remapReference offset) (scopeReferences scope)
    , scopeEdges = map (remapEdge' offset) (scopeEdges scope)
    }

remapDeclaration :: Int -> Declaration -> Declaration
remapDeclaration offset declaration =
  declaration
    { declScope = remapScopeId offset (declScope declaration)
    , declAssocScope = fmap (remapScopeId offset) (declAssocScope declaration)
    }

remapReference :: Int -> Reference -> Reference
remapReference offset reference = reference{refScope = remapScopeId offset (refScope reference)}

remapEdge' :: Int -> Edge -> Edge
remapEdge' offset edge =
  edge
    { edgeSource = remapScopeId offset (edgeSource edge)
    , edgeTarget = remapScopeId offset (edgeTarget edge)
    }

-- | prefer the first file path over the second
chooseFile :: Maybe FilePath -> Maybe FilePath -> Maybe FilePath
chooseFile (Just a) _ = Just a
chooseFile Nothing b = b

{- | add Import edges connecting file roots under a synthetic global root
single-file graphs just use that file as root; multi-file gets a virtual parent
-}
addImportEdges :: ScopeGraph -> [ScopeId] -> ScopeGraph
addImportEdges scopeGraph [] = scopeGraph
addImportEdges scopeGraph [single] = scopeGraph{sgRoot = single}
addImportEdges scopeGraph fileRoots =
  let globalRoot = ScopeId (sgNextId scopeGraph)
      globalScope =
        Scope globalRoot [] [] (map (\fr -> Edge globalRoot fr Import) fileRoots) FileScope
   in scopeGraph
        { sgScopes = Map.insert globalRoot globalScope (sgScopes scopeGraph)
        , sgRoot = globalRoot
        , sgNextId = sgNextId scopeGraph + 1
        }

-- ── expression traversal helpers ─────────────────────────────────

-- | register a symbol reference at the current scope
buildSymbolRef :: SrcSpan -> VarName -> Build ()
buildSymbolRef srcSpan name = do
  scope <- currentScope
  addRef $
    Reference
      { refName = coerce name
      , refSpan = toSourceSpan srcSpan
      , refScope = scope
      , refKind = VarRef
      }

-- | register an attribute reference (e.name) at a given scope
addAttrRef :: SrcSpan -> ScopeId -> NKeyName NExprLoc -> Build ()
addAttrRef srcSpan scope keyName =
  addRef $
    Reference
      { refName = keyToText keyName
      , refSpan = toSourceSpan srcSpan
      , refScope = scope
      , refKind = AttrRef
      }

{- | build scope sub-graph for `with expr; body`
creates two scopes: one for the with-expression, one for the body
body scope has a With-edge to the expr scope
-}
buildWithExpr :: SrcSpan -> NExprLoc -> NExprLoc -> Build ()
buildWithExpr _srcSpan withExpr body = do
  parent <- currentScope
  withExprScope <- freshScope WithScope
  addEdge (Edge withExprScope parent Parent)
  withScope withExprScope $ buildExpr withExpr
  bodyScopeId <- freshScope LetScope
  addEdge (Edge bodyScopeId parent Parent)
  addEdge (Edge bodyScopeId withExprScope With)
  withScope bodyScopeId $ buildExpr body

-- ── string part extraction ──────────────────────────────────────

-- | extract all Nix expressions embedded within string antiquotations
exprsFromString :: NString NExprLoc -> [NExprLoc]
exprsFromString (DoubleQuoted parts) = mapMaybe extractExpr parts
exprsFromString (Indented _ parts) = mapMaybe extractExpr parts

extractExpr :: Antiquoted Text NExprLoc -> Maybe NExprLoc
extractExpr (Antiquoted e) = Just e
extractExpr _ = Nothing

-- ── walk an expression, building scope graph nodes ─────────────────

{- | dispatch on AST node to create scopes, declarations, and references
each scope-creating AST form (let, set, lambda, with) opens a child scope
-}
buildExpr :: NExprLoc -> Build ()
-- let ... in ...: child scope, declare all bindings in it
buildExpr (Layer (NLet bindings body)) =
  withChildScope LetScope $ \letScope -> do
    mapM_ (addBindingDecl letScope) bindings
    mapM_ buildBinding bindings
    buildExpr body
-- non-recursive set: child scope
buildExpr (Layer (NSet NonRecursive bindings)) =
  withChildScope AttrSetScope $ \attrScope -> do
    mapM_ (addBindingDecl attrScope) bindings
    mapM_ buildBinding bindings
-- recursive set: a separate scope kind so we can distinguish it
buildExpr (Layer (NSet Recursive bindings)) =
  withChildScope RecAttrSetScope $ \attrScope -> do
    mapM_ (addBindingDecl attrScope) bindings
    mapM_ buildBinding bindings
-- lambda: function scope with parameter declarations
buildExpr (Layer (NAbs params body)) =
  withChildScope FunctionScope $ \funScope -> do
    addParamDecls funScope params
    buildExpr body
-- with expr; body: a With-scope linked to the expr scope
buildExpr (LayerAnn srcSpan (NWith withExpr body)) = buildWithExpr srcSpan withExpr body
-- symbol reference
buildExpr (LayerAnn srcSpan (NSym name)) = buildSymbolRef srcSpan name
-- attribute select: base + attr references
buildExpr (LayerAnn srcSpan (NSelect _ base (attr :| rest))) = do
  buildExpr base
  scope <- currentScope
  addAttrRef srcSpan scope attr
  mapM_ (addAttrRef srcSpan scope) rest
-- application / binary / unary / conditional / assert: recurse into subexprs
buildExpr (Layer (NApp func arg)) = buildExpr func >> buildExpr arg
buildExpr (Layer (NBinary _ left right)) = buildExpr left >> buildExpr right
buildExpr (Layer (NUnary _ operand)) = buildExpr operand
buildExpr (Layer (NIf cond thenBranch elseBranch)) = do
  buildExpr cond
  buildExpr thenBranch
  buildExpr elseBranch
buildExpr (Layer (NAssert cond body)) = buildExpr cond >> buildExpr body
-- list: every element; string: each antiquoted expression (e.g. ${srv.host})
buildExpr (Layer (NList elements)) = mapM_ buildExpr elements
buildExpr (Layer (NStr strParts)) = mapM_ buildExpr (exprsFromString strParts)
-- has-attr: walk the base; leaves (paths, holes) and everything else: nothing
buildExpr (Layer (NHasAttr base _pat)) = buildExpr base
buildExpr _ = pure ()

addBindingDecl :: ScopeId -> Nix.Binding NExprLoc -> Build ()
addBindingDecl scope (Nix.NamedVar (StaticKey name :| []) _ srcSpan) =
  addDecl
    Declaration
      { declName = coerce name
      , declSpan = toSourceSpan' srcSpan
      , declScope = scope
      , declAssocScope = Nothing
      , declType = Nothing
      , declDoc = Nothing
      }
addBindingDecl scope (Nix.Inherit _ names srcSpan) =
  forM_ names $ \varName ->
    addDecl
      Declaration
        { declName = coerce varName
        , declSpan = toSourceSpan' srcSpan
        , declScope = scope
        , declAssocScope = Nothing
        , declType = Nothing
        , declDoc = Nothing
        }
addBindingDecl _ _ = pure ()

buildBinding :: Nix.Binding NExprLoc -> Build ()
buildBinding (Nix.NamedVar _ expr _) = buildExpr expr
buildBinding (Nix.Inherit (Just expr) _ _) = buildExpr expr
buildBinding (Nix.Inherit Nothing _ _) = pure ()

-- ── register parameter declarations in the function's scope ─────────

-- | declare lambda parameters: simple name, or set-pattern (with optional @-bind)
addParamDecls :: ScopeId -> Params NExprLoc -> Build ()
-- simple param: f = x: ...
addParamDecls scope (Param name) =
  addDecl
    Declaration
      { declName = coerce name
      , declSpan = emptySpan
      , declScope = scope
      , declAssocScope = Nothing
      , declType = Nothing
      , declDoc = Nothing
      }
-- set pattern: { name ? default, ... } @ self ->
addParamDecls scope (ParamSet mname _variadic pset) = do
  addParamSetAtName scope mname
  forM_ pset $ \(pname, mdefault) -> do
    addDecl
      Declaration
        { declName = coerce pname
        , declSpan = emptySpan
        , declScope = scope
        , declAssocScope = Nothing
        , declType = Nothing
        , declDoc = Nothing
        }
    mapM_ buildExpr mdefault
 where
  addParamSetAtName sc (Just pname) =
    addDecl
      Declaration
        { declName = coerce pname
        , declSpan = emptySpan
        , declScope = sc
        , declAssocScope = Nothing
        , declType = Nothing
        , declDoc = Nothing
        }
  addParamSetAtName _ Nothing = pure ()

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                      // utilities
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- ── Nix SrcSpan → our SourceSpan ─────────────────────────────────

-- | convert a ranged Nix SrcSpan (begin..end) to our SourceSpan
toSourceSpan :: SrcSpan -> SourceSpan
toSourceSpan srcSpan =
  let begin = getSpanBegin srcSpan
      end = getSpanEnd srcSpan
   in SourceSpan
        { spanStart = SourcePos (sourceLine begin) (sourceCol begin)
        , spanEnd = SourcePos (sourceLine end) (sourceCol end)
        , spanFile = Just (coerce (sourcePath begin))
        }
 where
  sourcePath (NSourcePos path _ _) = path
  sourceLine (NSourcePos _ (NPos l) _) = unPos l
  sourceCol (NSourcePos _ _ (NPos c)) = unPos c

-- | convert a point Nix NSourcePos to a zero-width SourceSpan
toSourceSpan' :: NSourcePos -> SourceSpan
toSourceSpan' (NSourcePos path (NPos l) (NPos c)) =
  SourceSpan
    { spanStart = SourcePos (unPos l) (unPos c)
    , spanEnd = SourcePos (unPos l) (unPos c)
    , spanFile = Just (coerce path)
    }

-- | sentinel span used for synthetic nodes (parameter declarations, etc.)
emptySpan :: SourceSpan
emptySpan = SourceSpan (SourcePos 0 0) (SourcePos 0 0) Nothing

-- ── key → text (dynamic keys get a placeholder) ──────────────────
keyToText :: NKeyName r -> Text
keyToText (StaticKey name) = coerce name
keyToText (DynamicKey _) = "<dynamic>"
