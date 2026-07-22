{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                       // layout // scope // types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "A map of the dataspace, every node and edge of it, drawn in the dark
--    behind his eyes."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The scope-graph vocabulary: nodes (scopes), the declarations and
--   references they hold, the labelled edges between them, and source
--   locations. Dependency-free — every other Scope module builds on this one —
--   plus the canonical JSON projection (the instances live with the types they
--   serialize, so there are no orphans).
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Scope.Types (
  -- * Core Types
  ScopeGraph (..),
  Scope (..),
  ScopeId (..),
  ScopeKind (..),
  Declaration (..),
  Reference (..),
  RefKind (..),
  Edge (..),
  EdgeLabel (..),

  -- * Source Locations
  SourceSpan (..),
  SourcePos (..),
)
where

import Data.Aeson (ToJSON (..), ToJSONKey (..), (.=))
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import Dhall (ToDhall (..))
import GHC.Generics (Generic)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                  // core // types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

{- | the whole scope graph for one file: all scopes by id, the root scope, the
next fresh id to hand out, and the originating file path.
-}
data ScopeGraph = ScopeGraph
  { sgScopes :: Map ScopeId Scope
  , sgRoot :: ScopeId
  , sgNextId :: Int
  , sgFile :: Maybe FilePath
  }
  deriving stock (Eq, Show, Generic)

{- | a single scope (graph node): its id, the declarations and references it
holds, its outgoing edges, and what kind of scope it is.
-}
data Scope = Scope
  { scopeId :: ScopeId
  , scopeDeclarations :: [Declaration]
  , scopeReferences :: [Reference]
  , scopeEdges :: [Edge]
  , scopeKind :: ScopeKind
  }
  deriving stock (Eq, Show, Generic)

{- | what syntactic construct introduced a scope (file, @let@, attrset, @rec@
attrset, lambda, or @with@).
-}
data ScopeKind
  = FileScope
  | LetScope
  | AttrSetScope
  | RecAttrSetScope
  | FunctionScope
  | WithScope
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToDhall)

-- | a scope's unique identifier within a 'ScopeGraph'.
newtype ScopeId = ScopeId {unScopeId :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToJSON, ToJSONKey)

{- | a name bound in a scope: its name, source span, owning scope, an optional
associated scope (e.g. a lambda body), and optional inferred type and doc.
-}
data Declaration = Declaration
  { declName :: Text
  , declSpan :: SourceSpan
  , declScope :: ScopeId
  , declAssocScope :: Maybe ScopeId
  , declType :: Maybe Text
  , declDoc :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

{- | a use of a name: the referenced name, its source span, the scope it occurs
in, and the kind of reference.
-}
data Reference = Reference
  { refName :: Text
  , refSpan :: SourceSpan
  , refScope :: ScopeId
  , refKind :: RefKind
  }
  deriving stock (Eq, Show, Generic)

-- | how a name is referenced: plain variable, attribute access, @inherit@, or import.
data RefKind
  = VarRef
  | AttrRef
  | InheritRef
  | ImportRef
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToDhall)

-- | a labelled, directed edge between two scopes in the graph.
data Edge = Edge
  { edgeSource :: ScopeId
  , edgeTarget :: ScopeId
  , edgeLabel :: EdgeLabel
  }
  deriving stock (Eq, Show, Generic)

{- | the label on a scope edge: lexical @Parent@, @Import@, @With@, @Inherit@,
or @AttrAccess@ — the relation that resolution follows.
-}
data EdgeLabel
  = Parent
  | Import
  | With
  | Inherit
  | AttrAccess
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToDhall)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                            // source // locations
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | a source range: start and end positions plus an optional file path.
data SourceSpan = SourceSpan
  { spanStart :: SourcePos
  , spanEnd :: SourcePos
  , spanFile :: Maybe FilePath
  }
  deriving stock (Eq, Show, Generic)

-- | a 1-based line/column source position.
data SourcePos = SourcePos
  { posLine :: Int
  , posCol :: Int
  }
  deriving stock (Eq, Show, Generic)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                 // json // export
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

instance ToJSON ScopeGraph where
  toJSON scopeGraph =
    Aeson.object
      [ "scopes" .= sgScopes scopeGraph
      , "root" .= sgRoot scopeGraph
      , "file" .= sgFile scopeGraph
      ]

instance ToJSON Scope where
  toJSON s =
    Aeson.object
      [ "id" .= scopeId s
      , "declarations" .= scopeDeclarations s
      , "references" .= scopeReferences s
      , "edges" .= scopeEdges s
      , "kind" .= show (scopeKind s)
      ]

instance ToJSON Declaration where
  toJSON d =
    Aeson.object
      [ "name" .= declName d
      , "span" .= declSpan d
      , "scope" .= declScope d
      , "assocScope" .= declAssocScope d
      , "type" .= declType d
      , "doc" .= declDoc d
      ]

instance ToJSON Reference where
  toJSON r =
    Aeson.object
      [ "name" .= refName r
      , "span" .= refSpan r
      , "scope" .= refScope r
      , "kind" .= show (refKind r)
      ]

instance ToJSON Edge where
  toJSON e =
    Aeson.object
      [ "source" .= edgeSource e
      , "target" .= edgeTarget e
      , "label" .= show (edgeLabel e)
      ]

instance ToJSON SourceSpan where
  toJSON s =
    Aeson.object
      [ "start" .= spanStart s
      , "end" .= spanEnd s
      , "file" .= spanFile s
      ]

instance ToJSON SourcePos where
  toJSON p =
    Aeson.object
      [ "line" .= posLine p
      , "col" .= posCol p
      ]
