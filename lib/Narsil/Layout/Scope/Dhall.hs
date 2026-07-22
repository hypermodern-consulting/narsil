{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                       // layout // scope // dhall
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The whole construct, frozen and rendered, ready to be handed across to
--    something that spoke another language entirely."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Dhall projection of a scope graph (for zeitschrift): a flat, Natural-keyed
--   mirror of the in-memory types — IDs become naturals, the Map of scopes
--   becomes a list — and the 'ToDhall'-driven render. The export types are
--   deliberately separate from the working types so the wire shape can move
--   independently of the engine.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Scope.Dhall (
  toDhall,
)
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Dhall (ToDhall (..))
import Dhall qualified
import Dhall.Core qualified as Dhall
import Dhall.Marshal.Encode qualified as Encode
import GHC.Generics (Generic)
import Narsil.Layout.Scope.Types
import Numeric.Natural (Natural)

data SourcePosExport = SourcePosExport
  { line :: Natural
  , col :: Natural
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToDhall)

data SourceSpanExport = SourceSpanExport
  { start :: SourcePosExport
  , end :: SourcePosExport
  , file :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToDhall)

data DeclarationExport = DeclarationExport
  { name :: Text
  , span :: SourceSpanExport
  , scope :: Natural
  , assocScope :: Maybe Natural
  , type_ :: Maybe Text
  , doc :: Maybe Text
  , kind :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToDhall DeclarationExport where
  injectWith _normalizer =
    let opts =
          Encode.defaultInterpretOptions
            { Encode.fieldModifier = T.dropWhileEnd (== '_')
            }
     in Encode.genericToDhallWith opts

data ReferenceExport = ReferenceExport
  { name :: Text
  , span :: SourceSpanExport
  , scope :: Natural
  , kind :: RefKind
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToDhall)

data EdgeExport = EdgeExport
  { source :: Natural
  , target :: Natural
  , label :: EdgeLabel
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToDhall)

data ScopeExport = ScopeExport
  { id :: Natural
  , declarations :: [DeclarationExport]
  , references :: [ReferenceExport]
  , edges :: [EdgeExport]
  , kind :: ScopeKind
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToDhall)

data ScopeGraphExport = ScopeGraphExport
  { scopes :: [ScopeExport]
  , root :: Natural
  , file :: Maybe Text
  , files :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToDhall)

toExport :: ScopeGraph -> ScopeGraphExport
toExport scopeGraph =
  ScopeGraphExport
    { scopes = map scopeToExport (Map.elems (sgScopes scopeGraph))
    , root = fromIntegral (unScopeId (sgRoot scopeGraph))
    , file = T.pack <$> sgFile scopeGraph
    , files = []
    }
 where
  scopeToExport :: Scope -> ScopeExport
  scopeToExport s =
    ScopeExport
      { id = fromIntegral (unScopeId (scopeId s))
      , declarations = map declToExport (scopeDeclarations s)
      , references = map refToExport (scopeReferences s)
      , edges = map edgeToExport (scopeEdges s)
      , kind = scopeKind s
      }

  declToExport :: Declaration -> DeclarationExport
  declToExport d =
    DeclarationExport
      { name = declName d
      , span = spanToExport (declSpan d)
      , scope = fromIntegral (unScopeId (declScope d))
      , assocScope = fromIntegral . unScopeId <$> declAssocScope d
      , type_ = declType d
      , doc = declDoc d
      , kind = Nothing
      }

  refToExport :: Reference -> ReferenceExport
  refToExport r =
    ReferenceExport
      { name = refName r
      , span = spanToExport (refSpan r)
      , scope = fromIntegral (unScopeId (refScope r))
      , kind = refKind r
      }

  edgeToExport :: Edge -> EdgeExport
  edgeToExport e =
    EdgeExport
      { source = fromIntegral (unScopeId (edgeSource e))
      , target = fromIntegral (unScopeId (edgeTarget e))
      , label = edgeLabel e
      }

  spanToExport :: SourceSpan -> SourceSpanExport
  spanToExport sourceSpan =
    SourceSpanExport
      { start = posToExport (spanStart sourceSpan)
      , end = posToExport (spanEnd sourceSpan)
      , file = T.pack <$> spanFile sourceSpan
      }

  posToExport :: SourcePos -> SourcePosExport
  posToExport p =
    SourcePosExport
      { line = fromIntegral (posLine p)
      , col = fromIntegral (posCol p)
      }

-- | render a scope graph as pretty-printed Dhall text (the zeitschrift wire shape).
toDhall :: ScopeGraph -> Text
toDhall scopeGraph = Dhall.pretty (Dhall.embed Dhall.inject (toExport scopeGraph))
