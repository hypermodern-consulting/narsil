-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                 // Narsil.Docs.Types // types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He raised himself on one elbow to look at her."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                             // Docs // shared documentation types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Narsil.Docs.Types (
  DocItem (..),
  DocKind (..),
)
where

import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON (..), object, (.=))

import Narsil.Core.Span (Span)

{- | A documented binding: its name, description (extracted comment), optional
inferred type, source span, and kind. Serializes to JSON for the docs output.
-}
data DocItem = DocItem
  { docName :: Text
  , docDescription :: Text
  , docType :: Maybe Text
  , docSpan :: Span
  , docKind :: DocKind
  }
  deriving (Eq, Show, Generic)

instance ToJSON DocItem where
  toJSON d =
    object
      [ "name" .= docName d
      , "description" .= docDescription d
      , "type" .= docType d
      , "span" .= docSpan d
      , "kind" .= docKind d
      ]

{- | What kind of binding a 'DocItem' documents: a function, a plain variable, a
module option, or an attribute-set member.
-}
data DocKind = Function | Variable | Option | Attribute
  deriving (Eq, Show, Generic)

instance ToJSON DocKind where
  toJSON = toJSON . show
