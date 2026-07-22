-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                             // Narsil.Docs.Extract // extract
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "A stranger's face, but not the one his life in hotels had taught him to
--    expect."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                 // Docs // extract documentation from scope graph
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Narsil.Docs.Extract (
  extractDocs,
)
where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Docs.Types (
  DocItem (..),
  DocKind (Attribute, Function, Variable),
 )
import Narsil.Layout.Scope qualified as Scope
import Narsil.Layout.Scope.Types (
  Declaration (..),
  Scope (..),
  ScopeGraph (..),
  ScopeId,
  ScopeKind (..),
  SourcePos (..),
  SourceSpan (..),
 )

{- | Extract documentation items from a scope graph and its source text: one
'DocItem' per declaration (in source order), each carrying its preceding
@#@-comment block as the description and a kind inferred from its scope.
-}
extractDocs :: ScopeGraph -> Text -> [DocItem]
extractDocs sg src =
  let decls = concatMap scopeDeclarations (Map.elems (sgScopes sg))
      sortedDecls = sortOn (posLine . Scope.spanStart . declSpan) decls
      lines_ = T.lines src
      scopeKinds =
        Map.fromList
          [ (scopeId s, scopeKind s)
          | s <- Map.elems (sgScopes sg)
          ]
   in map (makeDocItem lines_ scopeKinds) sortedDecls

makeDocItem :: [Text] -> Map ScopeId ScopeKind -> Declaration -> DocItem
makeDocItem srcLines scopeKinds decl =
  DocItem
    { docName = declName decl
    , docDescription = extractComment srcLines (declSpan decl)
    , docType = declType decl
    , docSpan = toSpan (declSpan decl)
    , docKind = inferDocKind scopeKinds decl
    }

inferDocKind :: Map ScopeId ScopeKind -> Declaration -> DocKind
inferDocKind scopeKinds decl = kindFor (Map.lookup (declScope decl) scopeKinds)
 where
  kindFor (Just FunctionScope) = Function
  kindFor (Just AttrSetScope) = Attribute
  kindFor (Just RecAttrSetScope) = Attribute
  kindFor _ = Variable

extractComment :: [Text] -> SourceSpan -> Text
extractComment lines_ sourceSpan =
  let lineIdx = posLine (Scope.spanStart sourceSpan) - 1
      preceding = take lineIdx lines_
      comments = takeWhileEnd isComment preceding
   in T.unlines (map cleanComment comments)

isComment :: Text -> Bool
isComment t = "#" `T.isPrefixOf` T.stripStart t

cleanComment :: Text -> Text
cleanComment t =
  let trimmed = T.stripStart t
   in if "# " `T.isPrefixOf` trimmed
        then T.drop 2 trimmed
        else
          if "#" `T.isPrefixOf` trimmed
            then T.drop 1 trimmed
            else t

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

toSpan :: SourceSpan -> Span
toSpan (SourceSpan (SourcePos l1 c1) (SourcePos l2 c2) f) =
  Span (Loc l1 c1) (Loc l2 c2) f
