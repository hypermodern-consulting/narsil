{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                     // lsp // handlers // symbols
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "A map of the territory, drawn in light."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Document-symbol outline: turn the top-level attribute bindings of a file
--   into the nested 'DocumentSymbol' tree the editor shows in its outline /
--   breadcrumb, classifying each binding by the shape of its value. Pure.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.LSP.Handlers.Symbols (
  collectTopBindingSymbols,
)
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Syntax.Annotation (srcSpanToSpan, varNameText, pattern Layer, pattern LayerAnn)
import Nix.Atoms (NAtom (..))
import Nix.Expr.Types (Binding (..), NExprF (..), NKeyName (..))
import Nix.Expr.Types.Annotated (NExprLoc)

{- | Pure: the document-symbol outline for a file — its top-level attribute
  bindings as a nested 'DocumentSymbol' tree, each classified by value shape.
  Descends through leading lambda/let/with wrappers to the outline attrset.
-}
collectTopBindingSymbols :: NExprLoc -> [DocumentSymbol]
collectTopBindingSymbols (Layer (NSet _ bindings)) = concatMap bindingToSymbol bindings
collectTopBindingSymbols (Layer (NAbs _ body)) = collectTopBindingSymbols body
-- a `let` contributes its OWN bindings to the outline (the common file shape)
-- and then whatever the body contributes
collectTopBindingSymbols (Layer (NLet bindings body)) =
  concatMap bindingToSymbol bindings ++ collectTopBindingSymbols body
collectTopBindingSymbols (Layer (NWith _ body)) = collectTopBindingSymbols body
collectTopBindingSymbols _ = []

bindingToSymbol :: Binding NExprLoc -> [DocumentSymbol]
bindingToSymbol (NamedVar (StaticKey name :| []) expr _) =
  let kind = symKind expr
      sp = exprSpan expr
   in [mkDocumentSymbol (varNameText name) kind sp (childSymbols expr)]
bindingToSymbol (Inherit{}) = []
bindingToSymbol _ = []

exprSpan :: NExprLoc -> Range
exprSpan (LayerAnn srcSpan _) =
  let sp = srcSpanToSpan srcSpan
   in Range
        ( Position
            (fromIntegral (locLine (spanStart sp) - 1))
            (fromIntegral (locCol (spanStart sp) - 1))
        )
        ( Position
            (fromIntegral (locLine (spanEnd sp) - 1))
            (fromIntegral (locCol (spanEnd sp) - 1))
        )

mkDocumentSymbol :: Text -> SymbolKind -> Range -> [DocumentSymbol] -> DocumentSymbol
mkDocumentSymbol name kind range children =
  DocumentSymbol name Nothing kind Nothing Nothing range range (Just children)

symKind :: NExprLoc -> SymbolKind
symKind (Layer (NAbs _ _)) = SymbolKind_Function
symKind (Layer (NSet _ _)) = SymbolKind_Object
symKind (Layer (NList _)) = SymbolKind_Array
symKind (Layer (NStr _)) = SymbolKind_String
symKind (Layer (NConstant (NInt _))) = SymbolKind_Number
symKind (Layer (NConstant (NFloat _))) = SymbolKind_Number
symKind (Layer (NConstant (NBool _))) = SymbolKind_Boolean
symKind (Layer (NApp _ _)) = SymbolKind_Function
symKind _ = SymbolKind_Variable

childSymbols :: NExprLoc -> [DocumentSymbol]
childSymbols (Layer (NSet _ bindings)) = concatMap bindingToSymbol bindings
childSymbols _ = []
