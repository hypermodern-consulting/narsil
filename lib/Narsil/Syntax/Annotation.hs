{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                   // nix // utils
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Complex geometric forms began to click into place in the tank, aligned
--    with the nearly invisible planes of a three-dimensional grid. Beauvoir
--    was sketching in the cyberspace coordinates for Barrytown, Bobby saw.
--    'We'll call you this blue pyramid, Bobby. There you are.' A blue
--    pyramid began to pulse softly at the very center of the tank."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                       // coordinate // transforms
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Syntax.Annotation (
  -- * AST views
  pattern Layer,
  pattern LayerAnn,

  -- * VarName extraction
  varNameText,

  -- * Key extraction
  keyText,

  -- * Post-parse normalization
  normalizeStaticKeys,

  -- * Span conversion
  toSpan,
  srcSpanToSpan,
)
where

import Data.Coerce (coerce)
import Data.Fix (Fix (..))
import Data.Text (Text)
import Narsil.Core.Span (Loc (..), Span (..))
import Nix.Expr.Types (
  Antiquoted (..),
  Binding (..),
  NExprF (..),
  NKeyName (..),
  NPos (..),
  NSourcePos (..),
  NString (..),
  VarName (..),
 )
import Nix.Expr.Types.Annotated (AnnUnit (..), Compose (..), NExprLoc, SrcSpan (..))
import Nix.Utils (Path (..))
import Text.Megaparsec.Pos (unPos)

{- | View an annotated expression as its underlying functor layer.

Every hnix node is wrapped in an annotation spine, @Fix (Compose (AnnUnit span)
…)@. Matching that wrapper inline turns every structural decision into a @case@;
'Layer' strips it in a clause /head/ instead, so AST dispatch across the codebase
reads as equations rather than a @case@ staircase (per doc/HOUSE_STYLE.md, the
binding law: guards and equations over @case@).

  detectFromBody (Layer (NSet _ bs)) = …
  detectFromBody (Layer (NLet _ e))  = …
  detectFromBody _                   = []

The @COMPLETE@ pragma records that 'Layer' alone exhausts 'NExprLoc' (it does —
every value has the spine), so a total function need not add a catch-all.
-}
pattern Layer :: NExprF NExprLoc -> NExprLoc
pattern Layer e <- Fix (Compose (AnnUnit _ e))

{-# COMPLETE Layer #-}

{- | 'Layer', but also binding the node's source span. For the dispatches that
need the location as well as the shape — @extractString (LayerAnn s (NStr …))@.
-}
pattern LayerAnn :: SrcSpan -> NExprF NExprLoc -> NExprLoc
pattern LayerAnn s e <- Fix (Compose (AnnUnit s e))

{-# COMPLETE LayerAnn #-}

-- | Extract Text from the VarName newtype.
varNameText :: VarName -> Text
varNameText = coerce

-- | Extract Text from an NKeyName; a dynamic (interpolated) key has no static text.
keyText :: NKeyName r -> Text
keyText (StaticKey k) = varNameText k
keyText (DynamicKey _) = ""

{- | Rewrite QUOTED-BUT-CONSTANT keys (@"version" = …@, @x."foo"@, generator
output like graalvm's hashes.nix) into plain 'StaticKey's, everywhere keys
occur: attrset/let bindings, select paths, has-attr paths. hnix parses them
as 'DynamicKey' antiquotations, which every static-key consumer downstream
(binding inference, path folding, unsupported-construct detection) would
otherwise treat as dynamic — typing such files as the empty record. Run once
on the parse result, before any analysis.
-}
normalizeStaticKeys :: NExprLoc -> NExprLoc
normalizeStaticKeys = go
 where
  go (Fix (Compose (AnnUnit sp e))) = Fix (Compose (AnnUnit sp (norm (fmap go e))))
  norm (NSet r bs) = NSet r (map normBinding bs)
  norm (NLet bs body) = NLet (map normBinding bs) body
  norm (NSelect alt base path) = NSelect alt base (fmap normKey path)
  norm (NHasAttr base path) = NHasAttr base (fmap normKey path)
  norm e = e
  normBinding (NamedVar path v pos) = NamedVar (fmap normKey path) v pos
  normBinding b = b
  normKey (DynamicKey (Plain str)) | Just t <- constantText str = StaticKey (VarName t)
  normKey k = k
  constantText (DoubleQuoted [Plain t]) = Just t
  constantText (Indented _ [Plain t]) = Just t
  constantText _ = Nothing

-- | Convert an hnix 'SrcSpan' to our 'Span'.
srcSpanToSpan :: SrcSpan -> Span
srcSpanToSpan (SrcSpan begin end) =
  Span
    { spanStart = Loc (sourceLine begin) (sourceCol begin)
    , spanEnd = Loc (sourceLine end) (sourceCol end)
    , spanFile = Just (coerce (sourcePath begin))
    }
 where
  sourcePath (NSourcePos path _ _) = path
  sourceLine (NSourcePos _ (NPos l) _) = fromIntegral (unPos l)
  sourceCol (NSourcePos _ _ (NPos c)) = fromIntegral (unPos c)

-- | 'srcSpanToSpan', with an optional explicit file overriding the span's own.
toSpan :: SrcSpan -> Maybe FilePath -> Span
toSpan srcSpan = maybe sp (\file -> sp{spanFile = Just file})
 where
  sp = srcSpanToSpan srcSpan
