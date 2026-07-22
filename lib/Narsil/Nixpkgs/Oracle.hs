{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                             // nixpkgs // oracle
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The box knew things about the world that Bobby didn't, and when he
--    asked the right way, it told him."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The bridge from the nixpkgs eval backend to the type
--   inferencer: scan a file for its @pkgs.<path>@ references and turn each into a
--   precomputed 'NixType', producing the @Map [Text] NixType@ that seeds
--   'Narsil.Inference.Nix.Environment.envPkgsOracle'. With that, a nixpkgs
--   reference the engine would otherwise treat as opaque 'TAny' carries its real
--   shape — better hover, real attribute-typo errors, sharper unification.
--
--   Each referenced path gets ONE of two shapes, by what the value actually is:
--
--     * a SCALAR leaf (@pkgs.hello.pname@ → 'TString') — from 'evalFieldType'
--       (force one field, read its @typeOf@);
--     * a RECORD (@pkgs.hello@ → @{pname, version, …}@) — from 'evalSpine' (force
--       the spine, the complete attribute set, so the record is CLOSED and a bogus
--       member is a real error). Its fields are typed 'TAny' unless the same field
--       was also referenced directly, in which case its scalar leaf type is reused.
--
--   Bounded by the file: only the paths it mentions are evaluated, and the cache
--   makes a repeat free. The engine stays pure; this IO pass just feeds it facts.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Nixpkgs.Oracle (
  buildPkgsOracle,

  -- * Attribute-typo diagnostics
  collectPkgsChainsAnn,
  pkgsAttrTypos,

  -- * Internals (exposed for testing)
  collectPkgsChains,
  isScalarType,
)
where

import Control.Monad (foldM, mfilter)
import Data.Foldable (toList)
import Data.List (inits, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Nix.Expr.Types (NExprF (..), NKeyName (..))
import Nix.Expr.Types.Annotated (NExprLoc)

import Narsil.Core.Span (Span)
import Narsil.Inference.Nix.Type (NixType (..), pattern TAttrs)
import Narsil.Nixpkgs.Eval (EvalBackend (..))
import Narsil.Nixpkgs.Index (NixpkgsIndex)
import Narsil.Syntax.Annotation (srcSpanToSpan, varNameText, pattern Layer, pattern LayerAnn)

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- building the oracle
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{- | Build the @pkgs.<path>@ type oracle for one file: evaluate the (bounded) set
of nixpkgs paths it references and key each to its 'NixType'. Eval failures are
skipped silently (that path just falls back to the engine's opaque handling).
-}
buildPkgsOracle :: EvalBackend -> NixpkgsIndex -> NExprLoc -> IO (Map [Text] NixType)
buildPkgsOracle backend idx expr = do
  -- Every non-empty PREFIX of every referenced chain — so `pkgs.hello.bogus`
  -- still builds the `pkgs.hello` record that flags the typo, even when the file
  -- never names `pkgs.hello` on its own.
  let chains = nub (concatMap (drop 1 . inits) (collectPkgsChains expr))
  -- pass 1: each path's own type via typeOf; keep only the scalar leaves.
  leaves <- foldM addLeaf Map.empty chains
  -- pass 2: a non-scalar (set-valued) path becomes a closed record of its spine,
  -- with fields enriched from the scalar leaves where they were also referenced.
  records <- foldM (addRecord leaves) Map.empty (filter (`Map.notMember` leaves) chains)
  pure (Map.union leaves records)
 where
  addLeaf acc path = do
    mt <- leafType backend idx path
    pure (maybe acc (\t -> Map.insert path t acc) (mfilter isScalarType mt))
  addRecord leaves acc path = do
    spine <- evalSpine backend idx path
    pure (either (const acc) (insertRecord leaves acc path) spine)
  -- skip pathologically large sets (namespaces): a 10k-field record is costly and
  -- rarely the thing you typo into; derivations (~tens of attrs) sail under the cap.
  insertRecord leaves acc path names
    | length names > recordFieldCap = acc
    | otherwise = Map.insert path (recordOf leaves path names) acc
  recordOf leaves path names =
    TAttrs (Map.fromList [(n, (fieldType leaves path n, False)) | n <- names])
  -- a directly-referenced field's scalar leaf wins; otherwise a well-known
  -- derivation field gets its conventional type; otherwise it stays opaque.
  fieldType leaves path n = Map.findWithDefault (conventionalFieldType n) (path <> [n]) leaves

-- | Skip building a record for a set larger than this (keeps namespaces out).
recordFieldCap :: Int
recordFieldCap = 1024

{- | Well-known derivation scalar fields whose type is fixed by nixpkgs convention
(@pname@, @version@, … are always strings). Applied ONLY to fields actually present
in a value's spine, so a non-derivation record that lacks them is unaffected. This
makes a let-bound @pkgs.<pkg>.<field>@ precise without an extra eval per field.
-}
conventionalFieldType :: Text -> NixType
conventionalFieldType "pname" = TString
conventionalFieldType "version" = TString
conventionalFieldType "name" = TString
conventionalFieldType "system" = TString
conventionalFieldType "outputName" = TString
conventionalFieldType _ = TAny

{- | The @typeOf@-derived type of the value at a path (force just that field), or
'Nothing' on the empty path or an eval failure.
-}
leafType :: EvalBackend -> NixpkgsIndex -> [Text] -> IO (Maybe NixType)
leafType backend idx path =
  maybe (pure Nothing) query (unsnoc path)
 where
  query (parent, field) = either (const Nothing) Just <$> evalFieldType backend idx parent field

-- | Is this a scalar (or list) type worth pinning as a leaf? Sets/functions are not.
isScalarType :: NixType -> Bool
isScalarType TInt = True
isScalarType TFloat = True
isScalarType TBool = True
isScalarType TString = True
isScalarType (TStrLit _) = True
isScalarType TPath = True
isScalarType TNull = True
isScalarType (TList _) = True
isScalarType _ = False

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- collecting pkgs references
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{- | Every @pkgs.<path>@ selection in an expression, as the static attribute path
after @pkgs@ (@["hello","pname"]@ for @pkgs.hello.pname@). A select whose base is
the bare symbol @pkgs@ and whose keys are all static; dynamic keys exclude it.
-}
collectPkgsChains :: NExprLoc -> [[Text]]
collectPkgsChains (Layer node) = here <> concatMap collectPkgsChains (toList node)
 where
  here = maybe [] (: []) (pkgsChainOf node)

-- | The static path of a @pkgs.<…>@ selection node, if this node is one.
pkgsChainOf :: NExprF NExprLoc -> Maybe [Text]
pkgsChainOf (NSelect _ base path)
  | isPkgsSym base = traverse staticKey (toList path)
pkgsChainOf _ = Nothing

{- | Like 'collectPkgsChains', but each chain carries the source span of its
selection node — for placing attribute-typo diagnostics.
-}
collectPkgsChainsAnn :: NExprLoc -> [(Span, [Text])]
collectPkgsChainsAnn (LayerAnn sp node) = here <> concatMap collectPkgsChainsAnn (toList node)
 where
  here = maybe [] (\ks -> [(srcSpanToSpan sp, ks)]) (pkgsChainOf node)

{- | The attribute-typo diagnostics for a file's @pkgs.<path>@ references, given an
oracle: a selection that names an attribute a known CLOSED record (a derivation's
complete spine) does not have is flagged with the offending span and a message.
Only closed records trigger this — open records and scalars yield nothing, so
there are no false positives on partial knowledge.
-}
pkgsAttrTypos :: Map [Text] NixType -> [(Span, [Text])] -> [(Span, Text)]
pkgsAttrTypos oracle = mapMaybe check
 where
  check (sp, keys) = (\(prefix, key) -> (sp, missingMsg prefix key)) <$> firstTypo keys
  firstTypo keys =
    listToMaybe
      [ (prefix, key)
      | (prefix, key) <- closedSplits keys
      , Just (TAttrs fields) <- [Map.lookup prefix oracle]
      , key `Map.notMember` fields
      ]
  closedSplits keys = [(take i keys, keys !! i) | i <- [1 .. length keys - 1]]
  missingMsg prefix key =
    "nixpkgs: `pkgs." <> T.intercalate "." prefix <> "` has no attribute `" <> key <> "`"

-- | Is this expression the bare symbol @pkgs@?
isPkgsSym :: NExprLoc -> Bool
isPkgsSym (Layer (NSym n)) = varNameText n == "pkgs"
isPkgsSym _ = False

-- | A static attribute key's text, or 'Nothing' for a dynamic @${…}@ key.
staticKey :: NKeyName NExprLoc -> Maybe Text
staticKey (StaticKey k) = Just (varNameText k)
staticKey (DynamicKey _) = Nothing

-- | Split a list into its leading elements and its last, or 'Nothing' if empty.
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x : xs) = prepend <$> unsnoc xs
 where
  prepend (initEls, lastEl) = (x : initEls, lastEl)
