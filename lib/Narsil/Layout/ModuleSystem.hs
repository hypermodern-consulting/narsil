{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                           // nix // module system
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The sky above the port was the color of television, tuned to a dead
--    channel."
--
--                                                                                     — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                     // module // option // system
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.ModuleSystem (
  -- * Types
  OptionInfo (..),
  ModuleOptions (..),

  -- * Extraction
  extractOptions,
  collectModuleOptions,

  -- * Queries
  optionAtPath,
  allOptionPaths,
)
where

import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Inference.Nix.Type
import Narsil.Layout.Edge qualified as Edge
import Narsil.Syntax.Annotation (pattern Layer)
import Nix.Expr.Types
import Nix.Expr.Types.Annotated

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- types
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | one declared module option: its dotted path, inferred type, optional
default expression and description, and source span.
-}
data OptionInfo = OptionInfo
  { optPath :: !Text
  , optType :: !NixType
  , optDefault :: !(Maybe NExprLoc)
  , optDescription :: !(Maybe Text)
  , optSpan :: !Span
  }
  deriving (Eq, Show)

{- | a module's extracted metadata: its options by path, its @config@ body (if
any), and the paths it imports.
-}
data ModuleOptions = ModuleOptions
  { moOptions :: !(Map Text OptionInfo)
  , moConfig :: !(Maybe NExprLoc)
  , moImports :: ![FilePath]
  }
  deriving (Eq, Show)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- extraction
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | extract all declared options from a NixOS-style module
extractOptions :: NExprLoc -> Map Text OptionInfo
extractOptions expr = goOptions [] expr Map.empty
 where
  goOptions pathPrefix (Layer (NSet _ bindings)) acc = foldr (collectOption pathPrefix) acc bindings
  goOptions pathPrefix (Layer (NAbs _ body)) acc = goOptions pathPrefix body acc
  goOptions pathPrefix (Layer (NLet _ body)) acc = goOptions pathPrefix body acc
  goOptions pathPrefix (Layer (NWith _ body)) acc = goOptions pathPrefix body acc
  goOptions _ _ acc = acc

  collectOption pathPrefix (NamedVar (StaticKey name :| []) val _) acc
    | "options" `T.isPrefixOf` fullPath =
        maybe
          (goOptions (coerceVarName name : pathPrefix) val acc)
          (\oi -> Map.insert (optPath oi) oi acc)
          (collectOneOption fullPath val)
    | otherwise = acc
   where
    fullPath = buildPath pathPrefix (coerceVarName name)
  collectOption _ _ acc = acc

  buildPath prefix name = T.intercalate "." (reverse (name : prefix))

-- | collect a single option from a mkOption/mkEnableOption call
collectOneOption :: Text -> NExprLoc -> Maybe OptionInfo
collectOneOption path expr@(Layer (NApp func _)) = dispatch (funcName func)
 where
  dispatch (Just "mkOption") = parseMkOption path expr
  dispatch (Just "mkEnableOption") = parseMkEnableOption path expr
  dispatch _ = Nothing
collectOneOption _ _ = Nothing

-- | parse mkOption { type = ..., default = ..., description = ... }
parseMkOption :: Text -> NExprLoc -> Maybe OptionInfo
parseMkOption path (Layer (NApp _ arg@(Layer (NSet _ bindings)))) =
  Just (OptionInfo path optType optDefault optDescription optSpan)
 where
  optType = maybe TAny inferTypeExpr (Edge.findAttr "type" bindings)
  optDefault = Edge.findAttr "default" bindings
  optDescription = Edge.findAttr "description" bindings >>= extractStringLit
  optSpan = spanFromExpr arg
parseMkOption _ _ = Nothing

-- | parse mkEnableOption "description" → Bool option
parseMkEnableOption :: Text -> NExprLoc -> Maybe OptionInfo
parseMkEnableOption path expr@(Layer (NApp _ arg)) =
  Just (OptionInfo path TBool Nothing (extractStringLit arg) (spanFromExpr expr))
parseMkEnableOption _ _ = Nothing

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- type inference for option types
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | infer NixType from a lib.types.* expression
inferTypeExpr :: NExprLoc -> NixType
-- lib.types.bool → Bool
inferTypeExpr (Layer (NSelect _ base (StaticKey name :| [])))
  | Just "types" <- attrLastName base = inferTypeFromName (coerceVarName name)
-- lib.types.listOf lib.types.str → [String], etc.
inferTypeExpr (Layer (NApp func arg)) = dispatchApp (funcName func)
 where
  dispatchApp (Just "types.listOf") = TList (inferTypeExpr arg)
  dispatchApp (Just "types.attrsOf") = tRecOpenAnon (Map.singleton "_" (inferTypeExpr arg, False))
  dispatchApp (Just "types.nullOr") = TUnion [TNull, inferTypeExpr arg]
  dispatchApp (Just "types.either") = TUnion (collectEitherTypes arg)
  dispatchApp (Just "types.enum") = inferEnumType arg
  dispatchApp (Just "types.submodule") = inferSubmoduleType arg
  dispatchApp _ = TAny
inferTypeExpr _ = TAny

-- | find the last StaticKey name in a NSelect chain
attrLastName :: NExprLoc -> Maybe Text
attrLastName (Layer (NSelect _ _ (StaticKey name :| []))) = Just (coerceVarName name)
attrLastName _ = Nothing

-- | get the function name from an expression (lib.types.listOf → "listOf")
funcName :: NExprLoc -> Maybe Text
funcName (Layer (NSym name)) = Just (coerceVarName name)
funcName (Layer (NSelect _ _ (StaticKey name :| []))) = Just (coerceVarName name)
funcName _ = Nothing

-- | map common NixOS type names to NixType
inferTypeFromName :: Text -> NixType
inferTypeFromName "bool" = TBool
inferTypeFromName "str" = TString
inferTypeFromName "int" = TInt
inferTypeFromName "float" = TFloat
inferTypeFromName "path" = TPath
inferTypeFromName "string" = TString
inferTypeFromName "number" = TUnion [TInt, TFloat]
inferTypeFromName "anything" = TAny
inferTypeFromName "unspecified" = TAny
inferTypeFromName "derivation" = TDerivation
inferTypeFromName "package" = TDerivation
inferTypeFromName "lines" = TString
inferTypeFromName "commas" = TString
inferTypeFromName "envVar" = TString
inferTypeFromName _ = TAny

-- | collect types from either a b → Union [typeOf a, typeOf b]
collectEitherTypes :: NExprLoc -> [NixType]
collectEitherTypes e@(Layer (NApp f a))
  | Just "lib.types.either" <- funcName f = inferTypeExpr a : collectEitherTypes f
  | otherwise = map inferTypeExpr (nixListExprs e)
collectEitherTypes _ = []

-- | infer enum type from list of strings
inferEnumType :: NExprLoc -> NixType
inferEnumType (Layer (NList literals))
  | not (null values) = TUnion (map TStrLit values)
  | otherwise = TString
 where
  values = mapMaybe extractStringLit literals
inferEnumType _ = TString

-- | infer submodule type from import or options set
inferSubmoduleType :: NExprLoc -> NixType
inferSubmoduleType (Layer (NSet _ bindings)) = TAttrs (Map.map (,True) (Map.fromList opts))
 where
  opts = mapMaybe pairToOpt (mapMaybe bindingToPair bindings)
  pairToOpt (StaticKey name, v) = Just (coerceVarName name, inferTypeExpr v)
  pairToOpt _ = Nothing
inferSubmoduleType _ = tRecOpenAnon Map.empty

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- module-level collection
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | collect all options and module metadata from a NixOS module
collectModuleOptions :: NExprLoc -> ModuleOptions
collectModuleOptions expr =
  let opts = extractOptions expr
      cfg = Edge.findAttr "config" (Edge.topBindings expr)
      imports = findImports' expr
   in ModuleOptions opts cfg imports

-- | find imports from the top-level `imports` binding (the shared 'Edge' scanner)
findImports' :: NExprLoc -> [FilePath]
findImports' = Edge.flakeImportPaths

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- queries
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | look up the option declared at a given dotted path, if any.
optionAtPath :: ModuleOptions -> Text -> Maybe OptionInfo
optionAtPath mos path = Map.lookup path (moOptions mos)

-- | every declared option path in the module.
allOptionPaths :: ModuleOptions -> [Text]
allOptionPaths mos = Map.keys (moOptions mos)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- helpers
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

coerceVarName :: VarName -> Text
coerceVarName = coerce

-- | extract a string literal from an expression
extractStringLit :: NExprLoc -> Maybe Text
extractStringLit (Layer (NStr (DoubleQuoted [Plain t]))) = Just t
extractStringLit (Layer (NStr (Indented _ [Plain t]))) = Just t
extractStringLit _ = Nothing

-- | extract binding as (key, value) pair if it has a static key
bindingToPair :: Binding NExprLoc -> Maybe (NKeyName NExprLoc, NExprLoc)
bindingToPair (NamedVar (StaticKey _ :| []) val _) = Just (StaticKey "", val) -- placeholder
bindingToPair _ = Nothing

-- | extract all expressions from a list literal
nixListExprs :: NExprLoc -> [NExprLoc]
nixListExprs (Layer (NList es)) = es
nixListExprs _ = []

-- | crude span extraction from an expression
spanFromExpr :: NExprLoc -> Span
spanFromExpr _ = Span (Loc 0 0) (Loc 0 0) Nothing

-- n.b. real spans require SrcSpan conversion; placeholder for now
