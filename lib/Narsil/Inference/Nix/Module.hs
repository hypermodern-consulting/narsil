{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                     // inference // nix // module
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He knew the number of grains of sand in the construct of the beach."
--
--                                                                                   — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The module-system ontology (doc/design/module-system.md). The module
--   system carries a REIFIED type language — `mkOption { type = types.… }`
--   states the option's type at the value level — so module typing is
--   reading, not guessing:
--
--     * 'declaredOptions' walks a module body's `options` subtree into an
--       'OptionTree' (paths → declared 'NixType');
--     * 'configRecordOf' turns that tree into the type of the `config`
--       PARAMETER: precise at declared paths, anon-open everywhere else
--       (the rest of the option universe legitimately lives outside the
--       file — nothing new can false-positive);
--     * 'definitionSiteFor' navigates the body's `config` section (or
--       shorthand body) to the value expression defining a declared path,
--       so inference can hold definitions to their declarations.
--
--   All extraction is SYNTACTIC (like 'builtinsFieldScheme'): `types.str`,
--   `lib.types.str`, and bare names via `with types;` all resolve by final
--   key. Unrecognized types map to 'TAny' — never guess.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Inference.Nix.Module (
  OptionTree (..),
  OptionNode (..),
  declaredOptions,
  emptyTree,
  nullTree,
  configRecordOf,
  lookupOption,
  definitionSiteFor,
  definitionRoots,
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Narsil.Inference.Nix.Type
import Narsil.Syntax.Annotation (varNameText, pattern Layer)
import Nix.Expr.Types (
  Antiquoted (..),
  Binding (..),
  NBinaryOp (..),
  NExprF (..),
  NKeyName (..),
  NString (..),
 )
import Nix.Expr.Types.Annotated (NExprLoc)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- the option tree
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

newtype OptionTree = OptionTree (Map Text OptionNode)
  deriving (Eq, Show)

data OptionNode
  = -- | a declared option and its (mapped) type
    OptLeaf NixType
  | -- | an interior namespace (`options.services.foo.…`)
    OptSub OptionTree
  deriving (Eq, Show)

emptyTree :: OptionTree
emptyTree = OptionTree Map.empty

nullTree :: OptionTree -> Bool
nullTree (OptionTree m) = Map.null m

-- | the declared type at a dotted path, if any
lookupOption :: [Text] -> OptionTree -> Maybe NixType
lookupOption [] _ = Nothing
lookupOption (k : rest) (OptionTree m) = case Map.lookup k m of -- CASE-OK: shape dispatch
  Just (OptLeaf t) | null rest -> Just t
  Just (OptSub sub) | not (null rest) -> lookupOption rest sub
  _ -> Nothing

{- | The type of the `config` PARAMETER given the file's declarations:
declared paths at their types, every record open on the anonymous row
(selects at undeclared paths stay dynamic — the full option universe is
not in this file). Fields are OPTIONAL: config is a fixpoint the module
reads, never a record it must fully supply.
-}
configRecordOf :: OptionTree -> NixType
configRecordOf (OptionTree m) = TRec (Map.map fieldOf m) (ROpen anonRowVar)
 where
  fieldOf (OptLeaf t) = (t, True)
  fieldOf (OptSub sub) = (configRecordOf sub, True)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- declaration extraction
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | The options a module body declares. The body may be the module's
attrset directly or sit behind lets/withs; we walk `options`-headed
bindings (`options = { … }` and dotted forms `options.a.b = …`) and fold
every `mkOption`/`mkEnableOption`/`mkPackageOption` leaf into the tree.
-}
declaredOptions :: NExprLoc -> OptionTree
declaredOptions body = case peel body of -- CASE-OK: shape dispatch
  Layer (NSet _ bindings) ->
    foldr insertPath emptyTree (concatMap optionBindings bindings)
  _ -> emptyTree
 where
  -- look through the wrappers module bodies actually use
  peel (Layer (NLet _ e)) = peel e
  peel (Layer (NWith _ e)) = peel e
  peel (Layer (NAssert _ e)) = peel e
  peel e = e
  -- bindings whose path starts with `options`, re-rooted below it
  optionBindings (NamedVar path v _)
    | (k : rest) <- staticPath path
    , k == "options" =
        declsAt rest v
  optionBindings _ = []

-- | the static keys of a binding path ([] if any key is dynamic)
staticPath :: NonEmpty (NKeyName NExprLoc) -> [Text]
staticPath path = mapMaybe staticKey (NE.toList path)
 where
  staticKey (StaticKey k) = Just (varNameText k)
  staticKey (DynamicKey _) = Nothing

{- | Declarations found at @prefix@ within a value expression: an option
constructor is a leaf; a plain attrset recurses (an interior namespace).
-}
declsAt :: [Text] -> NExprLoc -> [([Text], NixType)]
declsAt prefix v
  | Just t <- optionLeaf v = [(prefix, t)]
  | Layer (NSet _ bindings) <- v =
      concat
        [ declsAt (prefix ++ ks) inner
        | NamedVar path inner _ <- bindings
        , let ks = staticPath path
        , not (null ks)
        ]
  | otherwise = []

insertPath :: ([Text], NixType) -> OptionTree -> OptionTree
insertPath ([], _) tree = tree
insertPath ([k], t) (OptionTree m) = OptionTree (Map.insert k (OptLeaf t) m)
insertPath (k : rest, t) (OptionTree m) =
  OptionTree (Map.alter step k m)
 where
  step (Just (OptSub sub)) = Just (OptSub (insertPath (rest, t) sub))
  step _ = Just (OptSub (insertPath (rest, t) emptyTree))

{- | Is this expression an option DECLARATION, and if so at what type?
`mkOption { type = …; }` reads the reified type; `mkEnableOption _` is
Bool; `mkPackageOption …` is a package. `decl // { … }` overrides
metadata, not the type — look through it.
-}
optionLeaf :: NExprLoc -> Maybe NixType
optionLeaf (Layer (NBinary NUpdate l _)) = optionLeaf l
optionLeaf e = case appSpine e of -- CASE-OK: shape dispatch
  (headName -> Just "mkOption", [arg]) -> Just (mkOptionType arg)
  (headName -> Just "mkEnableOption", _ : _) -> Just TBool
  (headName -> Just "mkPackageOption", _ : _) -> Just TDerivation
  _ -> Nothing

{- | the `type = …` field of a `mkOption { … }` argument, mapped
n.b. an `apply` transform rewrites the value CONSUMERS see — the declared
type describes definitions, not reads, so the option goes dynamic
-}
mkOptionType :: NExprLoc -> NixType
mkOptionType (Layer (NSet _ bindings))
  | any isApplyBinding bindings = TAny
  | otherwise =
      case [v | NamedVar path v _ <- bindings, staticPath path == ["type"]] of -- CASE-OK
        (tyExpr : _) -> optionTypeOf tyExpr
        [] -> TAny
 where
  isApplyBinding (NamedVar path _ _) = staticPath path == ["apply"]
  isApplyBinding _ = False
mkOptionType _ = TAny

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- the reified type language
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | flatten an application spine: @f a b@ → (f, [a, b])
appSpine :: NExprLoc -> (NExprLoc, [NExprLoc])
appSpine = go []
 where
  go acc (Layer (NApp f a)) = go (a : acc) f
  go acc f = (f, acc)

{- | The name an expression resolves to, syntactically: a bare symbol (via
@with types;@) or the FINAL key of any select path (`types.str`,
`lib.types.str`, `types.ints.unsigned`).
-}
headName :: NExprLoc -> Maybe Text
headName (Layer (NSym n)) = Just (varNameText n)
headName (Layer (NSelect Nothing _ path))
  | StaticKey k <- NE.last path = Just (varNameText k)
headName _ = Nothing

-- | is the expression under the `ints` namespace (`ints.u8`, `types.ints.between`)?
underInts :: NExprLoc -> Bool
underInts (Layer (NSelect Nothing base path)) =
  "ints" `elem` staticPath path || underInts base
underInts _ = False

{- | `types.*` value → 'NixType' (doc/design/module-system.md has the full
table). Unrecognized → 'TAny': never guess. `with types; listOf str` — the
dominant nixos-modules idiom — peels its @with@ first.
-}
optionTypeOf :: NExprLoc -> NixType
optionTypeOf (Layer (NWith _ e)) = optionTypeOf e
optionTypeOf expr = case appSpine expr of -- CASE-OK: shape dispatch
  (h, []) -> nullary h
  (h, args) -> applied h args
 where
  nullary h
    | underInts h = TInt
    | otherwise = case headName h of -- CASE-OK: shape dispatch
        Just "bool" -> TBool
        Just "int" -> TInt
        Just "port" -> TInt
        Just "float" -> TFloat
        Just "number" -> TUnion [TInt, TFloat]
        Just "str" -> TString
        Just "string" -> TString
        Just "nonEmptyStr" -> TString
        Just "singleLineStr" -> TString
        Just "lines" -> TString
        Just "commas" -> TString
        Just "envVar" -> TString
        -- derivations coerce to paths (outPath) — `listOf path` accepting
        -- packages is everywhere in service modules
        Just "path" -> TUnion [TPath, TString, TDerivation]
        Just "pathInStore" -> TUnion [TPath, TString, TDerivation]
        Just "package" -> TDerivation
        Just "shellPackage" -> TDerivation
        Just "pkgs" -> TAny
        _ -> TAny
  applied h args
    | underInts h = TInt
    | otherwise = case (headName h, args) of -- CASE-OK: shape dispatch
        (Just "nullOr", [t]) -> TUnion [TNull, optionTypeOf t]
        (Just "listOf", [t]) -> TList (optionTypeOf t)
        (Just "nonEmptyListOf", [t]) -> TList (optionTypeOf t)
        (Just "attrsOf", [_]) -> TRec Map.empty (ROpen anonRowVar)
        (Just "lazyAttrsOf", [_]) -> TRec Map.empty (ROpen anonRowVar)
        (Just "either", [a, b]) -> TUnion [optionTypeOf a, optionTypeOf b]
        (Just "oneOf", [Layer (NList ts)]) -> TUnion (map optionTypeOf ts)
        (Just "enum", [Layer (NList ls)]) -> enumType ls
        (Just "separatedString", [_]) -> TString
        (Just "strMatching", [_]) -> TString
        (Just "passwdEntry", [_]) -> TString
        (Just "functionTo", [t]) -> TFun TAny (optionTypeOf t)
        (Just "uniq", [t]) -> optionTypeOf t
        (Just "unique", [_, t]) -> optionTypeOf t
        (Just "coercedTo", [_, _, t]) -> optionTypeOf t
        (Just "submodule", [m]) -> submoduleType m
        _ -> TAny
  enumType ls =
    let lits = map litOf ls
     in if all (/= TAny) lits && not (null lits) then TUnion lits else TAny
  litOf (Layer (NStr (DoubleQuoted [Plain t]))) = TStrLit t
  litOf (Layer (NStr (Indented _ [Plain t]))) = TStrLit t
  litOf (Layer (NConstant _)) = TAny -- numeric/bool enums: legal, unmodeled
  litOf _ = TAny
  -- `submodule { options = …; }` (or a module function) — reuse the
  -- declaration walker on its body; a function body works too since
  -- 'declaredOptions' peels nothing extra from a plain set
  submoduleType m = case m of -- CASE-OK: shape dispatch
    Layer (NAbs _ b) -> fromBody b
    _ -> fromBody m
   where
    fromBody b =
      let tree = declaredOptions' b
       in if nullTree tree then TAny else configRecordOf tree
    -- inside a submodule the whole set IS the module: its `options` key
    declaredOptions' = declaredOptions

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- definition sites
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | The expressions to search for definitions: the body's `config` section
when present, otherwise the SHORTHAND body (top-level bindings minus the
module system's reserved keys).
-}
definitionRoots :: NExprLoc -> [([Text], NExprLoc)]
definitionRoots body = case peel body of -- CASE-OK: shape dispatch
  Layer (NSet _ bindings) ->
    let named =
          [ (ks, v)
          | NamedVar path v _ <- bindings
          , let ks = staticPath path
          , not (null ks)
          ]
        configRooted = [(rest, v) | (k : rest, v) <- named, k == "config"]
     in if not (null configRooted)
          then configRooted
          else [(ks, v) | (ks@(k : _), v) <- named, k `notElem` reservedKeys]
  _ -> []
 where
  peel (Layer (NLet _ e)) = peel e
  peel (Layer (NWith _ e)) = peel e
  peel (Layer (NAssert _ e)) = peel e
  peel e = e
  reservedKeys :: [Text]
  reservedKeys =
    [ "options"
    , "imports"
    , "meta"
    , "key"
    , "_class"
    , "_file"
    , "disabledModules"
    , "freeformType"
    ]

{- | Navigate the definition roots to the value expression at a declared
path, walking nested attrset literals and dotted binding paths. Stops at
anything wrapped (`mkIf`-guarded interiors, merges) — those are simply not
checked, never mis-checked.
-}
definitionSiteFor :: [Text] -> NExprLoc -> Maybe NExprLoc
definitionSiteFor path body =
  firstJust [go path (drop (length p) path) v | (p, v) <- roots, p `prefixes` path]
 where
  roots = definitionRoots body
  prefixes p q = p == take (length p) q
  firstJust xs = case [x | Just x <- xs] of -- CASE-OK: shape dispatch
    (x : _) -> Just x
    [] -> Nothing
  go _ [] v = Just v
  go orig (k : rest) (Layer (NSet _ bindings)) =
    firstJust
      [ if ks `prefixes'` (k : rest)
          then go orig (drop (length ks) (k : rest)) v
          else Nothing
      | NamedVar bpath v _ <- bindings
      , let ks = staticPath bpath
      , not (null ks)
      ]
   where
    prefixes' p q = p == take (length p) q && length p <= length q
  go _ _ _ = Nothing
