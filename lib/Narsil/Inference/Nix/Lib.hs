{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                        // inference // nix // lib
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The library was a sea of information, and he was learning to swim."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Polymorphic schemes for the nixpkgs `lib` namespace — the library record
--   threaded through every flake-parts / NixOS module as the `lib` parameter.
--   A pure table; consumed by 'Narsil.Inference.Nix.Builtins' to back the
--   `lib.<name>` selection path.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Inference.Nix.Lib (
  libSchemeTable,
)
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Narsil.Inference.Nix.Type

{- | Polymorphic schemes for the nixpkgs `lib` namespace, the library record
threaded through every flake-parts module and NixOS module as the `lib`
parameter. Like the builtins table these are SCHEMES (instantiated fresh per
use), so a single `lib.mkIf` may be applied at many result types — the whole
point: `{ lib }: { a = lib.mkIf c { x = 1; }; b = lib.mkIf c 2; }` must check.

Modeled structurally where the shape is stable (mkIf/mkMerge/mkDefault/… are
`a -> a`-shaped) and left permissive (TAny) where the real type is an
options-DSL value we don't model. There is no oracle coverage for module code
(it isn't a closed term), so permissive entries can't introduce soundness
mismatches — they only avoid false positives.
-}
libSchemeTable :: Map Text Scheme
libSchemeTable =
  Map.fromList
    [ -- module-system combinators
      -- n.b. mkIf keeps its `a -> a` shape (it feeds guarded-combinator
      -- narrowing); the priority/order wrappers do NOT thread the value
      -- through — the runtime returns `{ _type; priority; content }`, and
      -- nixpkgs selects `.priority`/`.content` off the result, so an
      -- identity type false-positives there. mkMerge's elements are
      -- partial config fragments that must NOT unify with each other
      -- (the module merger combines them; a homogeneous `[a] -> a`
      -- rigidified free params to the first literal fragment).
      ("mkIf", scheme1 (\a -> TFun TBool (TFun a a)))
    , ("mkMerge", Forall [] (TFun (TList TAny) TAny))
    , ("mkDefault", Forall [] (TFun TAny TAny))
    , ("mkForce", Forall [] (TFun TAny TAny))
    , ("mkOverride", Forall [] (TFun TInt (TFun TAny TAny)))
    , ("mkBefore", Forall [] (TFun TAny TAny))
    , ("mkAfter", Forall [] (TFun TAny TAny))
    , ("mkOptionDefault", Forall [] (TFun TAny TAny))
    , -- conditional attrset / list helpers
      ("optionalAttrs", scheme1 (\a -> TFun TBool (TFun a a)))
    , ("optional", scheme1 (\a -> TFun TBool (TFun a (TList a))))
    , -- n.b. `optionals cond ''…''` (a STRING second argument) is a live
      -- nixpkgs idiom — mkDerivation coerces the `[]` arm to "" so both
      -- arms work — 17 files in the sweep. The honest domain is
      -- value-shaped, not list-shaped; `optional` (singular) keeps its
      -- precise `Bool -> a -> [a]`.
      ("optionals", scheme1 (\a -> TFun TBool (TFun a a)))
    , ("optionalString", Forall [] (TFun TBool (TFun TString TString)))
    , -- string helpers
      ("concatStringsSep", Forall [] (TFun TString (TFun (TList TString) TString)))
    , ("makeBinPath", scheme1 (\a -> TFun (TList a) TString))
    , ("getExe", Forall [] (TFun TDerivation TString))
    , ("getExe'", Forall [] (TFun TDerivation (TFun TString TString)))
    , -- list/attr utilities whose precise row type we don't model: permissive
      ("mkOption", Forall [] (TFun TAny TAny))
    , -- n.b. `mkEnableOption null // { description = … }` is an accepted
      -- nixpkgs idiom (the argument is only spliced lazily into the default
      -- description, which the override immediately replaces) — 11 modules
      -- in the sweep. Null must be in the domain.
      ("mkEnableOption", Forall [] (TFun (TUnion [TString, TNull]) TAny))
    , ("mkPackageOption", Forall [] (TFun TAny TAny))
    , ("mapAttrs", Forall [] (TFun (TFun TString (TFun TAny TAny)) (TFun TAny TAny)))
    , ("filterAttrs", Forall [] (TFun (TFun TString (TFun TAny TBool)) (TFun TAny TAny)))
    , ("recursiveUpdate", Forall [] (TFun TAny (TFun TAny TAny)))
    , ("genAttrs", Forall [] (TFun (TList TString) (TFun TAny TAny)))
    , -- radicle keys nameValuePair by store path; both coerce
      ("nameValuePair", Forall [] (TFun (TUnion [TString, TPath]) (TFun TAny TAny)))
    , -- ── review-7 (tail sweep): scheme-table gaps ──
      -- A `lib.<fn>` MISS falls through to row-extension on the `lib` var,
      -- minting ONE monotype field shared by every use site in the file —
      -- the first caller's constraints contaminate all later calls. Any
      -- lib function used at two types in one file must be here (or reach
      -- the builtin mirror via the fallback in 'builtinsFieldScheme').
      -- `pipe` is heterogeneous by design — HM cannot type the stage list.
      ("pipe", Forall [] (TFun TAny (TFun (TList TAny) TAny)))
    , ("mapAttrsToList", scheme2 (\a b -> TFun (TFun TString (TFun a b)) (TFun TAny (TList b))))
    , ("unique", scheme1 (\a -> TFun (TList a) (TList a)))
    , ("attrByPath", scheme1 (\a -> TFun (TList TString) (TFun a (TFun TAny a))))
    , ("evalModules", Forall [] (TFun TAny TAny))
    , ("isNull", Forall [] (TFun TAny TBool))
    , ("isInt", Forall [] (TFun TAny TBool))
    , ("isFloat", Forall [] (TFun TAny TBool))
    , ("isBool", Forall [] (TFun TAny TBool))
    , ("isString", Forall [] (TFun TAny TBool))
    , ("isList", Forall [] (TFun TAny TBool))
    , ("isAttrs", Forall [] (TFun TAny TBool))
    , ("isFunction", Forall [] (TFun TAny TBool))
    , ("isPath", Forall [] (TFun TAny TBool))
    , ("isDerivation", Forall [] (TFun TAny TBool))
    , ("isStringLike", Forall [] (TFun TAny TBool))
    ]
 where
  scheme1 builder = let a = TypeVar 0 in Forall [a] (builder (TVar a))
  scheme2 builder = let a = TypeVar 0; b = TypeVar 1 in Forall [a, b] (builder (TVar a) (TVar b))
