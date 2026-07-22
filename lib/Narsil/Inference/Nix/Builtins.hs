{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                   // inference // nix // builtins
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Everything was built, nothing was born."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The builtin typing prelude: hand-maintained signatures for `builtins.*`,
--   the polymorphic-scheme table for the bare names, and the starting
--   'builtinEnv'. 'builtinsFieldScheme' is the selection interceptor that makes
--   `builtins.<name>` / `lib.<name>` instantiate fresh per use.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Inference.Nix.Builtins (
  builtinEnv,
  builtinSchemeTable,
  builtinsFieldScheme,
  isNamespaceVar,
)
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Narsil.Inference.Nix.Environment
import Narsil.Inference.Nix.Lib (libSchemeTable)
import Narsil.Inference.Nix.Type
import Narsil.Syntax.Annotation (varNameText, pattern Layer)
import Nix.Expr.Types (NExprF (..), NKeyName (..))
import Nix.Expr.Types.Annotated (NExprLoc)

{- | Polymorphic builtins as SCHEMES, instantiated fresh at each use site. They
cannot be stored in the 'builtins' record as monotypes — that would prematurely
monomorphize them (every use would share one set of vars). This table backs both
the bare names and `builtins.<name>` (via 'builtinsFieldScheme').
-}
builtinSchemeTable :: Map Text Scheme
builtinSchemeTable = Map.union polymorphicListBuiltins rowBuiltins
 where
  polymorphicListBuiltins =
    Map.fromList
      [ ("head", scheme1 (\a -> TFun (TList a) a))
      , ("tail", scheme1 (\a -> TFun (TList a) (TList a)))
      , ("length", scheme1 (\a -> TFun (TList a) TInt))
      , ("elemAt", scheme1 (\a -> TFun (TList a) (TFun TInt a)))
      , ("filter", scheme1 (\a -> TFun (TFun a TBool) (TFun (TList a) (TList a))))
      , ("concatLists", scheme1 (\a -> TFun (TList (TList a)) (TList a)))
      , ("map", scheme2 (\a b -> TFun (TFun a b) (TFun (TList a) (TList b))))
      , ("concatMap", scheme2 (\a b -> TFun (TFun a (TList b)) (TFun (TList a) (TList b))))
      , ("foldl'", scheme2 (\a b -> TFun (TFun b (TFun a b)) (TFun b (TFun (TList a) b))))
      , -- review-5 (nixpkgs sweep, missing-builtins class)
        ("elem", scheme1 (\a -> TFun a (TFun (TList a) TBool)))
      , ("sort", scheme1 (\a -> TFun (TFun a (TFun a TBool)) (TFun (TList a) (TList a))))
      , ("any", scheme1 (\a -> TFun (TFun a TBool) (TFun (TList a) TBool)))
      , ("all", scheme1 (\a -> TFun (TFun a TBool) (TFun (TList a) TBool)))
      , ("genList", scheme1 (\a -> TFun (TFun TInt a) (TFun TInt (TList a))))
      , -- review-7 (tail sweep): `dirOf` is kind-preserving (String→String,
        -- Path→Path) — the old Path↦Path monotype rejected string arguments.
        -- `lessThan` compares strings too (`sort builtins.lessThan`).
        ("dirOf", scheme1 (\a -> TFun a a))
      , ("lessThan", scheme1 (\a -> TFun a (TFun a TBool)))
      ]
  -- row-polymorphic attribute-set builtins: reject non-records, return the
  -- right shape. `getAttr` is value-dependent so its result stays TAny.
  rowBuiltins =
    Map.fromList
      [ ("attrNames", schemeRow (\r -> TFun (openRec r) (TList TString)))
      , ("attrValues", schemeRow (\r -> TFun (openRec r) (TList TAny)))
      , ("hasAttr", schemeRow (\r -> TFun TString (TFun (openRec r) TBool)))
      , ("getAttr", schemeRow (\r -> TFun TString (TFun (openRec r) TAny)))
      , ("removeAttrs", schemeRow (\r -> TFun (openRec r) (TFun (TList TString) (openRec r))))
      ]
  openRec r = TRec Map.empty (ROpen r)
  scheme1 builder = let a = TypeVar 0 in Forall [a] (builder (TVar a))
  scheme2 builder = let a = TypeVar 0; b = TypeVar 1 in Forall [a, b] (builder (TVar a) (TVar b))
  schemeRow builder = let r = TypeVar 0 in Forall [r] (builder r)

{- | If a selection is `<ns>.<name>` for a modeled namespace (`builtins`, `lib`),
return the field's polymorphic scheme (to be instantiated fresh). This is what
makes `builtins.attrNames` row-polymorphic and `lib.mkIf` reusable at many
result types even though the namespace record itself holds monotypes. n.b. a
purely syntactic check on the namespace symbol; locally shadowing it
(pathological) is not handled.
-}
builtinsFieldScheme :: NExprLoc -> NonEmpty (NKeyName NExprLoc) -> Maybe Scheme
builtinsFieldScheme base (StaticKey k :| [])
  | isNamespaceVar "builtins" base = Map.lookup (varNameText k) builtinSchemeTable
  | isNamespaceVar "lib" base =
      -- lib re-exports the polymorphic list/attr builtins under the same
      -- names (lib.filter, lib.head, lib.elemAt, …); falling back to the
      -- builtin schemes keeps those instantiating fresh per use instead of
      -- minting a shared monotype row field on `lib`
      case Map.lookup (varNameText k) libSchemeTable of -- CASE-OK: shape dispatch
        Just s -> Just s
        Nothing -> Map.lookup (varNameText k) builtinSchemeTable
builtinsFieldScheme _ _ = Nothing

isNamespaceVar :: Text -> NExprLoc -> Bool
isNamespaceVar name (Layer (NSym n)) = varNameText n == name
isNamespaceVar _ _ = False

{- | the starting typing environment: @builtins@ as an attrset, the polymorphic
builtin schemes, and monotype signatures for the bare builtin names.
-}
builtinEnv :: TypeEnv
builtinEnv =
  TypeEnv
    { envBindings = builtinBindings
    , envWith = []
    , envImportTypes = Map.empty
    , envCallPackageTypes = Map.empty
    , envLenient = False
    , envModuleParams = False
    , envPkgsOracle = Map.empty
    }
 where
  -- ── core type scheme helpers ──────────────────────────────────
  mono type_ = Forall [] type_
  req type_ = (type_, False)

  -- everything Nix's `toString` coerces to a string (see note at the entry)
  toStringDomain = TUnion [TInt, TFloat, TBool, TPath, TString, TNull, TList TAny, TDerivation]

  -- ── builtins attrset ──────────────────────────────────────────
  -- 'builtins' itself is typed as attrset of all function entries
  builtinsAttr = Map.singleton "builtins" (mono $ TAttrs builtinsTypes)
  -- n.b. polymorphic/row builtins live in the top-level 'builtinSchemeTable'
  -- as SCHEMES (instantiated fresh per use) — they cannot be baked into the
  -- 'builtins' record as monotypes without prematurely monomorphizing them.
  -- The same table backs `builtins.<name>` via selection interception (see
  -- 'builtinsFieldScheme').
  -- n.b. `path` is NOT a Nix global — promoting it to a bare name shadows
  -- `with types; path` / `with lib; path` resolutions with the builtin's
  -- function signature. It stays reachable as `builtins.path`.
  builtinBindings =
    Map.union
      builtinsAttr
      (Map.union builtinSchemeTable (Map.map (mono . fst) (Map.delete "path" builtinsTypes)))

  -- n.b. hand-maintained signatures — must stay in sync with nixpkgs
  builtinsTypes :: Map Text (NixType, Bool)
  builtinsTypes =
    Map.fromList $
      map
        (\(name, type_) -> (name, req type_))
        -- ── string / path conversions ──
        -- n.b. `toString` coerces numbers, bools, paths, strings, null (→ ""),
        -- derivations (→ outPath), LISTS (space-joined — `toString
        -- [ "-fpermissive" ]` is valid Nix), and sets carrying a coercion
        -- witness (`__toString` / `outPath` — matched against the TDerivation
        -- member by 'unionMemberAccepts', which also covers oracle-typed
        -- packages, since the pkgs oracle produces records, not TDerivation).
        -- The earlier scalar-only domain false-positived on every
        -- `toString <list>` (347 skips in pkgs/by-name alone). A bare set
        -- (no witness) is still correctly rejected — it is not in the domain.
        [ ("toString", TFun toStringDomain TString)
        , -- `placeholder "out"` and `fromTOML` are Nix GLOBALS (exposed unqualified,
          -- not just under `builtins.`) — both appear bare in package files.
          ("placeholder", TFun TString TString)
        , ("fromTOML", TFun TString TAny)
        , -- n.b. the path-ish builtins accept STRINGS too (`import (path +
          -- "/lib")` where `path` unified to String, `baseNameOf "a/b"` —
          -- both valid Nix); results stay precise where they are
          -- input-independent. `dirOf` is kind-preserving — its precise
          -- `a -> a` scheme lives in 'builtinSchemeTable' (which shadows
          -- this record fallback on both access paths).
          ("baseNameOf", TFun (TUnion [TPath, TString]) TString)
        , ("dirOf", TFun (TUnion [TPath, TString]) (TUnion [TPath, TString]))
        , ("stringLength", TFun TString TInt)
        , ("substring", TFun TInt (TFun TInt (TFun TString TString)))
        , ("replaceStrings", TFun (TList TString) (TFun (TList TString) (TFun TString TString)))
        , -- ── list operations ──
          ("head", TFun (TList TAny) TAny)
        , ("tail", TFun (TList TAny) (TList TAny))
        , ("length", TFun (TList TAny) TInt)
        , ("elemAt", TFun (TList TAny) (TFun TInt TAny))
        , ("filter", TFun (TFun TAny TBool) (TFun (TList TAny) (TList TAny)))
        , ("map", TFun (TFun TAny TAny) (TFun (TList TAny) (TList TAny)))
        , ("foldl'", TFun (TFun TAny (TFun TAny TAny)) (TFun TAny (TFun (TList TAny) TAny)))
        , ("concatLists", TFun (TList (TList TAny)) (TList TAny))
        , ("concatMap", TFun (TFun TAny (TList TAny)) (TFun (TList TAny) (TList TAny)))
        , -- ── attribute set introspection ──
          -- n.b. record args are TAny here; real row-polymorphic signatures
          -- for these land in rows stage 4 (these feed the `builtins.X` path).
          ("attrNames", TFun TAny (TList TString))
        , ("attrValues", TFun TAny (TList TAny))
        , ("hasAttr", TFun TString (TFun TAny TBool))
        , ("getAttr", TFun TString (TFun TAny TAny))
        , ("removeAttrs", TFun TAny (TFun (TList TString) TAny))
        , -- n.b. the element record is OPEN: real listToAttrs ignores extra
          -- fields, and a closed row here collides with map-lambda open rows
          -- through the function-domain join

          ( "listToAttrs"
          , TFun
              ( TList
                  (TAttrsOpen (Map.fromList [("name", (TString, False)), ("value", (TAny, False))]))
              )
              TAny
          )
        , -- ── type predicates ──
          ("isNull", TFun TAny TBool)
        , ("isInt", TFun TAny TBool)
        , ("isFloat", TFun TAny TBool)
        , ("isBool", TFun TAny TBool)
        , ("isString", TFun TAny TBool)
        , ("isList", TFun TAny TBool)
        , ("isAttrs", TFun TAny TBool)
        , ("isFunction", TFun TAny TBool)
        , ("isPath", TFun TAny TBool)
        , -- ── arithmetic ──
          ("add", TFun TInt (TFun TInt TInt))
        , ("sub", TFun TInt (TFun TInt TInt))
        , ("mul", TFun TInt (TFun TInt TInt))
        , ("div", TFun TInt (TFun TInt TInt))
        , ("lessThan", TFun TInt (TFun TInt TBool))
        , -- ── file / derivation I/O ──
          ("import", TFun (TUnion [TPath, TString]) TAny)
        , ("readFile", TFun (TUnion [TPath, TString]) TString)
        , ("toPath", TFun TString TPath)
        , ("derivation", TFun TAny TDerivation)
        , -- ── control flow / debugging ──
          ("throw", TFun TString TAny)
        , ("abort", TFun TString TAny)
        , ("trace", TFun TAny (TFun TAny TAny))
        , ("seq", TFun TAny (TFun TAny TAny))
        , ("deepSeq", TFun TAny (TFun TAny TAny))
        ,
          ( "tryEval"
          , TFun
              TAny
              (TAttrs (Map.fromList [("success", (TBool, False)), ("value", (TAny, False))]))
          )
        , -- ── review-5: the missing-builtins FP class mined by the nixpkgs oracle
          -- sweep (38 names / 551 files of "attribute 'X' missing on closed
          -- attribute set"). Signatures follow the Nix manual; kinds verified
          -- against `builtins.typeOf` where non-obvious (`toFile` yields a
          -- store-path STRING, not a path). Value-dependent results (`match` is
          -- null-or-list, `fromJSON`, `readDir`) stay TAny — claiming a union
          -- here would false-positive every consumer that destructures the
          -- result directly (a union value only unifies member-wise, see
          -- 'unionMemberAccepts').
          ("toJSON", TFun TAny TString)
        , ("fromJSON", TFun TString TAny)
        , ("toFile", TFun TString (TFun TString TString))
        , ("typeOf", TFun TAny TString)
        , ("concatStringsSep", TFun TString (TFun (TList TString) TString))
        , ("match", TFun TString (TFun TString TAny))
        , ("split", TFun TString (TFun TString (TList TAny)))
        , ("hashString", TFun TString (TFun TString TString))
        , ("getEnv", TFun TString TString)
        , ("pathExists", TFun (TUnion [TPath, TString]) TBool)
        , ("readDir", TFun TPath TAny)
        , ("filterSource", TFun (TFun TPath (TFun TString TBool)) (TFun TPath TPath))
        , ("path", TFun TAny TString)
        , ("fetchurl", TFun TAny TAny)
        , ("compareVersions", TFun TString (TFun TString TInt))
        , ("splitVersion", TFun TString (TList TString))
        ,
          ( "parseDrvName"
          , TFun
              TString
              (TAttrs (Map.fromList [("name", (TString, False)), ("version", (TString, False))]))
          )
        , ("parseFlakeRef", TFun TString TAny)
        , ("flakeRefToString", TFun TAny TString)
        , ("functionArgs", TFun TAny TAny)
        , ("mapAttrs", TFun (TFun TString (TFun TAny TAny)) (TFun TAny TAny))
        , ("catAttrs", TFun TString (TFun (TList TAny) (TList TAny)))
        , ("intersectAttrs", TFun TAny (TFun TAny TAny))
        , ("unsafeGetAttrPos", TFun TString (TFun TAny TAny))
        , ("unsafeDiscardStringContext", TFun TString TString)
        , ("unsafeDiscardOutputDependency", TFun TString TString)
        , ("addDrvOutputDependencies", TFun TString TString)
        , ("addErrorContext", TFun TString (TFun TAny TAny))
        , ("traceVerbose", TFun TAny (TFun TAny TAny))
        , -- ── review-6: tail classes from the full-corpus dump ──
          -- `fetchTarball`/`fetchGit`/`__curPos` are GLOBALS (bare names in
          -- real files); string-context introspection rounds out the record.
          -- `fetchTarball` yields a store PATH (typeOf → "path"); `fetchGit`
          -- and `fetchTree` yield info sets (outPath &c.) — TAny, since
          -- consumers destructure them. `hashFile` accepts a path or string.
          ("fetchTarball", TFun TAny TPath)
        , ("fetchGit", TFun TAny TAny)
        , ("fetchTree", TFun TAny TAny)
        , ("fetchClosure", TFun TAny TAny)
        , ("getFlake", TFun TAny TAny)
        , ("hasContext", TFun TString TBool)
        , ("getContext", TFun TString TAny)
        , ("appendContext", TFun TString (TFun TAny TString))
        , ("hashFile", TFun TString (TFun (TUnion [TPath, TString]) TString))
        , ("toXML", TFun TAny TString)
        , ("ceil", TFun (TUnion [TInt, TFloat]) TInt)
        , ("floor", TFun (TUnion [TInt, TFloat]) TInt)
        , ("bitAnd", TFun TInt (TFun TInt TInt))
        , ("bitOr", TFun TInt (TFun TInt TInt))
        , ("bitXor", TFun TInt (TFun TInt TInt))
        , ("groupBy", TFun (TFun TAny TString) (TFun (TList TAny) TAny))
        ,
          ( "partition"
          , TFun
              (TFun TAny TBool)
              ( TFun
                  (TList TAny)
                  ( TAttrs
                      ( Map.fromList
                          [("right", (TList TAny, False)), ("wrong", (TList TAny, False))]
                      )
                  )
              )
          )
        , ("zipAttrsWith", TFun (TFun TString (TFun (TList TAny) TAny)) (TFun (TList TAny) TAny))
        , ("genericClosure", TFun TAny (TList TAny))
        , ("storePath", TFun (TUnion [TPath, TString]) TString)
        , ("readFileType", TFun (TUnion [TPath, TString]) TString)
        , ("scopedImport", TFun TAny TAny)
        , ("derivationStrict", TFun TAny TAny)
        , -- values, not functions
          -- `__curPos` is a global VALUE: the source position of the token,
          -- as { file, line, column } (used by nixpkgs' position machinery).

          ( "__curPos"
          , TAttrs
              ( Map.fromList
                  [("file", (TString, False)), ("line", (TInt, False)), ("column", (TInt, False))]
              )
          )
        , ("currentSystem", TString)
        , ("nixVersion", TString)
        , ("storeDir", TString)
        , ("currentTime", TInt)
        , ("langVersion", TInt)
        , ("nixPath", TList TAny)
        , -- mono fallbacks for the new scheme-table entries (the record view,
          -- like `head`/`map` above)
          ("elem", TFun TAny (TFun (TList TAny) TBool))
        , ("sort", TFun (TFun TAny (TFun TAny TBool)) (TFun (TList TAny) (TList TAny)))
        , ("any", TFun (TFun TAny TBool) (TFun (TList TAny) TBool))
        , ("all", TFun (TFun TAny TBool) (TFun (TList TAny) TBool))
        , ("genList", TFun (TFun TInt TAny) (TFun TInt (TList TAny)))
        ]
