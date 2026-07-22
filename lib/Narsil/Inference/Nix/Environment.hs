{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                // inference // nix // environment
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "A name is a powerful thing, in the hands of those who know how to use it."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The typing environment threaded through inference: name → scheme bindings,
--   the enclosing `with` scope, and the per-file imported-module types. Pure
--   data + accessors, no Infer monad — it sits at the bottom of the engine's
--   dependency graph so everything above can name it.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Inference.Nix.Environment (
  TypeEnv (..),
  emptyEnv,
  extendEnv,
  lookupEnv,
  extendImport,
  extendImports,
  lookupImport,
  extendCallPackage,
  lookupCallPackage,
  withPkgsOracle,
)
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Narsil.Inference.Nix.Type

{- | the typing environment threaded through inference: name → scheme bindings,
the enclosing @with@ scope, per-file imported-module types, and the lenient /
module-param mode flags.
-}
data TypeEnv = TypeEnv
  { envBindings :: Map Text Scheme
  , envWith :: [NixType]
  {- ^ the enclosing @with@ scopes, INNERMOST FIRST — Nix searches all of
  them (inner shadowing outer), so symbol fallthrough must too
  (`with lib; with builtins; mapAttrs'` comes from lib, not builtins)
  -}
  , envImportTypes :: Map FilePath NixType
  , envCallPackageTypes :: Map FilePath NixType
  {- ^ precomputed RESULT types for @callPackage ./path { }@ call sites, keyed by
    the raw path as written. Where 'envImportTypes' holds what @import ./p@ yields
    (the file's value — a package is a function), this holds what @callPackage ./p
    { }@ yields: that function applied to its auto-filled arguments, i.e. the
    package itself. Seeded by the closure ("Narsil.Layout.Closure"); empty by
    default, so absent it changes nothing.
  -}
  , envLenient :: Bool
  {- ^ when True, treat unbound names as fresh polymorphic vars instead of
    errors. Used for backwards compatibility with libraries that mention
    builtins we don't yet model. Default: False (strict).
  -}
  , envModuleParams :: Bool
  {- ^ when True, lambda parameters whose names are well-known external module
    / flake inputs (self, inputs, config, pkgs, the @-bound input set, …) are
    typed as dynamic ('TAny') rather than fresh inference vars. These values are
    supplied by the flake / module system, not by the file under analysis, so
    inferring precise types for them only produces false positives (e.g. the
    self-referential @inputs in `mkFlake { inherit inputs; }`). Matched by name
    so ordinary inner lambdas (`x: x + 1`) keep precise inference. Default: False.
  -}
  , envPkgsOracle :: Map [Text] NixType
  {- ^ precomputed types for @pkgs.<path>@ attribute references, keyed by the
    path AFTER @pkgs@ (@["hello","pname"]@ for @pkgs.hello.pname@). Seeded by the
    caller from the nixpkgs eval backend (see "Narsil.Nixpkgs.Oracle"); the
    inferencer consults it syntactically on a @pkgs.…@ selection, turning what was
    an opaque 'TAny' into a real type — better hover, real attribute-typo errors,
    sharper unification. Empty by default, so absent it changes nothing.
  -}
  }
  deriving (Eq, Show)

-- | the empty environment: no bindings, no @with@, no imports, strict mode.
emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty [] Map.empty Map.empty False False Map.empty

-- | seed the @pkgs.<path>@ type oracle (replacing any existing entries).
withPkgsOracle :: Map [Text] NixType -> TypeEnv -> TypeEnv
withPkgsOracle oracle env = env{envPkgsOracle = oracle}

{- | extend the env with one name → scheme binding
n.b. this shadows — if a name already exists the new scheme wins
-}
extendEnv :: Text -> Scheme -> TypeEnv -> TypeEnv
extendEnv name scheme environment =
  environment{envBindings = Map.insert name scheme (envBindings environment)}

-- | look up a name; returns Nothing if absent (type defaults to fresh var downstream)
lookupEnv :: Text -> TypeEnv -> Maybe Scheme
lookupEnv name environment = Map.lookup name (envBindings environment)

-- | register the exported type of an imported file
extendImport :: FilePath -> NixType -> TypeEnv -> TypeEnv
extendImport path t env = env{envImportTypes = Map.insert path t (envImportTypes env)}

-- | extend env with multiple imported modules at once
extendImports :: Map FilePath NixType -> TypeEnv -> TypeEnv
extendImports imports env = env{envImportTypes = Map.union imports (envImportTypes env)}

-- | look up a previously imported module's type
lookupImport :: FilePath -> TypeEnv -> Maybe NixType
lookupImport path env = Map.lookup path (envImportTypes env)

-- | register the RESULT type of a @callPackage ./path { }@ site (keyed by raw path)
extendCallPackage :: FilePath -> NixType -> TypeEnv -> TypeEnv
extendCallPackage path t env =
  env{envCallPackageTypes = Map.insert path t (envCallPackageTypes env)}

-- | look up the result type a @callPackage ./path { }@ site was precomputed to have
lookupCallPackage :: FilePath -> TypeEnv -> Maybe NixType
lookupCallPackage path env = Map.lookup path (envCallPackageTypes env)
