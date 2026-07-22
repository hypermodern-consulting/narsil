{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                               // nixpkgs // eval
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He had the shape of it now, the bones of the thing, before any flesh
--    of detail hung on it."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Demand-controlled partial evaluation of a nixpkgs value, behind a SWAPPABLE
--   backend — the seam where the symbol/type completion engine plugs in.
--
--   A backend answers two questions about the value at an attribute path:
--   its attribute NAMES (force the spine, not the values) and the TYPE of one
--   field (force just that field). Backends, weakest to strongest:
--
--     * 'shapeBackend' — the derivation-shape template: the attrs every
--       mkDerivation output carries, served with NO evaluation. The always-on
--       floor (tier 1).
--     * an hnix mock spine-forcer — real package attr names, ~87%, a stopgap.
--     * the in-house nixlang compiler — the destination: arena-allocated,
--       Boehm-free, long-lived-process-safe; names AND types; the real engine.
--
--   The INTERFACE is the durable asset; the engine is fungible. See
--   doc/nixpkgs-symbol-completion.md.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Nixpkgs.Eval (
  -- * Backend interface
  EvalBackend (..),
  EvalError (..),
  defaultEvalBackend,
  composeBackend,

  -- * Tier 1 — the derivation shape
  derivationShapeAttrs,
  shapeBackend,
)
where

import Data.Maybe (isJust)
import Data.Text (Text)
import Narsil.Inference.Nix.Type (NixType)
import Narsil.Nixpkgs.Index (NixpkgsIndex, lookupPackage)

{- | Why a backend declined or failed to answer. 'Unsupported' means "not this
backend's job" — the caller falls back to a weaker tier; 'EvalFailed' means it
tried and the evaluation errored.
-}
data EvalError
  = Unsupported
  | EvalFailed !Text
  deriving (Eq, Show)

{- | A pluggable partial-evaluation backend over a nixpkgs checkout. Each op takes
the index (to resolve an attribute path to a source) and an attribute PATH from
the package-set root, e.g. @["hello"]@ or @["python3Packages","requests"]@.
-}
data EvalBackend = EvalBackend
  { backendName :: !Text
  , evalSpine :: !(NixpkgsIndex -> [Text] -> IO (Either EvalError [Text]))
  -- ^ the attribute names of the value at the path (force the spine, not values)
  , evalFieldType :: !(NixpkgsIndex -> [Text] -> Text -> IO (Either EvalError NixType))
  -- ^ the type of one field (force just that field's value)
  }

{- | The no-external-process default: the shape template. Callers that want real
evaluation compose a stronger backend in front via 'composeBackend' (the handler
does: nix-repl pool, then shape).
-}
defaultEvalBackend :: EvalBackend
defaultEvalBackend = shapeBackend

{- | Try the first backend; on 'Left' (declined or failed), fall back to the
second. So a strong-but-fallible engine (the nix-repl pool, or the compiler) can
sit in front of the always-available shape template.
-}
composeBackend :: EvalBackend -> EvalBackend -> EvalBackend
composeBackend front back =
  EvalBackend
    { backendName = backendName front <> "+" <> backendName back
    , evalSpine = \idx p -> evalSpine front idx p >>= orElse (evalSpine back idx p)
    , evalFieldType = \idx p f ->
        evalFieldType front idx p f >>= orElse (evalFieldType back idx p f)
    }
 where
  orElse fallback = either (const fallback) (pure . Right)

{- | The attributes essentially every @stdenv.mkDerivation@ output carries,
independent of the package — the structural attrs mkDerivation/stdenv always add
plus the conventional ones present on nearly all derivations. Tier 1: offered
for any known package with zero evaluation. (Empirically grounded against real
spine-forced derivations; see the spike in doc/nixpkgs-symbol-completion.md.)
-}
derivationShapeAttrs :: [Text]
derivationShapeAttrs =
  [ -- identity / outputs
    "name"
  , "pname"
  , "version"
  , "type"
  , "system"
  , "outPath"
  , "drvPath"
  , "outputName"
  , "outputs"
  , "out"
  , "all"
  , "drvAttrs"
  , "inputDerivation"
  , -- the override family
    "override"
  , "overrideAttrs"
  , "overrideDerivation"
  , -- metadata / escape hatches
    "meta"
  , "passthru"
  , "tests"
  , -- conventional build inputs (present on ~all derivations)
    "src"
  , "buildInputs"
  , "nativeBuildInputs"
  , "propagatedBuildInputs"
  , "buildPhase"
  , "installPhase"
  , "configureFlags"
  , "patches"
  , "doCheck"
  , "doInstallCheck"
  , "enableParallelBuilding"
  , "strictDeps"
  , "stdenv"
  , "builder"
  , "args"
  ]

{- | Tier 1 backend: the shape template for any known package, no evaluation.
Returns 'Unsupported' for nested paths (only a real evaluator instantiates
@python3Packages.requests@) and for field types (force needs an evaluator).
-}
shapeBackend :: EvalBackend
shapeBackend =
  EvalBackend
    { backendName = "shape-template"
    , evalSpine = \idx path -> pure (spineFor idx path)
    , evalFieldType = \_ _ _ -> pure (Left Unsupported)
    }
 where
  spineFor idx [pkg]
    | isJust (lookupPackage idx pkg) = Right derivationShapeAttrs
  spineFor _ _ = Left Unsupported
