-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                  // layout // convention // types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "Wintermute was hive mind, decision maker, effecting change in
--      the world outside."
--
--                                                                                     — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The convention vocabulary: a 'Convention' is a set of 'ConventionRule's
--   (module kind → where it may live, via 'PathPattern') plus the naming
--   policy ('NamingConvention') for files, attributes, and identifiers. A
--   violation is a 'LayoutError' tagged with an 'ErrorCode'. Dependency-free —
--   the preset catalog, naming checks, and the validation engine all build on
--   this.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Convention.Types (
  -- * Conventions
  Convention (..),
  ConventionRule (..),
  PathPattern (..),
  NamingConvention (..),

  -- * Results
  LayoutError (..),
  ErrorCode (..),
)
where

import Data.Text (Text)
import Narsil.Layout.ModuleKind (ModuleKind)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                          // types
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | A layout convention defines where things should live and what they're called.
data Convention = Convention
  { convName :: !Text
  , convDescription :: !Text
  , convRules :: ![ConventionRule]
  , convFileNaming :: !NamingConvention
  -- ^ File name convention
  , convAttrNaming :: !NamingConvention
  -- ^ Attribute name convention
  , convIdentNaming :: !NamingConvention
  -- ^ Identifier convention
  , convRequireFlakeMod :: !Bool
  -- ^ Require everything to be flake module
  }
  deriving (Eq, Show)

-- | A single rule mapping module kind to expected location.
data ConventionRule = ConventionRule
  { ruleKind :: !ModuleKind
  , rulePattern :: !PathPattern
  , ruleForbidden :: ![PathPattern]
  , ruleExportName :: !(Maybe Text)
  -- ^ Required export path (e.g., "perSystem.packages")
  }
  deriving (Eq, Show)

-- | Path pattern for matching.
data PathPattern
  = Prefix [String]
  | Contains [String]
  | Exact [String]
  | AnyOf [PathPattern]
  | None
  deriving (Eq, Show)

-- | Naming convention for identifiers.
data NamingConvention
  = -- | kebab-case (lisp-case) — straylight
    KebabCase
  | -- | snake_case
    SnakeCase
  | -- | camelCase — nixpkgs
    CamelCase
  | -- | PascalCase
    PascalCase
  | -- | No enforcement
    NoNaming
  deriving (Eq, Show)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                         // errors
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | a layout-violation code; each constructor names one rule that can fail.
data ErrorCode
  = -- | File in wrong location for its module kind
    E001
  | -- | File in forbidden location
    E002
  | -- | Wrong file name convention
    E003
  | -- | Wrong attribute name convention
    E004
  | -- | Wrong identifier convention
    E005
  | -- | Must be flake module but isn't
    E006
  | -- | _index.nix files are banned
    E007
  | -- | _main.nix files are banned
    E008
  | -- | Missing required _class attribute
    E009
  | -- | _class value doesn't match location
    E010
  deriving (Eq, Show)

{- | a single layout violation: its code, the offending path and module kind, a
human message, and the expected value (e.g. corrected name / location) if any.
-}
data LayoutError = LayoutError
  { errCode :: !ErrorCode
  , errPath :: !FilePath
  , errKind :: !ModuleKind
  , errMessage :: !Text
  , errExpected :: !(Maybe Text)
  }
  deriving (Eq, Show)
