{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                  // bash // types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He had a feel for the shape of the data, the way a sculptor feels the
--    stone."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The shared vocabulary of the bash analysis pipeline: the small type
--   language ('Type' / 'Subst' / 'Constraint') its Hindley-Milner solver runs
--   on, the 'Fact's the parser observes, the 'Command' / config / store-path
--   model, and the 'Schema' those facts resolve into. Source locations live in
--   'Narsil.Core.Span'; everything here is bash-pipeline-specific.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Bash.Types (
  -- * Types
  Type (..),
  TypeVar (..),

  -- * Constraints
  Constraint (..),
  Subst,
  emptySubst,
  singleSubst,
  composeSubst,
  applySubst,

  -- * Literals
  Literal (..),
  literalType,

  -- * Facts (observations from parsing)
  Fact (..),
  Quoted (..),

  -- * Config paths
  ConfigPath,
  ConfigPart (..),

  -- * Commands
  Command (..),
  Arg (..),

  -- * Store paths
  StorePath (..),
  isStorePath,

  -- * Schema (final output)
  Schema (..),
  EnvSpec (..),
  mergeEnvSpec,
  ConfigSpec (..),
  mergeConfigSpec,
  CommandSpec (..),
  emptySchema,
  mergeSchemas,

  -- * Scripts
  Script (..),

  -- * Errors
  TypeError (..),
)
where

import GHC.Generics (Generic)

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Span (Span (..))

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | a unification variable in the bash type language, named by 'Text'.
newtype TypeVar = TypeVar {unTypeVar :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | the small monotype language the bash Hindley-Milner solver unifies over.
data Type
  = TInt
  | TString
  | TBool
  | TPath
  | TNumeric
  | TVar TypeVar
  deriving stock (Eq, Ord, Show, Generic)

instance FromJSON Type
instance ToJSON Type

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Constraints
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | an equality constraint @t1 :~: t2@ the solver must satisfy.
data Constraint = Type :~: Type
  deriving stock (Eq, Show, Generic)

infix 4 :~:

-- | a substitution mapping type variables to types; the solver's running state.
type Subst = Map TypeVar Type

-- | the identity substitution (binds nothing).
emptySubst :: Subst
emptySubst = Map.empty

-- | a substitution binding a single variable to a type.
singleSubst :: TypeVar -> Type -> Subst
singleSubst = Map.singleton

-- | compose two substitutions; the first is applied to the range of the second.
composeSubst :: Subst -> Subst -> Subst
composeSubst substitution1 substitution2 =
  Map.map (applySubst substitution1) substitution2 `Map.union` substitution1

-- | apply a substitution to a type, chasing variable bindings to a fixed point.
applySubst :: Subst -> Type -> Type
applySubst substitution = go
 where
  go (TVar variable) = maybe (TVar variable) go (Map.lookup variable substitution)
  go typ = typ

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Literals
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | a fully-resolved literal value observed in a script (int, string, bool, store path).
data Literal
  = LitInt !Int
  | LitString !Text
  | LitBool !Bool
  | LitPath !StorePath
  deriving stock (Eq, Show, Generic)

instance FromJSON Literal
instance ToJSON Literal

-- | the 'Type' a literal inhabits.
literalType :: Literal -> Type
literalType (LitInt _) = TInt
literalType (LitString _) = TString
literalType (LitBool _) = TBool
literalType (LitPath _) = TPath

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Facts
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | whether a value appeared inside double quotes; affects config value semantics.
data Quoted = Quoted | Unquoted
  deriving stock (Eq, Show, Generic)

instance FromJSON Quoted
instance ToJSON Quoted

-- | a dotted @config.*@ key, split into its segments (e.g. @["server","port"]@).
type ConfigPath = [Text]

-- | one piece of a config-value template: literal text or a variable expansion form.
data ConfigPart
  = ConfigText !Text
  | ConfigVar !Text
  | ConfigVarDefault !Text !Text
  | ConfigVarRequired !Text
  | ConfigVarAlternate !Text !Text
  deriving stock (Eq, Show, Generic)

instance FromJSON ConfigPart
instance ToJSON ConfigPart

-- | an observation the fact extractor reads off the bash AST; the raw input to schema inference.
data Fact
  = DefaultIs !Text !Literal !Span
  | DefaultFrom !Text !Text !Span
  | Required !Text !Span
  | AssignFrom !Text !Text !Span
  | AssignLit !Text !Literal !Span
  | ConfigAssign !ConfigPath !Text !Quoted !Span
  | ConfigLit !ConfigPath !Literal !Span
  | ConfigTemplate !ConfigPath ![ConfigPart] !Quoted !Span
  | CmdArg !Text !Text !Text !Span
  | UsesStorePath !StorePath !Span
  | BareCommand !Text !Span
  | DynamicCommand !Text !Span
  deriving stock (Eq, Show, Generic)

instance FromJSON Fact
instance ToJSON Fact

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Commands
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | a single command-line argument: a literal, a variable reference, or a flag.
data Arg
  = ArgLit !Text
  | ArgVar !Text
  | ArgFlag !Text
  deriving stock (Eq, Show, Generic)

instance FromJSON Arg
instance ToJSON Arg

-- | a parsed command invocation: name, optional resolved store path, args, and source span.
data Command = Command
  { cmdName :: !Text
  , cmdPath :: !(Maybe StorePath)
  , cmdArgs :: ![Arg]
  , cmdSpan :: !Span
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Command
instance ToJSON Command

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Store Paths
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | a @/nix/store/…@ path referenced by a script.
newtype StorePath = StorePath {unStorePath :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | does this text look like a safe @/nix/store/@ path (no @..@ or @//@ traversal)?
isStorePath :: Text -> Bool
isStorePath text =
  "/nix/store/" `T.isPrefixOf` text
    && not (".." `T.isInfixOf` text)
    && not ("//" `T.isInfixOf` text)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Schema
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | the inferred contract for one environment variable: type, requiredness, default, origin.
data EnvSpec = EnvSpec
  { envType :: !Type
  , envRequired :: !Bool
  , envDefault :: !(Maybe Literal)
  , envSpan :: !Span
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON EnvSpec
instance ToJSON EnvSpec

-- | merge two specs for the same variable: required if either is, first's type/default/span win.
mergeEnvSpec :: EnvSpec -> EnvSpec -> EnvSpec
mergeEnvSpec envSpec1 envSpec2 =
  EnvSpec
    { envType = envType envSpec1
    , envRequired = envRequired envSpec1 || envRequired envSpec2
    , -- keep envSpec1's default if it has one, else fall back to envSpec2's
      envDefault = envDefault envSpec1 <|> envDefault envSpec2
    , envSpan = envSpan envSpec1
    }

-- | merge two specs for the same config key; the later assignment wins.
mergeConfigSpec :: ConfigSpec -> ConfigSpec -> ConfigSpec
mergeConfigSpec _ configSpec2 = configSpec2

-- | the inferred contract for one @config.*@ key: type and how its value is sourced.
data ConfigSpec = ConfigSpec
  { cfgType :: !Type
  , cfgFrom :: !(Maybe Text)
  , cfgQuoted :: !(Maybe Quoted)
  , cfgLit :: !(Maybe Literal)
  , cfgTemplate :: !(Maybe [ConfigPart])
  , cfgSpan :: !Span
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ConfigSpec
instance ToJSON ConfigSpec

-- | a command the script invokes: its name, optional resolved store path, and source span.
data CommandSpec = CommandSpec
  { cmdSpecName :: !Text
  , cmdSpecPath :: !(Maybe StorePath)
  , cmdSpecSpan :: !Span
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON CommandSpec
instance ToJSON CommandSpec

-- | the final inferred interface of a script: env vars, config keys, commands, and store paths.
data Schema = Schema
  { schemaEnv :: !(Map Text EnvSpec)
  , schemaConfig :: !(Map ConfigPath ConfigSpec)
  , schemaCommands :: ![CommandSpec]
  , schemaStorePaths :: !(Set StorePath)
  , schemaBareCommands :: ![Text]
  , schemaDynamicCommands :: ![Text]
  , schemaDefaultedVars :: ![Text]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Schema
instance ToJSON Schema

-- | the empty schema: no env vars, config, commands, or store paths.
emptySchema :: Schema
emptySchema =
  Schema
    { schemaEnv = Map.empty
    , schemaConfig = Map.empty
    , schemaCommands = []
    , schemaStorePaths = Set.empty
    , schemaBareCommands = []
    , schemaDynamicCommands = []
    , schemaDefaultedVars = []
    }

-- | combine two schemas: env specs merge per-key, the rest concatenate or union.
mergeSchemas :: Schema -> Schema -> Schema
mergeSchemas schema1 schema2 =
  Schema
    { schemaEnv = Map.unionWith mergeEnvSpec (schemaEnv schema1) (schemaEnv schema2)
    , schemaConfig = schemaConfig schema1 `Map.union` schemaConfig schema2
    , schemaCommands = schemaCommands schema1 ++ schemaCommands schema2
    , schemaStorePaths = schemaStorePaths schema1 `Set.union` schemaStorePaths schema2
    , schemaBareCommands = schemaBareCommands schema1 ++ schemaBareCommands schema2
    , schemaDynamicCommands = schemaDynamicCommands schema1 ++ schemaDynamicCommands schema2
    , schemaDefaultedVars = schemaDefaultedVars schema1 ++ schemaDefaultedVars schema2
    }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Scripts
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | a script bundled with its analysis results: source text, extracted facts, inferred schema.
data Script = Script
  { scriptSource :: !Text
  , scriptFacts :: ![Fact]
  , scriptSchema :: !Schema
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Script
instance ToJSON Script

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Errors
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | a failure from the bash type solver: type mismatch, occurs-check, or unresolved variable.
data TypeError
  = Mismatch !Type !Type !Span
  | OccursCheck !TypeVar !Type !Span
  | Ambiguous !TypeVar !Span
  deriving stock (Eq, Show, Generic)

instance FromJSON TypeError
instance ToJSON TypeError
