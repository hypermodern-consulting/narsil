{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                     // nix // compile // profiles
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Rules are rules, Case."
--
--                                                                                     — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The STRICTNESS HIERARCHY, resolved. `config/profiles.dhall` is the source
--   of truth for the built-in profiles (strict / standard / minimal / nixpkgs /
--   security / off); this module is its executable mirror, and the parity test
--   (test/ProfileSpec.hs) holds the two together — the tables here may not
--   drift from the Dhall file.
--
--   Resolution: a profile contributes an ordered override list, CHILD rules
--   first, then the parent chain ('resolveProfile'). The user's explicit
--   `overrides` in `.nix-compile.dhall` always beat the profile — see
--   'Narsil.Core.Config.effectiveSeverity'.
--
--   This is the enforcement surface of doc/design/contract.md's shipping
--   posture: strict-by-default judgments, with lax modes expressed as
--   enumerated severity remaps (e.g. the `nixpkgs` profile carries
--   `type-check-failure = Warning` while upstream PRs are in flight) — never
--   by re-judging.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Core.Profiles (
  BuiltinProfile (..),
  builtinProfiles,
  lookupProfile,
  resolveProfile,
  profileIgnores,

  -- * profile-aware config queries (what the CLI consumes)
  effectiveSeverity,
  isSuppressed,
  isIgnored,
  defaultOffRules,
)
where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import System.FilePath qualified as FP

import Narsil.Core.Config (
  Config (..),
  RuleOverride (..),
  Severity (..),
  matchGlob,
 )

-- | one built-in profile: a name, an optional parent, ordered rule overrides, ignore globs.
data BuiltinProfile = BuiltinProfile
  { bpName :: !Text
  , bpParent :: !(Maybe Text)
  , bpRules :: ![RuleOverride]
  , bpIgnores :: ![Text]
  }
  deriving (Eq, Show)

-- | the built-in profile by name, if any.
lookupProfile :: Text -> Maybe BuiltinProfile
lookupProfile name = find ((== name) . bpName) builtinProfiles

{- | The full override list a profile name resolves to: its own rules first,
then the parent chain's (so a child entry shadows a parent entry for the same
rule id under first-match lookup). Unknown names resolve to no overrides; a
parent cycle stops at the first repeat.
-}
resolveProfile :: Text -> [RuleOverride]
resolveProfile = go Set.empty
 where
  go seen name
    | name `Set.member` seen = []
    | otherwise =
        case lookupProfile name of -- CASE-OK: shape dispatch
          Nothing -> []
          Just p -> bpRules p ++ maybe [] (go (Set.insert name seen)) (bpParent p)

-- | the ignore globs a profile contributes (own + parent chain).
profileIgnores :: Text -> [Text]
profileIgnores = go Set.empty
 where
  go seen name
    | name `Set.member` seen = []
    | otherwise =
        case lookupProfile name of -- CASE-OK: shape dispatch
          Nothing -> []
          Just p -> bpIgnores p ++ maybe [] (go (Set.insert name seen)) (bpParent p)

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- Profile-aware queries
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{- | a rule's effective severity: the user's explicit `overrides` win, then the
resolved profile chain, then 'Nothing' (the rule's built-in default).
-}
effectiveSeverity :: Config -> Text -> Maybe Severity
effectiveSeverity config ruleId =
  firstMatch (configOverrides config)
    <|> firstMatch (resolveProfile (configProfile config))
 where
  firstMatch rs = overrideSeverity <$> listToMaybe (filter ((== ruleId) . overrideId) rs)

{- | is this rule suppressed? 'SevOff' by explicit override or profile, or —
when nothing anywhere mentions it — a rule whose BUILT-IN default is Off
('defaultOffRules'). Opt-in rules only fire when a profile or override
turns them on (strict does for @non-lisp-case@).
-}
isSuppressed :: Config -> Text -> Bool
isSuppressed config ruleId = decide (effectiveSeverity config ruleId)
 where
  decide (Just sev) = sev == SevOff
  decide Nothing = ruleId `elem` defaultOffRules

{- | rules whose @default-severity@ is Off in config\/rules.dhall — the
opt-in tier. Parity-tested against the Dhall file (test\/ProfileSpec.hs).
-}
defaultOffRules :: [Text]
defaultOffRules = ["non-lisp-case"]

-- | does any configured ignore glob — explicit or profile-contributed — match this path?
isIgnored :: Config -> FilePath -> Bool
isIgnored config filePath = any (`matchGlob` normalisedPath) globs
 where
  globs = configExtraIgnores config ++ profileIgnores (configProfile config)
  normalisedPath = FP.normalise filePath

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- The tables (mirror of config/profiles.dhall — parity-tested)
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

rule :: Text -> Severity -> Maybe Text -> RuleOverride
rule = RuleOverride

builtinProfiles :: [BuiltinProfile]
builtinProfiles = [strict, standard, minimal, nixpkgs, security, off]

-- | Full straylight conventions: everything error, lisp-case enforced.
strict :: BuiltinProfile
strict =
  BuiltinProfile
    { bpName = "strict"
    , bpParent = Nothing
    , bpRules =
        [ rule "non-lisp-case" SevError (Just "Enforces prelude-only code paths")
        , rule "no-substitute-all" SevError (Just "All text templating via Dhall")
        , rule "no-raw-mkderivation" SevError Nothing
        , rule "no-raw-runcommand" SevError Nothing
        , rule "no-raw-writeshellapplication" SevError Nothing
        , rule "no-translate-attrs-outside-prelude" SevError Nothing
        , rule "rec-anywhere" SevError Nothing
        , rule "with-lib" SevError Nothing
        , rule "no-heredoc-in-inline-bash" SevError Nothing
        , rule "missing-meta" SevError Nothing
        , rule "missing-description" SevError Nothing
        , rule "cpp-using-namespace-header" SevError Nothing
        , rule "cpp-raw-new-delete" SevError (Just "Memory safety")
        , rule "missing-class" SevError (Just "Module compatibility must be explicit")
        ]
    , bpIgnores = []
    }

-- | Sensible defaults: no lisp-case, no prelude requirements.
standard :: BuiltinProfile
standard =
  BuiltinProfile
    { bpName = "standard"
    , bpParent = Nothing
    , bpRules =
        [ rule "non-lisp-case" SevOff (Just "Most projects use nixpkgs naming conventions")
        , rule "no-substitute-all" SevOff (Just "Dhall templating is straylight-specific")
        , rule "no-raw-mkderivation" SevOff (Just "Prelude wrappers are straylight-specific")
        , rule "no-raw-runcommand" SevOff (Just "Prelude wrappers are straylight-specific")
        , rule
            "no-raw-writeshellapplication"
            SevOff
            (Just "Prelude wrappers are straylight-specific")
        , rule "no-translate-attrs-outside-prelude" SevOff (Just "Prelude is straylight-specific")
        , rule "rec-anywhere" SevWarning (Just "rec is sometimes legitimate")
        , rule "with-lib" SevError (Just "Universally bad practice")
        , rule "no-heredoc-in-inline-bash" SevError (Just "Fragile pattern, common source of bugs")
        , rule "prefer-write-shell-application" SevWarning Nothing
        , rule "missing-meta" SevWarning Nothing
        , rule "missing-description" SevInfo Nothing
        , rule "missing-class" SevError Nothing
        , rule "cpp-using-namespace-header" SevError Nothing
        , rule "cpp-raw-new-delete" SevWarning Nothing
        ]
    , bpIgnores = []
    }

-- | Essential safety checks only, for gradual adoption.
minimal :: BuiltinProfile
minimal =
  BuiltinProfile
    { bpName = "minimal"
    , bpParent = Nothing
    , bpRules =
        [ rule "with-lib" SevWarning (Just "Common source of confusion")
        , rule "no-heredoc-in-inline-bash" SevError (Just "Almost always buggy")
        , rule "missing-class" SevWarning (Just "Recommended for module safety")
        , rule "cpp-using-namespace-header" SevError (Just "Namespace pollution")
        ]
    , bpIgnores = []
    }

-- | nixpkgs contribution conventions (inherits standard).
nixpkgs :: BuiltinProfile
nixpkgs =
  BuiltinProfile
    { bpName = "nixpkgs"
    , bpParent = Just "standard"
    , bpRules =
        [ rule
            "with-lib"
            SevOff
            (Just "nixpkgs uses with pervasively as the primary scope mechanism")
        , rule "rec-anywhere" SevOff (Just "nixpkgs uses rec in several legitimate patterns")
        , rule
            "prefer-write-shell-application"
            SevOff
            (Just "both patterns are legitimate in nixpkgs")
        , rule "or-null-fallback" SevOff (Just "or-null is a standard nixpkgs pattern")
        , rule "long-inline-string" SevOff (Just "cosmetic; nixpkgs has its own formatting rules")
        , rule
            "type-check-failure"
            SevWarning
            ( Just
                ( "lax while upstream PRs land: the residual stock-nixpkgs diagnostics "
                    <> "are real bugs or ledgered exclusions (doc/design/contract.md)"
                )
            )
        , rule "missing-meta" SevError (Just "Required by nixpkgs guidelines")
        , rule "missing-description" SevError (Just "Required by nixpkgs guidelines")
        , rule "missing-class" SevError (Just "Module compatibility must be explicit")
        ]
    , bpIgnores = []
    }

-- | Security-focused checks (inherits minimal).
security :: BuiltinProfile
security =
  BuiltinProfile
    { bpName = "security"
    , bpParent = Just "minimal"
    , bpRules =
        [ rule "no-heredoc-in-inline-bash" SevError (Just "Potential injection vector")
        , rule "no-substitute-all" SevWarning (Just "Text interpolation can be injection vector")
        , rule "cpp-raw-new-delete" SevError (Just "Memory safety")
        ]
    , bpIgnores = []
    }

-- | All rules disabled; base for custom profiles.
off :: BuiltinProfile
off =
  BuiltinProfile
    { bpName = "off"
    , bpParent = Nothing
    , bpRules = []
    , bpIgnores = ["**/*"]
    }
