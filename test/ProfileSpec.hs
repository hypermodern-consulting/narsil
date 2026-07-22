{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // tests // profiles
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "You can't let the little pricks generation-gap you."
--
--                                                                            — Mona Lisa Overdrive
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The strictness hierarchy, held together: `config/profiles.dhall` is the
--   source of truth, `Narsil.Core.Profiles` is its executable mirror, and
--   the PARITY test here refuses to let them drift. The behavior tests pin the
--   resolution semantics: explicit overrides beat the profile, child rules
--   beat the parent chain, `off` ignores the world.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module ProfileSpec (profileTests) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Dhall (FromDhall, defaultInterpretOptions, fieldModifier, genericAutoWith)
import Dhall qualified
import GHC.Generics (Generic)

import Data.List (sort)
import Narsil.Core.Config (Config (..), RuleOverride (..), Severity (..), defaultConfig)
import Narsil.Core.Profiles qualified as Profiles

-- ── the Dhall side, decoded locally ─────────────────────────────────

data DhallProfile = DhallProfile
  { dpName :: Text
  , dpDescription :: Text
  , dpParent :: Maybe Text
  , dpRules :: [RuleOverride]
  , dpIgnores :: [Text]
  , dpFiles :: Maybe [Text]
  }
  deriving (Generic, Show)

instance FromDhall DhallProfile where
  autoWith _norm = genericAutoWith (defaultInterpretOptions{fieldModifier = rename})
   where
    rename "dpName" = "name"
    rename "dpDescription" = "description"
    rename "dpParent" = "parent"
    rename "dpRules" = "rules"
    rename "dpIgnores" = "ignores"
    rename "dpFiles" = "files"
    rename n = n

data DhallProfiles = DhallProfiles
  { strict :: DhallProfile
  , standard :: DhallProfile
  , minimal :: DhallProfile
  , nixpkgs :: DhallProfile
  , security :: DhallProfile
  , off :: DhallProfile
  }
  deriving (Generic, Show)

instance FromDhall DhallProfiles

-- ── parity: the Haskell mirror may not drift from the Dhall truth ───

testDhallParity :: IO Bool
testDhallParity = do
  loaded <-
    try (Dhall.inputFile Dhall.auto "config/profiles.dhall") ::
      IO (Either SomeException DhallProfiles)
  case loaded of
    Left err -> do
      putStr ("(dhall load failed: " <> takeWhile (/= '\n') (show err) <> ") ")
      pure False
    Right ps ->
      pure $
        all
          matches
          [ strict ps
          , standard ps
          , minimal ps
          , nixpkgs ps
          , security ps
          , off ps
          ]
 where
  matches dp = case Profiles.lookupProfile (dpName dp) of
    Nothing -> False
    Just bp ->
      Profiles.bpParent bp == dpParent dp
        && Profiles.bpRules bp == dpRules dp
        && Profiles.bpIgnores bp == dpIgnores dp

-- ── parity: the default-Off tier mirrors rules.dhall ────────────────

data DhallLanguage = Nix | Cpp | Bash | Any
  deriving (Generic, Show)

instance FromDhall DhallLanguage

data DhallRule = DhallRule
  { drId :: Text
  , drLanguage :: DhallLanguage
  , drSeverity :: Severity
  , drDescription :: Text
  , drRationale :: Text
  }
  deriving (Generic, Show)

instance FromDhall DhallRule where
  autoWith _norm = genericAutoWith (defaultInterpretOptions{fieldModifier = rename})
   where
    rename "drId" = "id"
    rename "drLanguage" = "language"
    rename "drSeverity" = "default-severity"
    rename "drDescription" = "description"
    rename "drRationale" = "rationale"
    rename n = n

testDefaultOffParity :: IO Bool
testDefaultOffParity = do
  loaded <-
    try (Dhall.input Dhall.auto "(./config/rules.dhall).all-rules") ::
      IO (Either SomeException [DhallRule])
  case loaded of
    Left err -> do
      putStr ("(dhall load failed: " <> takeWhile (/= '\n') (show err) <> ") ")
      pure False
    Right rs ->
      pure $
        sort [drId r | r <- rs, drSeverity r == SevOff]
          == sort Profiles.defaultOffRules

-- ── behavior: resolution semantics ──────────────────────────────────

withProfile :: Text -> Config
withProfile name = defaultConfig{configProfile = name}

-- the nixpkgs profile's own rule: type checking degrades to warning (the
-- contract's lax shipping mode)
testNixpkgsLaxTypeRule :: IO Bool
testNixpkgsLaxTypeRule =
  pure $
    Profiles.effectiveSeverity (withProfile "nixpkgs") "type-check-failure"
      == Just SevWarning

-- inheritance: a rule the child does not mention resolves through the parent
-- (nixpkgs → standard: no-heredoc-in-inline-bash; security → minimal: with-lib)
testParentChainResolves :: IO Bool
testParentChainResolves =
  pure $
    Profiles.effectiveSeverity (withProfile "nixpkgs") "no-heredoc-in-inline-bash"
      == Just SevError
      && Profiles.effectiveSeverity (withProfile "security") "with-lib"
        == Just SevWarning

-- the child's entry shadows the parent's for the same rule id
testChildShadowsParent :: IO Bool
testChildShadowsParent =
  pure $
    Profiles.effectiveSeverity (withProfile "nixpkgs") "with-lib" == Just SevOff
      && Profiles.effectiveSeverity (withProfile "standard") "with-lib" == Just SevError

-- an explicit user override beats the profile
testUserOverrideWins :: IO Bool
testUserOverrideWins =
  pure $
    Profiles.effectiveSeverity config "type-check-failure" == Just SevError
 where
  config =
    (withProfile "nixpkgs")
      { configOverrides = [RuleOverride "type-check-failure" SevError Nothing]
      }

-- unknown profile names contribute nothing (built-in defaults apply)
testUnknownProfileIsNeutral :: IO Bool
testUnknownProfileIsNeutral =
  pure $
    Profiles.effectiveSeverity (withProfile "no-such-profile") "with-lib" == Nothing

-- `off` ignores everything via its glob
testOffIgnoresAll :: IO Bool
testOffIgnoresAll =
  pure $
    Profiles.isIgnored (withProfile "off") "pkgs/anything/default.nix"
      && not (Profiles.isIgnored (withProfile "standard") "pkgs/anything/default.nix")

-- the opt-in tier: non-lisp-case is silent by default and under standard,
-- LIVE under strict, and an explicit user override can force it anywhere
testOptInRuleTier :: IO Bool
testOptInRuleTier =
  pure $
    Profiles.isSuppressed defaultConfig "non-lisp-case"
      && Profiles.isSuppressed (withProfile "standard") "non-lisp-case"
      && not (Profiles.isSuppressed (withProfile "strict") "non-lisp-case")
      && not (Profiles.isSuppressed forced "non-lisp-case")
 where
  forced =
    defaultConfig
      { configOverrides = [RuleOverride "non-lisp-case" SevError Nothing]
      }

-- every parent named by a built-in profile exists (no dangling chain)
testParentsExist :: IO Bool
testParentsExist =
  pure $
    all
      (maybe True (\p -> Profiles.lookupProfile p /= Nothing) . Profiles.bpParent)
      Profiles.builtinProfiles

profileTests :: [(String, IO Bool)]
profileTests =
  [ ("profile_dhall_parity", testDhallParity)
  , ("profile_default_off_parity", testDefaultOffParity)
  , ("profile_opt_in_rule_tier", testOptInRuleTier)
  , ("profile_nixpkgs_lax_type_rule", testNixpkgsLaxTypeRule)
  , ("profile_parent_chain_resolves", testParentChainResolves)
  , ("profile_child_shadows_parent", testChildShadowsParent)
  , ("profile_user_override_wins", testUserOverrideWins)
  , ("profile_unknown_is_neutral", testUnknownProfileIsNeutral)
  , ("profile_off_ignores_all", testOffIgnoresAll)
  , ("profile_parents_exist", testParentsExist)
  ]
