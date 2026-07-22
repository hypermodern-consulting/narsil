{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                // layout // convention // presets
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "The known nets, the corporate cores, each with its own internal
--      logic, its own way of arranging the world."
--
--                                                                                     — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The convention catalog: the concrete 'Convention' values the tool ships
--   with — 'straylight' (kebab-case, uniform module layout), plus the
--   nixpkgs-by-name, flake-parts, nixos-config, and all-flake-module presets —
--   and 'layoutFromName' to pick one by name. Pure data over
--   "Narsil.Layout.Convention.Types".
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Convention.Presets (
  straylight,
  nixpkgsByName,
  flakeParts,
  nixosConfig,
  allFlakeModule,
  layoutFromName,
)
where

import Data.Text (Text)
import Narsil.Layout.Convention.Types
import Narsil.Layout.ModuleKind

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                          // straylight convention
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Straylight convention.

Everything is a flake module. Uniform structure.

Structure:
  nix/
    modules/
      flake/      # flake-parts modules → perSystem.*, flake.*
      nixos/      # NixOS modules → flake.nixosModules.*
      home/       # home-manager modules → flake.homeModules.*
      darwin/     # nix-darwin modules → flake.darwinModules.*
    packages/     # Packages → perSystem.packages.*
    overlays/     # Overlays → flake.overlays.*
    lib/          # Library → flake.lib.*
  flake.nix

Naming: kebab-case everywhere (files, attrs, identifiers)
-}
straylight :: Convention
straylight =
  Convention
    { convName = "straylight"
    , convDescription = "Straylight: module layout with kebab-case everywhere"
    , convRules =
        [ ConventionRule
            { ruleKind = FlakeModule
            , rulePattern = Prefix ["nix", "modules", "flake"]
            , ruleForbidden = [Prefix ["nix", "packages"]]
            , ruleExportName = Nothing -- varies
            }
        , ConventionRule
            { ruleKind = NixOSModule
            , rulePattern = Prefix ["nix", "modules", "nixos"]
            , ruleForbidden = [Prefix ["nix", "packages"]]
            , ruleExportName = Just "flake.nixosModules"
            }
        , ConventionRule
            { ruleKind = HomeModule
            , rulePattern =
                AnyOf
                  [ Prefix ["nix", "modules", "home"]
                  , Prefix ["nix", "modules", "home-manager"]
                  ]
            , ruleForbidden = [Prefix ["nix", "packages"]]
            , ruleExportName = Just "flake.homeModules"
            }
        , ConventionRule
            { ruleKind = DarwinModule
            , rulePattern = Prefix ["nix", "modules", "darwin"]
            , ruleForbidden = []
            , ruleExportName = Just "flake.darwinModules"
            }
        , ConventionRule
            { ruleKind = Package
            , rulePattern = Prefix ["nix", "packages"]
            , ruleForbidden = [Prefix ["nix", "modules"]]
            , ruleExportName = Just "perSystem.packages"
            }
        , ConventionRule
            { ruleKind = Overlay
            , rulePattern = Prefix ["nix", "overlays"]
            , ruleForbidden = []
            , ruleExportName = Just "flake.overlays"
            }
        , ConventionRule
            { ruleKind = Library
            , rulePattern = Prefix ["nix", "lib"]
            , ruleForbidden = []
            , ruleExportName = Just "flake.lib"
            }
        , ConventionRule
            { ruleKind = Shell
            , rulePattern = Prefix ["nix", "shells"]
            , ruleForbidden = []
            , ruleExportName = Just "perSystem.devShells"
            }
        , ConventionRule
            { ruleKind = Flake
            , rulePattern = Exact ["flake.nix"]
            , ruleForbidden = []
            , ruleExportName = Nothing
            }
        ]
    , convFileNaming = KebabCase
    , convAttrNaming = KebabCase
    , convIdentNaming = KebabCase
    , convRequireFlakeMod = False
    }

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                              // other conventions
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | the nixpkgs @pkgs/by-name@ layout (camelCase attrs, packages under by-name).
nixpkgsByName :: Convention
nixpkgsByName =
  Convention
    { convName = "nixpkgs-by-name"
    , convDescription = "Nixpkgs pkgs/by-name layout"
    , convRules =
        [ ConventionRule
            { ruleKind = Package
            , rulePattern = Prefix ["pkgs", "by-name"]
            , ruleForbidden = []
            , ruleExportName = Nothing
            }
        ]
    , convFileNaming = NoNaming
    , convAttrNaming = CamelCase -- nixpkgs uses camelCase
    , convIdentNaming = CamelCase
    , convRequireFlakeMod = False
    }

-- | the standard flake-parts layout (modules/, packages/, overlays/; no naming policy).
flakeParts :: Convention
flakeParts =
  Convention
    { convName = "flake-parts"
    , convDescription = "Standard flake-parts layout"
    , convRules =
        [ ConventionRule
            { ruleKind = FlakeModule
            , rulePattern = AnyOf [Prefix ["modules"], Prefix ["flake-modules"]]
            , ruleForbidden = []
            , ruleExportName = Nothing
            }
        , ConventionRule
            { ruleKind = NixOSModule
            , rulePattern = AnyOf [Prefix ["modules", "nixos"], Prefix ["nixos-modules"]]
            , ruleForbidden = []
            , ruleExportName = Just "flake.nixosModules"
            }
        , ConventionRule
            { ruleKind = Package
            , rulePattern = Prefix ["packages"]
            , ruleForbidden = []
            , ruleExportName = Just "perSystem.packages"
            }
        , ConventionRule
            { ruleKind = Overlay
            , rulePattern = Prefix ["overlays"]
            , ruleForbidden = []
            , ruleExportName = Just "flake.overlays"
            }
        ]
    , convFileNaming = NoNaming
    , convAttrNaming = NoNaming
    , convIdentNaming = NoNaming
    , convRequireFlakeMod = False
    }

-- | a NixOS system-configuration layout (modules/ or hosts/, users/ or home/).
nixosConfig :: Convention
nixosConfig =
  Convention
    { convName = "nixos-config"
    , convDescription = "NixOS system configuration layout"
    , convRules =
        [ ConventionRule
            { ruleKind = NixOSModule
            , rulePattern = AnyOf [Prefix ["modules"], Prefix ["hosts"]]
            , ruleForbidden = []
            , ruleExportName = Nothing
            }
        , ConventionRule
            { ruleKind = HomeModule
            , rulePattern = AnyOf [Prefix ["users"], Prefix ["home"]]
            , ruleForbidden = []
            , ruleExportName = Nothing
            }
        ]
    , convFileNaming = NoNaming
    , convAttrNaming = NoNaming
    , convIdentNaming = NoNaming
    , convRequireFlakeMod = False
    }

{- | The all-flake-module convention (modeled on github:nixified-ai/flake):
every .nix under flake-modules/ is a flake-parts module wiring its children
via `imports`, with leaf package.nix derivations. Requires every recognized
file to be a flake module or a package (convRequireFlakeMod).
-}
allFlakeModule :: Convention
allFlakeModule =
  Convention
    { convName = "all-flake-module"
    , convDescription = "Every .nix is a flake-parts module under flake-modules/ (nixified-ai)"
    , convRules =
        [ ConventionRule
            { ruleKind = FlakeModule
            , rulePattern = Prefix ["flake-modules"]
            , ruleForbidden = []
            , ruleExportName = Nothing
            }
        , ConventionRule
            { ruleKind = Package
            , rulePattern = Prefix ["flake-modules"]
            , ruleForbidden = []
            , ruleExportName = Nothing
            }
        ]
    , convFileNaming = NoNaming
    , convAttrNaming = NoNaming
    , convIdentNaming = NoNaming
    , convRequireFlakeMod = True
    }

-- | Look up a convention by name. Defaults to 'straylight' if unrecognised.
layoutFromName :: Text -> Convention
layoutFromName "straylight" = straylight
layoutFromName "nixpkgs-by-name" = nixpkgsByName
layoutFromName "flake-parts" = flakeParts
layoutFromName "nixos-config" = nixosConfig
layoutFromName "all-flake-module" = allFlakeModule
layoutFromName _ = straylight
