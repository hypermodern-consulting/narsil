-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                         // render
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "The sky above the port was the color of television."
--
--                                                                                     — Neuromancer
--
-- Renders a Config to a list of rule configurations for ast-grep.
--

let Types = ./types.dhall

let Profiles = ./profiles.dhall

let Severity = Types.Severity

let RuleOverride = Types.RuleOverride

let Config = Types.Config

-- Severity to ast-grep severity string
let severity-to-string =
      λ(s : Severity) →
        merge
          { Error = "error"
          , Warning = "warning"
          , Info = "hint"
          , Off = "off"
          }
          s

-- Check if a rule is enabled (not Off)
let is-enabled =
      λ(s : Severity) →
        merge
          { Error = True
          , Warning = True
          , Info = True
          , Off = False
          }
          s

-- Rendered rule for ast-grep
let RenderedRule =
      { id : Text
      , severity : Text
      , enabled : Bool
      }

-- Render a single override
let render-override =
      λ(o : RuleOverride) →
        { id = o.id
        , severity = severity-to-string o.severity
        , enabled = is-enabled o.severity
        }

-- Get profile by name
let get-profile =
      λ(name : Text) →
        if    name == "strict"
        then  Some Profiles.strict
        else  if name == "standard"
        then  Some Profiles.standard
        else  if name == "minimal"
        then  Some Profiles.minimal
        else  if name == "nixpkgs"
        then  Some Profiles.nixpkgs
        else  if name == "security"
        then  Some Profiles.security
        else  if name == "off"
        then  Some Profiles.off
        else  None Types.Profile

-- Render a config to a list of rule configurations
-- n.b. this is a simplified version - full implementation would merge
-- profile rules with overrides
let render-config =
      λ(config : Config) →
        let profile = get-profile config.profile

        let profile-rules =
              merge
                { Some = λ(p : Types.Profile) → p.rules
                , None = [] : List RuleOverride
                }
                profile

        let all-rules = profile-rules # config.overrides

        in  List/map RuleOverride RenderedRule render-override all-rules

in  { severity-to-string
    , is-enabled
    , RenderedRule
    , render-override
    , get-profile
    , render-config
    }
