-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                          // types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "Vodou isn't like that. It isn't concerned with notions of salvation 
--      and transcendence. What it's about is getting things done."
--
--                                                                                      — Count Zero
--

let Severity = < Error | Warning | Info | Off >

let Language = < Nix | Cpp | Bash | Any >

let RuleOverride =
      { id : Text
      , severity : Severity
      , reason : Optional Text
      }

let Profile =
      { name : Text
      , description : Text
      , parent : Optional Text
      , rules : List RuleOverride
      , ignores : List Text
      , files : Optional (List Text)
      }

let Lsp =
      { max-threads : Natural
      , max-memory-mb : Natural
      , max-disk-mb : Natural
      }

let Config =
      { profile : Text
      , layout : Text
      , extra-ignores : List Text
      , overrides : List RuleOverride
      , lsp : Lsp
      }

in  { Severity, Language, RuleOverride, Profile, Lsp, Config }
