let Severity = < Off | Info | Warning | Error >

let overrideOff = \(id : Text) -> { id, severity = Severity.Off, reason = None Text }

in  { profile = "standard"
    , layout = "flake-parts"
    , extra-ignores = [] : List Text
    , overrides = [ overrideOff "long-inline-string" ]
    , lsp = { max-threads = 4, max-memory-mb = 256, max-disk-mb = 512 }
    }
