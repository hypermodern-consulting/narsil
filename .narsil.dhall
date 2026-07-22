let Severity = < Off | Info | Warning | Error >

let overrideOff = \(id : Text) -> { id, severity = Severity.Off, reason = None Text }

in  { profile = "strict"
    , layout = "flake-parts"
    , extra-ignores =
      [ "test/fixtures/**"
      , "tools/adversarial_output/**"
      , "tools/fmtparity/**"
      ]
    , overrides = [ overrideOff "long-inline-string" ]
    , lsp = { max-threads = 4, max-memory-mb = 256, max-disk-mb = 512 }
    }
