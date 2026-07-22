{ pkgs, ... }:
let
  # Theorem 1: Identity Wrapper Transparency
  # Any function that takes a string and returns a derivation
  # preserves the "script-ness" property if content signatures exist.
  mkIdentity = name: text: pkgs.writeShellScriptBin name text;
in
{
  # Proving Theorem 1: We catch scripts even through identity functions
  wrapper-transparency = mkIdentity "proof-1" ''
    #!/bin/bash
    # This dependency is hidden in a string, but the wrapper is transparent
    # to our content-based detection.
    ''$dependency --flag


  '';
  # Proving Theorem 2: We reject dynamic evaluation structures
  eval-purity = pkgs.writeShellScriptBin "proof-2" ''
    # This structure is rejected structurally
    source <(echo "dynamic code")


  '';
  # Proving Theorem 3: We reject bare strings as commands
  # (enforcing structurally explicit dependencies)
  dependency-explicitness = pkgs.writeShellScriptBin "proof-3" ''
    # "curl" is a string here, not a structural dependency
    cmd="curl"
    ''$cmd https://example.com


  '';
}
