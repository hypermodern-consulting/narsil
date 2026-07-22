# Run with:
#   NIX_SHOW_STATS=1 nix eval --expr 'import ./measure-bytes-per-char.nix { control = false; size = 10; }' --impure
#   NIX_SHOW_STATS=1 nix eval --expr 'import ./measure-bytes-per-char.nix { control = true; size = 10; }' --impure
{ control, size }:
let
  lib = import ./nixpkgs/lib;
  inherit
    (import ./memoize.nix {
      inherit lib;
    })
    memoizeStr
    ;
  # Create a string of the specified size
  key = lib.concatStrings (lib.genList (i: "a") size);
  # Memoized identity function
  memoId = memoizeStr (x: x);
  # Prime the trie with a minimal query to force its construction
  prime = memoId "";
in
if control then builtins.seq prime key else builtins.seq prime (memoId key)
