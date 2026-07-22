{
  stdenv,
  lib,
  runtimeShell,
}:
let
  inherit (lib.fileset) toSource unions;
in
stdenv.mkDerivation (finalAttrs: {
  name = "hello";
  src = toSource {
    root = ./.;
    fileset = unions [ ./hello.sh ];
  };
  buildPhase = ''
    # Note that Nixpkgs has builder functions for simple packages
    # like this, but this template avoids it to make for a more
    # complete example.
    substitute hello.sh hello --replace '@shell@' ${runtimeShell}
    cat hello
    chmod a+x hello


  '';
  installPhase = ''
    install -D hello ''$out/bin/hello


  '';
})
