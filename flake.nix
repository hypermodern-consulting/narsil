{
  description = "narsil - Type inference for bash scripts at Nix eval time";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    { flake-parts, treefmt-nix, ... }@inputs:
    flake-parts.lib.mkFlake
      {
        inherit inputs;
      }
      {
        imports = [
          treefmt-nix.flakeModule
          ./flake-modules
        ];

        systems = [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ];

        perSystem =
          {
            config,
            pkgs,
            ...
          }:
          let
            haskell-packages = pkgs.haskellPackages.override {
              overrides = hself: hsuper: {
                cryptonite = pkgs.haskell.lib.dontCheck hsuper.cryptonite;
                hashing = pkgs.haskell.lib.dontCheck hsuper.hashing;
                hnix-store-core = pkgs.haskell.lib.dontCheck hsuper.hnix-store-core;
                hnix-store-remote = pkgs.haskell.lib.dontCheck hsuper.hnix-store-remote;
                hnix = pkgs.haskell.lib.dontCheck hsuper.hnix;
                ShellCheck = hsuper.ShellCheck;
              };
            };

            narsil = haskell-packages.callCabal2nix "narsil" ./. { };
          in
          {
            packages = {
              default = narsil;
              narsil = narsil;
            };

            checks = {
              narsil-test = pkgs.haskell.lib.doCheck narsil;
            };

            treefmt = {
              projectRootFile = "flake.nix";
              programs.fourmolu.enable = true;
              # Keep the deep-vendored nixfmt (vendor/) byte-faithful to upstream
              # so it stays diff-able against the nixfmt release it tracks; we
              # diverge by deliberate edits, not by reflowing every line.
              settings.global.excludes = [ "vendor/**" ];
            };

            devShells.default = pkgs.mkShell {
              name = "narsil-dev";
              inputsFrom = [
                narsil.env
                config.treefmt.build.devShell
              ];
              buildInputs = [
                pkgs.ghc
                pkgs.cabal-install
                # nixpkgs HLS ships only `haskell-language-server-<ghcver>` and
                # `haskell-language-server-wrapper` — no plain `haskell-language-server`.
                # Keep both, and add a plain symlink to the wrapper for clients/users
                # that invoke the unversioned name.
                pkgs.haskell-language-server
                # runCommandLocal (not raw runCommand): trivial local symlink, and
                # it keeps our own flake clean under `narsil check` (ALEPH-N007).
                (pkgs.runCommandLocal "haskell-language-server-plain" { } ''
                  mkdir -p "$out/bin"
                  ln -s ${pkgs.haskell-language-server}/bin/haskell-language-server-wrapper \
                    "$out/bin/haskell-language-server"
                '')
                pkgs.hlint
                pkgs.jq
                pkgs.mdbook
              ];
              shellHook = ''
                echo "narsil development shell"
                echo "  narsil parse <script>   Show facts"
                echo "  narsil infer <script>   Show schema (JSON)"
                echo "  narsil check <script>   Check policies"
                echo "  treefmt                      Format all sources"



              '';
            };

            apps = {
              default = {
                type = "app";
                program = "${narsil}/bin/narsil";
              };
              narsil = {
                type = "app";
                program = "${narsil}/bin/narsil";
              };
              doc = {
                type = "app";
                program = "${pkgs.mdbook}/bin/mdbook";
              };
            };
          };
      };
}
