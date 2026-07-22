{
  lib,
  self,
  ...
}:
{
  perSystem =
    {
      self',
      pkgs,
      system,
      ...
    }:
    let
      narsil = self'.packages.narsil or self.packages.${system}.narsil;
    in
    {
      checks = lib.mkIf (narsil != null) {
        # End-to-end layout-enforcement guard. The harness in
        # tools/layoutcheck/check.sh is invoked ONLY here, through
        # `nix flake check` — keeping a single disciplined runner so it can't
        # drift from a parallel manual invocation. It runs the real binary
        # against the committed good/perturbed flake-parts fixture projects and
        # asserts the clean one is layout-clean and the perturbed one fails.
        "narsil:layout-e2e" =
          pkgs.runCommandLocal "narsil-layout-e2e"
            {
              nativeBuildInputs = [
                pkgs.bash
                pkgs.gnugrep
                pkgs.coreutils
              ];
            }
            ''
              bash ${self}/tools/layoutcheck/check.sh ${narsil}/bin/narsil
              touch $out
            '';
        # narsil dogfoods itself: type-check, lint, and layout-check the
        # whole source tree. Uses the repo's own .narsil.dhall (layout =
        # flake-parts, ignores), so it must be run with --config pointing at it
        # (the build CWD is not ${self}). A non-zero exit fails the check.
        "narsil:ci" = pkgs.runCommandLocal "narsil-ci" { } ''
          echo "running narsil check on ${self}"
          ${narsil}/bin/narsil --config ${self}/.narsil.dhall check ${self}
          touch $out
        '';
        "narsil:lint-flake" = pkgs.runCommandLocal "narsil-lint-flake" { } ''
          echo "linting flake.nix and embedded bash"
          ${narsil}/bin/narsil --config ${self}/.narsil.dhall check ${self}/flake.nix
          touch $out
        '';
        # CLI smoke / jank guard. Runs the real binary across a good/bad/empty/
        # missing/dir input matrix and asserts exit codes, error categories, and
        # the stdout/stderr contract — the rough edges dogfooding surfaced
        # (crashes, mislabeled errors, hangs). Same single-runner discipline as
        # layout-e2e: invoked ONLY here so it cannot drift from a manual run.
        "narsil:cli" =
          pkgs.runCommandLocal "narsil-cli"
            {
              nativeBuildInputs = [
                pkgs.bash
                pkgs.gnugrep
                pkgs.coreutils
              ];
            }
            ''
              bash ${self}/tools/clicheck/check.sh ${narsil}/bin/narsil
              touch $out
            '';
        # straylint case-ban gate. Enforces zero `case` / `\case` across the
        # ENTIRE first-party Haskell tree (lib/, app/, straylint/): if a `case`
        # can be written as function-clause equations, guards, or an eliminator
        # (maybe/either/…), it is. The whole codebase is case-free by
        # construction — new files are covered automatically, so the rule can't
        # be regressed by adding a module. See doc/HOUSE_STYLE.md for the law.
        "narsil:case-ban" =
          let
            # every first-party .hs file (straylint takes an explicit file list;
            # it does not recurse directory arguments)
            haskell-sources = builtins.filter (path: lib.hasSuffix ".hs" (toString path)) (
              lib.filesystem.listFilesRecursive (self + "/lib")
              ++ lib.filesystem.listFilesRecursive (self + "/app")
              ++ lib.filesystem.listFilesRecursive (self + "/straylint")
            );
          in
          pkgs.runCommandLocal "narsil-case-ban" { } ''
            ${narsil}/bin/straylint --strict ${lib.concatMapStringsSep " " toString haskell-sources}
            touch $out
          '';
        # 100-column gate. doc/TYPOGRAPHY.md makes 100 the canonical width and
        # HOUSE_STYLE enforces it on code; fourmolu (column-limit: 100) wraps
        # what it can, but leaves operator chains / long application RHS that it
        # won't reflow, so this backstops the rest. Counts DISPLAY columns (box-
        # drawing glyphs are one column each), not bytes, so banners pinned at
        # 100 pass. Covers the WHOLE first-party tree incl. test/ + bench/.
        "narsil:col100" =
          let
            haskell-sources = builtins.filter (path: lib.hasSuffix ".hs" (toString path)) (
              lib.filesystem.listFilesRecursive (self + "/lib")
              ++ lib.filesystem.listFilesRecursive (self + "/app")
              ++ lib.filesystem.listFilesRecursive (self + "/straylint")
              ++ lib.filesystem.listFilesRecursive (self + "/test")
              ++ lib.filesystem.listFilesRecursive (self + "/bench")
            );
          in
          pkgs.runCommandLocal "narsil-col100" { nativeBuildInputs = [ pkgs.python3 ]; } ''
            python3 - ${lib.concatMapStringsSep " " toString haskell-sources} <<'PY'
            import sys
            bad = 0
            for path in sys.argv[1:]:
                with open(path, encoding="utf-8") as handle:
                    for lineNo, line in enumerate(handle, 1):
                        width = len(line.rstrip("\n"))
                        if width > 100:
                            print(f"{path}:{lineNo}: {width} columns")
                            bad += 1
            if bad:
                print(f"\n{bad} line(s) exceed 100 columns")
                sys.exit(1)
            print("col100: all first-party .hs <= 100 columns")
            PY
            touch $out
          '';
      };
      formatter = lib.mkIf (narsil != null) (
        builtins.derivation {
          name = "narsil-fmt";
          builder = "${pkgs.bash}/bin/bash";
          system = system;
          args = [
            "-euc"
            ''
                ${pkgs.coreutils}/bin/install -Dm755 /dev/stdin $out/bin/narsil-fmt << 'ENDOFSCRIPT'
              #!${pkgs.bash}/bin/bash
              set -e
              files=()
              for arg in "$@"; do
                if [ -d "$arg" ]; then
                  while IFS= read -r -d "" f; do
                    case "$f" in */adversarial_output/*|*/test/fixtures/*|*/fmtparity/corpus/*) continue ;; esac
                    files+=("$f")
                  done < <(${pkgs.findutils}/bin/find "$arg" -name '*.nix' -not -path '*/adversarial_output/*' -print0 2>/dev/null || true)
                elif [ -f "$arg" ]; then
                  files+=("$arg")
                fi
              done
              for f in "''${files[@]}"; do
                ${narsil}/bin/narsil fmt "$f" > "$f.tmp"
                ${pkgs.coreutils}/bin/mv "$f.tmp" "$f"
              done
              ENDOFSCRIPT
            ''
          ];
        }
      );
    };
}
