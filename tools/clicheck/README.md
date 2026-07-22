# CLI smoke / jank guard

Runs `nix-compile` in all its forms (`check`/`infer`/`fmt`/`emit`/`scope`/help/
unknown) across a good/bad/empty/missing/dir input matrix and asserts invariants:

- no crash markers (`INTERNAL ERROR`, `CallStack`, `Prelude.`, `fromJust`, `<<loop>>`);
- no mislabeled or double-prefixed errors (`Parse error: parse error`,
  `Parse error: I/O error`);
- correct exit codes per case;
- missing-file / directory inputs reported as `I/O error:`, not parse errors.

Usage (inside `nix develop`):

```bash
cabal build exe:nix-compile
bash tools/clicheck/check.sh "$(cabal list-bin nix-compile)"
```

Exits non-zero on any regression. Companion to the unit coverage in
`test/Props.hs` (`pretty_strlit_truncated`, `safety_error_categories`).
