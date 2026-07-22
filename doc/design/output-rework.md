# Output rework: uniform, friendly, professional CLI

Status: **accepted, in progress**. Author decisions recorded below.

## Goal

Replace today's hand-assembled, inconsistent CLI output with one diagnostic
model, one renderer, a strict stdout/stderr contract, and `katip` used properly —
landing in shippable phases, guarded by `tools/clicheck` + golden tests.

## Current state (the problem)

- **3+ overlapping status vocabularies:** `[OK]`/`[XX]`/`[UNC]` (`CLI/Types.hs`),
  katip's `[ERROR]`/`[WARN]` (`Log.hs`), `━━━ … ━━━` per-file boxes, `═══` CI
  banners, inline `TYPE WARNING:`.
- **4 bespoke diagnostic formatters:** `formatNixViolations`,
  `formatViolationsAt`, `formatPackageViolations`, `formatTypeError`.
- **Leaky streams:** data → stdout via `TIO.putStr` (good), but `Report.hs` /
  `Bash.hs` also emit *diagnostics* via raw `putStrLn` (stdout) while everything
  else logs to stderr.
- **Decorative severity:** violations, type errors, and genuine internal bugs all
  log `ErrorS`; success/progress/summaries all `InfoS`; `WarningS` ad hoc.
- **katip underused:** single `"main"` namespace, no per-phase/file
  `KatipContext`; the custom formatter only maps a severity→prefix string.
- **No modes:** only `-v` (Debug). No quiet, no machine-readable output.

## Principles

1. **stdout is a contract** — only the command's product (formatted/annotated
   source, scope JSON/Dhall, emitted bash, or `--format json` diagnostics).
2. **stderr is the conversation** — all progress/diagnostics/summaries via katip,
   never raw `putStrLn`.
3. **One diagnostic, one renderer** — every checker emits the same `Diagnostic`.
4. **Severity means something** — `Error` fails the run; `Warning` is
   degraded/suppressed/non-fatal; `Info` is progress; `Debug` is internals. Exit
   code derives from whether any `Error` was emitted, centrally — not scattered
   `exitFailure`.
5. **Friendly = boring & consistent** — one clippy-style layout; color on a TTY,
   plain when piped; aligned, deduplicated.

## Author decisions (2026-06-06)

1. **Layout:** rustc/clippy carets-and-gutter is the default to try. Keep the
   renderer **properly factored** (a pure `Diagnostic -> Text`) so the style is
   cheap to change; a compact `--format short` can come later.
2. **JSON schema:** **reuse the existing LSP `Diagnostic`** (one fewer serializer
   /parser, shared test surface). Diverge only if a CLI need can't be expressed in
   it.
3. **Sequencing:** do **`check` completely** — model + renderer + every `check`
   diagnostic + its katip structure + its `--format json` — and ship it as the
   template **before** touching `infer`/`scope`/`emit`/`fmt` messaging.
4. **Process:** this doc is staged and committed **before** implementation begins.

## Design

### `Diagnostic` model

```haskell
data Diagnostic = Diagnostic
  { diagSeverity :: Severity        -- reuse katip's Severity
  , diagCode     :: Maybe Text      -- "NARSIL-N001", "TYPE", "PARSE", …
  , diagSpan     :: Maybe Span      -- file:line:col(+range)
  , diagSummary  :: Text            -- one-line headline
  , diagHelp     :: [Text]          -- "= help: …" lines
  , diagSource   :: Maybe Snippet   -- offending source line + caret
  }
```

All checkers become `… -> [Diagnostic]`; the four formatters collapse into one
`renderDiagnostic`.

### Human layout (clippy idiom)

```
error[NARSIL-N001]: `with` expression is not allowed
  --> flake.nix:90:7
   |
90 |   with pkgs; [ git ];
   |   ^^^^^^^^^
   = help: use `inherit (pkgs) git;` instead
```

Run ends with one summary: `checked 16 files: 14 ok, 2 failed (3 errors, 1 warning)`.

### katip, done right

- Custom scribe/formatter renders `Diagnostic`s (color via `ColorIfTerminal`,
  aligned gutter, bracketed code) + plain `Info` progress lines.
- `KatipContext` namespaces: `check.typecheck` / `check.graph` / `check.bash`,
  per-file context — so `-vv` and JSON get structured `file`/`phase`/`rule`.
- Exit code computed centrally from the diagnostic stream.

### Modes

| Flag | Effect |
|---|---|
| `-q/--quiet` | errors only |
| *(default)* | errors + warnings + summary |
| `-v` | + progress (`Info`) |
| `-vv` | + internals (`Debug`) |
| `--format human\|json` | json = LSP-`Diagnostic`-shaped objects for editors/CI |
| `--color never\|always\|auto` | color control (default auto) |

## Plan — `check` first, completely

**C0 — contract & guardrails (no behavior change).** Audit every
`putStrLn`/`TIO.putStr`; enforce "data→stdout, diagnostics→stderr". Extend
`tools/clicheck` to assert it (success `fmt` ⇒ empty stderr; diagnostics never on
stdout). Add golden-output tests for representative `check` runs so later visual
changes review as diffs.

**C1 — `Diagnostic` + renderer.** Introduce the type + a pure `renderDiagnostic`,
unit-tested against golden strings. No call-site changes.

**C2 — migrate `check`.** Convert nix lint, bash lint, type, layout, package, and
parse paths used by `check` to emit `[Diagnostic]`; route through the one renderer.
Delete the markers + the formatters `check` used. Visible win.

**C3 — katip structure + exit codes for `check`.** Per-phase/file context;
severity-derived exit; `-q`/`-v`/`-vv`.

**C4 — `check --format json`.** JSON scribe emitting LSP-`Diagnostic`-shaped output.

Then, and only then, replicate the model across `infer`/`scope`/`emit`/`fmt`
messaging (their *data* output already obeys the stdout contract).

Every step keeps `narsil-test`, the oracle, and `clicheck` green.

## Non-goals / risks

- Not changing *what* is detected — presentation only (soundness/oracle untouched).
- Golden-output churn → land the renderer (C1) before migrating call sites (C2).
- Color/TTY edge cases → `ColorIfTerminal` + explicit `--color`.
