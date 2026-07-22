# narsil Specification v1.0

## Overview

narsil is a **compile-time static analyzer** for Nix expressions and embedded bash scripts. It provides:

1. **Type inference** for Nix expressions (Hindley-Milner with row polymorphism)
2. **Schema extraction** from bash scripts (environment variables, config structure)
3. **Policy enforcement** (banned constructs, store path requirements)
4. **Scope analysis** for tooling integration

This specification defines the **exact behavior** that implementations MUST exhibit. Property tests verify conformance.

---

## 1. Type System

### 1.1 Bash Types

```
BashType ::= TInt          -- Integer literals: -42, 0, 8080
           | TString       -- String values
           | TBool         -- true | false
           | TPath         -- Nix store paths: /nix/store/...
           | TNumeric      -- Supertype of TInt ∪ TBool (unquoted config values)
           | TVar α        -- Unification variable
```

**Subtyping:**
- `TInt <: TNumeric`
- `TBool <: TNumeric`
- No other subtyping relationships exist.

**Unification Rules:**

```
─────────────── (Refl)
  T ~ T ⊢ ∅

─────────────── (VarL)
  α ~ T ⊢ [α ↦ T]   where α ∉ FV(T)

─────────────── (VarR)
  T ~ α ⊢ [α ↦ T]   where α ∉ FV(T)

  TInt ~ TNumeric ⊢ ∅
  TBool ~ TNumeric ⊢ ∅
  TNumeric ~ TInt ⊢ ∅
  TNumeric ~ TBool ⊢ ∅
```

**Occurs Check:** If `α ∈ FV(T)` and `T ≠ α`, unification MUST fail with an error. Implementations MUST NOT silently allow infinite types.

### 1.2 Nix Types

```
NixType ::= TVar α                    -- Unification variable
          | TInt                      -- Integers
          | TFloat                    -- Floats
          | TBool                     -- Booleans
          | TString                   -- Strings
          | TPath                     -- Paths
          | TNull                     -- null
          | TList NixType             -- Homogeneous lists
          | TAttrs (Map Name (NixType, Optional))  -- Closed attribute sets
          | TAttrsOpen (Map Name (NixType, Optional))  -- Open attribute sets (row polymorphism)
          | TFun NixType NixType      -- Functions
          | TDerivation               -- Derivations
          | TUnion [NixType]          -- Union types
          | TAny                      -- Top type (escape hatch)
```

**Row Polymorphism:**
- `TAttrs` is a **closed** row: exactly the specified fields, no more.
- `TAttrsOpen` is an **open** row: at least the specified fields, possibly more.
- Unification: `TAttrs` with `TAttrsOpen` succeeds iff all required fields in `TAttrsOpen` exist in `TAttrs`.

### 1.3 Type Inference Invariants

**INV-1 (Determinism):** Given identical input, type inference MUST produce identical output. No randomness, no system-dependent behavior.

**INV-2 (Soundness):** If inference succeeds with type T, evaluation MUST NOT produce a type error (assuming correct runtime behavior). Note: We cannot guarantee termination.

**INV-3 (Principality):** Inferred types MUST be principal (most general). If `e : T` is inferred, then for any `T'` such that `e : T'`, there exists substitution `σ` where `σ(T) = T'`.

**INV-4 (Substitution Composition):** `apply (compose s1 s2) t = apply s1 (apply s2 t)`

**INV-5 (Constraint Satisfaction):** If `solve(C) = σ`, then for all `(T1 ~ T2) ∈ C`: `apply σ T1 = apply σ T2` (modulo TNumeric compatibility).

---

## 2. Bash Analysis

### 2.1 Parameter Expansion Recognition

The analyzer MUST recognize these bash parameter expansion forms:

| Pattern | Semantics | Extracted Fact |
|---------|-----------|----------------|
| `${VAR:-default}` | Use default if VAR unset or empty | `DefaultIs VAR (parse default)` |
| `${VAR-default}` | Use default if VAR unset (not if empty) | `DefaultIs VAR (parse default)` |
| `${VAR:=default}` | Assign default if VAR unset or empty | `DefaultIs VAR (parse default)` |
| `${VAR=default}` | Assign default if VAR unset | `DefaultIs VAR (parse default)` |
| `${VAR:?message}` | Error if VAR unset or empty | `Required VAR` |
| `${VAR?message}` | Error if VAR unset | `Required VAR` |
| `${VAR:+alt}` | Use alt if VAR is set and non-empty | (no type constraint) |
| `${VAR+alt}` | Use alt if VAR is set | (no type constraint) |
| `${VAR}` | Simple reference | `SimpleRef VAR` |
| `$VAR` | Simple reference | `SimpleRef VAR` |
| `${VAR:-}` | Default to empty string | `DefaultIs VAR (LitString "")` |
| `${VAR-}` | Default to empty string if unset | `DefaultIs VAR (LitString "")` |

**CRITICAL:** `${VAR:-}` (empty default) is NOT the same as `${VAR:?}` (required). Empty default means "default to empty string", NOT "variable is required".

### 2.2 Literal Parsing

```
parseLiteral : Text → Literal

parseLiteral "true"  = LitBool True
parseLiteral "false" = LitBool False
parseLiteral t | isInteger t = LitInt (parseInteger t)
parseLiteral t | isStorePath t = LitPath t
parseLiteral t = LitString t

isInteger : Text → Bool
isInteger t = t matches regex: ^-?[0-9]+$
            ∧ t ≠ "-"
            ∧ |t| ≤ 19  -- Fits in Int64

isStorePath : Text → Bool
isStorePath t = "/nix/store/" `isPrefixOf` t
              ∧ ¬ (".." `isInfixOf` t)  -- No path traversal
```

**Integer Bounds:** Integers MUST fit in a signed 64-bit integer. Values outside `[-2^63, 2^63-1]` MUST be treated as strings.

**Path Traversal:** Store paths containing `..` MUST NOT be recognized as valid store paths.

### 2.3 Config Assignment

Config assignments have two syntaxes:

```bash
config.path.to.key=$VAR       # Dot syntax
config.path.to.key="$VAR"     # Dot syntax, quoted
config[path.to.key]=$VAR      # Array syntax
config[path.to.key]="$VAR"    # Array syntax, quoted
config.path.to.key=literal    # Literal value
```

**Quoting Semantics:**
- Unquoted variable: Type inferred from variable's definition
- Quoted variable: Always TString (bash semantics: quoting stringifies)
- Unquoted literal: Parse as int/bool/string
- Quoted literal: Always TString

**Path Parsing:**
- Empty path segments are INVALID: `config..a` MUST error or be ignored
- Empty path is INVALID: `config.=$VAR` MUST error or be ignored

### 2.4 Command Analysis

Commands are categorized as:

1. **Store Path Commands:** Start with `/nix/store/` (no `..`)
2. **Dynamic Commands:** Start with `$` (variable as command)
3. **Shell Builtins:** Exactly these, no more:
   ```
   if then else elif fi case esac for while until do done
   function return break continue set unset export declare
   local readonly typeset let source . cd pwd pushd popd dirs
   echo printf read exit exec trap wait kill true false :
   test [ bg fg jobs disown alias unalias builtin command
   type hash help enable shopt bind complete compgen getopts
   shift times ulimit umask history fc eval
   ```
4. **Bare Commands:** Everything else → POLICY VIOLATION

**Note:** `eval` is a builtin but is ALSO a forbidden construct (see §3).

### 2.5 Argument Type Inference

When a command is invoked with flag-value pairs:
```bash
curl --connect-timeout "$TIMEOUT"
```

The analyzer MAY infer `TIMEOUT : TInt` if the command/flag is in the builtins database.

**Builtins Database Contract:**
- Entries MUST be conservative (only flags with unambiguous types)
- Unknown commands/flags produce NO constraints (not errors)
- Flag matching is exact (no prefix matching)

---

## 3. Forbidden Constructs

These constructs are **unconditionally banned**. No escape hatch. No configuration.

### 3.1 Bash Forbidden

| Construct | Reason | Error Code |
|-----------|--------|------------|
| Heredocs (`<<`, `<<-`) | Unparseable interpolation | NARSIL-B001 |
| Here-strings (`<<<`) | Unparseable interpolation | NARSIL-B002 |
| `eval` | Dynamic code execution | NARSIL-B003 |
| Backticks (`` `cmd` ``) | Deprecated, use `$(cmd)` | NARSIL-B004 |
| Bare commands | Must use store paths | NARSIL-B005 |
| Dynamic commands (`$cmd`) | Cannot analyze statically | NARSIL-B006 |

### 3.2 Nix Forbidden

| Construct | Reason | Error Code |
|-----------|--------|------------|
| `with expr;` | Obscures scope, breaks tooling | NARSIL-N001 |
| `rec { }` | Enables non-termination | NARSIL-N002 |

### 3.3 Error Reporting

Errors MUST include:
- Error code (e.g., NARSIL-B001)
- Human-readable message
- Source location
  - Nix diagnostics: file, line, column (from hnix spans)
  - Bash diagnostics: file, line, column (from ShellCheck `prTokenPositions` via `Position { posLine, posColumn, posFile }`). Falls back to line 0 col 0 when position data is unavailable.
  - Suggested fix (where applicable)

---

## 4. Schema Output

### 4.1 JSON Schema Format

```json
{
  "env": {
    "VAR_NAME": {
      "type": "TInt" | "TString" | "TBool" | "TPath" | "TNumeric",
      "required": boolean,
      "default": <literal> | null,
      "span": { "start": {"line": int, "col": int}, "end": {...} }
    }
  },
  "config": {
    "path.to.key": {
      "type": "...",
      "from": "VAR_NAME" | null,
      "literal": <value> | null,
      "quoted": "Quoted" | "Unquoted" | null,
      "span": {...}
    }
  },
  "commands": [...],
  "storePaths": [...],
  "bareCommands": [...],
  "dynamicCommands": [...]
}
```

### 4.2 emit-config Output

The generated `emit-config` function MUST:

1. **NOT use heredocs** (would violate own policy)
2. **Escape string values** for target format (JSON/YAML/TOML)
3. **Preserve literal types:** integers as integers, bools as bools
4. **Handle missing variables gracefully** (empty string, not crash)

---

## 5. Security Requirements

### 5.1 Input Validation

**SEC-1:** Variable names in generated code MUST be validated:
- Match regex `^[A-Za-z_][A-Za-z0-9_]*$`
- No shell metacharacters

**SEC-2:** File paths MUST be validated:
- No null bytes
- Resolved paths must not escape project root (for relative imports)

**SEC-3:** Integer parsing MUST NOT:
- Crash on overflow
- Consume unbounded memory
- Take unbounded time

### 5.2 Output Safety

**SEC-4:** Generated shell code MUST be injection-safe:
- All variable references properly quoted
- No unescaped user content in string literals

**SEC-5:** JSON/YAML/TOML output MUST be valid:
- Proper escaping of special characters
- No truncation on large values

---

## 6. Concurrency & Determinism

### 6.1 Pure Functions

These functions MUST be pure (no IO, deterministic):
- `parseScript`
- `parseBash`
- `parseParamExpansion`
- `parseLiteral`
- `parseConfigAssignment`
- `factsToConstraints`
- `solve`
- `buildSchema`
- `unify`
- All type inference functions

### 6.2 IO Functions

These functions perform IO:
- `parseScriptFile` (reads file)
- `parseNixFile` (reads file)
- `buildModuleGraph` (reads multiple files)

**Requirement:** IO functions MUST handle:
- File not found → Left with error message
- Permission denied → Left with error message
- File modified during read → Unspecified (best effort)

---

## 7. Property Test Requirements

### 7.1 Algebraic Properties

**PROP-1 (Unify Reflexive):** `∀T. unify T T = Right ∅`

**PROP-2 (Unify Symmetric):** `∀T1 T2. isRight(unify T1 T2) ⟺ isRight(unify T2 T1)`

**PROP-3 (Subst Identity):** `∀T. apply ∅ T = T`

**PROP-4 (Subst Composition):** `∀s1 s2 T. apply (compose s1 s2) T = apply s1 (apply s2 T)`

**PROP-5 (Solve Satisfies):** `∀C. solve C = Right σ ⟹ ∀(T1~T2)∈C. compatible(apply σ T1, apply σ T2)`

**PROP-6 (Inference Determinism):** `∀e. infer e = infer e` (same input → same output)

### 7.2 Roundtrip Properties

**PROP-7 (Literal Roundtrip):** `∀lit. parseLiteral (renderLiteral lit) = lit`

**PROP-8 (Config Roundtrip):** `∀cfg. parseConfigAssignment (renderConfig cfg) = Just cfg`

### 7.3 Adversarial Properties

**PROP-9 (No Crash on Arbitrary Input):** `∀bytes. parseBash bytes ∈ {Left _, Right _}` (never ⊥)

**PROP-10 (Bounded Memory):** `∀input. |input| ≤ 1MB ⟹ parse completes in < 1GB memory`

**PROP-11 (Bounded Time):** `∀input. |input| ≤ 1MB ⟹ parse completes in < 10s`

### 7.4 Security Properties

**PROP-12 (No Injection):** `∀varName ∈ ValidVarNames, value ∈ Text. emitConfig(...) produces valid shell`

**PROP-13 (Path Traversal Blocked):** `∀path with "..". ¬isStorePath(path)`

**PROP-14 (Integer Safety):** `∀t. parseLiteral t` never throws, never hangs

---

## 8. Test Vectors

### 8.1 Parameter Expansion

| Input | Expected |
|-------|----------|
| `${VAR:-default}` | `DefaultValue "VAR" (Just "default")` |
| `${VAR-default}` | `DefaultValue "VAR" (Just "default")` |
| `${VAR:-}` | `DefaultValue "VAR" (Just "")` |
| `${VAR-}` | `DefaultValue "VAR" (Just "")` |
| `${VAR:?}` | `ErrorIfUnset "VAR" Nothing` |
| `${VAR:?msg}` | `ErrorIfUnset "VAR" (Just "msg")` |
| `${VAR}` | `SimpleRef "VAR"` |
| `$VAR` | `SimpleRef "VAR"` |
| `${VAR:+alt}` | `UseAlternate "VAR" (Just "alt")` |
| `${VAR+alt}` | `UseAlternate "VAR" (Just "alt")` |
| `${X-}` | `DefaultValue "X" (Just "")` |
| `${_A1-x}` | `DefaultValue "_A1" (Just "x")` |

### 8.2 Literal Parsing

| Input | Expected |
|-------|----------|
| `"true"` | `LitBool True` |
| `"false"` | `LitBool False` |
| `"0"` | `LitInt 0` |
| `"-1"` | `LitInt (-1)` |
| `"42"` | `LitInt 42` |
| `"-"` | `LitString "-"` |
| `"--1"` | `LitString "--1"` |
| `"1-1"` | `LitString "1-1"` |
| `"/nix/store/abc-pkg"` | `LitPath "/nix/store/abc-pkg"` |
| `"/nix/store/../etc"` | `LitString "/nix/store/../etc"` |
| `"9223372036854775808"` | `LitString "9223372036854775808"` (overflow) |
| `""` | `LitString ""` |

### 8.3 Injection Vectors (MUST NOT cause injection)

```
VAR_NAME="foo; rm -rf /"
VAR_NAME='`whoami`'
VAR_NAME=$'\\x00'
VAR_NAME="$(cat /etc/passwd)"
```

### 8.4 Malformed Input (MUST NOT crash)

```
${
${}
${VAR:-
${VAR
${{{{
${VAR:-${NESTED}}
config.=
config..a=1
config.a.b=
```

---

## Appendix A: ABNF Grammar

```abnf
param-expansion = "${" var-name [ param-op ] "}"
                / "$" var-name

param-op = ":-" value
         / "-" value
         / ":=" value
         / "=" value
         / ":?" value
         / "?" value
         / ":+" value
         / "+" value

var-name = (ALPHA / "_") *(ALPHA / DIGIT / "_")

value = *(%x20-7E)  ; printable ASCII, may be empty

config-assign = "config" config-path "=" config-value

config-path = "." 1*path-segment
            / "[" path-key "]"

path-segment = 1*(ALPHA / DIGIT / "_" / "-")

path-key = 1*(ALPHA / DIGIT / "_" / "-" / ".")

config-value = quoted-value / unquoted-value

quoted-value = DQUOTE (var-ref / *CHAR) DQUOTE

unquoted-value = var-ref / literal

var-ref = "$" var-name / "${" var-name "}"

literal = integer / boolean / string

integer = ["-"] 1*DIGIT

boolean = "true" / "false"
```

---

## Appendix B: Changelog

- v1.0 (2025-02-05): Initial specification
