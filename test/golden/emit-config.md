# emit-config Golden Tests

Each test specifies:
- Input: Schema (as Haskell value or JSON)
- Expected: Generated bash command
- Validation: Output must parse as valid JSON/YAML/TOML

## Test 1: Simple string variable

**Input Schema:**
```haskell
Schema
  { schemaConfig =
      [ ConfigSpec
          { cfgPath = ["server", "host"]
          , cfgType = TString
          , cfgFrom = Just "HOST"
          , cfgLit = Nothing
          , cfgQuoted = Nothing
          }
      ]
  , schemaEnv = []
  , schemaCommands = []
  }
```

**Expected JSON output:**
```bash
printf '%s' '{"server": {"host": "' "$(__nix_compile_escape_json "$HOST")" '"}}'; printf '\n'
```

**Validation:** With `HOST=localhost`, output must be:
```json
{"server": {"host": "localhost"}}
```

---

## Test 2: Integer variable (no quotes)

**Input Schema:**
```haskell
Schema
  { schemaConfig =
      [ ConfigSpec
          { cfgPath = ["server", "port"]
          , cfgType = TInt
          , cfgFrom = Just "PORT"
          , cfgLit = Nothing
          , cfgQuoted = Nothing
          }
      ]
  , ...
  }
```

**Expected JSON output:**
```bash
printf '%s' '{"server": {"port": ' "$PORT" '}}'; printf '\n'
```

**Validation:** With `PORT=8080`, output must be:
```json
{"server": {"port": 8080}}
```

---

## Test 3: Literal values

**Input Schema:**
```haskell
Schema
  { schemaConfig =
      [ ConfigSpec
          { cfgPath = ["debug"]
          , cfgType = TBool
          , cfgFrom = Nothing
          , cfgLit = Just (LitBool True)
          , cfgQuoted = Nothing
          }
      ]
  , ...
  }
```

**Expected JSON output:**
```bash
printf '%s' '{"debug": true}'; printf '\n'
```

**Validation:** Output must be:
```json
{"debug": true}
```

---

## Test 4: Quoted string forces string type

**Input Schema:**
```haskell
Schema
  { schemaConfig =
      [ ConfigSpec
          { cfgPath = ["value"]
          , cfgType = TInt  -- Would be int, but quoted
          , cfgFrom = Just "NUM"
          , cfgLit = Nothing
          , cfgQuoted = Just Quoted  -- Forces string
          }
      ]
  , ...
  }
```

**Expected JSON output:**
```bash
printf '%s' '{"value": "' "$(__nix_compile_escape_json "$NUM")" '"}'; printf '\n'
```

**Validation:** With `NUM=42`, output must be:
```json
{"value": "42"}
```

---

## Test 5: YAML nested structure

**Input Schema:**
```haskell
Schema
  { schemaConfig =
      [ ConfigSpec { cfgPath = ["database", "host"], cfgType = TString, cfgFrom = Just "DB_HOST", ... }
      , ConfigSpec { cfgPath = ["database", "port"], cfgType = TInt, cfgFrom = Just "DB_PORT", ... }
      ]
  , ...
  }
```

**Expected YAML output:**
```bash
printf '%s' 'database:
  host: "' "$(__nix_compile_escape_json "$DB_HOST")" '"
  port: ' "$DB_PORT" ''; printf '\n'
```

**Validation:** With `DB_HOST=localhost DB_PORT=5432`, output must be:
```yaml
database:
  host: "localhost"
  port: 5432
```

---

## Test 6: TOML sections

**Input Schema:**
```haskell
Schema
  { schemaConfig =
      [ ConfigSpec { cfgPath = ["server", "host"], cfgType = TString, cfgFrom = Just "HOST", ... }
      , ConfigSpec { cfgPath = ["server", "port"], cfgType = TInt, cfgFrom = Just "PORT", ... }
      ]
  , ...
  }
```

**Expected TOML output:**
```bash
printf '%s' '[server]
host = "' "$(__nix_compile_escape_json "$HOST")" '"
port = ' "$PORT" '
'; printf '\n'
```

**Validation:** With `HOST=localhost PORT=8080`, output must be:
```toml
[server]
host = "localhost"
port = 8080
```

---

## Test 7: String escaping

**Input Schema:**
```haskell
Schema
  { schemaConfig =
      [ ConfigSpec
          { cfgPath = ["message"]
          , cfgType = TString
          , cfgFrom = Just "MSG"
          , ...
          }
      ]
  , ...
  }
```

**Validation:** With `MSG='hello "world"'`, JSON output must be:
```json
{"message": "hello \"world\""}
```

The `__nix_compile_escape_json` function must escape:
- `"` → `\"`
- `\` → `\\`
- newline → `\n`
- tab → `\t`
- carriage return → `\r`

---

## Test 8: Empty/missing values

**Input Schema:**
```haskell
Schema
  { schemaConfig =
      [ ConfigSpec
          { cfgPath = ["optional"]
          , cfgType = TString
          , cfgFrom = Nothing
          , cfgLit = Nothing
          , cfgQuoted = Nothing
          }
      ]
  , ...
  }
```

**Expected JSON output:**
```bash
printf '%s' '{"optional": ""}'; printf '\n'
```

---

## Validation Script

```bash
#!/usr/bin/env bash
# Run each test case and validate output

set -euo pipefail

# Helper function that must be defined
__nix_compile_escape_json() {
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  s="${s//$'\n'/\\n}"
  s="${s//$'\r'/\\r}"
  s="${s//$'\t'/\\t}"
  printf '%s' "$s"
}

# Test 2: Integer
PORT=8080
output=$(printf '%s' '{"server": {"port": ' "$PORT" '}}'; printf '\n')
echo "$output" | jq . > /dev/null || { echo "FAIL: Test 2 - invalid JSON"; exit 1; }
echo "PASS: Test 2"

# Test 7: Escaping
MSG='hello "world"'
output=$(printf '%s' '{"message": "' "$(__nix_compile_escape_json "$MSG")" '"}'; printf '\n')
echo "$output" | jq . > /dev/null || { echo "FAIL: Test 7 - invalid JSON"; exit 1; }
expected='{"message": "hello \"world\""}'
[[ "$output" == "$expected" ]] || { echo "FAIL: Test 7 - wrong output: $output"; exit 1; }
echo "PASS: Test 7"

echo "All tests passed"
```
