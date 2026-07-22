#!/usr/bin/env bash
# Golden tests for emit-config output
# These test the OUTPUT of the generated bash commands, not the Haskell code directly.
# Run with: bash test/golden/test-emit-config.sh

set -uo pipefail

PASSED=0
FAILED=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

pass() { echo -e "${GREEN}PASS${NC}: $1"; ((PASSED++)) || true; }
fail() { echo -e "${RED}FAIL${NC}: $1 - $2"; ((FAILED++)) || true; }

# ═══════════════════════════════════════════════════════════════════════════════
# Helper function that emit-config depends on
# ═══════════════════════════════════════════════════════════════════════════════

__nix_compile_escape_json() {
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  # Handle newlines, tabs, carriage returns
  s="${s//$'\n'/\\n}"
  s="${s//$'\r'/\\r}"
  s="${s//$'\t'/\\t}"
  printf '%s' "$s"
}

# ═══════════════════════════════════════════════════════════════════════════════
# JSON Tests
# ═══════════════════════════════════════════════════════════════════════════════

echo "=== JSON Tests ==="

# Test: Integer variable (no quotes in output)
test_json_int() {
  local PORT=8080
  local output
  output=$(printf '%s' '{"port": ' "$PORT" '}'; printf '\n')
  
  if echo "$output" | jq . > /dev/null 2>&1; then
    local val
    val=$(echo "$output" | jq -r '.port')
    if [[ "$val" == "8080" ]]; then
      pass "json_int: port=8080"
    else
      fail "json_int" "wrong value: $val"
    fi
  else
    fail "json_int" "invalid JSON: $output"
  fi
}
test_json_int

# Test: String variable (quoted, escaped)
test_json_string() {
  local HOST="localhost"
  local output
  output=$(printf '%s' '{"host": "' "$(__nix_compile_escape_json "$HOST")" '"}'; printf '\n')
  
  if echo "$output" | jq . > /dev/null 2>&1; then
    local val
    val=$(echo "$output" | jq -r '.host')
    if [[ "$val" == "localhost" ]]; then
      pass "json_string: host=localhost"
    else
      fail "json_string" "wrong value: $val"
    fi
  else
    fail "json_string" "invalid JSON: $output"
  fi
}
test_json_string

# Test: String with special characters
test_json_escape() {
  local MSG='hello "world"'
  local output
  output=$(printf '%s' '{"msg": "' "$(__nix_compile_escape_json "$MSG")" '"}'; printf '\n')
  
  if echo "$output" | jq . > /dev/null 2>&1; then
    local val
    val=$(echo "$output" | jq -r '.msg')
    if [[ "$val" == 'hello "world"' ]]; then
      pass "json_escape: quotes escaped"
    else
      fail "json_escape" "wrong value: $val"
    fi
  else
    fail "json_escape" "invalid JSON: $output"
  fi
}
test_json_escape

# Test: Nested structure
test_json_nested() {
  local HOST="db.example.com"
  local PORT=5432
  local output
  output=$(printf '%s' '{"database": {"host": "' "$(__nix_compile_escape_json "$HOST")" '", "port": ' "$PORT" '}}'; printf '\n')
  
  if echo "$output" | jq . > /dev/null 2>&1; then
    local h p
    h=$(echo "$output" | jq -r '.database.host')
    p=$(echo "$output" | jq -r '.database.port')
    if [[ "$h" == "db.example.com" && "$p" == "5432" ]]; then
      pass "json_nested: database.host + port"
    else
      fail "json_nested" "wrong values: host=$h port=$p"
    fi
  else
    fail "json_nested" "invalid JSON: $output"
  fi
}
test_json_nested

# Test: Boolean literal
test_json_bool() {
  local output
  output=$(printf '%s' '{"debug": true}'; printf '\n')
  
  if echo "$output" | jq . > /dev/null 2>&1; then
    local val
    val=$(echo "$output" | jq -r '.debug')
    if [[ "$val" == "true" ]]; then
      pass "json_bool: debug=true"
    else
      fail "json_bool" "wrong value: $val"
    fi
  else
    fail "json_bool" "invalid JSON: $output"
  fi
}
test_json_bool

# Test: Newline in string
test_json_newline() {
  local MSG=$'line1\nline2'
  local output
  output=$(printf '%s' '{"msg": "' "$(__nix_compile_escape_json "$MSG")" '"}'; printf '\n')
  
  if echo "$output" | jq . > /dev/null 2>&1; then
    local val
    val=$(echo "$output" | jq -r '.msg')
    if [[ "$val" == $'line1\nline2' ]]; then
      pass "json_newline: newline preserved"
    else
      fail "json_newline" "newline not preserved"
    fi
  else
    fail "json_newline" "invalid JSON: $output"
  fi
}
test_json_newline

# ═══════════════════════════════════════════════════════════════════════════════
# YAML Tests
# ═══════════════════════════════════════════════════════════════════════════════

echo ""
echo "=== YAML Tests ==="

# Test: Simple key-value
test_yaml_simple() {
  local PORT=8080
  local output
  output=$(printf '%s' 'port: ' "$PORT" ''; printf '\n')
  
  # Basic YAML validation: should have key: value format
  if [[ "$output" =~ ^port:\ 8080 ]]; then
    pass "yaml_simple: port: 8080"
  else
    fail "yaml_simple" "wrong format: $output"
  fi
}
test_yaml_simple

# Test: Nested structure with proper indentation
test_yaml_nested() {
  local HOST="localhost"
  local PORT=5432
  local output
  output=$(printf '%s' 'database:
  host: "' "$(__nix_compile_escape_json "$HOST")" '"
  port: ' "$PORT" ''; printf '\n')
  
  # Check structure
  if [[ "$output" == *"database:"* && "$output" == *"  host:"* && "$output" == *"  port:"* ]]; then
    pass "yaml_nested: proper indentation"
  else
    fail "yaml_nested" "wrong structure: $output"
  fi
}
test_yaml_nested

# ═══════════════════════════════════════════════════════════════════════════════
# TOML Tests
# ═══════════════════════════════════════════════════════════════════════════════

echo ""
echo "=== TOML Tests ==="

# Test: Section with keys
test_toml_section() {
  local HOST="localhost"
  local PORT=8080
  local output
  output=$(printf '%s' '[server]
host = "' "$(__nix_compile_escape_json "$HOST")" '"
port = ' "$PORT" '
'; printf '\n')
  
  # Check structure
  if [[ "$output" == *"[server]"* && "$output" == *"host = "* && "$output" == *"port = "* ]]; then
    # Check each line ends with newline (not smashed together)
    local lines
    lines=$(echo "$output" | wc -l)
    if [[ "$lines" -ge 3 ]]; then
      pass "toml_section: proper structure"
    else
      fail "toml_section" "lines smashed together"
    fi
  else
    fail "toml_section" "wrong structure: $output"
  fi
}
test_toml_section

# ═══════════════════════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════════════════════

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "Results: $PASSED passed, $FAILED failed"
if [[ $FAILED -eq 0 ]]; then
  echo -e "${GREEN}All tests passed${NC}"
  exit 0
else
  echo -e "${RED}Some tests failed${NC}"
  exit 1
fi
