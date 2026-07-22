# emit-config

narsil generates bash functions that output structured config (JSON/YAML/TOML) from `config.*` assignments. This replaces heredoc templating with statically analyzed, type-safe config generation.

## Before (heredoc templating)

```bash
cat << EOF > config.json
{
  "server": {
    "port": ${PORT},
    "host": "${HOST}"
  }
}
EOF
```

Problems: heredocs are banned (NARSIL-B001), interpolation is opaque to static analysis, no type checking, unset variables produce malformed output.

## After (emit-config)

```bash
config.server.port=$PORT
config.server.host="$HOST"
emit-config json > config.json
```

The `emit-config` function is generated at build time by `narsil emit`. Type safety is enforced: unquoted values become JSON numbers/booleans, quoted values become strings.

## Syntax

Config assignments use dot-separated paths:

```bash
# Unquoted: type inferred from variable usage (int, bool, path)
config.server.port=$PORT

# Quoted: always treated as string
config.server.host="$HOST"

# Associative array syntax (ShellCheck-compliant)
config[server.port]=$PORT

# Literal values
config.server.workers=4
config.server.debug=false
```

## Output formats

```bash
emit-config json    # JSON output
emit-config yaml    # YAML output
emit-config toml    # TOML output
```

## Runtime safety

All variable references use `${VAR:?VAR is required}` guards. If a variable is unset at runtime, the script fails immediately with a clear error message rather than producing malformed output.

## Type rules

| Assignment | Inferred Type | JSON Output |
|-----------|--------------|-------------|
| `config.x=$PORT` (unquoted, PORT has default 8080) | `TInt` | `8080` |
| `config.x="$HOST"` (quoted) | `TString` | `"localhost"` |
| `config.x=true` (literal) | `TBool` | `true` |
| `config.x=false` (literal) | `TBool` | `false` |
| `config.x=42` (literal) | `TInt` | `42` |
| `config.x=/nix/store/...` (store path) | `TPath` | `"/nix/store/..."` |
