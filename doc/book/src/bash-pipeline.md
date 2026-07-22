# Bash Analysis Pipeline

## Type system

Bash environment variables get simple first-order types:

| Type | Values | Example |
|------|--------|---------|
| `TInt` | Integers in Int64 range | `PORT=8080` |
| `TString` | Arbitrary text | `HOST="localhost"` |
| `TBool` | `true` or `false` | `DEBUG=true` |
| `TPath` | Nix store paths | `CURL=/nix/store/...-curl/bin/curl` |
| `TNumeric` | Supertype of `TInt \| TBool` | Unquoted config values |
| `TVar v` | Unresolved variable | Before constraint solving |

No polymorphism. No composite types. The type of a variable is determined by its usage across the entire script.

## Fact extraction

The fact extractor walks the ShellCheck AST and recognizes these patterns:

### Parameter expansions

| Pattern | Fact | Type constraint |
|---------|------|-----------------|
| `VAR="${VAR:-8080}"` | `DefaultIs "VAR" (LitInt 8080)` | `TVar "VAR" :~: TInt` |
| `VAR="${VAR:-hello}"` | `DefaultIs "VAR" (LitString "hello")` | `TVar "VAR" :~: TString` |
| `VAR="${VAR:?}"` | `Required "VAR"` | (none -- type from other usage) |
| `VAR="$OTHER"` | `AssignFrom "VAR" "OTHER"` | `TVar "VAR" :~: TVar "OTHER"` |
| `VAR="${VAR:-$OTHER}"` | `DefaultFrom "VAR" "OTHER"` | `TVar "VAR" :~: TVar "OTHER"` |

### Config assignments

| Pattern | Fact |
|---------|------|
| `config.server.port=$PORT` | `ConfigAssign ["server","port"] "PORT" Unquoted` |
| `config.server.host="$HOST"` | `ConfigAssign ["server","host"] "HOST" Quoted` |
| `config.server.workers=4` | `ConfigLit ["server","workers"] (LitInt 4)` |
| `config[server.port]=$PORT` | `ConfigAssign ["server","port"] "PORT" Unquoted` |

### Commands

| Pattern | Fact |
|---------|------|
| `/nix/store/...-curl/bin/curl -o ...` | `UsesStorePath (StorePath "/nix/store/...")` |
| `wget http://...` | `BareCommand "wget"` |
| `$CMD arg1` | `DynamicCommand "CMD"` |
| `echo hello` | (ignored -- builtin) |

### Command arguments

For commands in the builtins database (curl, jq, grep, etc.), flag-value pairs with variable references produce `CmdArg` facts:

```bash
/nix/store/...-curl/bin/curl --connect-timeout $TIMEOUT -o $OUTPUT $URL
```

Produces: `CmdArg "curl" "--connect-timeout" "TIMEOUT"` with constraint `TVar "TIMEOUT" :~: TInt` (because the builtins database knows `--connect-timeout` takes an integer).

## Constraint solving

Constraints are first-order equality: `Type :~: Type`. The solver is standard Robinson unification with occurs check. `TNumeric` unifies with `TInt` and `TBool` (it's a union type).

After solving, any remaining `TVar` is defaulted to `TString` (DESIGN-2 in the bug tracker -- this is a known conservative choice).

## Schema output

The solved types, facts, and config structure are assembled into a `Schema`:

```json
{
  "env": {
    "PORT": { "type": "TInt", "required": false, "default": 8080 },
    "HOST": { "type": "TString", "required": true }
  },
  "config": {
    "server": {
      "port": { "type": "TInt", "from": "PORT" },
      "host": { "type": "TString", "from": "HOST" }
    }
  },
  "commands": ["curl"],
  "storePaths": ["/nix/store/...-curl-8.5.0/bin/curl"],
  "bareCommands": [],
  "dynamicCommands": []
}
```
