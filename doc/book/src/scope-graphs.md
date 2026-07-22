# Scope Graphs

narsil builds [Visser-style scope graphs](https://doi.org/10.1007/978-3-662-46669-8_9) from Nix expressions. The scope graph is the foundation for IDE tooling: go-to-definition, find-references, rename refactoring.

## Structure

A scope graph consists of:

- **Scopes** -- regions of code where names are visible (file, let, attrset, function, with)
- **Declarations** -- names introduced into a scope (`x = 1` declares `x`)
- **References** -- names used in a scope (`x + 1` references `x`)
- **Edges** -- connections between scopes, labeled with priority

## Edge priority

Edges are labeled and resolved in priority order:

```
Parent > Import > With > Inherit > AttrAccess
```

When resolving a reference, higher-priority edges are tried first. If a `Parent` edge yields a declaration, `With` edges are not consulted. This implements Nix's scoping semantics where lexical bindings shadow `with`-imported names.

## Scope kinds

| Kind | Created by |
|------|-----------|
| `FileScope` | Top-level file scope |
| `LetScope` | `let ... in` |
| `AttrSetScope` | `{ ... }` |
| `RecAttrSetScope` | `rec { ... }` |
| `FunctionScope` | `{ args }: body` |
| `WithScope` | `with expr;` |

## Export formats

### JSON

```bash
narsil scope --json file.nix
```

Outputs the scope graph as JSON with scopes, declarations, references, edges, and source locations.

### Dhall

```bash
narsil scope --dhall file.nix
```

Outputs a Dhall expression matching the schema in `dhall/ScopeGraph.dhall`, for integration with zeitschrift.

## Current status

- Single-file scope graph construction: **working**
- Priority-based resolution: **working** (Parent > Import > With)
- `with` scoping: **working** (separate expression scope and body scope)
- Cross-file analysis: **working** (`fromModuleGraph` merges graphs with ID remapping and import edges)
- `findReferences`: **working** (verifies resolution, not just name matching)
