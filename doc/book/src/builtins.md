# Builtins Database

narsil knows the typed flag schemas of 21 common commands. When a known command is invoked with a flag-value pair containing a variable reference, the variable's type is constrained by the flag's type.

For example:

```bash
/nix/store/...-curl/bin/curl --connect-timeout $TIMEOUT
```

The database knows `curl --connect-timeout` takes `TInt`, so `TIMEOUT` is constrained to `TInt`.

Unknown commands and unknown flags produce no constraints (conservative).

## Commands

| Command | Description | Notable flags |
|---------|-------------|---------------|
| `curl` | HTTP client | `--connect-timeout` (TInt), `--max-time` (TInt), `-o` (TPath), `-H` (TString) |
| `jq` | JSON processor | `--indent` (TInt), `-r` (TBool), `-s` (TBool), `-c` (TBool) |
| `grep` | Pattern search | `-m` (TInt), `-A`/`-B`/`-C` (TInt), `-e` (TString), `-f` (TPath) |
| `wget` | HTTP download | `-O` (TPath), `-t` (TInt), `-T` (TInt) |
| `ssh` | Secure shell | `-p` (TInt), `-i` (TPath), `-l` (TString), `-F` (TPath) |
| `scp` | Secure copy | `-P` (TInt), `-i` (TPath), `-l` (TInt) |
| `rsync` | Remote sync | `--timeout` (TInt), `--port` (TInt), `--bwlimit` (TInt) |
| `find` | Find files | `-maxdepth` (TInt), `-name` (TString), `-type` (TString) |
| `head` | Show first lines | `-n` (TInt), `-c` (TInt) |
| `tail` | Show last lines | `-n` (TInt), `-c` (TInt) |
| `sleep` | Delay execution | positional: seconds (TInt) |
| `timeout` | Run with timeout | positional: duration (TInt), `-s` (TString) |
| `xargs` | Build command lines | `-n` (TInt), `-P` (TInt), `-d` (TString) |
| `nc` | Network utility | positional: host (TString), port (TInt) |
| `split` | Split files | `-n` (TInt), `-l` (TInt), `-b` (TString) |
| `dd` | Convert and copy | `count` (TInt), `bs` (TString), `if`/`of` (TPath) |
| `mkdir` | Create directories | `-m` (TString) |
| `chmod` | Change file mode | positional: mode (TString), file (TPath) |
| `chown` | Change file owner | positional: owner (TString), file (TPath) |
| `parallel` | Run in parallel | `-j` (TInt), `--timeout` (TInt), `--retries` (TInt) |
| `nix` | Nix package manager | `--max-jobs` (TInt), `--cores` (TInt) |

## Shell builtins allowlist

These commands are always allowed (not flagged as bare commands):

```
if then else elif fi case esac for while until do done
function return break continue set unset export declare
local readonly typeset let source . cd pwd pushd popd dirs
echo printf read exit exec trap wait kill true false :
test [ bg fg jobs disown alias unalias builtin command
type hash help enable shopt bind complete compgen getopts
shift times ulimit umask history fc
```
