# Package Manager

Loon's package manager is built into the `loon` CLI. It uses content-addressed caching, BLAKE3 hashing, and a capability-based security model.

## Quick Start

```bash
loon new my-app             # Create project with pkg.loon
cd my-app
loon add github.com/cam/json --version "^1.0"
loon run src/main.loon
```

## Manifest: `pkg.loon`

Every Loon project has a `pkg.loon` file at its root. It uses Loon's own data format (dogfooding).

```loon
{
  :name "my-app"
  :version "0.1.0"

  :deps {
    "github.com/cam/json" "^1.0"
    "github.com/cam/std#http" {:version "^2.0" :grant #["Net" "IO"]}
    "my-lib" {:path "../my-lib"}
  }
}
```

### Dependency Formats

**Version string shorthand** — pure dependency, version constraint only:

```loon
"github.com/cam/json" "^1.2"
```

**Map form** — full control over source, effects, and pinning:

```loon
"github.com/cam/std#http" {
  :version "^2.0"
  :grant #["Net" "IO"]
  :git "https://github.com/cam/std.git"
  :rev "v2.0.1"
  :hash "abc123..."
}
```

**Path dependency** — for local development:

```loon
"my-lib" {:path "../my-lib"}
```

### Dependency Fields

| Field | Type | Description |
|-------|------|-------------|
| `:version` | String | Version constraint (`^1.0`, `~1.2`, `>=1.0`, `=1.0.0`, `*`) |
| `:grant` | Vec | Effect capabilities to grant (e.g. `#["Net" "IO"]`) |
| `:path` | String | Local path (for development) |
| `:git` | String | Git clone URL (overrides auto-derived URL) |
| `:rev` | String | Git revision/tag to pin to |
| `:url` | String | Direct download URL |
| `:hash` | String | Expected BLAKE3 hash |

### Version Constraints

| Syntax | Meaning |
|--------|---------|
| `^1.2.3` | `>=1.2.3, <2.0.0` (caret — same major) |
| `~1.2.3` | `>=1.2.3, <1.3.0` (tilde — same minor) |
| `>=1.0` | `>=1.0.0` |
| `=1.0.0` | Exactly `1.0.0` |
| `*` | Any version |
| `1.2.3` | Same as `^1.2.3` (bare version = caret) |

## Lockfile: `lock.loon`

After fetching dependencies, `lock.loon` is written to pin exact versions and content hashes. This ensures reproducible builds.

```loon
{
  :version 1
  :packages #[
    {
      :source "github.com/cam/json"
      :version "1.0.0"
      :url "https://github.com/cam/json.git"
      :hash "7f3a2b1c..."
      :deps #[]
    }
  ]
}
```

The lockfile is automatically created/updated by `loon add` and `loon cache warm`. Commit it to version control for reproducible builds.

## Source Resolution

### Domain-Qualified Sources

Sources with a domain (containing `.` before the first `/`) are treated as remote:

```
github.com/cam/json       -> remote (github.com is a domain)
gitlab.com/user/repo      -> remote
http.server               -> local module (no slash = not a domain)
my-lib                    -> local dep or module
```

### Subpaths

A `#` separator indicates a subpackage within a monorepo:

```
github.com/cam/std#http   -> fetch github.com/cam/std, use the http/ subdirectory
```

### URL Derivation

For known hosts, archive and git URLs are derived automatically:

| Host | Git URL | Archive URL |
|------|---------|-------------|
| `github.com` | `https://github.com/user/repo.git` | `https://github.com/user/repo/archive/refs/tags/v{version}.tar.gz` |
| `gitlab.com` | `https://gitlab.com/user/repo.git` | `https://gitlab.com/user/repo/-/archive/v{version}/repo-v{version}.tar.gz` |
| `codeberg.org` | `https://codeberg.org/user/repo.git` | `https://codeberg.org/user/repo/archive/v{version}.tar.gz` |

Override with explicit `:git` or `:url` in the dependency map for other hosts.

### Fetch Strategy

1. If `:url` is specified, download directly
2. If `:git` is specified, clone that URL
3. Otherwise for domain-qualified sources:
   - Try the archive URL first (faster, no git required)
   - Fall back to git clone if archive fails

## Caching

Packages are cached at `~/.loon/cache/blake3/<hash>/` where `<hash>` is the BLAKE3 hash of the package's source tree.

### How Hashing Works

The BLAKE3 hash is computed by:
1. Walking all files in the package (sorted by relative path)
2. Skipping `.git/` and `target/` directories
3. Streaming each file's relative path and contents through the hasher

This means the hash is content-addressed — identical source trees always produce the same hash, regardless of when or where they were fetched.

### Cache Integrity

If a cached package's files are tampered with, re-verification will detect the mismatch. The `:hash` field in `lock.loon` serves as the source of truth.

## Entry Points

When loading a dependency, Loon looks for entry point files in order:

1. `src/lib.loon` (library entry point)
2. `src/main.loon` (executable entry point)

For path dependencies, it also checks `<dep-name>.loon` in the dep root.

## Capability-Based Security

Dependencies are pure by default — they cannot perform side effects unless explicitly granted.

```loon
"github.com/cam/http" {:version "^1.0" :grant #["Net" "IO"]}
```

The `:grant` field lists which effects the dependency is allowed to perform. A dependency without `:grant` (or with an empty grant) can only use pure computation.

Use `loon audit --capabilities` to see what each dependency requires:

```bash
$ loon audit --capabilities
  Dependency Capabilities
  ──────────────────────────────────────────────────
  github.com/cam/json -> pure (no effects)
  github.com/cam/http -> Net, IO
```

## CLI Commands

### Project Setup

```bash
loon new <name>           # Create a new project with pkg.loon
loon init                 # Initialize pkg.loon in current directory
```

### Dependency Management

```bash
loon add <source>                          # Add dep (any version)
loon add <source> --version "^1.0"         # Add with version constraint
loon add <source> --grant "Net,IO"         # Add with effect grants
loon remove <source>                       # Remove a dependency
loon why <source>                          # Show why a dep is included
```

### Cache Management

```bash
loon cache warm           # Fetch all unfetched dependencies
loon cache clean          # Remove all cached packages
```

### Discovery

```bash
loon search <query>       # Search the package index
loon audit --capabilities # Show effect grants for all deps
```

## Using Dependencies

Import a dependency with `[use]`:

```loon
[use github.com/cam/json]                    ; qualified: json.parse, json.stringify
[use github.com/cam/json :as j]             ; aliased: j.parse, j.stringify
[use github.com/cam/json {parse stringify}]  ; selective: parse, stringify
```

The module system resolves dependencies in this order:

1. **Path dependencies** — local `:path` deps from `pkg.loon`
2. **Remote dependencies** — domain-qualified sources, resolved via lockfile/cache/fetch
3. **Local modules** — file-relative `.loon` files (e.g. `http.server` -> `http/server.loon`)

## Package Structure

A typical Loon package:

```
my-package/
  pkg.loon              # Manifest
  lock.loon             # Lockfile (auto-generated)
  src/
    lib.loon            # Library entry point (pub exports)
    main.loon           # Executable entry point (optional)
    utils.loon          # Internal module
```

## Feature Gating

The fetch functionality (git clone, HTTP download, BLAKE3 hashing) is behind the `pkg-fetch` Cargo feature. This keeps the WASM build lean:

- `loon-cli`: enables `pkg-fetch` — full package manager functionality
- `loon-wasm`: no fetch — can resolve cached/path deps only
