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

Cached packages are verified against lockfile hashes at multiple points:

- **On load**: When a dependency is loaded at runtime, the cache directory is re-hashed and compared against the lockfile hash. Mismatches produce an error with remediation instructions.
- **`loon verify`**: Manually re-hash all cached packages and report any mismatches.
- **`loon audit`**: Includes cache integrity as part of its full audit report.

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

Grants are scoped to the direct dependency only — transitive deps do not inherit grants from their parents. Each transitive dep must declare its own needs, and the parent must grant them. Use `loon audit` to detect violations.

Use `loon audit --capabilities` to see what each dependency requires:

```bash
$ loon audit --capabilities
  Dependency Capabilities
  ──────────────────────────────────────────────────
  github.com/cam/json -> pure (no effects)
  github.com/cam/http -> Net, IO
```

Use `loon audit` (without flags) for a full audit including transitive grant checking:

```bash
$ loon audit
  Dependency Audit
  ──────────────────────────────────────────────────

  Capabilities
    github.com/cam/json -> pure (no effects)
    github.com/cam/http -> Net, IO

  Transitive Grants
    ✓ All transitive dependencies have required grants

  Cache Integrity
    ✓ 2/2 packages verified

  Lockfile
    ✓ All dependencies locked
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
loon update                                # Re-fetch and update all deps
loon update <source>                       # Re-fetch a specific dep
loon why <source>                          # Show why a dep is included
```

### Cache Management

```bash
loon cache warm           # Resolve and fetch all deps (including transitive)
loon cache clean          # Remove all cached packages
```

### Security & Verification

```bash
loon audit                # Full audit: capabilities, transitive grants, cache, lockfile
loon audit --capabilities # Show effect grants for all deps
loon verify               # Re-hash all cached packages against lockfile
loon publish              # Validate manifest, hash source tree, create tarball
```

### Discovery

```bash
loon search <query>       # Search the package index (builtin + custom)
```

## Transitive Dependencies

When a dependency has its own `pkg.loon`, its dependencies are resolved recursively. The resolver uses **Minimum Version Selection (MVS)**: when multiple packages depend on the same dep at different version constraints, it picks the minimum version that satisfies all constraints.

```
my-app
  ├── github.com/cam/json ^1.0       → resolved: 1.2.0
  └── github.com/cam/http ^2.0
        └── github.com/cam/json ^1.2  → already satisfied by 1.2.0
```

Version conflicts (e.g. `^1.0` and `^2.0` for the same dep) produce clear error messages.

All transitive deps are recorded in `lock.loon` with their hashes and dependency lists, enabling `loon why` to trace the full dependency chain.

## Package Index

### Built-in Index

Loon ships with a built-in index of official packages (`github.com/loon-lang/*`). Search it with:

```bash
loon search json
# github.com/loon-lang/json v0.1.0 (MIT) — JSON parser and serializer for Loon
```

### Custom Indices

Add custom package indices in `pkg.loon`:

```loon
{
  :name "my-app"
  :version "0.1.0"
  :indices {
    "company" "https://git.internal.co/loon-packages/index.loon"
  }
  :deps {}
}
```

Custom indices are fetched and merged with the built-in index. They use Loon data format:

```loon
{
  :name "company-index"
  :packages #[
    {
      :source "git.internal.co/team/utils"
      :description "Internal utilities"
      :license "Proprietary"
      :keywords #["internal" "utils"]
      :versions #[
        {:version "1.0.0" :hash "abc123..."}
      ]
    }
  ]
}
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

## Publishing

Prepare a package for publishing with `loon publish`:

```bash
$ loon publish

  Package: my-lib v1.0.0
  Entry:   src/lib.loon
  Hash:    7f3a2b1c4d5e6f...
  Size:    2.3 KB (4 files)
  Archive: target/my-lib-1.0.0.tar.gz

  Ready to publish. (Use `loon add` with git repos to share packages)
```

Requirements:
- `pkg.loon` must have `:name` and `:version` fields
- `src/lib.loon` must exist (library entry point)

The command creates a `.tar.gz` archive in `target/`, excluding `.git/`, `target/`, and `lock.loon`. The BLAKE3 hash of the source tree is printed for verification.

## Feature Gating

The fetch functionality (git clone, HTTP download, BLAKE3 hashing) is behind the `pkg-fetch` Cargo feature. This keeps the WASM build lean:

- `loon-cli`: enables `pkg-fetch` — full package manager functionality
- `loon-wasm`: no fetch — can resolve cached/path deps only
