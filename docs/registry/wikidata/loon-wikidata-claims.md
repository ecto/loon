# Wikidata submission — Loon

Copy-paste claim sheet for creating a new Wikidata item for Loon. Create the
item at <https://www.wikidata.org/wiki/Special:NewItem> and then add each
claim below. Property IDs (P…) are Wikidata's, item IDs (Q…) are the targets.

## Labels

- **Label (en)**: Loon
- **Description (en)**: functional programming language with invisible types, safe ownership, and algebraic effects
- **Aliases (en)**: loon-lang, loonlang

## Claims

| Property | Value | Notes |
| -------- | ----- | ----- |
| `instance of` (P31) | programming language (Q9143) | Required. |
| `instance of` (P31) | functional programming language (Q3839507) | Secondary classification. |
| `inception` (P571) | 2025 | Year of first public release. Adjust if wrong. |
| `official website` (P856) | https://loonlang.com/ | |
| `source code repository URL` (P1324) | https://github.com/ecto/loon | With qualifier `type of version control system (P8423): Git (Q186055)` |
| `programming paradigm` (P3966) | functional programming (Q193076) | |
| `programming paradigm` (P3966) | effect system (Q16935981) | If it exists; else skip. |
| `influenced by` (P737) | Clojure (Q29956) | |
| `influenced by` (P737) | Rust (Q575650) | |
| `influenced by` (P737) | OCaml (Q272515) | |
| `influenced by` (P737) | Haskell (Q35571) | |
| `influenced by` (P737) | Koka (Q… ) | Koka's Q-ID, used for effect typing. |
| `implemented in` (P277) | Rust (Q575650) | Compiler is Rust. |
| `file extension` (P1195) | oo | Primary. |
| `file extension` (P1195) | loon | Legacy fallback. |
| `license` (P275) | MIT License (Q334661) | Confirm against repo LICENSE. |
| `software version identifier` (P348) | (current tag) | Optional; pull from `git describe`. |
| `copyright license` (P275) | MIT (Q334661) | |
| `logo image` (P154) | (Commons filename) | Upload `web/public/loon.png` to Wikimedia Commons first. |
| `Rosetta Code identifier` (P9739) | Loon | Once the Rosetta Code page is live. |
| `PLDB identifier` (P…) | loon | If/when PLDB gets a Wikidata property. |

## External identifiers to add once present

These are wiki-cross-links — add them after each external listing is live so
the graph connects.

- `Rosetta Code language` (P9739)
- `99 Bottles of Beer language` (if property exists)
- GitHub repo, crates.io package (if `loon-cli` is published)
- npm package (if tree-sitter-loon is published)

## Notes for the editor

- Don't add a Wikipedia sitelink until an actual Wikipedia article exists
  and survives notability review. Wikidata items can live without one.
- If the item gets flagged for merge/deletion on notability, point
  reviewers at Wikidata:Notability — Wikidata's bar is much lower than
  Wikipedia's, only "serves a clear structural need" matters.
- After the item is created, record the `Q…` ID back in
  `docs/registry/wikidata/loon-wikidata-claims.md` so we remember it.
