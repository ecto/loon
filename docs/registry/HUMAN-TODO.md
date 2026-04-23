# Human todo — submitting Loon to external directories

Everything Claude could prepare locally lives in `docs/registry/`. The items
below require a human because they need an account, email, confirmation
captcha, or a judgement call Claude shouldn't make unilaterally.

Work top-to-bottom — later items depend on earlier ones.

---

## Day 1 — 30 minutes, unlocks everything else

### 1. PLDB — submit Loon
- **Where**: <https://build.pldb.com/create>
- **How**: paste the contents of
  [`pldb/loon.pldb`](./pldb/loon.pldb) into the form.
- **Notes**: sanity-check the `appeared` year before submitting; I guessed
  2025.

### 2. Wikidata — create the Q-item
- **Where**: <https://www.wikidata.org/wiki/Special:NewItem>
- **How**: follow
  [`wikidata/loon-wikidata-claims.md`](./wikidata/loon-wikidata-claims.md)
  line by line. Requires a Wikimedia account (sign up at
  <https://www.wikidata.org/wiki/Special:CreateAccount>).
- **After**: write the resulting `Q…` ID back into the claim sheet.

### 3. HOPL (Pigott's roster) — email submission
- **Where**: email contact on <https://hopl.info/>
- **How**: short message introducing Loon, link to loonlang.com, the
  GitHub repo, and the first release tag. Ask to be listed under
  "Languages → L".

---

## Day 2 — Rosetta Code seeds

### 4. Create the Rosetta Code language page
- **Where**: <https://rosettacode.org/wiki/Category:Loon>
- **How**: requires an RC Miraheze account (one-click from the login
  link). Paste the `{{language}}` template from
  [`rosetta-code/README.md`](./rosetta-code/README.md).

### 5. Seed 8-10 task solutions
- Go to each task page and paste the matching snippet from
  `rosetta-code/*.oo` under a `==={{header|Loon}}===` section.
- Start with: Hello world/Text, FizzBuzz, Fibonacci sequence, Factorial,
  99 Bottles of Beer, Reverse a string, Sum of squares, Higher-order
  functions, Loops/For, Ackermann function.

### 6. Back-link from Wikidata
- Add `Rosetta Code identifier (P9739): Loon` to the Wikidata item.

---

## Day 3 — sample-code collections

### 7. 99 Bottles of Beer
- **Where**: <https://www.99-bottles-of-beer.net/submitnewlanguage.html>
- **How**: fill the form, paste [`snippets/99-bottles.oo`](./snippets/99-bottles.oo).
  Language author / website / year all from the PLDB draft.
- **Expect**: months of backlog before approval.

### 8. Hello World Collection
- **Where**: email `info@helloworldcollection.de`
- **How**: body is [`snippets/hello-world.oo`](./snippets/hello-world.oo)
  preceded by a one-line comment `; Loon — https://loonlang.com/`.

### 9. Sample Programs in Every Language
- **Where**: <https://github.com/TheRenegadeCoder/sample-programs>
- **How**: follow their CONTRIBUTING. Add a `archive/l/loon/` directory
  and submit Hello World + a couple more projects. A PR per project is
  expected.

### 10. Learn X in Y Minutes PR
- **Where**: <https://github.com/adambard/learnxinyminutes-docs>
- **How**: add [`learnxiny/loon.html.markdown`](./learnxiny/loon.html.markdown)
  as `loon.html.markdown` at the repo root. Open a PR titled
  `[loon/en] Add Loon tour`.

---

## Day 4 — syntax highlighters

These unlock syntax highlighting in thousands of downstream tools.

### 11. Pygments
- **Where**: <https://github.com/pygments/pygments>
- **How**: copy [`lexers/loon_pygments.py`](./lexers/loon_pygments.py)
  to `pygments/lexers/loon.py`. Run `make mapfiles` to regenerate
  `_mapping.py`, then add a test file under `tests/examplefiles/loon/`.
  Open a PR.

### 12. Rouge (for Jekyll / GitHub Pages)
- **Where**: <https://github.com/rouge-ruby/rouge>
- **How**: copy [`lexers/loon.rouge.rb`](./lexers/loon.rouge.rb) to
  `lib/rouge/lexers/loon.rb`. Add a demo file at
  `lib/rouge/demos/loon` and a visual test. Open a PR.

### 13. highlight.js
- **Where**: <https://github.com/highlightjs/highlight.js>
- **How**: copy [`lexers/loon.highlightjs.js`](./lexers/loon.highlightjs.js)
  to `src/languages/loon.js`. Add tests under `test/markup/loon/`.
- **Alternative**: publish as a standalone package under the
  `highlightjs/` GitHub org named `highlightjs-loon`, which has a
  lower acceptance bar and can be npm-installed by users today.

### 14. Chroma (Hugo)
- **Where**: <https://github.com/alecthomas/chroma>
- **How**: Chroma lexers are XML, not Python — port the Pygments lexer
  to an XML file at `lexers/embedded/loon.xml`. Lower priority unless
  we care about Hugo users specifically.

---

## Day 5 — tree-sitter + editor ecosystems

### 15. Publish `tree-sitter-loon` as its own repo
- **Where**: create <https://github.com/loon-lang/tree-sitter-loon> (the
  `package.json` already references this URL).
- **How**: push the contents of the `tree-sitter-loon/` subdirectory as
  a standalone repo. Tag `v0.1.0`.
- Publish to npm: `npm publish` from the repo.
- Publish to crates.io: `cargo publish` from `bindings/rust/`.

### 16. List on the tree-sitter wiki
- **Where**: <https://github.com/tree-sitter/tree-sitter/wiki/List-of-parsers>
- **How**: edit the wiki, add a bullet under the alphabetical list.

### 17. nvim-treesitter PR
- **Where**: <https://github.com/nvim-treesitter/nvim-treesitter>
- **How**: add an entry to `lua/nvim-treesitter/parsers.lua` pointing at
  the tree-sitter-loon repo and the tagged revision.

### 18. VS Code extension
- **Where**: build in a new directory (e.g. `editors/vscode/` in the
  main repo, or a separate `vscode-loon` repo).
- **How**: scaffold with `yo code`, wire in:
  - TextMate grammar generated from `tree-sitter-loon/queries/highlights.scm`
    (or a hand-written `loon.tmLanguage.json` — simpler for Linguist).
  - LSP client pointing at `loon-lsp` binary.
- **Publish**: both
  - <https://marketplace.visualstudio.com/manage> (VS Code)
  - <https://open-vsx.org/> (VSCodium / Cursor / Theia)

---

## Day 6+ — curated awesome-lists

These are one-line PRs each — batch into a single afternoon.

### 19. langs-in-rust
- **Where**: <https://github.com/alilleybrinker/langs-in-rust>
- **How**: PR a row to the README table. Loon qualifies today.

### 20. awesome-functional-programming
- **Where**: <https://github.com/xgrommx/awesome-functional-programming>
- **How**: PR a bullet under "Languages" in the README.

### 21. awesome-programming-languages
- **Where**: <https://github.com/learn-anything/programming-languages>
- **How**: PR a bullet.

### 22. Emacs MELPA
- **Where**: <https://github.com/melpa/melpa>
- **How**: requires a `loon-mode.el` package first. Skip until we have
  Emacs support.

---

## Gated — don't attempt yet

These have real acceptance bars that Loon hasn't cleared. Revisit when
the conditions in parentheses are met.

- **Progopedia** — editor account is granted manually; ping them once
  the Wikidata item and a few blog posts exist so there's something
  concrete to point at.
- **programminglanguages.info** — low traffic, low value; do it after
  the rest.
- **Wikipedia article** — only after multiple independent reliable
  sources (e.g., a conference talk, a magazine article, a major blog
  post from someone we don't know). A premature article will be
  deleted.
- **GitHub Linguist PR** — only after ~200 unique public repos are
  using the `.oo` extension. The
  [`linguist/languages.yml.fragment`](./linguist/languages.yml.fragment)
  is ready and waiting.
- **TIOBE Index** — gated on Wikipedia article + >5,000 Google hits
  for `+"Loon programming"`. Email `tpci@tiobe.com` once both are
  true.
- **Computer Language Benchmarks Game** — selective; defer until the
  compiler is performance-competitive with reference implementations.

---

## Record-keeping

After each submission lands, append the URL to the corresponding entry
in `docs/plans/2026-04-23-language-directories.md` so we don't
double-submit and can point future collaborators at the live listings.
