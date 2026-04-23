# Programming Language Directories & Wikis

> **Status: Research / checklist** — list of external catalogs where Loon should
> be registered. Each entry records the registration mechanism, realistic bar
> for acceptance, and a "worth it?" note. Tackle the low-friction ones first.

Grouped by what they are, not by priority. A suggested order is at the bottom.

## Encyclopedias & general wikis

- **Wikipedia** — <https://en.wikipedia.org/wiki/List_of_programming_languages>.
  An article is notable per WP:GNG only if multiple independent, reliable
  sources have covered Loon. Don't try until there's real third-party
  coverage; a premature article will be deleted and salted. Gatekeeper for
  several other directories (TIOBE, DBpedia).
- **Wikidata** — <https://www.wikidata.org/wiki/Wikidata:WikiProject_Informatics/Languages>.
  Lower bar than Wikipedia; a Q-item can exist even without an en.wiki
  article as long as structured claims are sourced. Create a `Q…` with
  `instance of: programming language (Q9143)`, `inception`, `programmed in`,
  official website, source repo, file extension.
- **DBpedia** — <https://dbpedia.org/page/Programming_language>. Downstream
  of Wikipedia; auto-populates once a Wikipedia article exists. No direct
  submission.
- **HOPL — Online Historical Encyclopaedia of Programming Languages** —
  <https://hopl.info/>. Diarmuid Pigott's roster, ~8.9k languages with
  influence/genealogy links. Submission is via email to the maintainer
  with a bibliographic reference.
- **Progopedia** — <http://progopedia.com/>. Curated encyclopedia; editors
  are approved by hand. Accepts languages that don't meet Wikipedia
  notability, so a good early target. Contact the maintainers to request
  an editor account.
- **Esolang wiki** — <https://esolangs.org/wiki/Language_list>. Only
  relevant if we want to frame Loon as esoteric. Loon isn't — **skip**.

## Structured language databases

- **PLDB** (Programming Language DataBase) —
  <https://pldb.io/> · add via <https://build.pldb.com/create> or PR a
  `.pldb` file to <https://github.com/breck7/pldb>. Public-domain CSV of
  ~5k languages. Low friction, high value — **do this first**.
- **programminglanguages.info** —
  <https://programminglanguages.info/languages/>. Alphabetical catalog of
  ~1.6k languages; submission instructions on site.
- **HOPL (Gabriel/Steele ACM conference series)** — distinct from the
  Pigott roster; this is an academic conference, not a directory. Not
  actionable.

## Popularity indexes

- **TIOBE Index** — <https://www.tiobe.com/tiobe-index/>. Criteria: Turing
  complete, has a Wikipedia entry that says it's a programming language,
  and >5,000 hits for `+"Loon programming"` on Google. Email
  `tpci@tiobe.com` once those are satisfied. Gated on Wikipedia, so not
  near-term.
- **PYPL** — <https://pypl.github.io/PYPL.html>. Ranks by Google Trends
  tutorial search volume; no submission, auto-derived.
- **RedMonk Rankings** — derived from GitHub + Stack Overflow data; no
  submission, auto-derived.
- **IEEE Spectrum Top Programming Languages** — journalist-curated, no
  submission form.

## Source-hosting classifiers

- **GitHub Linguist** — <https://github.com/github-linguist/linguist>.
  Official bar: ~200 unique user/repo pairs using the extension on public
  GitHub. A PR needs `languages.yml` entry + a TextMate/VS Code grammar +
  ≥2 real-world samples. **Realistic only after real adoption**; low-
  friction alternative is the `linguist-language` `.gitattributes`
  override per-repo so our own repos highlight correctly.
- **linguist-language `.gitattributes` override** — in-repo fix, works
  immediately. Add `*.oo linguist-language=Loon` (or a close parent) so
  GitHub stops mislabeling the repo while we wait for upstream
  acceptance.
- **Open Hub / Ohcount** — <https://openhub.net/>. Adding a language
  means patching ohcount (<https://github.com/blackducksoftware/ohcount>).
  Stagnant project, low payoff — **defer**.
- **SourceForge / GitLab / Codeberg language lists** — all downstream of
  Linguist or their own classifiers; no separate submission.

## Editor & tooling ecosystems

- **tree-sitter organization** — we already have `tree-sitter-loon/`.
  Publish it to its own GitHub repo under `tree-sitter/` naming
  convention and list it on the wiki
  <https://github.com/tree-sitter/tree-sitter/wiki/List-of-parsers>.
- **nvim-treesitter** — `parsers/loon.lua` PR to
  <https://github.com/nvim-treesitter/nvim-treesitter>. Requires a
  hosted tree-sitter grammar.
- **tree-sitter-language-pack** — <https://github.com/Goldziher/tree-sitter-language-pack>.
  Bundles parsers for Python bindings; PR after the grammar has a
  stable tag.
- **VS Code Marketplace** — publish a Loon extension (syntax + LSP
  client wired to `loon-lsp`). Required for editor users and for
  Linguist's grammar field.
- **Open VSX** — <https://open-vsx.org/>. Mirror of the VS Code
  marketplace used by VSCodium / Cursor / Theia; same extension, second
  publish.
- **JetBrains Marketplace** — optional; only if we build a plugin.
- **Sublime Package Control / Atom (archived) / Emacs MELPA** — one PR
  each after we have a syntax file. MELPA is the highest value of the
  three given Emacs users' overlap with FP language crowds.
- **Vim / Neovim plugin registries** — `vim-polyglot`, `sheerun/vim-polyglot`,
  plus a plain `vim-loon` repo.
- **linguist grammars submodule** — implicit once Linguist accepts us.
- **Pygments** — <https://pygments.org/>. PR a lexer to
  <https://github.com/pygments/pygments>. Used by GitHub Gist, Rouge,
  Jupyter, countless static site generators. High leverage.
- **Rouge** — <https://github.com/rouge-ruby/rouge>. Pygments-compatible
  Ruby lexer used by Jekyll/GitHub Pages. PR a lexer.
- **highlight.js** — <https://github.com/highlightjs/highlight.js>.
  Separate language packs now live in
  <https://github.com/highlightjs/highlightjs-*>. For our Loon website
  blog code blocks we'd want this anyway.
- **Prism / Shiki** — Shiki uses TextMate grammars, so our VS Code
  extension grammar covers it. Prism wants its own `prism-loon.js`.
- **chroma** (Hugo's highlighter) — <https://github.com/alecthomas/chroma>.
  Go port of Pygments; accepts XML lexers.

## Code-sample collections

- **Rosetta Code** — <https://rosettacode.org/wiki/Category:Programming_Languages>.
  Create a language page + start filling in task solutions. No gatekeeping;
  low-friction, high-visibility. **Do early**.
- **Learn X in Y Minutes** — <https://learnxinyminutes.com/>. PR a
  single `loon.html.markdown` to
  <https://github.com/adambard/learnxinyminutes-docs>. Good intro
  surface; Loon's design lends itself to a terse tour.
- **99 Bottles of Beer** — <https://www.99-bottles-of-beer.net/submitnewlanguage.html>.
  Form submission; ~3.5k-language backlog but gets approved eventually.
- **Hello World Collection** — <http://helloworldcollection.de/>. Email
  `info@helloworldcollection.de` with a minimal program. Long-running
  (1994).
- **Sample Programs in Every Language** —
  <https://sampleprograms.io/> · <https://github.com/TheRenegadeCoder/sample-programs>.
  Active project, clear contribution workflow (file naming conventions,
  Hello World as the entry project).
- **Computer Language Benchmarks Game** —
  <https://salsa.debian.org/benchmarksgame-team/benchmarksgame>.
  ~25 languages, selective. Not realistic until Loon has a competitive
  compiler; **defer**.

## Awesome-lists & curated catalogs

- **awesome-functional-programming** —
  <https://github.com/xgrommx/awesome-functional-programming>. PR a
  bullet under "Languages".
- **awesome-programming-languages** (learn-anything) —
  <https://github.com/learn-anything/programming-languages>.
- **langs-in-rust** —
  <https://github.com/alilleybrinker/langs-in-rust>. Loon's compiler is
  in Rust; this one we qualify for today.
- **awesome-compilers** / **awesome-static-analysis** / **awesome-lsp** —
  once the respective components are public.

## Community & discovery

- **r/ProgrammingLanguages** — Reddit community, good for an
  announcement post.
- **Lobsters / Hacker News** — announcement posts, not directories.
- **lang-jam / PLDI / ICFP** — conferences, not directories, but worth
  a mention in the long-term strategy.
- **Discord: Programming Language Design** — active community, worth
  joining to answer questions once we announce.

## Prioritized order

Low friction / do now:
1. `.gitattributes` `linguist-language` override in the Loon repo.
2. PLDB entry (web form, minutes).
3. Wikidata Q-item.
4. Rosetta Code language page + first 5–10 task solutions.
5. Learn X in Y Minutes PR.
6. 99 Bottles of Beer + Hello World Collection + Sample Programs — all
   trivial once we have canonical snippets.
7. HOPL email to Pigott.
8. Progopedia editor application.
9. langs-in-rust PR.
10. awesome-functional-programming PR.

Needs our own artifact first:
11. Publish `tree-sitter-loon` as a standalone repo and list it on the
    tree-sitter wiki.
12. VS Code extension → Marketplace + Open VSX.
13. nvim-treesitter parser entry.
14. Pygments lexer PR (unlocks Rouge/Jekyll/GitHub gists).
15. highlight.js / Prism / Chroma lexers (for our own website and
    downstream blogs).
16. MELPA recipe.

Gated on adoption / coverage:
17. Wikipedia article (needs independent sources — don't rush).
18. GitHub Linguist PR (needs ~200 public repos using `.oo`).
19. TIOBE (gated on Wikipedia + search volume).
20. Benchmarks Game (gated on compiler maturity).
