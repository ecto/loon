# Loon language-directory registration drafts

Ready-to-submit artifacts for each external programming-language directory
and wiki we want Loon listed in. The human-facing checklist lives in
`docs/plans/2026-04-23-language-directories.md`; this directory holds the
actual payloads.

## Contents

| Path | Target | Submission route |
| ---- | ------ | ---------------- |
| `snippets/hello-world.oo` | Universal | Source for multiple submissions |
| `snippets/99-bottles.oo` | 99-bottles-of-beer.net | Submit form |
| `snippets/fizzbuzz.oo` | Rosetta Code / LXiYM | Copy into submissions |
| `learnxiny/loon.html.markdown` | Learn X in Y Minutes | PR to `adambard/learnxinyminutes-docs` |
| `lexers/loon_pygments.py` | Pygments | PR to `pygments/pygments` at `pygments/lexers/loon.py` |
| `lexers/loon.highlightjs.js` | highlight.js | PR to `highlightjs/highlight.js` (+ optional standalone package under `highlightjs/`) |
| `lexers/loon.rouge.rb` | Rouge / Jekyll | PR to `rouge-ruby/rouge` at `lib/rouge/lexers/loon.rb` |
| `pldb/loon.pldb` | PLDB | Either paste into <https://build.pldb.com/create> or PR to `breck7/pldb` under `database/things/loon.pldb` |
| `wikidata/loon-wikidata-claims.md` | Wikidata | Create item at <https://www.wikidata.org/wiki/Special:NewItem>, apply claims by hand |
| `rosetta-code/` | Rosetta Code | Create `Category:Loon` page, then paste each `.oo` into the matching task |
| `linguist/languages.yml.fragment` | GitHub Linguist | **Do not submit yet** — gated on ~200 public repos using `.oo` |
| `linguist/heuristic-note.md` | GitHub Linguist | Reference when the main PR is finally submitted |

## Editing notes

- Each artifact is self-contained — don't cross-import between them.
- When an external listing changes form (e.g., PLDB adds a new column),
  update the draft here first; the repo is the source of truth.
- When a submission lands, record the URL in
  `docs/plans/2026-04-23-language-directories.md` so we don't
  double-submit.
