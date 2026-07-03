# The Machine Interface

Most Loon code is written and checked by agents ([agent-first.md](agent-first.md)).
This document specifies the parts of the CLI that are contracts for machines
rather than displays for humans.

## `loon check --json`

Emits diagnostics as [JSON Lines](https://jsonlines.org/) on stdout: one
object per diagnostic, then exactly one summary object. In `--json` mode
stdout is pure JSONL — no ANSI, no prose (logs go to stderr). Exit codes are
unchanged from plain `loon check`: `0` when there are no diagnostics, `1`
otherwise.

### Schema (version 1)

Every line carries `"schema_version": 1`. The version is bumped on any
breaking change to field names, types, or meaning; purely additive fields do
not bump it. Consumers should ignore unknown fields.

Diagnostic line:

```json
{
  "type": "diagnostic",
  "schema_version": 1,
  "code": "E0201",
  "severity": "error",
  "message": "unbound symbol 'pritnln'",
  "why": "'pritnln' is not defined in this scope",
  "fix": "did you mean 'println'?",
  "spans": [
    {
      "file": "src/main.oo",
      "label": "not found in this scope",
      "primary": true,
      "start_byte": 14,
      "end_byte": 22,
      "line": 2,
      "col": 2,
      "end_line": 2,
      "end_col": 10
    }
  ],
  "explain_hint": "loon explain E0201"
}
```

- `code` — the Loon error code (`E01xx` parse, `E02xx` type, `E03xx`
  ownership, `E04xx` effect, `E05xx` module, `W01xx` warning).
- `severity` — `"error"` or `"warning"`, derived from the code.
- `message` / `why` / `fix` — the what/why/fix triple every Loon diagnostic
  carries. `why` and `fix` may be empty strings.
- `spans` — zero or more labeled source spans; at most one has
  `"primary": true`. `line`/`col` are 1-based (columns count Unicode scalar
  values); `start_byte`/`end_byte` are 0-based byte offsets into the file.
- `explain_hint` — the command that expands the code into a tutorial.

Summary line (always last, always present — a clean file emits only this):

```json
{"type":"summary","schema_version":1,"errors":1,"warnings":0}
```

### Notes

- Parse errors are reported through the same schema (as `E01xx`
  diagnostics) before exiting.
- `loon run` and `loon test` do not yet speak JSON; they are the next items
  on the machine-interface roadmap (see agent-first.md, Principle 3).

## `loon card`

Prints a compact language card (~1k tokens of markdown) designed to be
pasted into an LLM system prompt: one-sentence semantic rules, the syntax
skeleton, the falsy set, Option/Result idioms, effects in a paragraph, top
diagnostic codes, and the CLI verbs. The content is compiled into the binary
(`crates/loon-cli/src/card.md`) and stamped with the git-derived compiler
version at print time, so the card can never drift from the compiler that
serves it.

`loon card --json` emits one JSON object:

```json
{
  "schema_version": 1,
  "loon_version": "0.4.20",
  "sections": [{"title": "...", "body": "..."}]
}
```

The same content is served at `loonlang.com/llms.txt`
(`web/public/llms.txt`, kept in sync with `card.md` by an integration
test).
