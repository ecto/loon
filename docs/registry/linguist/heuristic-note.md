# Linguist heuristic — `.oo`

The `.oo` extension is not currently claimed in Linguist's `languages.yml` or
any heuristic in `lib/linguist/heuristics.yml`. A quick check against the
current file at
<https://github.com/github-linguist/linguist/blob/main/lib/linguist/heuristics.yml>
is required before submission — if any other language claims `.oo` by the
time we submit, we need to add a disambiguation heuristic.

Suggested heuristic if needed:

```yaml
disambiguations:
  - extensions: ['.oo']
    rules:
      - language: Loon
        pattern: '(?m)^\s*\[(?:fn|let|type|effect|use|module)\b'
      - language: <Other>
        # whatever the other claimant's distinguishing pattern is
```

The Loon pattern matches any file that starts with an s-expression whose
head is a Loon special form — essentially every non-trivial Loon file.

## `.loon` extension

`.loon` is unique enough to avoid a heuristic. Current Linguist languages
list as of April 2026 has no claimants on either extension.
