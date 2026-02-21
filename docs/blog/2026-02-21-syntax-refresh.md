# The Syntax Refresh

*February 2026*

Loon v0.4.1 ships nine changes that make the language smaller and more expressive. Three remove syntax, six add features. Every change was motivated by real patterns across 53 `.loon` files — the web app, sample programs, and a CAD library.

## Killing `=>`

The fat arrow was the only infix operator in a prefix language. It existed because early Loon match arms felt hard to read without a separator. But with good formatting the separator is just noise.

Match arms are now positional pairs — pattern, then body:

```
; before
[match shape
  [Circle r] => [* 3.14 r r]
  [Rect w h] => [* w h]]

; after
[match shape
  [Circle r] [* 3.14 r r]
  [Rect w h] [* w h]]
```

Same for handle blocks. The change touched every file in the codebase but was entirely mechanical.

## Killing `/`

Effect annotations used a `/` separator between params and effect set:

```
; before
[fn load [path] / #{IO Fail}
  [IO.read-file path]]

; after
[fn load [path] #{IO Fail}
  [IO.read-file path]]
```

The set literal `#{IO Fail}` sits directly after the param list. The parser knows it's an effect annotation by position — it's a set, it's between params and body. No separator needed.

## Universal string interpolation

Every string now supports `{expr}` interpolation. No wrapper function, no special syntax — just braces inside a string:

```
; before
[str "Hello, " name "!"]
[str "cube " x "x" y "x" z]

; after
"Hello, {name}!"
"cube {x}x{y}x{z}"
```

Escape literal braces with `\{` and `\}`. This eliminated roughly 40% of `str` calls in the codebase. The `str` function remains available for programmatic concatenation.

## Type methods

The centerpiece feature. Methods live inside type variant declarations. Loon generates dispatch functions automatically:

```
[type Shape
  [Circle f64
    [fn area [r] [* 3.14159 r r]]
    [fn describe [r] "circle r={r}"]]
  [Rect f64 f64
    [fn area [w h] [* w h]]
    [fn describe [w h] "{w}x{h}"]]
  Point
    [fn area [] 0.0]
    [fn describe [] "point"]]

[area [Circle 5.0]]        ; 78.539...
[describe [Rect 3.0 4.0]]  ; "3x4"
```

Method params correspond to the variant's fields. When you call `[area [Circle 5.0]]`, Loon destructures the ADT and binds `r = 5.0` before evaluating the body.

Types are open by default. A second `[type Shape ...]` declaration adds new variants and extends the dispatch functions. This solves the expression problem with zero new keywords.

## Record types and dot access

ADT variants can have named fields using `:keyword Type` pairs:

```
[type Route
  [Route :path String :handler Fn]]

[let r [Route {path: "/tour", handler: tour-page}]]
[r.path]     ; "/tour"
[r.handler]  ; tour-page
```

Constructors accept either positional args or a map. Dot access (`r.path`) looks up the field name, finds the positional index, and returns the value. The runtime representation is unchanged — fields are stored positionally with a name-to-index mapping on the side.

## Match guards

A lowercase function call in pattern position evaluates as a boolean guard:

```
[match true
  [= w ""]                   ""
  [contains? special-forms w] "syntax-keyword"
  [contains? builtin-names w] "syntax-builtin"
  _                           ""]
```

This is effectively `cond` but reuses existing `match` semantics. There's no ambiguity with constructor patterns because ADT constructors are always uppercase.

## Map destructuring with defaults

Function params can destructure maps with default values:

```
; before (5 lines of boilerplate per component)
[fn card [first & rest]
  [let props [if [map? first] first {}]]
  [let children [if [map? first] rest [cons first rest]]]
  [let accent [get props :accent false]]
  ...]

; after (one line)
[fn card [{accent false, class ""} & children]
  [div {:class [cx "card" class]} children]]
```

## And the rest

- **`dom/` → `dom.`**: All namespace access now uses `.` consistently, matching modules (`math.add`) and effects (`IO.read-file`)
- **Implicit `do`**: Multi-expression function bodies already worked — we verified and documented it

## The numbers

53 files changed, 717 insertions, 487 deletions. All 306 tests pass. Every sample program, the entire web app, and the test suite run on the new syntax.

The language's character set is now minimal:

| Character | Meaning |
|-----------|---------|
| `[ ]` | calls and forms |
| `{ }` | maps / effect annotations |
| `#[ ]` `#{ }` | vec and set literals |
| `:` | keywords |
| `"` | strings (with interpolation) |
| `;` | comments |
| `&` | rest args |
| `~` | unquote in macros |
| `.` | namespace separator |

No infix operators. No special separators. Just brackets and data.
