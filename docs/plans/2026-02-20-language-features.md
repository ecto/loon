# Loon Language Features

> **Status: Implemented** — All features shipped in commit a9039df (2026-02-21).

## Syntax Cleanup

### Kill `=>` in match — use positional pairs

Match arms become pattern-body pairs with no separator. Odd positions are patterns, even positions are bodies.

```
; before
[match shape
  [Circle r] => [* 3.14159 r r]
  [Rect w h] => [* w h]
  _          => 0.0]

; after
[match shape
  [Circle r] [* 3.14159 r r]
  [Rect w h] [* w h]
  _          0.0]
```

Eliminates the only infix operator in the language. Pattern and body are always one form each.

### Kill `/` in effect annotations — positional `{...}` after params

A `{...}` set after the param list is the effect annotation. No separator needed.

```
; before
[fn load-config [path] / {IO Fail}
  [IO.read-file path]]

; after
[fn load-config [path] {IO Fail}
  [IO.read-file path]]
```

### Fix `dom/` namespace — use `.` consistently

`dom/location` becomes `dom.location`. All namespace access uses `.`, which is already the convention for modules (`math.add`) and effects (`IO.read-file`).

### Final character set

| Character | Meaning |
|-----------|---------|
| `[ ]` | calls and forms |
| `{ }` | maps / effect annotations (by position) |
| `( )` | multi-arity grouping |
| `#[ ]` `#{ }` | vec and set literals |
| `:` | keywords |
| `"` | strings (with interpolation) |
| `;` | comments |
| `&` | rest args |
| `~` | unquote in macros |
| `.` | namespace separator (in identifiers) |

---

## New Features

### 1. Methods inside `type` — open by default

The centerpiece feature. Variants carry their own function implementations. The type declaration generates dispatch functions automatically. `type` is always additive — if the type already exists, new variants are appended.

```
; shape.loon
[type Shape
  [Circle f64
    [fn area [r] [* 3.14159 r r]]
    [fn describe [r] "circle r={r}"]]
  [Rect f64 f64
    [fn area [w h] [* w h]]
    [fn describe [w h] "rect {w}x{h}"]]
  Point
    [fn area [] 0.0]
    [fn describe [] "point"]]

; triangle.loon — same keyword, extends Shape
[type Shape
  [Triangle f64 f64
    [fn area [b h] [* 0.5 b h]]
    [fn describe [b h] "triangle {b}x{h}"]]]

; usage — area and describe are regular functions with automatic dispatch
[area [Circle 5.0]]        ; => 78.539...
[describe [Rect 3.0 4.0]]  ; => "rect 3x4"
[area [Triangle 6.0 3.0]]  ; => 9.0
```

No `type+`, no `extend` — just `type` everywhere. First declaration creates it, subsequent ones add variants. **Solves the expression problem** with zero new keywords. Multi-dispatch (two Shapes interacting) still uses `match`.

### 2. Universal string interpolation

All strings support `{expr}`. Escape literal braces with `\{`. Values are auto-coerced to strings.

```
; before
[str "1px solid " [color :border]]
[str greeting ", " name "!"]
[str "cube " x "x" y "x" z]

; after
"1px solid {[color :border]}"
"{greeting}, {name}!"
"cube {x}x{y}x{z}"
```

`str` remains for programmatic concatenation but is rarely needed.

### 3. Match guards via `match true`

No new syntax needed. Lowercase `[calls]` in pattern position evaluate as expressions rather than destructure (ADT variants are uppercase, so there's no ambiguity).

```
; before (nested ifs, 7 levels deep)
[if [= [len w] 0] ""
[if [contains? special-forms w] "syntax-keyword"
[if [contains? builtin-names w] "syntax-builtin"
  ...]]]

; after
[match true
  [= w ""]                   ""
  [contains? special-forms w] "syntax-keyword"
  [contains? builtin-names w] "syntax-builtin"
  [contains? dig-set [char-at w 0]] "syntax-number"
  _                           ""]
```

This is effectively `cond` but reuses existing `match` semantics.

### 4. Map destructuring with defaults in `fn` params

Eliminates the component boilerplate pattern that appears 15+ times in the web app codebase.

```
; before (5 lines of boilerplate per component)
[fn card [first & rest]
  [let props [if [map? first] first {}]]
  [let children [if [map? first] rest [cons first rest]]]
  [let accent [get props :accent false]]
  [let cls [get props :class ""]]
  ...]

; after (one signature line)
[fn card [{accent false, class ""} & children]
  [div {:class [cx "card" class]} children]]
```

`{key default, key2 default2}` in a param position destructures a map argument, binding named keys with defaults. Unmatched keys are ignored.

### 5. Record types / named fields

Named fields on ADT variants with dot access. Eliminates fragile `[nth route 0]` / `[nth route 1]` positional access.

```
[type Route
  [Route :path String :handler Fn]]

[let r [Route {path: "/tour", handler: tour-page}]]
[r.path]     ; => "/tour"
[r.handler]  ; => tour-page
```

### 6. Implicit `do`

Multiple forms in a function body are sequential without explicit `do`. Already works in some places (e.g. `main` functions) — just make it consistent everywhere.

```
; before
[fn go [i acc]
  [do
    [let result [step src slen i acc]]
    [go [nth result 0] [nth result 1]]]]

; after
[fn go [i acc]
  [let result [step src slen i acc]]
  [go [nth result 0] [nth result 1]]]
```

### 7. Macro-based element constructors (no language change)

Not a language feature — use existing macros to eliminate the 27 identical element constructor lines in `ui.loon`.

```
; before (27 lines)
[fn div [first & rest]          [make-element "div" first rest]]
[fn span [first & rest]         [make-element "span" first rest]]
[fn p [first & rest]            [make-element "p" first rest]]
...

; after (1 macro + invocations)
[macro def-el [name tag]
  [fn ~name [first & rest] [make-element ~tag first rest]]]

[def-el div "div"]
[def-el span "span"]
[def-el p "p"]
[def-el a-el "a"]
[def-el button-el "button"]
...
```

---

## Impact summary

| Feature | Lines saved (est.) | Files affected |
|---------|-------------------|----------------|
| Kill `=>` | net zero (cleaner) | every file with match |
| Kill `/` in effects | net zero (cleaner) | effect-using files |
| `type` methods (open) | ~30% of match boilerplate | types.loon, vcad/main.loon |
| String interpolation | ~40% of str calls | nearly every file |
| Match guards (`match true`) | kills nested ifs | code.loon, doc.loon, ui.loon |
| Map destructuring defaults | ~5 lines per component | components.loon, doc.loon |
| Named fields | kills nth calls | router.loon, code.loon |
| Implicit do | kills do wrappers | code.loon, ui.loon |
| Element macro | 27 lines -> 10 | ui.loon |

## Verification

- Review each feature against the existing .loon codebase to confirm it addresses real patterns
- For syntax changes (`=>` removal, `/` removal), audit all files for migration scope
- For `type` methods: prototype with Shape example, verify dispatch semantics
- For string interpolation: ensure `\{` escaping works and edge cases (nested braces, empty expressions) are handled
