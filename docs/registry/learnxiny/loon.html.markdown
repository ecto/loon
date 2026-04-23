---
language: Loon
filename: learnloon.oo
contributors:
    - ["Loon contributors", "https://loonlang.com/"]
---

Loon is a functional language with invisible types, safe ownership, and
algebraic effects. Everything is an s-expression in square brackets, type
inference means no annotations, and effects replace exceptions, async, and
mutable state.

Source files use the `.oo` extension (from l-**oo**-n). The older `.loon`
extension is accepted as a fallback.

```clojure
; Line comments start with a semicolon. That's the only comment syntax.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Calling things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Every form is [head args...]. The head goes first, inside the brackets.
; Parentheses are NOT function call — they build tuples. This surprises
; everyone at first.

[+ 1 2]              ; => 3
[* 6 7]              ; => 42
[str "hello, " "world"]  ; => "hello, world"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. Data literals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Numbers — integers and floats, with optional type suffixes.
42            ; i64 by default
42i32         ; 32-bit int
3.14          ; f64 by default
3.14f32       ; 32-bit float

; Strings — double-quoted, with \n \t \\ \" escapes.
"hello"

; String interpolation: {expr} inside a string.
[let name "world"]
[println [fmt "hello, {name}!"]]   ; => hello, world!

; Booleans.
true
false

; Keywords — self-evaluating, start with a colon.
:ok
:not-found

; Vectors — persistent, #[...].
#[1 2 3 4]

; Sets — persistent, #{...}.
#{:red :green :blue}

; Maps — {:key value ...}.
{:name "ada" :age 36}

; Tuples — (a b c). Fixed-arity, heterogeneous.
(1 "two" 3.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. Bindings and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let binds a name in the enclosing scope.
[let x 10]
[let y [* x x]]
[println y]        ; => 100

; Functions — [fn name [params] body].
[fn square [x]
  [* x x]]

[square 9]          ; => 81

; Anonymous functions — [fn [params] body], no name.
[map [fn [n] [* n n]] #[1 2 3]]   ; => #[1 4 9]

; Multi-arity — one fn can accept different argument counts.
[fn greet
  ([name]          [str "hello, " name "!"])
  ([greet name]    [str greet ", " name "!"])]

[greet "world"]           ; => "hello, world!"
[greet "howdy" "world"]   ; => "howdy, world!"

; Types are inferred. You never have to write them — the checker uses
; Hindley-Milner with let-polymorphism and row types. `loon explain` can
; tell you what the inferred type was.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 4. Pattern matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Match uses positional pattern/body pairs. Odd positions are patterns,
; even positions are bodies. No `=>` or `|` separator.
[fn fib [n]
  [match n
    0 0
    1 1
    n [+ [fib [- n 1]] [fib [- n 2]]]]]

; Destructuring works in `let`, `fn` params, and `match`.
[let [a b c] #[1 2 3]]
[println a]     ; => 1

; Match on tuples by constructing a tuple scrutinee.
[fn fizzbuzz [n]
  [match ([% n 3] [% n 5])
    (0 0) "FizzBuzz"
    (0 _) "Fizz"
    (_ 0) "Buzz"
    _     [str n]]]

; Or use `match true` with expression-guard arms — each lowercase-headed
; list pattern is evaluated; the arm matches when the result is truthy.
[fn fizzbuzz-alt [n]
  [match true
    [= 0 [% n 15]] "FizzBuzz"
    [= 0 [% n 3]]  "Fizz"
    [= 0 [% n 5]]  "Buzz"
    _              [str n]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 5. Algebraic data types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Sum types with `type`. Nullary variants have no args.
[type Shape
  [Circle f64]
  [Rect f64 f64]
  Point]

[fn area [shape]
  [match shape
    [Circle r] [* 3.14159 [* r r]]
    [Rect w h] [* w h]
    Point      0.0]]

[area [Circle 5.0]]   ; => 78.53975

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 6. Pipes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `pipe` threads a value through a chain of transformations. The first
; argument is the seed; each subsequent form gets it as an implicit first
; arg. No nesting, no temp variables.
[pipe #[1 2 3 4 5]
  [filter [fn [n] [> n 2]]]
  [map [fn [n] [* n n]]]
  [each println]]
; => 9
; => 16
; => 25

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 7. Algebraic effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Effects replace exceptions, async/await, and mutable state. Declare the
; operations an effect supports, then let any function perform them.
[effect Fs
  [read-file [String] String]
  [list-dir  [String] String]]

; A function that performs effects declares them in its signature with a
; #{...} effect set after the param list. Effects are inferred, so you
; rarely have to write this by hand.
[fn load-config [path] #{Fs}
  [Fs.read-file path]]

; `handle` runs an expression with handlers for each operation.
[fn main []
  [let result
    [handle [load-config "app.toml"]
      [Fs.read-file path] [resume "name = test"]
      [Fs.list-dir path]  [resume "a.oo b.oo"]]]
  [println result]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 8. Ownership
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Loon has Rust-style move semantics and borrow checking. Unlike Rust,
; there's no lifetime syntax — ownership and borrows are inferred from
; dataflow. If the checker can prove a value is never used after move, the
; program is safe.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 9. Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Source files use `.oo`. A project has a `pkg.oo` manifest and a `lock.oo`
; lockfile. Modules are the filesystem — `src/math/vec.oo` is `math.vec`.

; Use `use` to import from another module:
[use math.vec]
[vec/dot #[1 2 3] #[4 5 6]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 10. Running it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; loon run hello.oo      — run a file
; loon repl              — REPL with time-travel
; loon fmt .             — format
; loon new my-proj       — scaffold a project (pkg.oo + src/main.oo)
; loon explain E0201     — interactive explanation of an error
```

## Further reading

- [The Loon website](https://loonlang.com/)
- [Try Loon in your browser](https://loonlang.com/play) — compiles to WASM
- [Samples](https://loonlang.com/samples)
