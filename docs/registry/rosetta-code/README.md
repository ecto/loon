# Rosetta Code seed tasks

Each file here is a Loon solution to a canonical Rosetta Code task, formatted
for copy-paste into the Loon section on the corresponding task page.

After creating the language page at
<https://rosettacode.org/wiki/Category:Loon>, paste each of these snippets
into the relevant task page under a `==={{header|Loon}}===` section.

## Language page template

Paste this at <https://rosettacode.org/wiki/Category:Loon>:

```mediawiki
{{language
|exec=machine
|site=https://loonlang.com/
|gc=no
|parampass=value
|safety=safe
|strength=strong
|compat=static
|checking=static
|tags=loon
|LCT=no
}}
Loon is a functional language with invisible types, safe ownership, and
algebraic effects. Source files use the <code>.oo</code> extension
(from l-oo-n); <code>.loon</code> is accepted as a fallback.

Key features:
* Hindley-Milner type inference with let-polymorphism and row types
* Rust-style ownership and borrow checking, inferred from dataflow
* Algebraic effects replacing exceptions, async/await, and mutable state
* S-expression syntax in square brackets
* Pattern matching with positional pairs
* Pipe operator for dataflow chains
```

## Included tasks

| File | Rosetta Code task |
| ---- | ----------------- |
| `hello-world.oo` | [Hello world/Text](https://rosettacode.org/wiki/Hello_world/Text) |
| `fizzbuzz.oo` | [FizzBuzz](https://rosettacode.org/wiki/FizzBuzz) |
| `fibonacci.oo` | [Fibonacci sequence](https://rosettacode.org/wiki/Fibonacci_sequence) |
| `factorial.oo` | [Factorial](https://rosettacode.org/wiki/Factorial) |
| `99-bottles.oo` | [99 Bottles of Beer](https://rosettacode.org/wiki/99_Bottles_of_Beer) |
| `reverse-string.oo` | [Reverse a string](https://rosettacode.org/wiki/Reverse_a_string) |
| `sum-of-squares.oo` | [Sum of squares](https://rosettacode.org/wiki/Sum_of_squares) |
| `higher-order.oo` | [Higher-order functions](https://rosettacode.org/wiki/Higher-order_functions) |
| `map-range.oo` | [Map range of integers](https://rosettacode.org/wiki/Loops/For) |
| `ackermann.oo` | [Ackermann function](https://rosettacode.org/wiki/Ackermann_function) |
