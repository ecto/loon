//! Single source of truth for the Loon builtin surface.
//!
//! Every named builtin that must exist on *all* execution surfaces — the
//! type checker's initial environment, the tree-walking interpreter, and
//! the EIR VM — has one entry here. Implementations stay per-backend, but
//! the *set* and *signatures* come from this table, so a builtin cannot
//! exist on one surface and be missing on another. `loon card` renders its
//! Builtins section from this table too.
//!
//! The conformance test in `crates/loon-lang/tests/builtin_registry.rs`
//! iterates this table and asserts coverage on every surface. Adding an
//! entry here without wiring all backends fails that test.
//!
//! Deliberately excluded: operators (`+`, `=`, …— typed as trait-bounded
//! schemes and lowered to `BinOp`), special forms (`and`/`or`/`if`),
//! effects (IO/Net/…, see `effects::EffectRegistry`), and backend-specific
//! extras (channels, `push!`, DOM, physics constants).

/// Primitive type atoms used by monomorphic registry signatures.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ty {
    Int,
    Float,
    Bool,
    Str,
    /// Numeric parameter: checker gives it a fresh var bounded by `Num`
    /// (implemented by Int and Float).
    Num,
    /// `Option Int`
    OptionInt,
    /// `Option Float`
    OptionFloat,
}

/// How the type checker derives this entry's scheme.
#[derive(Debug, Clone, Copy)]
pub enum Typing {
    /// Monomorphic (or Num-bounded) signature the checker derives directly
    /// from the table: params → ret.
    Mono(&'static [Ty], Ty),
    /// A constant value of the given type (e.g. `pi`).
    Const(Ty),
    /// Polymorphic or otherwise bespoke — the checker registers the scheme
    /// in hand-written code, but the conformance test still asserts the
    /// name is present in the initial environment.
    Special,
}

/// One builtin: name, human-readable signature, doc line, arity forms.
#[derive(Debug, Clone, Copy)]
pub struct BuiltinSpec {
    pub name: &'static str,
    /// Human-readable signature, shown in `loon card` and docs.
    pub sig: &'static str,
    /// One-line doc.
    pub doc: &'static str,
    /// Minimum number of arguments (0 for constants).
    pub min_args: u8,
    /// Maximum number of arguments; `None` = variadic.
    pub max_args: Option<u8>,
    pub typing: Typing,
}

const fn f(
    name: &'static str,
    sig: &'static str,
    doc: &'static str,
    min_args: u8,
    max_args: u8,
    typing: Typing,
) -> BuiltinSpec {
    BuiltinSpec {
        name,
        sig,
        doc,
        min_args,
        max_args: Some(max_args),
        typing,
    }
}

use Ty::*;
use Typing::*;

/// The registry. Grouped roughly by domain; order is the order `loon card`
/// prints.
pub const BUILTINS: &[BuiltinSpec] = &[
    // ── Output / debug ──────────────────────────────────────────────
    BuiltinSpec {
        name: "println",
        sig: "a → ()",
        doc: "print value followed by newline",
        min_args: 0,
        max_args: None,
        typing: Special,
    },
    BuiltinSpec {
        name: "print",
        sig: "a → ()",
        doc: "print value without newline",
        min_args: 0,
        max_args: None,
        typing: Special,
    },
    BuiltinSpec {
        name: "str",
        sig: "a … → Str",
        doc: "convert/concatenate values to a string",
        min_args: 0,
        max_args: None,
        typing: Special,
    },
    f(
        "assert-eq",
        "a → a → ()",
        "panic unless both values are equal",
        2,
        2,
        Special,
    ),
    // ── Collections ─────────────────────────────────────────────────
    f(
        "len",
        "Vec a | Map k v | Str → Int",
        "number of elements / chars",
        1,
        1,
        Special,
    ),
    f(
        "get",
        "Map k v → k → v | Vec a → Int → a",
        "lookup by key/index",
        2,
        3,
        Special,
    ),
    f("nth", "Vec a → Int → a", "element at index", 2, 2, Special),
    f("first", "Vec a → a", "first element", 1, 1, Special),
    f("last", "Vec a → a", "last element", 1, 1, Special),
    f(
        "range",
        "Int → Int → Vec Int",
        "half-open integer range",
        1,
        2,
        Special,
    ),
    f(
        "empty?",
        "coll → Bool",
        "true when the collection/string is empty",
        1,
        1,
        Special,
    ),
    f(
        "contains?",
        "coll → a → Bool",
        "membership test (collections and strings)",
        2,
        2,
        Special,
    ),
    f(
        "conj",
        "Vec a → a → Vec a",
        "append an element",
        2,
        2,
        Special,
    ),
    f(
        "cons",
        "a → Vec a → Vec a",
        "prepend an element",
        2,
        2,
        Special,
    ),
    f(
        "assoc",
        "Map k v → k → v → Map k v",
        "insert/replace a key",
        3,
        3,
        Special,
    ),
    f(
        "update",
        "Map k v → k → (v → v) → Map k v",
        "transform the value at a key",
        3,
        3,
        Special,
    ),
    f(
        "merge",
        "Map k v → Map k v → Map k v",
        "right-biased map merge",
        2,
        2,
        Special,
    ),
    f(
        "remove",
        "Map k v → k → Map k v",
        "drop a key (maps) / element (sets)",
        2,
        2,
        Special,
    ),
    f(
        "entries",
        "Map k v → Vec (k, v)",
        "key-value pairs",
        1,
        1,
        Special,
    ),
    f("keys", "Map k v → Vec k", "map keys", 1, 1, Special),
    f("vals", "Map k v → Vec v", "map values", 1, 1, Special),
    f("values", "Map k v → Vec v", "alias of vals", 1, 1, Special),
    f("sort", "Vec a → Vec a", "sort ascending", 1, 1, Special),
    f(
        "sort-by",
        "(a → k) → Vec a → Vec a",
        "sort by key function",
        2,
        2,
        Special,
    ),
    f("reverse", "Vec a → Vec a", "reverse order", 1, 1, Special),
    f(
        "flatten",
        "Vec (Vec a) → Vec a",
        "flatten one level",
        1,
        1,
        Special,
    ),
    f(
        "zip",
        "Vec a → Vec b → Vec (a, b)",
        "pair up two vectors",
        2,
        2,
        Special,
    ),
    f(
        "chunk",
        "Vec a → Int → Vec (Vec a)",
        "split into chunks of n",
        2,
        2,
        Special,
    ),
    f(
        "take",
        "Int → Vec a → Vec a",
        "first n elements",
        2,
        2,
        Special,
    ),
    f(
        "drop",
        "Int → Vec a → Vec a",
        "all but the first n",
        2,
        2,
        Special,
    ),
    f(
        "slice",
        "Vec a | Str → Int → Int → same",
        "sub-range [start, end)",
        3,
        3,
        Special,
    ),
    f(
        "concat",
        "Vec a → Vec a → Vec a",
        "concatenate collections",
        2,
        2,
        Special,
    ),
    f(
        "find",
        "(a → Bool) → Vec a → a",
        "first element matching predicate",
        2,
        2,
        Special,
    ),
    f(
        "index-of",
        "Vec a → a → Int | Str → Str → Int",
        "index of element/substring, -1 if absent",
        2,
        2,
        Special,
    ),
    f(
        "any?",
        "(a → Bool) → Vec a → Bool",
        "true if any element matches",
        2,
        2,
        Special,
    ),
    f(
        "all?",
        "(a → Bool) → Vec a → Bool",
        "true if all elements match",
        2,
        2,
        Special,
    ),
    f(
        "map",
        "(a → b) → Vec a → Vec b",
        "transform each element",
        2,
        2,
        Special,
    ),
    f(
        "filter",
        "(a → Bool) → Vec a → Vec a",
        "keep matching elements",
        2,
        2,
        Special,
    ),
    f(
        "fold",
        "b → (b → a → b) → Vec a → b",
        "left fold with initial accumulator",
        2,
        3,
        Special,
    ),
    f(
        "reduce",
        "b → (b → a → b) → Vec a → b",
        "alias of fold",
        2,
        3,
        Special,
    ),
    f(
        "each",
        "(a → ()) → Vec a → ()",
        "run a function for each element",
        2,
        2,
        Special,
    ),
    f(
        "flat-map",
        "(a → Vec b) → Vec a → Vec b",
        "map then flatten",
        2,
        2,
        Special,
    ),
    f(
        "group-by",
        "(a → k) → Vec a → Map k (Vec a)",
        "group elements by key function",
        2,
        2,
        Special,
    ),
    f(
        "into-map",
        "Vec (k, v) → Map k v",
        "build a map from pairs",
        1,
        1,
        Special,
    ),
    f(
        "collect",
        "Rx a → Vec a",
        "drain a channel into a vector",
        1,
        1,
        Special,
    ),
    f(
        "sum",
        "Vec Num → Num",
        "sum of numeric vector",
        1,
        1,
        Special,
    ),
    f("min", "Vec a → a", "smallest element", 1, 1, Special),
    f("max", "Vec a → a", "largest element", 1, 1, Special),
    // ── Strings ─────────────────────────────────────────────────────
    f(
        "split",
        "Str → Str → Vec Str",
        "split on separator",
        2,
        2,
        Special,
    ),
    f(
        "join",
        "Str → Vec Str → Str",
        "join with separator",
        2,
        2,
        Special,
    ),
    f(
        "trim",
        "Str → Str",
        "strip surrounding whitespace",
        1,
        1,
        Mono(&[Str], Str),
    ),
    f(
        "starts-with?",
        "Str → Str → Bool",
        "prefix test",
        2,
        2,
        Mono(&[Str, Str], Bool),
    ),
    f(
        "ends-with?",
        "Str → Str → Bool",
        "suffix test",
        2,
        2,
        Mono(&[Str, Str], Bool),
    ),
    f(
        "replace",
        "Str → Str → Str → Str",
        "replace all occurrences",
        3,
        3,
        Mono(&[Str, Str, Str], Str),
    ),
    f(
        "uppercase",
        "Str → Str",
        "upper-case",
        1,
        1,
        Mono(&[Str], Str),
    ),
    f(
        "lowercase",
        "Str → Str",
        "lower-case",
        1,
        1,
        Mono(&[Str], Str),
    ),
    f(
        "capitalize",
        "Str → Str",
        "upper-case the first character",
        1,
        1,
        Mono(&[Str], Str),
    ),
    f(
        "pad-left",
        "Str → Int → Str → Str",
        "left-pad to width with pad string",
        3,
        3,
        Mono(&[Str, Int, Str], Str),
    ),
    f(
        "pad-right",
        "Str → Int → Str → Str",
        "right-pad to width with pad string",
        3,
        3,
        Mono(&[Str, Int, Str], Str),
    ),
    f(
        "repeat",
        "Str → Int → Str",
        "repeat a string n times",
        2,
        2,
        Mono(&[Str, Int], Str),
    ),
    f(
        "char-at",
        "Str → Int → Str",
        "character at index",
        2,
        2,
        Mono(&[Str, Int], Str),
    ),
    f(
        "substring",
        "Str → Int → Int → Str",
        "substring [start, end)",
        3,
        3,
        Mono(&[Str, Int, Int], Str),
    ),
    // ── Conversion / predicates ─────────────────────────────────────
    f(
        "int",
        "Num | Str → Int",
        "truncate to integer / parse (unparseable → ())",
        1,
        1,
        Special,
    ),
    f(
        "float",
        "Num | Str → Float",
        "convert to float / parse (unparseable → ())",
        1,
        1,
        Special,
    ),
    f(
        "parse-int",
        "Str → Option Int",
        "parse an integer, None on failure",
        1,
        1,
        Mono(&[Str], OptionInt),
    ),
    f(
        "parse-float",
        "Str → Option Float",
        "parse a float, None on failure",
        1,
        1,
        Mono(&[Str], OptionFloat),
    ),
    f(
        "keyword",
        "Str → Keyword",
        "string to keyword",
        1,
        1,
        Special,
    ),
    f(
        "keywordize-keys",
        "Map Str v → Map Keyword v",
        "convert string keys to keywords",
        1,
        1,
        Special,
    ),
    f("name", "Keyword → Str", "keyword to string", 1, 1, Special),
    f("type-of", "a → Str", "runtime type name", 1, 1, Special),
    f(
        "not",
        "Bool → Bool",
        "logical negation",
        1,
        1,
        Mono(&[Bool], Bool),
    ),
    f("map?", "a → Bool", "true for maps", 1, 1, Special),
    f("vec?", "a → Bool", "true for vectors", 1, 1, Special),
    f(
        "some?",
        "a → Bool",
        "false for None/(), true otherwise",
        1,
        1,
        Special,
    ),
    f("none?", "a → Bool", "true for None/()", 1, 1, Special),
    f("nil?", "a → Bool", "alias of none?", 1, 1, Special),
    // ── Math ────────────────────────────────────────────────────────
    f("abs", "Num a ⇒ a → a", "absolute value", 1, 1, Special),
    f(
        "sqrt",
        "Num → Float",
        "square root",
        1,
        1,
        Mono(&[Num], Float),
    ),
    f(
        "pow",
        "Num → Num → Float",
        "base to the power of exponent",
        2,
        2,
        Mono(&[Num, Num], Float),
    ),
    f(
        "floor",
        "Num → Int",
        "round down to integer",
        1,
        1,
        Mono(&[Num], Int),
    ),
    f(
        "ceil",
        "Num → Int",
        "round up to integer",
        1,
        1,
        Mono(&[Num], Int),
    ),
    f(
        "round",
        "Num → Int",
        "round half away from zero to integer",
        1,
        1,
        Mono(&[Num], Int),
    ),
    f(
        "sin",
        "Num → Float",
        "sine (radians)",
        1,
        1,
        Mono(&[Num], Float),
    ),
    f(
        "cos",
        "Num → Float",
        "cosine (radians)",
        1,
        1,
        Mono(&[Num], Float),
    ),
    f(
        "tan",
        "Num → Float",
        "tangent (radians)",
        1,
        1,
        Mono(&[Num], Float),
    ),
    f("asin", "Num → Float", "arcsine", 1, 1, Mono(&[Num], Float)),
    f(
        "acos",
        "Num → Float",
        "arccosine",
        1,
        1,
        Mono(&[Num], Float),
    ),
    f(
        "atan",
        "Num → Float",
        "arctangent",
        1,
        1,
        Mono(&[Num], Float),
    ),
    f(
        "atan2",
        "Num → Num → Float",
        "arctangent of y/x using signs",
        2,
        2,
        Mono(&[Num, Num], Float),
    ),
    f(
        "log",
        "Num → Float",
        "natural logarithm",
        1,
        1,
        Mono(&[Num], Float),
    ),
    f(
        "log10",
        "Num → Float",
        "base-10 logarithm",
        1,
        1,
        Mono(&[Num], Float),
    ),
    f(
        "exp",
        "Num → Float",
        "e to the power of x",
        1,
        1,
        Mono(&[Num], Float),
    ),
    f("pi", "Float", "π ≈ 3.14159", 0, 0, Const(Float)),
    f("e", "Float", "Euler's number ≈ 2.71828", 0, 0, Const(Float)),
];

/// Look up a builtin by name.
pub fn lookup(name: &str) -> Option<&'static BuiltinSpec> {
    BUILTINS.iter().find(|b| b.name == name)
}

/// True when the name is a registry constant (lowered to a literal).
pub fn is_const(name: &str) -> bool {
    matches!(
        lookup(name),
        Some(BuiltinSpec {
            typing: Const(_),
            ..
        })
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_duplicate_names() {
        let mut seen = std::collections::HashSet::new();
        for b in BUILTINS {
            assert!(seen.insert(b.name), "duplicate registry entry: {}", b.name);
        }
    }

    #[test]
    fn arity_forms_are_sane() {
        for b in BUILTINS {
            if let Some(max) = b.max_args {
                assert!(b.min_args <= max, "{}: min_args > max_args", b.name);
            }
            if let Typing::Mono(params, _) = b.typing {
                assert_eq!(
                    params.len(),
                    b.max_args.unwrap_or(b.min_args) as usize,
                    "{}: Mono param count must match max arity",
                    b.name
                );
            }
        }
    }
}
