use super::codes::ErrorCode;

pub struct Tutorial {
    pub code: ErrorCode,
    pub title: String,
    pub steps: Vec<TutorialStep>,
}

pub enum TutorialStep {
    Text(String),
    Demo {
        code: String,
        explanation: String,
    },
    Fix {
        before: String,
        after: String,
        explanation: String,
    },
    Try {
        prompt: String,
        hint: String,
        expected_output: Option<String>,
    },
}

pub fn get_tutorial(code: ErrorCode) -> Option<Tutorial> {
    match code {
        ErrorCode::E0100 => Some(tutorial_unexpected_char()),
        ErrorCode::E0101 => Some(tutorial_unexpected_token()),
        ErrorCode::E0102 => Some(tutorial_unclosed_delimiter()),
        ErrorCode::E0103 => Some(tutorial_invalid_literal()),
        ErrorCode::E0200 => Some(tutorial_type_mismatch()),
        ErrorCode::E0201 => Some(tutorial_unbound_symbol()),
        ErrorCode::E0202 => Some(tutorial_arity_mismatch()),
        ErrorCode::E0203 => Some(tutorial_infinite_type()),
        ErrorCode::E0204 => Some(tutorial_signature_mismatch()),
        ErrorCode::E0205 => Some(tutorial_missing_trait_impl()),
        ErrorCode::E0206 => Some(tutorial_non_exhaustive_match()),
        ErrorCode::E0207 => Some(tutorial_field_mismatch()),
        ErrorCode::E0300 => Some(tutorial_use_after_move()),
        ErrorCode::E0301 => Some(tutorial_mutate_immutable()),
        ErrorCode::E0302 => Some(tutorial_double_borrow()),
        ErrorCode::E0400 => Some(tutorial_unhandled_effect()),
        ErrorCode::E0401 => Some(tutorial_undeclared_effect()),
        ErrorCode::E0500 => Some(tutorial_unresolved_module()),
        ErrorCode::E0501 => Some(tutorial_private_symbol()),
        ErrorCode::E0502 => Some(tutorial_circular_dependency()),
    }
}

// ── Parse errors ─────────────────────────────────────────────────────

fn tutorial_unexpected_char() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0100,
        title: "Unexpected character".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when the lexer encounters a character that \
                 isn't valid in Loon syntax. Common causes include using curly \
                 braces for grouping (use square brackets instead) or stray \
                 unicode characters."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[let x 10$]  ;; error: unexpected character '$'"#.to_string(),
                explanation: "The '$' character has no meaning in Loon.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[let x 10$]"#.to_string(),
                after: r#"[let x 10]"#.to_string(),
                explanation: "Remove the invalid character.".to_string(),
            },
            TutorialStep::Try {
                prompt: "Write a valid let binding that assigns 42 to a variable.".to_string(),
                hint: "Try: [let answer 42]".to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_unexpected_token() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0101,
        title: "Unexpected token".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when the parser encounters a token in an \
                 unexpected position. For example, two values next to each other \
                 without a function call, or a missing closing bracket."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[let x 1 2]  ;; error: unexpected token"#.to_string(),
                explanation: "`let` expects exactly one name and one value.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[let x 1 2]"#.to_string(),
                after: r#"[let x [+ 1 2]]"#.to_string(),
                explanation: "If you meant to add them, wrap in a function call.".to_string(),
            },
            TutorialStep::Try {
                prompt: "Bind `total` to the sum of 10 and 20.".to_string(),
                hint: "Try: [let total [+ 10 20]]".to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_unclosed_delimiter() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0102,
        title: "Unclosed delimiter".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when an opening bracket `[`, `{`, or `#[` \
                 is never closed. Every opening delimiter must have a matching \
                 closing delimiter."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[let x [+ 1 2]  ;; error: unclosed delimiter '['"#.to_string(),
                explanation: "The outer `[` from `[let ...` is never closed.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[let x [+ 1 2]"#.to_string(),
                after: r#"[let x [+ 1 2]]"#.to_string(),
                explanation: "Add the missing closing `]`.".to_string(),
            },
            TutorialStep::Try {
                prompt: "Write a nested expression that computes [* 2 [+ 3 4]].".to_string(),
                hint: "Make sure every [ has a matching ].".to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_invalid_literal() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0103,
        title: "Invalid literal".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when a literal value is malformed. For \
                 example, an unterminated string or an invalid number format."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[let s "hello]  ;; error: unterminated string literal"#.to_string(),
                explanation: "The string is missing its closing quote.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[let s "hello]"#.to_string(),
                after: r#"[let s "hello"]"#.to_string(),
                explanation: "Close the string with a matching double quote.".to_string(),
            },
            TutorialStep::Try {
                prompt: "Create a valid string binding.".to_string(),
                hint: r#"Try: [let greeting "hi there"]"#.to_string(),
                expected_output: None,
            },
        ],
    }
}

// ── Type errors ──────────────────────────────────────────────────────

fn tutorial_type_mismatch() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0200,
        title: "Type mismatch".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when the type checker finds a type that \
                 doesn't match what was expected. For example, passing a String \
                 where an Int is expected."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[+ 1 "hello"]  ;; error: cannot unify Int with String"#.to_string(),
                explanation: "The + operator expects both arguments to be the same numeric type."
                    .to_string(),
            },
            TutorialStep::Fix {
                before: r#"[+ 1 "2"]"#.to_string(),
                after: r#"[+ 1 [int "2"]]"#.to_string(),
                explanation: "Convert the String to an Int with the `int` function.".to_string(),
            },
            TutorialStep::Try {
                prompt: "Add two integers together using [+].".to_string(),
                hint: "Try: [+ 10 20]".to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_unbound_symbol() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0201,
        title: "Unbound symbol".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when you use a name that hasn't been defined \
                 in the current scope. Common causes include typos, forgetting \
                 to import a module, or using a variable before it's defined."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[println mesage]  ;; error: unbound symbol 'mesage'"#.to_string(),
                explanation: "The name 'mesage' is not defined. Did you mean 'message'?"
                    .to_string(),
            },
            TutorialStep::Fix {
                before: r#"[println mesage]"#.to_string(),
                after: r#"[let message "hello"]
[println message]"#
                    .to_string(),
                explanation: "Define the binding before using it, or fix the typo.".to_string(),
            },
            TutorialStep::Try {
                prompt: "Define a binding and use it in a println call.".to_string(),
                hint: r#"Try: [let name "loon"] [println name]"#.to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_arity_mismatch() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0202,
        title: "Arity mismatch".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when a function is called with the wrong \
                 number of arguments. Every function in Loon has a fixed arity \
                 determined by its parameter list."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[fn greet [name] [str "hi " name]]
[greet "alice" "bob"]  ;; error: arity mismatch"#
                    .to_string(),
                explanation: "`greet` takes 1 argument but was called with 2.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[greet "alice" "bob"]"#.to_string(),
                after: r#"[greet "alice"]"#.to_string(),
                explanation: "Pass exactly the number of arguments the function expects."
                    .to_string(),
            },
            TutorialStep::Try {
                prompt: "Define a 2-argument function and call it correctly.".to_string(),
                hint: "Try: [fn add [a b] [+ a b]] [add 1 2]".to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_infinite_type() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0203,
        title: "Infinite type".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when the type checker detects a type that \
                 would be infinitely recursive — a type variable unifying with \
                 a type that contains itself. This usually means a function is \
                 being passed as its own argument."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[fn apply [f] [f f]]  ;; error: infinite type"#.to_string(),
                explanation: "`f` would need type `(a -> b)` where `a = (a -> b)`, \
                              creating an infinite loop in the type."
                    .to_string(),
            },
            TutorialStep::Fix {
                before: r#"[fn apply [f] [f f]]"#.to_string(),
                after: r#"[fn apply [f x] [f x]]"#.to_string(),
                explanation: "Restructure so the function receives a separate argument."
                    .to_string(),
            },
            TutorialStep::Try {
                prompt: "Write a function that applies a function to a value.".to_string(),
                hint: "Try: [fn apply [f x] [f x]] [apply str 42]".to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_signature_mismatch() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0204,
        title: "Signature mismatch".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when a function's type annotation doesn't \
                 match its actual inferred type. The declared signature must \
                 agree with what the body returns."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[fn double [x : Int] : String [* x 2]]  ;; error: signature mismatch"#
                    .to_string(),
                explanation: "The body returns an Int, but the signature says String.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[fn double [x : Int] : String [* x 2]]"#.to_string(),
                after: r#"[fn double [x : Int] : Int [* x 2]]"#.to_string(),
                explanation: "Fix the return type to match what the body actually returns."
                    .to_string(),
            },
            TutorialStep::Try {
                prompt: "Write a function with a correct type annotation.".to_string(),
                hint: "Try: [fn inc [x : Int] : Int [+ x 1]]".to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_missing_trait_impl() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0205,
        title: "Missing trait implementation".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when a value is used in a context that \
                 requires a trait it doesn't implement. For example, using \
                 `+` on a type that doesn't support addition."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[+ true false]  ;; error: Bool does not implement Add"#.to_string(),
                explanation: "The `+` operator requires the Add trait, which Bool doesn't have."
                    .to_string(),
            },
            TutorialStep::Fix {
                before: r#"[+ true false]"#.to_string(),
                after: r#"[or true false]"#.to_string(),
                explanation: "Use the correct operation for the type.".to_string(),
            },
            TutorialStep::Try {
                prompt: "Use an arithmetic operator on two numbers.".to_string(),
                hint: "Try: [* 6 7]".to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_non_exhaustive_match() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0206,
        title: "Non-exhaustive match".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when a `match` expression doesn't cover \
                 all possible variants of the matched type. Every branch of \
                 an ADT must be handled."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[type Color Red Green Blue]
[let c Red]
[match c
  Red  => "red"
  Green => "green"]  ;; error: non-exhaustive, missing Blue"#
                    .to_string(),
                explanation: "The `Blue` variant is not handled.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[match c
  Red   => "red"
  Green => "green"]"#
                    .to_string(),
                after: r#"[match c
  Red   => "red"
  Green => "green"
  Blue  => "blue"]"#
                    .to_string(),
                explanation: "Add the missing variant, or use a wildcard `_` to catch the rest."
                    .to_string(),
            },
            TutorialStep::Try {
                prompt: "Define a simple enum and write an exhaustive match over it.".to_string(),
                hint: "Try: [type Dir Left Right] [match Left Left => 0 Right => 1]".to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_field_mismatch() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0207,
        title: "Field mismatch".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when a record or struct is constructed with \
                 the wrong fields — either missing fields, extra fields, or \
                 fields with the wrong types."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[type Point {:x Int :y Int}]
[Point {:x 1}]  ;; error: missing field 'y'"#
                    .to_string(),
                explanation: "The `Point` type requires both `:x` and `:y` fields.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[Point {:x 1}]"#.to_string(),
                after: r#"[Point {:x 1 :y 2}]"#.to_string(),
                explanation: "Provide all required fields with the correct types.".to_string(),
            },
            TutorialStep::Try {
                prompt: "Create a record with two fields and construct it correctly.".to_string(),
                hint: "Try: [type Pair {:a Int :b Int}] [Pair {:a 10 :b 20}]".to_string(),
                expected_output: None,
            },
        ],
    }
}

// ── Ownership errors ─────────────────────────────────────────────────

fn tutorial_use_after_move() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0300,
        title: "Use after move".to_string(),
        steps: vec![
            TutorialStep::Text(
                "In Loon, when a non-Copy value (like a Vec or String) is passed \
                 to a function that consumes it, ownership transfers and the \
                 original binding becomes invalid. This prevents use-after-free \
                 bugs and data races."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[fn take [s] s]
[let name "alice"]
[take name]
[println name]     ;; error: use of moved value 'name'"#
                    .to_string(),
                explanation: "After `[take name]`, the value of `name` has been moved \
                              into the function and can no longer be used."
                    .to_string(),
            },
            TutorialStep::Fix {
                before: r#"[take name]
[println name]"#
                    .to_string(),
                after: r#"[take [clone name]]
[println name]"#
                    .to_string(),
                explanation: "Clone the value before passing it to a consuming function, \
                              or restructure your code to avoid reusing a moved value."
                    .to_string(),
            },
            TutorialStep::Try {
                prompt: "Use clone to keep a value alive after passing it to a function."
                    .to_string(),
                hint: r#"Try: [fn consume [s] s] [let x "hi"] [consume [clone x]] [println x]"#
                    .to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_mutate_immutable() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0301,
        title: "Mutating an immutable binding".to_string(),
        steps: vec![
            TutorialStep::Text(
                "Loon bindings are immutable by default. To mutate a binding \
                 (e.g. with push!), you must declare it with `let mut`."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[let v #[1 2 3]]
[push! v 4]        ;; error: cannot mutably borrow immutable binding 'v'"#
                    .to_string(),
                explanation: "`v` was declared with `let`, not `let mut`.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[let v #[1 2 3]]
[push! v 4]"#
                    .to_string(),
                after: r#"[let mut v #[1 2 3]]
[push! v 4]"#
                    .to_string(),
                explanation: "Add `mut` to the binding declaration.".to_string(),
            },
            TutorialStep::Try {
                prompt: "Create a mutable vector and push a value into it.".to_string(),
                hint: "Try: [let mut xs #[1 2]] [push! xs 3]".to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_double_borrow() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0302,
        title: "Double mutable borrow".to_string(),
        steps: vec![
            TutorialStep::Text(
                "Loon enforces exclusive mutable access: a value can only have \
                 one mutable borrow at a time. This prevents data races and \
                 iterator invalidation."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[let mut xs #[1 2 3]]
[push! xs [len xs]]  ;; error: cannot borrow 'xs' as mutable more than once"#
                    .to_string(),
                explanation: "`push!` needs a mutable borrow of `xs`, but `len` also \
                              borrows `xs` in the same expression."
                    .to_string(),
            },
            TutorialStep::Fix {
                before: r#"[push! xs [len xs]]"#.to_string(),
                after: r#"[let n [len xs]]
[push! xs n]"#
                    .to_string(),
                explanation: "Compute the value in a separate let-binding first.".to_string(),
            },
            TutorialStep::Try {
                prompt: "Use a let binding to avoid a double borrow.".to_string(),
                hint: "Try: [let mut v #[1 2]] [let n [len v]] [push! v n]".to_string(),
                expected_output: None,
            },
        ],
    }
}

// ── Effect errors ────────────────────────────────────────────────────

fn tutorial_unhandled_effect() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0400,
        title: "Unhandled effect".to_string(),
        steps: vec![
            TutorialStep::Text(
                "Loon tracks side effects in the type system. When a function \
                 performs an effect (like IO or Fail), it must be handled by \
                 an enclosing `handle` block or declared in the function's \
                 effect annotation."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[fn load [path] / #{Fail}
  [IO.read-file path]]  ;; error: undeclared effect 'IO'"#
                    .to_string(),
                explanation: "The function performs IO but only declares Fail.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[fn load [path] / #{Fail} [IO.read-file path]]"#.to_string(),
                after: r#"[fn load [path] / #{IO Fail} [IO.read-file path]]"#.to_string(),
                explanation: "Add IO to the effect set, or wrap the call in a handle block."
                    .to_string(),
            },
            TutorialStep::Try {
                prompt: "Write a pure function (no effects) that adds two numbers.".to_string(),
                hint: "Try: [fn add [a b] [+ a b]] [add 3 4]".to_string(),
                expected_output: None,
            },
        ],
    }
}

fn tutorial_undeclared_effect() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0401,
        title: "Undeclared effect".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when a function performs an effect that \
                 isn't listed in its effect annotation. Every effect a function \
                 can perform must be declared in its `/ #{...}` set."
                    .to_string(),
            ),
            TutorialStep::Demo {
                code: r#"[fn save [data] / #{}
  [IO.write-file "out.txt" data]]  ;; error: undeclared effect 'IO'"#
                    .to_string(),
                explanation: "The effect set is empty `#{}` but the body performs IO.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[fn save [data] / #{} [IO.write-file "out.txt" data]]"#.to_string(),
                after: r#"[fn save [data] / #{IO} [IO.write-file "out.txt" data]]"#.to_string(),
                explanation: "Add the missing effect to the annotation.".to_string(),
            },
            TutorialStep::Try {
                prompt: "Write a pure function with no effects.".to_string(),
                hint: "Try: [fn square [x] [* x x]] [square 5]".to_string(),
                expected_output: None,
            },
        ],
    }
}

// ── Module errors (text-only, no live execution) ─────────────────────

fn tutorial_unresolved_module() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0500,
        title: "Unresolved module".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when an `import` refers to a module that \
                 can't be found. Loon resolves modules relative to the current \
                 file's directory."
                    .to_string(),
            ),
            TutorialStep::Text(
                "  Example:\n\
                 \n\
                 \x20   [import utils]       ;; error: cannot find module 'utils'\n\
                 \n\
                 The file `utils.loon` doesn't exist in the expected location."
                    .to_string(),
            ),
            TutorialStep::Text(
                "  Fix: Create the file or correct the path.\n\
                 \n\
                 \x20   ;; Create utils.loon in the same directory:\n\
                 \x20   ;; utils.loon\n\
                 \x20   [fn helper [] 42]\n\
                 \n\
                 \x20   ;; main.loon\n\
                 \x20   [import utils]\n\
                 \x20   [utils.helper]"
                    .to_string(),
            ),
        ],
    }
}

fn tutorial_private_symbol() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0501,
        title: "Private symbol".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when you try to use a symbol from another \
                 module that isn't exported. By default, only top-level `fn` \
                 definitions are public."
                    .to_string(),
            ),
            TutorialStep::Text(
                "  Example:\n\
                 \n\
                 \x20   ;; helpers.loon\n\
                 \x20   [let secret 42]\n\
                 \n\
                 \x20   ;; main.loon\n\
                 \x20   [import helpers]\n\
                 \x20   [println helpers.secret]  ;; error: 'secret' is private"
                    .to_string(),
            ),
            TutorialStep::Text(
                "  Fix: Use `fn` to export, or access through a public function.\n\
                 \n\
                 \x20   ;; helpers.loon\n\
                 \x20   [fn get-secret [] 42]\n\
                 \n\
                 \x20   ;; main.loon\n\
                 \x20   [import helpers]\n\
                 \x20   [println [helpers.get-secret]]"
                    .to_string(),
            ),
        ],
    }
}

fn tutorial_circular_dependency() -> Tutorial {
    Tutorial {
        code: ErrorCode::E0502,
        title: "Circular dependency".to_string(),
        steps: vec![
            TutorialStep::Text(
                "This error occurs when two or more modules import each other, \
                 creating a cycle. Loon doesn't support circular module \
                 dependencies."
                    .to_string(),
            ),
            TutorialStep::Text(
                "  Example:\n\
                 \n\
                 \x20   ;; a.loon\n\
                 \x20   [import b]\n\
                 \n\
                 \x20   ;; b.loon\n\
                 \x20   [import a]   ;; error: circular dependency: a -> b -> a"
                    .to_string(),
            ),
            TutorialStep::Text(
                "  Fix: Break the cycle by extracting shared code into a third module.\n\
                 \n\
                 \x20   ;; shared.loon\n\
                 \x20   [fn helper [] 42]\n\
                 \n\
                 \x20   ;; a.loon\n\
                 \x20   [import shared]\n\
                 \n\
                 \x20   ;; b.loon\n\
                 \x20   [import shared]"
                    .to_string(),
            ),
        ],
    }
}
