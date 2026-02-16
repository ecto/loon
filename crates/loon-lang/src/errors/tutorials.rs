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
}

pub fn get_tutorial(code: ErrorCode) -> Option<Tutorial> {
    match code {
        ErrorCode::E0201 => Some(tutorial_unbound_symbol()),
        ErrorCode::E0200 => Some(tutorial_type_mismatch()),
        ErrorCode::E0300 => Some(tutorial_use_after_move()),
        ErrorCode::E0301 => Some(tutorial_mutate_immutable()),
        ErrorCode::E0302 => Some(tutorial_double_borrow()),
        ErrorCode::E0400 => Some(tutorial_unhandled_effect()),
        _ => None,
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
        ],
    }
}

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
        ],
    }
}

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
                code: r#"[defn take [s] s]
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
        ],
    }
}

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
                code: r#"[defn load [path] / #{Fail}
  [IO.read-file path]]  ;; error: undeclared effect 'IO'"#
                    .to_string(),
                explanation: "The function performs IO but only declares Fail.".to_string(),
            },
            TutorialStep::Fix {
                before: r#"[defn load [path] / #{Fail} [IO.read-file path]]"#.to_string(),
                after: r#"[defn load [path] / #{IO Fail} [IO.read-file path]]"#.to_string(),
                explanation: "Add IO to the effect set, or wrap the call in a handle block."
                    .to_string(),
            },
        ],
    }
}
