//! A malformed document must produce an error, never a panic.
//!
//! Hosts evaluate loon in-process (vcad.app recompiles the open document on
//! every edit), so a panic in a builtin aborts the whole app. The crash that
//! prompted this: a mid-edit `[- 1]` indexed `args[1]` and took vcad.app down.

use loon_lang::interp::builtins::capture_output;
use loon_lang::interp::{register_builtins_pub, Env, Value};
use std::panic::{catch_unwind, AssertUnwindSafe};

fn builtins() -> Vec<(String, Value)> {
    let mut env = Env::new();
    register_builtins_pub(&mut env);
    let mut out: Vec<(String, Value)> = env
        .globals()
        .into_iter()
        .filter(|(_, v)| matches!(v, Value::Builtin(..)))
        .collect();
    out.sort_by(|a, b| a.0.cmp(&b.0));
    out
}

fn call(f: &Value, args: &[Value]) -> Result<Value, String> {
    let Value::Builtin(name, f) = f else {
        unreachable!()
    };
    f(name, args).map_err(|e| e.message)
}

/// Builtins that legitimately take no arguments.
const NULLARY: &[&str] = &[
    "and",
    "channel",
    "HashMap.new",
    "or",
    "print",
    "println",
    "str",
];

#[test]
fn every_builtin_with_no_args_errors_instead_of_panicking() {
    for (name, f) in builtins() {
        let result = catch_unwind(AssertUnwindSafe(|| capture_output(|| call(&f, &[])).0));
        let Ok(result) = result else {
            panic!("[{name}] with no arguments panicked");
        };
        if NULLARY.contains(&name.as_str()) {
            continue;
        }
        let msg = result.expect_err(&format!("[{name}] with no arguments should be an error"));
        assert!(
            msg.contains("requires"),
            "[{name}]: unhelpful arity error {msg:?}"
        );
    }
}

/// Every builtin, called with 0..=3 arguments drawn from a spread of kinds,
/// returns (Ok or Err) without panicking.
#[test]
fn every_builtin_survives_any_short_or_mistyped_call() {
    let mut env = Env::new();
    register_builtins_pub(&mut env);
    let callable = env.get("first").unwrap();
    let kinds = [
        Value::Int(0),
        Value::Int(-1),
        Value::Int(i64::MIN),
        Value::Float(f64::NAN),
        Value::Str("héllo".into()),
        Value::Keyword("desc".into()),
        Value::Vec(imbl::vector![
            Value::Int(2),
            Value::Float(1.5),
            Value::Str("a".into())
        ]),
        Value::Map(Default::default()),
        Value::Unit,
        callable,
    ];
    let mut calls = 0usize;
    for (name, f) in builtins() {
        let mut stack: Vec<Vec<Value>> = vec![vec![]];
        while let Some(args) = stack.pop() {
            let result = catch_unwind(AssertUnwindSafe(|| capture_output(|| call(&f, &args)).0));
            assert!(result.is_ok(), "[{name} {args:?}] panicked");
            calls += 1;
            if args.len() < 3 {
                for k in &kinds {
                    let mut next = args.clone();
                    next.push(k.clone());
                    stack.push(next);
                }
            }
        }
    }
    assert!(calls > 10_000, "fuzzed only {calls} calls");
}

#[test]
fn arity_errors_name_the_builtin_and_the_count() {
    let all = builtins();
    let get = |n: &str| all.iter().find(|(k, _)| k == n).unwrap().1.clone();
    let one = [Value::Int(1)];
    assert_eq!(
        call(&get("-"), &one).unwrap_err(),
        "- requires 2 arguments, got 1"
    );
    assert_eq!(
        call(&get("+"), &[]).unwrap_err(),
        "+ requires at least 2 arguments, got 0"
    );
    assert_eq!(
        call(&get("nth"), &one).unwrap_err(),
        "nth requires 2 to 3 arguments, got 1"
    );
    assert_eq!(
        call(&get("trim"), &[]).unwrap_err(),
        "trim requires 1 argument, got 0"
    );
}

#[test]
fn partials_returned_by_builtins_are_guarded_too() {
    let all = builtins();
    let get = |n: &str| all.iter().find(|(k, _)| k == n).unwrap().1.clone();
    let f = get("first");
    for name in [
        "map", "filter", "each", "group-by", "flat-map", "take", "drop",
    ] {
        let arg = if matches!(name, "take" | "drop") {
            Value::Int(1)
        } else {
            f.clone()
        };
        let partial = call(&get(name), &[arg]).unwrap();
        let msg = call(&partial, &[]).unwrap_err();
        assert!(msg.contains("requires 1 argument"), "{name}-partial: {msg}");
    }
    let partial = call(&get("fold"), &[Value::Int(0), get("+")]).unwrap();
    assert!(call(&partial, &[]).is_err());
    let partial = call(&get("sort-by"), &[f, Value::Keyword("desc".into())]).unwrap();
    assert!(call(&partial, &[]).is_err());
}

/// Extra arguments stay ignored (thread-last pipe partials rely on it).
#[test]
fn extra_arguments_are_still_ignored() {
    let all = builtins();
    let get = |n: &str| all.iter().find(|(k, _)| k == n).unwrap().1.clone();
    let r = call(&get("-"), &[Value::Int(10), Value::Int(3), Value::Int(5)]).unwrap();
    assert_eq!(r, Value::Int(7));
}

#[test]
fn integer_edge_cases_do_not_panic() {
    let all = builtins();
    let get = |n: &str| all.iter().find(|(k, _)| k == n).unwrap().1.clone();
    let (min, neg1, max) = (Value::Int(i64::MIN), Value::Int(-1), Value::Int(i64::MAX));
    assert_eq!(call(&get("/"), &[min.clone(), neg1.clone()]).unwrap(), min);
    assert_eq!(
        call(&get("%"), &[min.clone(), neg1.clone()]).unwrap(),
        Value::Int(0)
    );
    assert_eq!(call(&get("+"), &[max.clone(), Value::Int(1)]).unwrap(), min);
    assert_eq!(call(&get("-"), &[min.clone(), Value::Int(1)]).unwrap(), max);
    assert!(call(&get("*"), &[max.clone(), max.clone()]).is_ok());
    assert_eq!(call(&get("abs"), &[min.clone()]).unwrap(), min);
    let v = Value::Vec(imbl::vector![max.clone(), max]);
    assert!(call(&get("sum"), &[v]).is_ok());
}

#[test]
fn runaway_sizes_are_errors_not_aborts() {
    let all = builtins();
    let get = |n: &str| all.iter().find(|(k, _)| k == n).unwrap().1.clone();
    let v = Value::Vec(imbl::vector![Value::Int(1), Value::Int(2)]);
    // A zero or negative chunk size used to loop forever.
    assert!(call(&get("chunk"), &[Value::Int(0), v.clone()]).is_err());
    assert!(call(&get("chunk"), &[Value::Int(-3), v]).is_err());
    assert!(call(&get("range"), &[Value::Int(0), Value::Int(i64::MAX)]).is_err());
    assert!(call(&get("range"), &[Value::Int(i64::MIN), Value::Int(i64::MAX)]).is_err());
    // `str::repeat` panics on capacity overflow.
    let s = Value::Str("ab".into());
    assert!(call(&get("repeat"), &[s.clone(), Value::Int(i64::MAX)]).is_err());
    let pad = Value::Str(" ".into());
    assert!(call(&get("pad-left"), &[s, Value::Int(i64::MAX), pad]).is_err());
}

/// `sort` on mixed kinds and NaN: a non-total comparator can make std's sort
/// panic, so `value_cmp` must be a total order.
#[test]
fn sort_of_mixed_values_does_not_panic() {
    let all = builtins();
    let get = |n: &str| all.iter().find(|(k, _)| k == n).unwrap().1.clone();
    let mut items = imbl::Vector::new();
    for i in 0..200i64 {
        items.push_back(match i % 7 {
            0 => Value::Int(i * 7919 % 101),
            1 => Value::Float(f64::NAN),
            2 => Value::Float((i * 31 % 17) as f64 - 0.5),
            3 => Value::Str(format!("{}", i * 13 % 23).into()),
            4 => Value::Unit,
            5 => Value::Bool(i % 2 == 0),
            _ => Value::Vec(imbl::Vector::new()),
        });
    }
    let r = catch_unwind(AssertUnwindSafe(|| {
        call(&get("sort"), &[Value::Vec(items)])
    }));
    assert!(r.expect("sort panicked").is_ok());
    let sorted = call(
        &get("sort"),
        &[Value::Vec(imbl::vector![
            Value::Float(2.5),
            Value::Int(3),
            Value::Int(1)
        ])],
    )
    .unwrap();
    assert_eq!(
        sorted,
        Value::Vec(imbl::vector![
            Value::Int(1),
            Value::Float(2.5),
            Value::Int(3)
        ])
    );
}

/// The documents an editor holds mid-keystroke evaluate to errors.
#[test]
fn partial_documents_evaluate_to_errors() {
    let docs = [
        "[let x [- 1]]",
        "[let x [-]]",
        "[let x [+]]",
        "[let x [/ 4]]",
        "[let x [nth #[1 2]]]",
        "[let x [get {}]]",
        "[let x [assoc {} :a]]",
        "[let x [substring \"abc\" 1]]",
        "[let x [replace \"abc\" \"a\"]]",
        "[let x [cons 1]]",
        "[let x [map]]",
        "[let x [pipe #[1 2] [map]]]",
        "[let x [chunk 0 #[1 2 3]]]",
        "[fn main [] [let x [- 1]] x]",
    ];
    for src in docs {
        let exprs = loon_lang::parser::parse(src).unwrap_or_else(|e| panic!("{src}: {e:?}"));
        let r = catch_unwind(AssertUnwindSafe(|| {
            loon_lang::interp::eval_program_with_modules(&exprs, None, None, None)
        }));
        let r = r.unwrap_or_else(|_| panic!("{src} panicked"));
        assert!(r.is_err(), "{src} should be an error, got {:?}", r.ok());
    }
}

/// Runaway recursion is an error, not a native stack overflow (which aborts
/// the process), even on a small host thread like a 512 KiB dispatch thread.
#[test]
fn runaway_recursion_is_an_error_on_any_thread() {
    let docs = [
        "[fn f [x] [+ 1 [f x]]] [fn main [] [f 1]]",
        "[fn g [x] [map g #[x]]] [fn main [] [g 1]]",
        "[fn h [x] [if [> x 0] [+ 1 [+ 1 [+ 1 [+ 1 [h [- x 1]]]]]] 0]] [fn main [] [h 100000000]]",
    ];
    for stack_kb in [256usize, 512, 8 * 1024] {
        for src in docs {
            let msg = std::thread::Builder::new()
                .stack_size(stack_kb * 1024)
                .spawn(move || {
                    let exprs = loon_lang::parser::parse(src).unwrap();
                    loon_lang::interp::eval_program_with_modules(&exprs, None, None, None)
                        .map(|v| v.to_string())
                        .map_err(|e| e.message)
                })
                .unwrap()
                .join()
                .unwrap_or_else(|_| panic!("{src} panicked on a {stack_kb} KiB thread"));
            let msg = msg.expect_err(&format!("{src} should fail on a {stack_kb} KiB thread"));
            assert!(msg.contains("stack overflow"), "{src}: {msg}");
        }
    }
}

/// The guard must not get in the way of ordinary recursion.
#[test]
fn moderate_recursion_still_works() {
    // 100 deep fits an 8 MiB thread even unoptimized (~40 KiB per call).
    let v = std::thread::Builder::new()
        .stack_size(8 << 20)
        .spawn(|| {
            let src =
                "[fn sum-to [n] [if [= n 0] 0 [+ n [sum-to [- n 1]]]]] [fn main [] [sum-to 100]]";
            let exprs = loon_lang::parser::parse(src).unwrap();
            loon_lang::interp::eval_program_with_modules(&exprs, None, None, None)
                .map_err(|e| e.message)
        })
        .unwrap()
        .join()
        .unwrap();
    assert_eq!(v, Ok(Value::Int(5050)));
}

/// Absurd bracket nesting is a parse error, not a stack overflow.
#[test]
fn deep_nesting_is_a_parse_error() {
    let r = std::thread::Builder::new()
        .stack_size(512 * 1024)
        .spawn(|| {
            let src = format!("{}1{}", "[+ 1 ".repeat(200_000), "]".repeat(200_000));
            loon_lang::parser::parse(&src)
                .map(|_| ())
                .map_err(|e| e.message)
        })
        .unwrap()
        .join()
        .expect("parser panicked");
    assert_eq!(r, Err("expression nested too deeply".to_string()));
}

/// Mid-edit special forms that used to index past their arguments.
#[test]
fn incomplete_match_guard_is_an_error() {
    for src in [
        "[match 1 _ [when] 2]",
        "[fn f [x] [match x _ [when] 1]] [fn main [] [f 0]]",
    ] {
        let exprs = loon_lang::parser::parse(src).unwrap();
        let r = catch_unwind(AssertUnwindSafe(|| {
            loon_lang::interp::eval_program_with_modules(&exprs, None, None, None)
        }))
        .unwrap_or_else(|_| panic!("{src} panicked"));
        let msg = r.expect_err(src).message;
        assert!(msg.contains("[when] needs a condition"), "{src}: {msg}");
    }
    // The type checker (reachable through `catch-errors`) too.
    let exprs = loon_lang::parser::parse("[catch-errors \"[match 1 _ [when] 2]\"]").unwrap();
    let r = catch_unwind(AssertUnwindSafe(|| {
        loon_lang::interp::eval_program_with_modules(&exprs, None, None, None)
    }));
    assert!(r.is_ok(), "checker panicked on [when]");
}

/// `Value::Json` display truncates by chars, not bytes.
#[test]
fn json_display_truncation_respects_char_boundaries() {
    // `["` + 61 bytes puts the 2-byte é across byte 64.
    let s = format!("{}é{}", "a".repeat(61), "b".repeat(100));
    let v = Value::from_json(&serde_json::json!([s]));
    let shown = catch_unwind(AssertUnwindSafe(|| format!("{v}"))).expect("display panicked");
    assert!(!shown.is_empty());
}
