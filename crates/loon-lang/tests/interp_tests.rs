use loon_lang::eir::vm::eval_eir;
use loon_lang::interp::machine::eval_program_vm;
use loon_lang::interp::{eval_program, eval_program_with_base_dir, Value};
use loon_lang::parser::parse;

fn run(src: &str) -> Value {
    let exprs = parse(src).expect("parse failed");
    eval_program(&exprs).expect("eval failed")
}

fn run_vm(src: &str) -> Value {
    let exprs = parse(src).expect("parse failed");
    eval_program_vm(&exprs).expect("vm eval failed")
}

/// Helper: build a Value::Vec from a std Vec
fn v(items: Vec<Value>) -> Value {
    Value::Vec(items.into_iter().collect())
}

/// Helper: build a Value::Map from pairs
fn m(pairs: Vec<(Value, Value)>) -> Value {
    Value::Map(pairs.into_iter().collect())
}

#[test]
fn hello_world() {
    let result = run(r#"[println "hello, world!"]"#);
    assert_eq!(result, Value::Unit);
}

#[test]
fn arithmetic() {
    assert_eq!(run("[+ 1 2]"), Value::Int(3));
    assert_eq!(run("[- 10 3]"), Value::Int(7));
    assert_eq!(run("[* 4 5]"), Value::Int(20));
    assert_eq!(run("[> 3 2]"), Value::Bool(true));
    assert_eq!(run("[< 3 2]"), Value::Bool(false));
}

#[test]
fn let_binding() {
    assert_eq!(run("[do [let x 42] x]"), Value::Int(42));
}

#[test]
fn defn_and_call() {
    assert_eq!(
        run(r#"
            [fn add [x y] [+ x y]]
            [add 3 4]
        "#),
        Value::Int(7)
    );
}

#[test]
fn fibonacci() {
    assert_eq!(
        run(r#"
            [fn fib [n]
              [match n
                0 0
                1 1
                n [+ [fib [- n 1]] [fib [- n 2]]]]]
            [fib 10]
        "#),
        Value::Int(55)
    );
}

#[test]
fn if_expression() {
    assert_eq!(run("[if true 1 2]"), Value::Int(1));
    assert_eq!(run("[if false 1 2]"), Value::Int(2));
}

#[test]
fn closures() {
    assert_eq!(
        run(r#"
            [let inc [fn [x] [+ x 1]]]
            [inc 41]
        "#),
        Value::Int(42)
    );
}

#[test]
fn persistent_vector() {
    assert_eq!(run("[len #[1 2 3]]"), Value::Int(3));
    assert_eq!(run("[nth #[10 20 30] 1]"), Value::Int(20));
}

#[test]
fn map_filter_pipe() {
    assert_eq!(
        run(r#"
            [pipe #[1 2 3 4 5]
              [filter [fn [x] [> x 3]]]
              [len]]
        "#),
        Value::Int(2)
    );
}

#[test]
fn string_operations() {
    assert_eq!(
        run(r#"[str "hello" ", " "world"]"#),
        Value::Str("hello, world".into())
    );
    assert_eq!(run(r#"[len "hello"]"#), Value::Int(5));
}

#[test]
fn map_data_structure() {
    assert_eq!(
        run(r#"[get {:name "loon" :version "0.1"} :name]"#),
        Value::Str("loon".into())
    );
}

#[test]
fn set_operations() {
    assert_eq!(run("[contains? #{1 2 3} 2]"), Value::Bool(true));
    assert_eq!(run("[contains? #{1 2 3} 4]"), Value::Bool(false));
}

#[test]
fn multi_arity() {
    assert_eq!(
        run(r#"
            [fn greet
              ([name] [str "hello, " name])
              ([greeting name] [str greeting ", " name])]
            [greet "world"]
        "#),
        Value::Str("hello, world".into())
    );
    assert_eq!(
        run(r#"
            [fn greet
              ([name] [str "hello, " name])
              ([greeting name] [str greeting ", " name])]
            [greet "hey" "world"]
        "#),
        Value::Str("hey, world".into())
    );
}

#[test]
fn adt_pattern_matching() {
    assert_eq!(
        run(r#"
            [type Shape
              [Circle f64]
              [Rect f64 f64]
              Point]
            [fn area [shape]
              [match shape
                [Circle r] [* 3.14 [* r r]]
                [Rect w h] [* w h]
                Point       0.0]]
            [area [Rect 3.0 4.0]]
        "#),
        Value::Float(12.0)
    );
}

#[test]
fn match_with_guard() {
    assert_eq!(
        run(r#"
            [fn classify [n]
              [match n
                0 "zero"
                n [when [> n 0]] "positive"
                _ "negative"]]
            [classify 5]
        "#),
        Value::Str("positive".into())
    );
    assert_eq!(
        run(r#"
            [fn classify [n]
              [match n
                0 "zero"
                n [when [> n 0]] "positive"
                _ "negative"]]
            [classify -3]
        "#),
        Value::Str("negative".into())
    );
}

#[test]
fn range_and_map() {
    assert_eq!(
        run("[pipe [range 0 5] [map [fn [x] [* x x]]] [collect]]"),
        v(vec![
            Value::Int(0),
            Value::Int(1),
            Value::Int(4),
            Value::Int(9),
            Value::Int(16),
        ])
    );
}

#[test]
fn conj() {
    assert_eq!(
        run("[conj #[1 2 3] 4]"),
        v(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4)
        ])
    );
}

#[test]
fn boolean_logic() {
    assert_eq!(run("[not true]"), Value::Bool(false));
    assert_eq!(run("[not false]"), Value::Bool(true));
    assert_eq!(run("[and true true]"), Value::Bool(true));
    assert_eq!(run("[and true false]"), Value::Bool(false));
    assert_eq!(run("[or false true]"), Value::Bool(true));
}

#[test]
fn effect_handle_resume() {
    assert_eq!(
        run(r#"
            [fn load [path]
              [IO.read-file path]]
            [handle [load "test.txt"]
              [IO.read-file path] [resume "mock data"]]
        "#),
        Value::Str("mock data".into())
    );
}

#[test]
fn effect_handle_no_resume() {
    assert_eq!(
        run(r#"
            [fn risky []
              [Fail.fail "boom"]]
            [handle [risky]
              [Fail.fail msg] [str "caught: " msg]]
        "#),
        Value::Str("caught: boom".into())
    );
}

#[test]
fn user_defined_effect_handled() {
    assert_eq!(
        run(r#"
            [effect Fs [read-file [String] String]]
            [fn main []
              [handle [Fs.read-file "test.txt"]
                [Fs.read-file path] [resume "mocked content"]]]
            [main]
        "#),
        Value::Str("mocked content".into())
    );
}

#[test]
fn user_defined_effect_multi_ops() {
    assert_eq!(
        run(r#"
            [effect Fs
                [read-file [String] String]
                [list-dir [String] String]]
            [handle
              [do
                [let content [Fs.read-file "a.txt"]]
                [let listing [Fs.list-dir "."]]
                [str content " | " listing]]
              [Fs.read-file p] [resume "file-data"]
              [Fs.list-dir p] [resume "dir-listing"]]
        "#),
        Value::Str("file-data | dir-listing".into())
    );
}

#[test]
fn user_defined_effect_declaration_is_noop() {
    // effect declarations return Unit at runtime
    assert_eq!(
        run(r#"
            [effect Fs [read-file [String] String]]
            42
        "#),
        Value::Int(42)
    );
}

#[test]
fn fn_param_destructuring() {
    assert_eq!(
        run(r#"
            [fn first-of-pair [[a b]] a]
            [first-of-pair (1 2)]
        "#),
        Value::Int(1)
    );
}

#[test]
fn nested_let_destructuring() {
    assert_eq!(
        run(r#"
            [do
              [let [x [y z]] #[1 #[2 3]]]
              [+ y z]]
        "#),
        Value::Int(5)
    );
}

#[test]
fn pipe_thread_last() {
    // Pipe should thread value as last arg when explicit args present
    assert_eq!(
        run(r#"
            [pipe #[1 2 3 4 5]
              [map [fn [x] [* x x]]]
              [filter [fn [x] [> x 5]]]
              [len]]
        "#),
        Value::Int(3)
    );
}

#[test]
fn division_and_modulo() {
    assert_eq!(run("[/ 10 3]"), Value::Int(3));
    assert_eq!(run("[% 10 3]"), Value::Int(1));
}

#[test]
fn string_builtins() {
    assert_eq!(
        run(r#"[join ", " #["a" "b" "c"]]"#),
        Value::Str("a, b, c".into())
    );
    assert_eq!(run(r#"[trim "  hello  "]"#), Value::Str("hello".into()));
    assert_eq!(
        run(r#"[starts-with? "hello world" "hello"]"#),
        Value::Bool(true)
    );
    assert_eq!(
        run(r#"[ends-with? "hello world" "world"]"#),
        Value::Bool(true)
    );
    assert_eq!(
        run(r#"[replace "hello world" "world" "loon"]"#),
        Value::Str("hello loon".into())
    );
    assert_eq!(run(r#"[uppercase "hello"]"#), Value::Str("HELLO".into()));
    assert_eq!(run(r#"[lowercase "HELLO"]"#), Value::Str("hello".into()));
}

#[test]
fn vec_builtins() {
    // zip
    assert_eq!(
        run("[zip #[1 2 3] #[4 5 6]]"),
        v(vec![
            Value::Tuple(vec![Value::Int(1), Value::Int(4)]),
            Value::Tuple(vec![Value::Int(2), Value::Int(5)]),
            Value::Tuple(vec![Value::Int(3), Value::Int(6)]),
        ])
    );
    // flatten
    assert_eq!(
        run("[flatten #[#[1 2] #[3 4]]]"),
        v(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4)
        ])
    );
    // chunk
    assert_eq!(
        run("[chunk 2 #[1 2 3 4 5]]"),
        v(vec![
            v(vec![Value::Int(1), Value::Int(2)]),
            v(vec![Value::Int(3), Value::Int(4)]),
            v(vec![Value::Int(5)]),
        ])
    );
    // reverse
    assert_eq!(
        run("[reverse #[1 2 3]]"),
        v(vec![Value::Int(3), Value::Int(2), Value::Int(1)])
    );
    // drop
    assert_eq!(
        run("[drop 2 #[1 2 3 4 5]]"),
        v(vec![Value::Int(3), Value::Int(4), Value::Int(5)])
    );
    // any? and all?
    assert_eq!(
        run("[any? [fn [x] [> x 3]] #[1 2 3 4 5]]"),
        Value::Bool(true)
    );
    assert_eq!(
        run("[all? [fn [x] [> x 3]] #[1 2 3 4 5]]"),
        Value::Bool(false)
    );
}

#[test]
fn find_returns_option() {
    assert_eq!(
        run(r#"
            [match [find [fn [x] [> x 3]] #[1 2 3 4 5]]
              [Some x] x
              None 0]
        "#),
        Value::Int(4)
    );
    assert_eq!(
        run(r#"
            [match [find [fn [x] [> x 10]] #[1 2 3]]
              [Some x] x
              None 0]
        "#),
        Value::Int(0)
    );
}

#[test]
fn map_builtins() {
    // keys — order is not guaranteed with HashMap, so sort result
    let keys_result = run("[sort [keys {:a 1 :b 2}]]");
    assert_eq!(
        keys_result,
        v(vec![Value::Keyword("a".into()), Value::Keyword("b".into()),])
    );
    // values — check length and membership instead of order
    let vals_result = run("[sort [values {:a 1 :b 2}]]");
    assert_eq!(vals_result, v(vec![Value::Int(1), Value::Int(2)]));
    // remove
    assert_eq!(
        run("[remove {:a 1 :b 2} :b]"),
        m(vec![(Value::Keyword("a".into()), Value::Int(1))])
    );
}

#[test]
fn question_ok() {
    assert_eq!(run("[Ok 42]?"), Value::Int(42));
}

#[test]
fn question_err_caught() {
    assert_eq!(
        run(r#"
            [handle [Err "oops"]?
              [Fail.fail msg] [str "caught: " msg]]
        "#),
        Value::Str("caught: oops".into())
    );
}

#[test]
fn io_println_at_top_level() {
    // IO.println at top level should work via built-in handler
    let result = run(r#"[IO.println "hello from IO"]"#);
    assert_eq!(result, Value::Unit);
}

#[test]
fn io_read_file_missing_fails() {
    // IO.read-file on missing file: the built-in handler converts it to Fail.fail,
    // which propagates as an unhandled effect error at the top level
    let exprs = parse(r#"[IO.read-file "/nonexistent/path/foo.txt"]"#).expect("parse failed");
    let result = eval_program(&exprs);
    assert!(result.is_err(), "should fail for missing file");
    let e = result.unwrap_err();
    assert!(e.performed_effect.is_some(), "should be a Fail effect");
    let performed = e.performed_effect.unwrap();
    assert_eq!(performed.effect, "Fail");
    assert_eq!(performed.operation, "fail");
}

#[test]
fn io_read_file_mock_handler_still_works() {
    // Mock handler should still intercept before built-in handler
    assert_eq!(
        run(r#"
            [handle [IO.read-file "test.txt"]
              [IO.read-file path] [resume "mock data"]]
        "#),
        Value::Str("mock data".into())
    );
}

#[test]
fn question_in_defn_propagates_fail() {
    assert_eq!(
        run(r#"
            [fn wrap [x] x]
            [fn try-it [x] [wrap x]?]
            [handle [try-it [Err "bad"]]
              [Fail.fail msg] [str "got: " msg]]
        "#),
        Value::Str("got: bad".into())
    );
}

#[test]
fn channel_send_recv() {
    assert_eq!(
        run(r#"
            [let [tx rx] [channel]]
            [send tx 42]
            [recv rx]
        "#),
        Value::Int(42)
    );
}

#[test]
fn channel_fifo_order() {
    assert_eq!(
        run(r#"
            [let [tx rx] [channel]]
            [send tx 1]
            [send tx 2]
            [send tx 3]
            [recv rx]
        "#),
        Value::Int(1)
    );
}

#[test]
fn channel_recv_empty_errors() {
    let exprs = parse(
        r#"
        [let [tx rx] [channel]]
        [recv rx]
    "#,
    )
    .expect("parse failed");
    let result = eval_program(&exprs);
    assert!(result.is_err(), "recv on empty should error");
}

#[test]
fn process_args_mock() {
    // Test Process.args with a mock handler
    assert_eq!(
        run(r#"
            [handle [Process.args]
              [Process.args] [resume #["loon" "test"]]]
        "#),
        v(vec![Value::Str("loon".into()), Value::Str("test".into())])
    );
}

#[test]
fn process_env_mock() {
    // Test Process.env with a mock handler
    assert_eq!(
        run(r#"
            [handle [Process.env "HOME"]
              [Process.env k] [resume [Some "/home"]]]
        "#),
        Value::Adt("Some".to_string(), vec![Value::Str("/home".into())])
    );
}

#[test]
fn resumable_sequential_effects() {
    // Both IO.println calls should run, and handle should return 42
    assert_eq!(
        run(r#"
            [handle [do [IO.println "a"] [IO.println "b"] 42]
              [IO.println msg] [resume ()]]
        "#),
        Value::Int(42)
    );
}

#[test]
fn resumable_with_value() {
    // Resume value substitutes into the effect call site, even in nested expressions
    assert_eq!(
        run(r#"
            [handle [+ 1 [int [IO.read-line]]]
              [IO.read-line] [resume "5"]]
        "#),
        Value::Int(6)
    );
}

#[test]
fn try_success() {
    assert_eq!(run(r#"[try [+ 1 2] [fn [_] 0]]"#), Value::Int(3));
}

#[test]
fn try_failure() {
    assert_eq!(
        run(r#"[try [Err "oops"]? [fn [msg] [str "caught: " msg]]]"#),
        Value::Str("caught: oops".into())
    );
}

#[test]
fn stdlib_number_parsing() {
    assert_eq!(run(r#"[int "42"]"#), Value::Int(42));
    assert_eq!(run(r#"[float "3.14"]"#), Value::Float(3.14));
    assert_eq!(run("[int 5]"), Value::Int(5));
    assert_eq!(run("[float 5]"), Value::Float(5.0));
}

#[test]
fn stdlib_number_parsing_error() {
    let exprs = parse(r#"[int "abc"]"#).expect("parse failed");
    let result = eval_program(&exprs);
    assert!(result.is_err(), "int of non-numeric string should error");
}

#[test]
fn stdlib_string_ops() {
    assert_eq!(run(r#"[char-at "hello" 1]"#), Value::Str("e".into()));
    assert_eq!(
        run(r#"[substring "hello world" 0 5]"#),
        Value::Str("hello".into())
    );
    assert_eq!(
        run(r#"[contains? "hello world" "world"]"#),
        Value::Bool(true)
    );
    assert_eq!(
        run(r#"[contains? "hello world" "xyz"]"#),
        Value::Bool(false)
    );
    assert_eq!(run(r#"[index-of "hello world" "world"]"#), Value::Int(6));
    assert_eq!(run(r#"[index-of "hello world" "xyz"]"#), Value::Int(-1));
}

#[test]
fn stdlib_group_by() {
    assert_eq!(
        run(r#"[group-by [fn [x] [% x 2]] #[1 2 3 4 5]]"#),
        m(vec![
            (
                Value::Int(1),
                v(vec![Value::Int(1), Value::Int(3), Value::Int(5)])
            ),
            (Value::Int(0), v(vec![Value::Int(2), Value::Int(4)])),
        ])
    );
}

#[test]
fn stdlib_flat_map() {
    assert_eq!(
        run(r#"[flat-map [fn [x] #[x [* x 2]]] #[1 2 3]]"#),
        v(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(2),
            Value::Int(4),
            Value::Int(3),
            Value::Int(6),
        ])
    );
}

#[test]
fn stdlib_sort() {
    assert_eq!(
        run("[sort #[3 1 2]]"),
        v(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
    );
}

#[test]
fn stdlib_min_max_sum() {
    assert_eq!(run("[min #[3 1 2]]"), Value::Int(1));
    assert_eq!(run("[max #[3 1 2]]"), Value::Int(3));
    assert_eq!(run("[sum #[1 2 3 4]]"), Value::Int(10));
}

#[test]
fn stdlib_to_string() {
    assert_eq!(run(r#"[str 42]"#), Value::Str("42".into()));
    assert_eq!(run(r#"[str true]"#), Value::Str("true".into()));
}

#[test]
fn stdlib_into_map() {
    assert_eq!(
        run("[into-map #[(1 2) (3 4)]]"),
        m(vec![
            (Value::Int(1), Value::Int(2)),
            (Value::Int(3), Value::Int(4)),
        ])
    );
}

#[test]
fn fmt_interpolation() {
    assert_eq!(
        run(r#"[let name "world"] [fmt "hello {name}"]"#),
        Value::Str("hello world".into())
    );
    assert_eq!(
        run(r#"[fmt "2 + 2 = {[+ 2 2]}"]"#),
        Value::Str("2 + 2 = 4".into())
    );
    assert_eq!(
        run(r#"[fmt "no interpolation"]"#),
        Value::Str("no interpolation".into())
    );
    assert_eq!(
        run(r#"[fmt "escaped {{braces}}"]"#),
        Value::Str("escaped {braces}".into())
    );
}

#[test]
fn async_spawn_await() {
    // Async.spawn evaluates a thunk immediately, wraps result in Future;
    // Async.await unwraps it
    assert_eq!(
        run(r#"
            [let f [Async.spawn [fn [] 42]]]
            [Async.await f]
        "#),
        Value::Int(42)
    );
}

#[test]
fn async_sleep_noop() {
    // Async.sleep is a no-op mock that returns Unit
    assert_eq!(run("[Async.sleep 100]"), Value::Unit);
}

#[test]
fn async_spawn_await_string() {
    // Spawn a thunk that returns a string
    assert_eq!(
        run(r#"
            [let f [Async.spawn [fn [] [str "hello" " " "async"]]]]
            [Async.await f]
        "#),
        Value::Str("hello async".into())
    );
}

#[test]
fn async_handle_override() {
    // Async effects can be intercepted with handle, just like IO
    assert_eq!(
        run(r#"
            [handle [Async.spawn [fn [] 99]]
              [Async.spawn thunk] [resume 77]]
        "#),
        Value::Int(77)
    );
}

#[test]
fn async_sequential_spawn_await() {
    // Multiple spawn/await in sequence
    assert_eq!(
        run(r#"
            [let a [Async.spawn [fn [] 10]]]
            [let b [Async.spawn [fn [] 20]]]
            [+ [Async.await a] [Async.await b]]
        "#),
        Value::Int(30)
    );
}

#[test]
fn module_use() {
    // Write a temp module and import it
    let dir = std::env::temp_dir().join("loon_test_modules");
    let _ = std::fs::create_dir_all(&dir);
    std::fs::write(
        dir.join("mymath.oo"),
        "[pub fn double [x] [* x 2]]\n[pub fn triple [x] [* x 3]]\n",
    )
    .unwrap();

    let src = "[use mymath]\n[mymath.double 5]";
    let exprs = parse(src).expect("parse failed");
    let result = eval_program_with_base_dir(&exprs, Some(dir.as_path())).expect("eval failed");
    assert_eq!(result, Value::Int(10));

    // Test alias import
    let src = "[use mymath :as m]\n[m.triple 4]";
    let exprs = parse(src).expect("parse failed");
    let result = eval_program_with_base_dir(&exprs, Some(dir.as_path())).expect("eval failed");
    assert_eq!(result, Value::Int(12));

    // Cleanup
    let _ = std::fs::remove_dir_all(&dir);
}

// --- catch-errors tests ---

#[test]
fn catch_errors_valid_code_returns_empty() {
    let result = run(r#"[catch-errors "[+ 1 2]"]"#);
    match result {
        Value::Vec(v) => assert!(
            v.is_empty(),
            "valid code should return empty vec, got: {:?}",
            v
        ),
        other => panic!("expected Vec, got: {:?}", other),
    }
}

#[test]
fn catch_errors_invalid_code_returns_errors() {
    let result = run(r#"[catch-errors "[+ true false]"]"#);
    match result {
        Value::Vec(v) => {
            assert!(!v.is_empty(), "invalid code should return errors");
            // Each error should be a map with :what key
            if let Value::Map(pairs) = &v[0] {
                let has_what = pairs
                    .iter()
                    .any(|(k, _)| *k == Value::Keyword("what".into()));
                assert!(
                    has_what,
                    "error map should have :what key, got: {:?}",
                    pairs
                );
            } else {
                panic!("expected Map in error vec, got: {:?}", v[0]);
            }
        }
        other => panic!("expected Vec, got: {:?}", other),
    }
}

#[test]
fn catch_errors_parse_error_returns_errors() {
    let result = run(r#"[catch-errors "[fn"]"#);
    match result {
        Value::Vec(v) => assert!(!v.is_empty(), "parse error should return errors"),
        other => panic!("expected Vec, got: {:?}", other),
    }
}

// --- derive Copy tests ---

#[test]
fn derive_copy_evaluates_type() {
    // derive Copy should still register the type constructor
    let result = run(r#"
        [derive Copy [type Point [Point Int Int]]]
        [Point 1 2]
    "#);
    match result {
        Value::Adt(tag, fields) => {
            assert_eq!(tag, "Point");
            assert_eq!(fields.len(), 2);
        }
        other => panic!("expected Adt, got: {:?}", other),
    }
}

// --- Grant enforcement tests ---

#[test]
fn grant_enforcement_blocks_ungranted_effect() {
    use loon_lang::interp::{eval, set_current_module, set_effect_grants, Env};
    use loon_lang::pkg::capability::EffectGrants;

    // Set up grants: module "dep-a" is granted nothing
    let grants = EffectGrants::new();
    set_effect_grants(grants);
    set_current_module(Some("dep-a".to_string()));

    let exprs = parse("[IO.println \"hello\"]").unwrap();
    let mut env = Env::new();
    loon_lang::interp::register_builtins_pub(&mut env);
    let result = eval(&exprs[0], &mut env);

    // Restore to root
    set_current_module(None);

    assert!(result.is_err(), "should block ungranted effect");
    let msg = result.unwrap_err().message;
    assert!(
        msg.contains("not granted"),
        "error should mention grants: {msg}"
    );
}

#[test]
fn grant_enforcement_allows_granted_effect() {
    use loon_lang::interp::{eval, set_current_module, set_effect_grants, Env};
    use loon_lang::pkg::capability::EffectGrants;
    use std::collections::HashSet;

    // Set up grants: module "dep-b" is granted IO
    let mut grants = EffectGrants::new();
    grants.grants.insert("dep-b".to_string(), {
        let mut s = HashSet::new();
        s.insert("IO".to_string());
        s
    });
    set_effect_grants(grants);
    set_current_module(Some("dep-b".to_string()));

    let exprs = parse("[IO.println \"hello\"]").unwrap();
    let mut env = Env::new();
    loon_lang::interp::register_builtins_pub(&mut env);
    let result = eval(&exprs[0], &mut env);

    // Restore
    set_current_module(None);

    assert!(
        result.is_ok(),
        "granted effect should succeed: {:?}",
        result.err()
    );
}

#[test]
fn grant_enforcement_root_is_unrestricted() {
    use loon_lang::interp::{eval, set_current_module, set_effect_grants, Env};
    use loon_lang::pkg::capability::EffectGrants;

    // Set up empty grants but root module (None)
    set_effect_grants(EffectGrants::new());
    set_current_module(None);

    let exprs = parse("[IO.println \"hello\"]").unwrap();
    let mut env = Env::new();
    loon_lang::interp::register_builtins_pub(&mut env);
    let result = eval(&exprs[0], &mut env);

    assert!(
        result.is_ok(),
        "root module should be unrestricted: {:?}",
        result.err()
    );
}

#[test]
fn dot_access_string_keyed_map() {
    // String-keyed map: dot access should fall back to string keys
    assert_eq!(
        run(r#"
            [let m {"name" "cam"}]
            m.name
        "#),
        Value::Str("cam".into())
    );
}

#[test]
fn newline_inside_interpolation() {
    // \n inside {…} interpolation should be preserved for re-parse
    assert_eq!(
        run(r#"
            [let items #["a" "b"]]
            [str "list:\n{[join \"\n\" items]}"]
        "#),
        Value::Str("list:\na\nb".into())
    );
}

#[test]
fn keyword_builtin() {
    assert_eq!(run(r#"[keyword "hello"]"#), Value::Keyword("hello".into()));
    assert_eq!(run(r#"[name [keyword "x"]]"#), Value::Str("x".into()));
}

#[test]
fn keywordize_keys_builtin() {
    assert_eq!(
        run(r#"
            [let m {"a" 1 "b" 2}]
            [let kw [keywordize-keys m]]
            kw.a
        "#),
        Value::Int(1)
    );
}

#[test]
fn json_parse_keyword_keys() {
    // IO.parse-json returns keyword keys; use \{ \} to get literal braces
    assert_eq!(
        run(r#"
            [let m [IO.parse-json "\{\"x\":42\}"]]
            [get m :x]
        "#),
        Value::Int(42)
    );
}

#[test]
fn json_dot_access() {
    // IO.parse-json returns keyword keys, dot access works via keyword match
    assert_eq!(
        run(r#"
            [let m [IO.parse-json "\{\"name\":\"cam\"\}"]]
            m.name
        "#),
        Value::Str("cam".into())
    );
}

#[test]
fn all_source_files_parse() {
    // Catch parse errors in web/, samples/, and docs/ at build time.
    // Every .oo and .loon file in the repo must parse without errors.
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    let mut failures = Vec::new();
    for dir in &["web", "samples"] {
        let base = root.join(dir);
        if !base.exists() {
            continue;
        }
        for entry in walkdir(&base) {
            let path = entry.path();
            let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");
            if ext != "oo" && ext != "loon" {
                continue;
            }
            let src = std::fs::read_to_string(&path).unwrap();
            if let Err(e) = parse(&src) {
                failures.push(format!(
                    "{}:{}: {}",
                    path.display(),
                    e.span.start,
                    e.message
                ));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "parse errors in source files:\n  {}",
        failures.join("\n  ")
    );
}

// ─── VM (effect machine) tests ─────────────────────────────────────────

/// Run the same source through both interpreters and assert identical results.
fn assert_vm_matches(src: &str) {
    let expected = run(src);
    let actual = run_vm(src);
    assert_eq!(
        expected,
        actual,
        "VM mismatch for: {}",
        src.trim().chars().take(80).collect::<String>()
    );
}

#[test]
fn vm_arithmetic() {
    assert_vm_matches("[+ 1 2]");
    assert_vm_matches("[- 10 3]");
    assert_vm_matches("[* 4 5]");
    assert_vm_matches("[> 3 2]");
    assert_vm_matches("[< 3 2]");
}

#[test]
fn vm_let_and_do() {
    assert_vm_matches("[do [let x 42] x]");
    assert_vm_matches("[do [let x 1] [let y 2] [+ x y]]");
}

#[test]
fn vm_functions() {
    assert_vm_matches("[fn add [x y] [+ x y]] [add 3 4]");
    assert_vm_matches(
        r#"
        [fn fib [n]
          [match n
            0 0
            1 1
            n [+ [fib [- n 1]] [fib [- n 2]]]]]
        [fib 10]
    "#,
    );
}

#[test]
fn vm_closures() {
    assert_vm_matches(
        r#"
        [fn make-adder [n]
          [fn [x] [+ x n]]]
        [let add5 [make-adder 5]]
        [add5 10]
    "#,
    );
}

#[test]
fn vm_if_expression() {
    assert_vm_matches("[if true 1 2]");
    assert_vm_matches("[if false 1 2]");
}

#[test]
fn vm_persistent_vector() {
    assert_vm_matches(r#"[do [let v #[1 2 3]] [len v]]"#);
}

#[test]
fn vm_map_filter_pipe() {
    assert_vm_matches(
        r#"
        [pipe #[1 2 3 4 5]
          [filter [fn [x] [> x 2]]]
          [map [fn [x] [* x 10]]]]
    "#,
    );
}

#[test]
fn vm_strings() {
    assert_vm_matches(r#"[str "hello" " " "world"]"#);
    assert_vm_matches(r#"[len "hello"]"#);
}

#[test]
fn vm_adt_pattern_matching() {
    assert_vm_matches(
        r#"
        [type Color [Rgb Int Int Int] [Hex String]]
        [fn red [c] [match c [Rgb r g b] r [Hex s] 0]]
        [red [Rgb 255 128 0]]
    "#,
    );
}

#[test]
fn vm_match_with_guard() {
    assert_vm_matches(
        r#"
        [fn classify [n]
          [match n
            n [when [< n 0]] :negative
            0 :zero
            n :positive]]
        [classify -5]
    "#,
    );
    assert_vm_matches(
        r#"
        [fn classify [n]
          [match n
            n [when [< n 0]] :negative
            0 :zero
            n :positive]]
        [classify 0]
    "#,
    );
    assert_vm_matches(
        r#"
        [fn classify [n]
          [match n
            n [when [< n 0]] :negative
            0 :zero
            n :positive]]
        [classify 5]
    "#,
    );
}

#[test]
fn vm_multi_arity() {
    assert_vm_matches(
        r#"
        [fn greet
          ([name] [str "hi " name])
          ([first last] [str "hello " first " " last])]
        [greet "world"]
    "#,
    );
    assert_vm_matches(
        r#"
        [fn greet
          ([name] [str "hi " name])
          ([first last] [str "hello " first " " last])]
        [greet "Jane" "Doe"]
    "#,
    );
}

#[test]
fn vm_conj() {
    assert_vm_matches("[conj #[1 2] 3]");
    assert_vm_matches("[conj #[] 1]");
}

#[test]
fn vm_range_and_map() {
    assert_vm_matches("[range 0 5]");
    assert_vm_matches("[map [fn [x] [* x x]] [range 0 5]]");
}

#[test]
fn vm_loop_recur() {
    assert_vm_matches(
        r#"
        [loop [i 0 sum 0]
          [if [>= i 10] sum
            [recur [+ i 1] [+ sum i]]]]
    "#,
    );
}

#[test]
fn vm_fn_recur() {
    assert_vm_matches(
        r#"
        [fn countdown [n]
          [if [= n 0] :done [recur [- n 1]]]]
        [countdown 100]
    "#,
    );
}

#[test]
fn vm_set_operations() {
    assert_vm_matches("[len #{1 2 3}]");
    assert_vm_matches("[contains? #{1 2 3} 2]");
}

#[test]
fn vm_destructuring() {
    assert_vm_matches(
        r#"
        [fn first-two [[a b & rest]] [+ a b]]
        [first-two #[10 20 30 40]]
    "#,
    );
}

#[test]
fn vm_pipe_thread_last() {
    assert_vm_matches(
        r#"
        [pipe 10
          [+ 5]
          [* 2]]
    "#,
    );
}

#[test]
fn vm_vec_builtins() {
    assert_vm_matches("[nth #[10 20 30] 1]");
    assert_vm_matches("[reverse #[1 2 3]]");
    assert_vm_matches("[take 2 #[1 2 3 4]]");
    assert_vm_matches("[drop 2 #[1 2 3 4]]");
    assert_vm_matches("[flatten #[#[1 2] #[3 4]]]");
    assert_vm_matches("[zip #[1 2 3] #[4 5 6]]");
    assert_vm_matches("[any? [fn [x] [> x 3]] #[1 2 3]]");
    assert_vm_matches("[all? [fn [x] [> x 0]] #[1 2 3]]");
    assert_vm_matches("[sum #[1 2 3 4 5]]");
}

#[test]
fn vm_string_builtins() {
    assert_vm_matches(r#"[join ", " #["a" "b" "c"]]"#);
    assert_vm_matches(r#"[trim "  hello  "]"#);
    assert_vm_matches(r#"[starts-with? "hello" "he"]"#);
    assert_vm_matches(r#"[uppercase "hello"]"#);
    assert_vm_matches(r#"[lowercase "HELLO"]"#);
}

#[test]
fn vm_map_builtins() {
    assert_eq!(run_vm("[len {:a 1 :b 2}]"), Value::Int(2));
    assert_eq!(run_vm("[get {:a 1 :b 2} :a]"), Value::Int(1));
    assert_eq!(run_vm("[get [assoc {:a 1} :b 2] :b]"), Value::Int(2));
    assert_eq!(run_vm("[contains? {:a 1 :b 2} :a]"), Value::Bool(true));
}

#[test]
fn vm_try_success() {
    assert_vm_matches("[try [+ 1 2] [fn [e] :err]]");
}

#[test]
fn vm_try_failure() {
    assert_vm_matches(r#"[try [Fail.fail "boom"] [fn [e] [str "caught: " e]]]"#);
}

#[test]
fn vm_effect_handle_resume() {
    assert_vm_matches(
        r#"
        [handle [IO.read-file "test.txt"]
          [IO.read-file path] [resume "mock-data"]]
    "#,
    );
}

#[test]
fn vm_user_defined_effect() {
    assert_vm_matches(
        r#"
        [effect Db [query [String] String]]
        [fn get-user [id] [Db.query id]]
        [handle [get-user "42"]
          [Db.query id] [resume [str "User:" id]]]
    "#,
    );
}

#[test]
fn vm_resumable_sequential_effects() {
    assert_vm_matches(
        r#"
        [effect Fs [read [String] String]]
        [handle
          [do [let a [Fs.read "x"]] [let b [Fs.read "y"]] [str a "+" b]]
          [Fs.read path] [resume [str "data:" path]]]
    "#,
    );
}

#[test]
fn vm_fmt_interpolation() {
    assert_vm_matches(
        r#"
        [let name "world"]
        [str "hello " name "!"]
    "#,
    );
}

#[test]
fn vm_question_ok() {
    // x? desugaring: use explicit match form (x? tokenization is parser-dependent)
    assert_eq!(
        run_vm("[do [let x [Ok 42]] [match x [Ok v] v [Err e] e]]"),
        Value::Int(42)
    );
}

#[test]
fn vm_boolean_logic() {
    assert_vm_matches("[and true true]");
    assert_vm_matches("[and true false]");
    assert_vm_matches("[or false true]");
    assert_vm_matches("[not true]");
}

#[test]
fn vm_division_and_modulo() {
    assert_vm_matches("[/ 10 3]");
    assert_vm_matches("[% 10 3]");
}

#[test]
fn vm_stdlib_sort() {
    assert_vm_matches("[sort #[3 1 2]]");
}

#[test]
fn vm_stdlib_min_max_sum() {
    assert_vm_matches("[min #[3 1 2]]");
    assert_vm_matches("[max #[3 1 2]]");
    assert_vm_matches("[sum #[1 2 3 4 5]]");
}

#[test]
fn vm_derive_copy() {
    assert_vm_matches(
        r#"
        [derive Copy [type Point [XY Int Int]]]
        [let p [XY 1 2]]
        [match p [XY x y] [+ x y]]
    "#,
    );
}

#[test]
fn vm_dot_access_map() {
    assert_eq!(
        run_vm(
            r#"
            [let m {"name" "Alice" "age" 30}]
            m.name
        "#
        ),
        Value::Str("Alice".into())
    );
}

#[test]
fn vm_keyword_builtin() {
    assert_vm_matches(r#"[keyword "hello"]"#);
}

fn walkdir(dir: &std::path::Path) -> Vec<std::fs::DirEntry> {
    let mut results = Vec::new();
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                results.extend(walkdir(&path));
            } else {
                results.push(entry);
            }
        }
    }
    results
}

// ─── EIR register VM tests ────────────────────────────────────────────

#[allow(dead_code)]
fn run_eir(src: &str) -> loon_lang::eir::vm::Val {
    eval_eir(src).expect("EIR eval failed").value
}

/// Assert that a program runs successfully through the EIR pipeline.
fn assert_eir_works(src: &str) {
    let result = eval_eir(src);
    assert!(
        result.is_ok(),
        "EIR failed for: {}\nerror: {}",
        src.trim().chars().take(80).collect::<String>(),
        result.unwrap_err()
    );
}

#[test]
fn eir_parity() {
    // ── Arithmetic ──────────────────────────────────────────────────
    assert_eir_works("[+ 1 2]");
    assert_eir_works("[- 10 3]");
    assert_eir_works("[* 4 5]");
    assert_eir_works("[/ 10 2]");
    assert_eir_works("[% 10 3]");
    assert_eir_works("[> 3 2]");
    assert_eir_works("[< 3 2]");
    assert_eir_works("[>= 3 3]");
    assert_eir_works("[<= 2 3]");
    assert_eir_works("[= 5 5]");
    assert_eir_works("[!= 5 6]");

    // ── Let / Do ────────────────────────────────────────────────────
    assert_eir_works("[do [let x 42] x]");
    assert_eir_works("[do [let x 1] [let y 2] [+ x y]]");
    assert_eir_works(
        r#"[do
            [let a 10]
            [let b 20]
            [let c [+ a b]]
            c]"#,
    );

    // ── If / When ───────────────────────────────────────────────────
    assert_eir_works("[if true 1 2]");
    assert_eir_works("[if false 1 2]");
    assert_eir_works("[if [> 3 2] :yes :no]");
    assert_eir_works("[when true 42]");
    assert_eir_works("[when false 42]");

    // ── Named functions ─────────────────────────────────────────────
    assert_eir_works("[fn add [x y] [+ x y]] [add 3 4]");
    assert_eir_works(
        r#"
        [fn fib [n]
          [match n
            0 0
            1 1
            n [+ [fib [- n 1]] [fib [- n 2]]]]]
        [fib 10]
    "#,
    );
    assert_eir_works(
        r#"
        [fn greet
          ([name] [str "hi " name])
          ([first last] [str "hello " first " " last])]
        [greet "world"]
    "#,
    );
    assert_eir_works(
        r#"
        [fn greet
          ([name] [str "hi " name])
          ([first last] [str "hello " first " " last])]
        [greet "Jane" "Doe"]
    "#,
    );

    // ── Anonymous functions ─────────────────────────────────────────
    assert_eir_works("[let double [fn [x] [* x 2]]] [double 5]");

    // ── Closures ────────────────────────────────────────────────────
    assert_eir_works(
        r#"
        [fn make-adder [n]
          [fn [x] [+ x n]]]
        [let add5 [make-adder 5]]
        [add5 10]
    "#,
    );

    // ── Loop / Recur ────────────────────────────────────────────────
    assert_eir_works(
        r#"
        [loop [i 0 sum 0]
          [if [>= i 10] sum
            [recur [+ i 1] [+ sum i]]]]
    "#,
    );

    // ── fn / Recur (TCO) ────────────────────────────────────────────
    assert_eir_works(
        r#"
        [fn countdown [n]
          [if [= n 0] :done [recur [- n 1]]]]
        [countdown 100]
    "#,
    );

    // ── Pipe ────────────────────────────────────────────────────────
    // NOTE: pipe with partial application syntax (e.g. [+ 5]) is not yet
    // supported by the EIR VM. Use explicit lambdas for now.
    assert_eir_works(
        r#"
        [pipe #[1 2 3 4 5]
          [filter [fn [x] [> x 2]]]
          [map [fn [x] [* x 10]]]]
    "#,
    );

    // ── Match / ADTs ────────────────────────────────────────────────
    assert_eir_works(
        r#"
        [type Color [Rgb Int Int Int] [Hex String]]
        [fn red [c] [match c [Rgb r g b] r [Hex s] 0]]
        [red [Rgb 255 128 0]]
    "#,
    );
    assert_eir_works(
        r#"
        [fn classify [n]
          [match n
            n [when [< n 0]] :negative
            0 :zero
            n :positive]]
        [classify -5]
    "#,
    );
    assert_eir_works(
        r#"
        [fn classify [n]
          [match n
            n [when [< n 0]] :negative
            0 :zero
            n :positive]]
        [classify 0]
    "#,
    );
    assert_eir_works(
        r#"
        [fn classify [n]
          [match n
            n [when [< n 0]] :negative
            0 :zero
            n :positive]]
        [classify 42]
    "#,
    );
    // NOTE: `derive Copy` is not yet supported by the EIR VM.
    // assert_eir_works(r#"[derive Copy [type Point [XY Int Int]]] [let p [XY 1 2]] [match p [XY x y] [+ x y]]"#);

    // ── Vectors ─────────────────────────────────────────────────────
    assert_eir_works("[do [let v #[1 2 3]] [len v]]");
    assert_eir_works("[conj #[1 2] 3]");
    assert_eir_works("[conj #[] 1]");
    assert_eir_works("[nth #[10 20 30] 1]");
    assert_eir_works("[reverse #[1 2 3]]");
    assert_eir_works("[take 2 #[1 2 3 4]]");
    assert_eir_works("[drop 2 #[1 2 3 4]]");
    assert_eir_works("[flatten #[#[1 2] #[3 4]]]");
    assert_eir_works("[zip #[1 2 3] #[4 5 6]]");
    assert_eir_works("[range 0 5]");

    // ── Maps ────────────────────────────────────────────────────────
    assert_eir_works("[len {:a 1 :b 2}]");
    assert_eir_works("[get {:a 1 :b 2} :a]");
    assert_eir_works("[contains? {:a 1 :b 2} :a]");

    // ── Sets ────────────────────────────────────────────────────────
    assert_eir_works("[len #{1 2 3}]");
    assert_eir_works("[contains? #{1 2 3} 2]");

    // ── Strings ─────────────────────────────────────────────────────
    assert_eir_works(r#"[str "hello" " " "world"]"#);
    assert_eir_works(r#"[len "hello"]"#);
    assert_eir_works(r#"[join ", " #["a" "b" "c"]]"#);
    assert_eir_works(r#"[trim "  hello  "]"#);
    assert_eir_works(r#"[starts-with? "hello" "he"]"#);
    assert_eir_works(r#"[uppercase "hello"]"#);
    assert_eir_works(r#"[lowercase "HELLO"]"#);

    // ── Builtins: map, filter, fold, each, sort ─────────────────────
    assert_eir_works("[map [fn [x] [* x x]] [range 0 5]]");
    assert_eir_works("[filter [fn [x] [> x 2]] #[1 2 3 4 5]]");
    assert_eir_works("[fold 0 [fn [acc x] [+ acc x]] #[1 2 3 4 5]]");
    assert_eir_works("[each [fn [x] x] #[1 2 3]]");
    assert_eir_works("[sort #[3 1 2]]");
    assert_eir_works("[min #[3 1 2]]");
    assert_eir_works("[max #[3 1 2]]");
    assert_eir_works("[sum #[1 2 3 4 5]]");
    assert_eir_works("[any? [fn [x] [> x 3]] #[1 2 3]]");
    assert_eir_works("[all? [fn [x] [> x 0]] #[1 2 3]]");

    // ── Boolean logic ───────────────────────────────────────────────
    assert_eir_works("[and true true]");
    assert_eir_works("[and true false]");
    assert_eir_works("[or false true]");
    assert_eir_works("[not true]");

    // ── Effects: handle / try ───────────────────────────────────────
    assert_eir_works("[try [+ 1 2] [fn [e] :err]]");
    assert_eir_works(r#"[try [Fail.fail "boom"] [fn [e] [str "caught: " e]]]"#);
    assert_eir_works(
        r#"
        [handle [IO.read-file "test.txt"]
          [IO.read-file path] [resume "mock-data"]]
    "#,
    );
    assert_eir_works(
        r#"
        [effect Db [query [String] String]]
        [fn get-user [id] [Db.query id]]
        [handle [get-user "42"]
          [Db.query id] [resume [str "User:" id]]]
    "#,
    );
    assert_eir_works(
        r#"
        [effect Fs [read [String] String]]
        [handle
          [do [let a [Fs.read "x"]] [let b [Fs.read "y"]] [str a "+" b]]
          [Fs.read path] [resume [str "data:" path]]]
    "#,
    );

    // ── Destructuring ───────────────────────────────────────────────
    assert_eir_works(
        r#"
        [fn first-two [[a b & rest]] [+ a b]]
        [first-two #[10 20 30 40]]
    "#,
    );

    // ── Keywords ────────────────────────────────────────────────────
    assert_eir_works(r#"[keyword "hello"]"#);

    // ── String interpolation ────────────────────────────────────────
    assert_eir_works(
        r#"
        [let name "world"]
        [str "hello " name "!"]
    "#,
    );
}
