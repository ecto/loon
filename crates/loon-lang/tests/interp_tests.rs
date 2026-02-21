use loon_lang::interp::{eval_program, eval_program_with_base_dir, Value};
use loon_lang::parser::parse;

fn run(src: &str) -> Value {
    let exprs = parse(src).expect("parse failed");
    eval_program(&exprs).expect("eval failed")
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
    assert_eq!(
        run("[len #[1 2 3]]"),
        Value::Int(3)
    );
    assert_eq!(
        run("[nth #[10 20 30] 1]"),
        Value::Int(20)
    );
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
        Value::Str("hello, world".to_string())
    );
    assert_eq!(
        run(r#"[len "hello"]"#),
        Value::Int(5)
    );
}

#[test]
fn map_data_structure() {
    assert_eq!(
        run(r#"[get {:name "loon" :version "0.1"} :name]"#),
        Value::Str("loon".to_string())
    );
}

#[test]
fn set_operations() {
    assert_eq!(
        run("[contains? #{1 2 3} 2]"),
        Value::Bool(true)
    );
    assert_eq!(
        run("[contains? #{1 2 3} 4]"),
        Value::Bool(false)
    );
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
        Value::Str("hello, world".to_string())
    );
    assert_eq!(
        run(r#"
            [fn greet
              ([name] [str "hello, " name])
              ([greeting name] [str greeting ", " name])]
            [greet "hey" "world"]
        "#),
        Value::Str("hey, world".to_string())
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
        Value::Str("positive".to_string())
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
        Value::Str("negative".to_string())
    );
}

#[test]
fn range_and_map() {
    assert_eq!(
        run("[pipe [range 0 5] [map [fn [x] [* x x]]] [collect]]"),
        Value::Vec(vec![
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
        Value::Vec(vec![Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(4)])
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
        Value::Str("mock data".to_string())
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
        Value::Str("caught: boom".to_string())
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
        Value::Str("a, b, c".to_string())
    );
    assert_eq!(
        run(r#"[trim "  hello  "]"#),
        Value::Str("hello".to_string())
    );
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
        Value::Str("hello loon".to_string())
    );
    assert_eq!(
        run(r#"[uppercase "hello"]"#),
        Value::Str("HELLO".to_string())
    );
    assert_eq!(
        run(r#"[lowercase "HELLO"]"#),
        Value::Str("hello".to_string())
    );
}

#[test]
fn vec_builtins() {
    // zip
    assert_eq!(
        run("[zip #[1 2 3] #[4 5 6]]"),
        Value::Vec(vec![
            Value::Tuple(vec![Value::Int(1), Value::Int(4)]),
            Value::Tuple(vec![Value::Int(2), Value::Int(5)]),
            Value::Tuple(vec![Value::Int(3), Value::Int(6)]),
        ])
    );
    // flatten
    assert_eq!(
        run("[flatten #[#[1 2] #[3 4]]]"),
        Value::Vec(vec![Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(4)])
    );
    // chunk
    assert_eq!(
        run("[chunk 2 #[1 2 3 4 5]]"),
        Value::Vec(vec![
            Value::Vec(vec![Value::Int(1), Value::Int(2)]),
            Value::Vec(vec![Value::Int(3), Value::Int(4)]),
            Value::Vec(vec![Value::Int(5)]),
        ])
    );
    // reverse
    assert_eq!(
        run("[reverse #[1 2 3]]"),
        Value::Vec(vec![Value::Int(3), Value::Int(2), Value::Int(1)])
    );
    // drop
    assert_eq!(
        run("[drop 2 #[1 2 3 4 5]]"),
        Value::Vec(vec![Value::Int(3), Value::Int(4), Value::Int(5)])
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
    // keys
    assert_eq!(
        run("[keys {:a 1 :b 2}]"),
        Value::Vec(vec![
            Value::Keyword("a".to_string()),
            Value::Keyword("b".to_string()),
        ])
    );
    // values
    assert_eq!(
        run("[values {:a 1 :b 2}]"),
        Value::Vec(vec![Value::Int(1), Value::Int(2)])
    );
    // remove
    assert_eq!(
        run("[remove {:a 1 :b 2} :b]"),
        Value::Map(vec![(Value::Keyword("a".to_string()), Value::Int(1))])
    );
}

#[test]
fn question_ok() {
    assert_eq!(
        run("[Ok 42]?"),
        Value::Int(42)
    );
}

#[test]
fn question_err_caught() {
    assert_eq!(
        run(r#"
            [handle [Err "oops"]?
              [Fail.fail msg] [str "caught: " msg]]
        "#),
        Value::Str("caught: oops".to_string())
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
        Value::Str("mock data".to_string())
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
        Value::Str("got: bad".to_string())
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
    let exprs = parse(r#"
        [let [tx rx] [channel]]
        [recv rx]
    "#).expect("parse failed");
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
        Value::Vec(vec![Value::Str("loon".to_string()), Value::Str("test".to_string())])
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
        Value::Adt("Some".to_string(), vec![Value::Str("/home".to_string())])
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
    assert_eq!(
        run(r#"[try [+ 1 2] [fn [_] 0]]"#),
        Value::Int(3)
    );
}

#[test]
fn try_failure() {
    assert_eq!(
        run(r#"[try [Err "oops"]? [fn [msg] [str "caught: " msg]]]"#),
        Value::Str("caught: oops".to_string())
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
    assert_eq!(
        run(r#"[char-at "hello" 1]"#),
        Value::Str("e".to_string())
    );
    assert_eq!(
        run(r#"[substring "hello world" 0 5]"#),
        Value::Str("hello".to_string())
    );
    assert_eq!(
        run(r#"[contains? "hello world" "world"]"#),
        Value::Bool(true)
    );
    assert_eq!(
        run(r#"[contains? "hello world" "xyz"]"#),
        Value::Bool(false)
    );
    assert_eq!(
        run(r#"[index-of "hello world" "world"]"#),
        Value::Int(6)
    );
    assert_eq!(
        run(r#"[index-of "hello world" "xyz"]"#),
        Value::Int(-1)
    );
}

#[test]
fn stdlib_group_by() {
    assert_eq!(
        run(r#"[group-by [fn [x] [% x 2]] #[1 2 3 4 5]]"#),
        Value::Map(vec![
            (Value::Int(1), Value::Vec(vec![Value::Int(1), Value::Int(3), Value::Int(5)])),
            (Value::Int(0), Value::Vec(vec![Value::Int(2), Value::Int(4)])),
        ])
    );
}

#[test]
fn stdlib_flat_map() {
    assert_eq!(
        run(r#"[flat-map [fn [x] #[x [* x 2]]] #[1 2 3]]"#),
        Value::Vec(vec![
            Value::Int(1), Value::Int(2),
            Value::Int(2), Value::Int(4),
            Value::Int(3), Value::Int(6),
        ])
    );
}

#[test]
fn stdlib_sort() {
    assert_eq!(
        run("[sort #[3 1 2]]"),
        Value::Vec(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
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
    assert_eq!(run(r#"[str 42]"#), Value::Str("42".to_string()));
    assert_eq!(run(r#"[str true]"#), Value::Str("true".to_string()));
}

#[test]
fn stdlib_into_map() {
    assert_eq!(
        run("[into-map #[(1 2) (3 4)]]"),
        Value::Map(vec![
            (Value::Int(1), Value::Int(2)),
            (Value::Int(3), Value::Int(4)),
        ])
    );
}

#[test]
fn fmt_interpolation() {
    assert_eq!(
        run(r#"[let name "world"] [fmt "hello {name}"]"#),
        Value::Str("hello world".to_string())
    );
    assert_eq!(
        run(r#"[fmt "2 + 2 = {[+ 2 2]}"]"#),
        Value::Str("2 + 2 = 4".to_string())
    );
    assert_eq!(
        run(r#"[fmt "no interpolation"]"#),
        Value::Str("no interpolation".to_string())
    );
    assert_eq!(
        run(r#"[fmt "escaped {{braces}}"]"#),
        Value::Str("escaped {braces}".to_string())
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
    assert_eq!(
        run("[Async.sleep 100]"),
        Value::Unit
    );
}

#[test]
fn async_spawn_await_string() {
    // Spawn a thunk that returns a string
    assert_eq!(
        run(r#"
            [let f [Async.spawn [fn [] [str "hello" " " "async"]]]]
            [Async.await f]
        "#),
        Value::Str("hello async".to_string())
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
        dir.join("mymath.loon"),
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
        Value::Vec(v) => assert!(v.is_empty(), "valid code should return empty vec, got: {:?}", v),
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
                let has_what = pairs.iter().any(|(k, _)| *k == Value::Keyword("what".to_string()));
                assert!(has_what, "error map should have :what key, got: {:?}", pairs);
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
