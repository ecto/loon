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
            [defn add [x y] [+ x y]]
            [add 3 4]
        "#),
        Value::Int(7)
    );
}

#[test]
fn fibonacci() {
    assert_eq!(
        run(r#"
            [defn fib [n]
              [match n
                0 => 0
                1 => 1
                n => [+ [fib [- n 1]] [fib [- n 2]]]]]
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
            [|> #[1 2 3 4 5]
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
            [defn greet
              ([name] [str "hello, " name])
              ([greeting name] [str greeting ", " name])]
            [greet "world"]
        "#),
        Value::Str("hello, world".to_string())
    );
    assert_eq!(
        run(r#"
            [defn greet
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
            [defn area [shape]
              [match shape
                [Circle r] => [* 3.14 [* r r]]
                [Rect w h] => [* w h]
                Point       => 0.0]]
            [area [Rect 3.0 4.0]]
        "#),
        Value::Float(12.0)
    );
}

#[test]
fn match_with_guard() {
    assert_eq!(
        run(r#"
            [defn classify [n]
              [match n
                0 => "zero"
                n [when [> n 0]] => "positive"
                _ => "negative"]]
            [classify 5]
        "#),
        Value::Str("positive".to_string())
    );
    assert_eq!(
        run(r#"
            [defn classify [n]
              [match n
                0 => "zero"
                n [when [> n 0]] => "positive"
                _ => "negative"]]
            [classify -3]
        "#),
        Value::Str("negative".to_string())
    );
}

#[test]
fn range_and_map() {
    assert_eq!(
        run("[|> [range 0 5] [map [fn [x] [* x x]]] [collect]]"),
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
            [defn load [path]
              [IO.read-file path]]
            [handle [load "test.txt"]
              [IO.read-file path] => [resume "mock data"]]
        "#),
        Value::Str("mock data".to_string())
    );
}

#[test]
fn effect_handle_no_resume() {
    assert_eq!(
        run(r#"
            [defn risky []
              [Fail.fail "boom"]]
            [handle [risky]
              [Fail.fail msg] => [str "caught: " msg]]
        "#),
        Value::Str("caught: boom".to_string())
    );
}

#[test]
fn fn_param_destructuring() {
    assert_eq!(
        run(r#"
            [defn first-of-pair [[a b]] a]
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
            [|> #[1 2 3 4 5]
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
              [Some x] => x
              None => 0]
        "#),
        Value::Int(4)
    );
    assert_eq!(
        run(r#"
            [match [find [fn [x] [> x 10]] #[1 2 3]]
              [Some x] => x
              None => 0]
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
fn module_use() {
    // Write a temp module and import it
    let dir = std::env::temp_dir().join("loon_test_modules");
    let _ = std::fs::create_dir_all(&dir);
    std::fs::write(
        dir.join("mymath.loon"),
        "[pub defn double [x] [* x 2]]\n[pub defn triple [x] [* x 3]]\n",
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
