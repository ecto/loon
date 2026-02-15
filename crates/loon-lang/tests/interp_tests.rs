use loon_lang::interp::{eval_program, Value};
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
