//! Placement is an effect, so a handler decides where a kernel runs.
//!
//! These tests pin the three claims that make that more than a slogan:
//! a program that never mentions placement still runs; a handler can observe
//! and redirect every launch without the program changing; and the direction
//! data has to travel is inferred from the kernel rather than annotated.

use loon_lang::eir::vm::eval_eir;

fn run(src: &str) -> Vec<String> {
    match eval_eir(src) {
        Ok(r) => r.output,
        Err(e) => panic!("VM error: {e:?}\nsource:\n{src}"),
    }
}

fn run_err(src: &str) -> String {
    match eval_eir(src) {
        Ok(r) => panic!("expected an error, got output {:?}", r.output),
        Err(e) => format!("{e:?}"),
    }
}

const SAXPY: &str = "[kernel saxpy [i a x y out] \
                       [put out i [+ [* a [at x i]] [at y i]]]]";

#[test]
fn a_kernel_runs_with_no_handler_at_all() {
    // The default answer to "where does this run" is "here". A program that
    // never mentions placement is still a working program.
    let out = run(&format!(
        "{SAXPY} \
         [fn main [] \
           [let x [buf #[1 2 3]]] \
           [let y [buf #[10 20 30]]] \
           [let mut out [buf-zeros 3]] \
           [Place.run saxpy 3 #[2.0 x y out]] \
           [IO.println [Place.read out]]]"
    ));
    assert_eq!(out, vec!["#[12 24 36]"]);
}

#[test]
fn a_handler_sees_every_launch_and_every_sync_point() {
    // This is the whole argument. The Rust offload work needs `Preload` and
    // `PreloadMut` types, whose `drop` marks where device data becomes visible
    // to the host, because nothing else in the language knows. Here every
    // launch and every read is an operation, so a handler learns the same
    // facts without the program being annotated at all — and a residency
    // policy is therefore writable as ordinary code.
    let out = run(&format!(
        "{SAXPY} \
         [fn work [] \
           [let x [buf #[1 2 3]]] \
           [let y [buf #[10 20 30]]] \
           [let mut out [buf-zeros 3]] \
           [Place.run saxpy 3 #[2.0 x y out]] \
           [Place.read out]] \
         [fn traced [thunk] \
           [handle [thunk] \
             [Place.run k n args] [do [IO.println \"launch\"] \
                                        [resume [Place.run k n args]]] \
             [Place.read b]         [do [IO.println \"sync\"] \
                                        [resume [Place.read b]]]]] \
         [fn main [] [IO.println [traced work]]]"
    ));
    assert_eq!(out, vec!["launch", "sync", "#[12 24 36]"]);
}

#[test]
fn a_handler_can_answer_without_running_anything() {
    // A handler is free not to forward. Answering from a recording — or from
    // a simulated device — is the same shape as answering for real, which is
    // why a program can be tested without the hardware it targets.
    let out = run(&format!(
        "{SAXPY} \
         [fn work [] \
           [let x [buf #[1 2 3]]] \
           [let mut out [buf-zeros 3]] \
           [Place.run saxpy 3 #[2.0 x x out]] \
           [Place.read out]] \
         [fn canned [thunk] \
           [handle [thunk] \
             [Place.run k n args] [resume []] \
             [Place.read b]         [resume #[99 99 99]]]] \
         [fn main [] [IO.println [canned work]]]"
    ));
    assert_eq!(out, vec!["#[99 99 99]"]);
}

#[test]
fn the_program_is_identical_under_every_handler() {
    // `work` is one function. What changes between these runs is the line that
    // wraps it, never the code that does the work.
    let common = format!(
        "{SAXPY} \
         [fn work [] \
           [let x [buf #[1 2 3]]] \
           [let y [buf #[10 20 30]]] \
           [let mut out [buf-zeros 3]] \
           [Place.run saxpy 3 #[2.0 x y out]] \
           [Place.read out]] \
         [fn counted [thunk] \
           [handle [thunk] \
             [Place.run k n args] [do [IO.println \"one launch\"] \
                                        [resume [Place.run k n args]]]]]"
    );
    let bare = run(&format!("{common} [fn main [] [IO.println [work]]]"));
    let wrapped = run(&format!(
        "{common} [fn main [] [IO.println [counted work]]]"
    ));

    assert_eq!(bare, vec!["#[12 24 36]"]);
    assert_eq!(wrapped, vec!["one launch", "#[12 24 36]"]);
}

#[test]
fn transfer_accounting_counts_what_crossed_the_boundary() {
    // The number that matters in an offload program is how many times bytes
    // moved. Making it observable is what lets a residency policy be checked
    // rather than assumed.
    let out = run(&format!(
        "{SAXPY} \
         [fn main [] \
           [let x [buf #[1 2 3 4]]] \
           [let mut out [buf-zeros 4]] \
           [Place.run saxpy 4 #[1.0 x x out]] \
           [Place.run saxpy 4 #[1.0 x x out]] \
           [let _ [Place.read out]] \
           [let s [Place.stats]] \
           [IO.println [get s :launches]] \
           [IO.println [get s :work-items]] \
           [IO.println [get s :downloads]] \
           [IO.println [get s :bytes-out]]]"
    ));
    // Two launches of four items each, and exactly one read back: 4 f32 = 16 B.
    assert_eq!(out, vec!["2", "8", "1", "16"]);
}

#[test]
fn a_kernel_only_reading_a_buffer_infers_an_input() {
    // Direction comes from the body. `x` and `y` are read with `at`, `out` is
    // written with `put`; nobody wrote `&` or `&mut` anywhere.
    use loon_lang::check::kernel;
    use loon_lang::check::ownership::{infer_param_modes, ParamMode};
    let exprs = loon_lang::parser::parse(SAXPY).expect("parses");
    // Kernels are ordinary functions by the time anything analyzes them, which
    // is why they get ownership modes at all.
    let (desugared, names) = kernel::desugar(&exprs);
    assert!(names.contains("saxpy"));
    let modes = infer_param_modes(&desugared);
    assert_eq!(
        modes.get("saxpy").expect("saxpy has modes"),
        &vec![
            ParamMode::Borrow,    // i
            ParamMode::Borrow,    // a
            ParamMode::Borrow,    // x  — input
            ParamMode::Borrow,    // y  — input
            ParamMode::MutBorrow, // out — must be synchronized back
        ]
    );
}

#[test]
fn reading_past_the_end_of_a_buffer_is_an_error() {
    // A device that answers zero for an out-of-range read teaches you to trust
    // an answer it invented. This one says so instead.
    let err = run_err("[fn main [] [let b [buf #[1 2 3]]] [IO.println [at b 7]]]");
    assert!(
        err.contains("outside a buffer"),
        "expected an out-of-range error, got: {err}"
    );
}

#[test]
fn placing_something_that_is_not_a_kernel_is_an_error() {
    let err = run_err("[fn main [] [Place.run 42 3]]");
    assert!(
        err.contains("expects a kernel"),
        "expected a kernel-shape error, got: {err}"
    );
}

#[test]
fn a_negative_work_count_is_an_error() {
    let err = run_err(&format!(
        "{SAXPY} \
         [fn main [] \
           [let x [buf #[1]]] [let mut o [buf-zeros 1]] \
           [Place.run saxpy -1 #[1.0 x x o]]]"
    ));
    assert!(
        err.contains("work count"),
        "expected a work-count error, got: {err}"
    );
}

#[test]
fn buffers_round_trip_through_every_element_type() {
    let out = run("[fn main [] \
           [IO.println [buf-dtype [buf #[1 2]]]] \
           [IO.println [buf-dtype [buf-i32 #[1 2]]]] \
           [IO.println [buf-dtype [buf-f64 #[1 2]]]] \
           [IO.println [buf->vec [buf-i32 #[1 2 3]]]] \
           [IO.println [buf-len [buf-zeros 5]]]]");
    assert_eq!(out, vec!["f32", "i32", "f64", "#[1 2 3]", "5"]);
}
