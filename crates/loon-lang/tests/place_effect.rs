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

// ── Residency: the gap a handler closes ─────────────────────────────────────

/// Run in device mode, where buffers live in a separate memory and transfers
/// are counted.
fn run_on_device(src: &str) -> loon_lang::eir::place::PlaceStats {
    let dir = std::env::current_dir().expect("cwd");
    match loon_lang::eir::vm::eval_eir_placed(src, &dir, loon_lang::eir::place::Mode::Device) {
        Ok((_, stats)) => stats,
        Err(e) => panic!("VM error: {e:?}\nsource:\n{src}"),
    }
}

/// A chain of `n` launches over the same buffer, optionally wrapped in a
/// handler. The program text is identical either way.
fn chain_program(launches: usize, wrapper: Option<&str>) -> String {
    let runs = (0..launches)
        .map(|_| "[Place.run step 4 #[1.0 b]]".to_string())
        .collect::<Vec<_>>()
        .join(" ");
    let call = match wrapper {
        Some(w) => format!("[{w} work]"),
        None => "[work]".to_string(),
    };
    format!(
        "[kernel step [i s b] [put b i [* s [at b i]]]] \
         [fn work [] [let mut b [buf #[1 2 3 4]]] {runs} [Place.read b]] \
         [fn resident [thunk] \
           [handle [thunk] \
             [Place.run k n args] [do [Place.pin args] [resume [Place.run k n args]]] \
             [Place.read b]       [resume [Place.read b]]]] \
         [fn main [] [IO.println {call}]]"
    )
}

#[test]
fn without_a_policy_every_launch_pays_for_its_own_transfer() {
    // A device that has not been told a buffer will be wanted again does not
    // keep it. This is the honest default, and it is the behaviour that makes
    // the naive interface of an offload compiler slow.
    for launches in [1usize, 4, 16] {
        let stats = run_on_device(&chain_program(launches, None));
        assert_eq!(
            stats.uploads, launches as u64,
            "{launches} launches should cost {launches} uploads"
        );
        assert_eq!(stats.resident_hits, 0);
    }
}

#[test]
fn a_residency_handler_pays_once_no_matter_how_long_the_chain() {
    // The same program, wrapped in a handler that pins what each launch
    // touches. One upload, and every launch after the first finds the buffer
    // already there. Nothing in `work` changed, and no compiler pass ran.
    for launches in [1usize, 4, 16] {
        let stats = run_on_device(&chain_program(launches, Some("resident")));
        assert_eq!(
            stats.uploads, 1,
            "a chain of {launches} should upload exactly once"
        );
        assert_eq!(
            stats.resident_hits,
            launches as u64 - 1,
            "every launch after the first should be a residency hit"
        );
        // The host still asks for its answer exactly once.
        assert_eq!(stats.downloads, 1);
    }
}

#[test]
fn the_saving_grows_with_the_chain_and_the_answer_does_not_change() {
    // The property worth stating: a policy changes what it costs to get the
    // answer, never the answer. A "policy" that changed the result would be a
    // bug wearing a nicer name.
    let long_chain: usize = 32;
    let naive = run_on_device(&chain_program(long_chain, None));
    let resident = run_on_device(&chain_program(long_chain, Some("resident")));

    assert_eq!(naive.uploads, long_chain as u64);
    assert_eq!(resident.uploads, 1);
    assert!(
        naive.bytes_in >= resident.bytes_in * 30,
        "expected roughly a {long_chain}x reduction in bytes moved, got {} vs {}",
        naive.bytes_in,
        resident.bytes_in
    );

    // Both programs computed the same thing.
    let bare = run(&chain_program(long_chain, None));
    let wrapped = run(&chain_program(long_chain, Some("resident")));
    assert_eq!(bare, wrapped);
}

#[test]
fn on_the_cpu_there_is_nothing_to_transfer() {
    // One memory means no uploads at all, whatever the policy says. The
    // handler is not wrong here, it is simply describing a distinction the
    // hardware does not have.
    let src = chain_program(8, Some("resident"));
    let dir = std::env::current_dir().expect("cwd");
    let (_, stats) =
        loon_lang::eir::vm::eval_eir_placed(&src, &dir, loon_lang::eir::place::Mode::Cpu)
            .expect("runs");
    assert_eq!(stats.uploads, 0);
    assert_eq!(stats.launches, 8);
}

// ── Every placement gives the same answer ───────────────────────────────────

#[test]
fn cpu_and_parallel_agree_on_everything() {
    // The property that makes placement a policy rather than a rewrite: the
    // answer does not depend on where the work happened. Parallel execution
    // hands each thread a disjoint slice of the output, so this is also the
    // test that the split is right.
    use loon_lang::eir::place::Mode;
    let dir = std::env::current_dir().expect("cwd");

    let programs = [
        "[kernel saxpy [i a x y out] [put out i [+ [* a [at x i]] [at y i]]]] \
         [fn main [] \
           [let x [buf [range 0 1000]]] [let y [buf [range 0 1000]]] \
           [let mut out [buf-zeros 1000]] \
           [Place.run saxpy 1000 #[2.0 x y out]] \
           [IO.println [sum [Place.read out]]]]",
        "[kernel clamp [i lo hi b] [let v [at b i]] \
           [put b i [if [< v lo] lo [if [> v hi] hi v]]]] \
         [fn main [] \
           [let mut b [buf [range 0 500]]] \
           [Place.run clamp 500 #[10.0 100.0 b]] \
           [IO.println [sum [Place.read b]]]]",
        "[kernel mathy [i b] [put b i [sqrt [abs [at b i]]]]] \
         [fn main [] \
           [let mut b [buf [range 0 777]]] \
           [Place.run mathy 777 #[b]] \
           [IO.println [len [Place.read b]]]]",
    ];

    for src in programs {
        let (cpu, _) = loon_lang::eir::vm::eval_eir_placed(src, &dir, Mode::Cpu).expect("cpu");
        let (par, _) = loon_lang::eir::vm::eval_eir_placed(src, &dir, Mode::Par).expect("par");
        assert_eq!(
            cpu.output, par.output,
            "parallel placement changed the answer for:\n{src}"
        );
        assert!(!cpu.output.is_empty());
    }
}

#[test]
fn a_kernel_outside_the_fast_subset_still_runs() {
    // The typed executor covers the numeric subset. Anything else falls back
    // to the general VM rather than failing, so adding the fast path cannot
    // have narrowed what a kernel is allowed to be.
    use loon_lang::eir::place::Mode;
    let dir = std::env::current_dir().expect("cwd");
    let src = "[kernel k [i b] [put b i [+ [at b i] 1.0]]] \
               [fn main [] [let mut b [buf-zeros 4]] \
                 [Place.run k 4 #[b]] [IO.println [Place.read b]]]";
    for mode in [Mode::Cpu, Mode::Par] {
        let (r, _) = loon_lang::eir::vm::eval_eir_placed(src, &dir, mode).expect("runs");
        assert_eq!(r.output, vec!["#[1 1 1 1]"]);
    }
}
