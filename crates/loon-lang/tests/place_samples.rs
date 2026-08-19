//! The placement samples, run end to end on the EIR VM.
//!
//! `samples/place/lib.oo` is ordinary Loon — no compiler support, no
//! privileges — so these run it the way a user would and check the claims the
//! demos exist to make: that every handler computes the same answer, that a
//! residency policy really does eliminate transfers, and that a parked
//! computation can be finished from outside.

use loon_lang::eir::vm::eval_eir_with_base_dir;
use std::path::{Path, PathBuf};

fn place_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join("samples")
        .join("place")
}

/// Run a Loon source string with `[use ...]` resolved against samples/place/.
fn run(src: &str) -> Vec<String> {
    eval_eir_with_base_dir(src, &place_dir())
        .unwrap_or_else(|e| panic!("vm error: {e}"))
        .output
}

/// Run a sample file and return its printed lines.
fn run_demo(name: &str) -> Vec<String> {
    let path = place_dir().join(name);
    let src = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {name}: {e}"));
    run(&src)
}

#[test]
fn place_demo_gives_the_same_answer_under_every_handler() {
    // The claim the demo exists to make: the program is untouched between
    // runs, only the handler wrapped around it changes, and the computed
    // result is identical in each case. A placement policy that changed the
    // answer would not be a policy, it would be a bug.
    let out = run_demo("demo-handlers.oo");
    let joined = out.join("\n");

    let expected = "#[11 21 31 41 51 61 71 81]";
    let answers = out.iter().filter(|l| l.contains(expected)).count();
    assert_eq!(
        answers, 3,
        "unhandled, traced, and resident runs should all agree:\n{joined}"
    );

    // Tracing is interposition: the handler sees each launch and the single
    // point where the host asks for data back.
    assert!(
        joined.contains("place: launch 8 work items, 3 args"),
        "{joined}"
    );
    assert!(joined.contains("place: sync"), "{joined}");

    // The dry run accounts for the work without performing any of it, which is
    // how a program can be exercised without the hardware it targets.
    assert!(
        joined.contains("#[]"),
        "dry run should return nothing:\n{joined}"
    );
}

#[test]
fn place_handlers_do_not_need_compiler_support() {
    // Every handler in samples/place/lib.oo is ordinary Loon. This test pins that a
    // policy can be written inline, in a test file, with no privileges: the
    // residency and transfer-hoisting logic a GPU compiler implements as an
    // optimization pass is expressible as user code here.
    let out = run("[kernel k [i b] [put b i [* 2.0 [at b i]]]] \
         [fn work [] [let mut b [buf #[1 2 3]]] [Place.run k 3 #[b]] [Place.read b]] \
         [fn twice-as-many [thunk] \
           [handle [thunk] \
             [Place.run kf n args] [do [Place.run kf n args] \
                                       [resume [Place.run kf n args]]]]] \
         [fn main [] [IO.println [work]] [IO.println [twice-as-many work]]]");
    // Running the kernel twice per launch really does double it again.
    assert_eq!(out, vec!["#[2 4 6]", "#[4 8 12]"]);
}

#[test]
fn residency_demo_closes_the_transfer_gap() {
    // The demo's whole claim, checked: the same eight-launch chain pays eight
    // uploads with no policy and one upload under a residency handler, and
    // both runs produce the same answer. This is the gap a GPU offload
    // compiler needs a dedicated optimization pass to close.
    let path = place_dir().join("demo-residency.oo");
    let src = std::fs::read_to_string(&path).expect("read demo-residency.oo");
    let (result, _stats) = loon_lang::eir::vm::eval_eir_placed(
        &src,
        &place_dir(),
        loon_lang::eir::place::Mode::Device,
    )
    .expect("demo runs");
    let joined = result.output.join("\n");

    assert!(
        joined.contains("no policy: uploads 8, resident hits 0"),
        "without a policy every launch transfers:\n{joined}"
    );
    assert!(
        joined.contains("place/resident: uploads 1, resident hits 7"),
        "the handler should upload once and hit seven times:\n{joined}"
    );
    assert_eq!(
        result
            .output
            .iter()
            .filter(|l| l.contains("#[8 8 8 8]"))
            .count(),
        2,
        "both runs must compute the same answer:\n{joined}"
    );
}

#[test]
fn on_the_cpu_the_residency_demo_moves_nothing() {
    // Same program, same handler, one memory: the policy is simply describing
    // a distinction this hardware does not have, and costs nothing to keep.
    let path = place_dir().join("demo-residency.oo");
    let src = std::fs::read_to_string(&path).expect("read demo-residency.oo");
    let (_, stats) =
        loon_lang::eir::vm::eval_eir_placed(&src, &place_dir(), loon_lang::eir::place::Mode::Cpu)
            .expect("demo runs");
    assert_eq!(stats.uploads, 0, "the CPU has nothing to upload to");
    assert_eq!(stats.launches, 16);
}

#[test]
fn a_computation_can_be_parked_and_finished_later() {
    // The mechanism an asynchronous host needs, and it is already here: a
    // handler clause that hands `resume` outward and returns unwinds the
    // computation without ending it. Whoever holds the continuation decides
    // when — and whether — the rest of it runs.
    //
    // This is what makes "the browser cannot answer a GPU read immediately"
    // a solvable problem rather than a blocking one. No VM support was added
    // for it; reified escaping continuations are what handlers already are.
    let out = run_demo("demo-park.oo");
    assert_eq!(
        out,
        vec![
            "  work: starting",
            "  host: computation parked; the rest of it is mine now",
            "  host: ...doing something slow...",
            "  work: continued with 21",
            "  host: finished with 42",
            "done",
        ],
        "the work should stop, unwind, and then continue where it left off"
    );
}

#[test]
fn a_parked_continuation_survives_its_handler_returning() {
    // The ordering is the claim: "work: continued" appears *after* the host
    // has already printed, which is only possible if the computation really
    // unwound and was restarted from the outside.
    let out = run_demo("demo-park.oo").join("\n");
    let parked = out.find("computation parked").expect("parked");
    let continued = out.find("work: continued").expect("continued");
    assert!(
        parked < continued,
        "the continuation must resume after the handler returned:\n{out}"
    );
}
