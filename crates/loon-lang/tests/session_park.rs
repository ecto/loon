//! A program that stops, and a host that finishes it later.
//!
//! This is the shape an asynchronous embedder needs. The browser cannot answer
//! `Place.read` immediately — reading a GPU buffer back is a promise — but it
//! does not have to answer immediately. It can take the continuation, go away,
//! and come back when the bytes arrive.
//!
//! The VM is not asynchronous and does not become asynchronous. It just has to
//! still be there when the answer shows up, which is what `Session` is for: a
//! parked continuation is a heap object, so dropping the VM would drop the rest
//! of the program with it.

use loon_lang::eir::place::Mode;
use loon_lang::eir::vm::{Session, Step};

fn dir() -> std::path::PathBuf {
    std::env::current_dir().expect("cwd")
}

/// A program that suspends once, in the middle of an expression.
const PARKING: &str = "[effect Slow [fetch [Int] Int]] \
     [effect Host [park [a] Unit]] \
     [fn work [] \
       [IO.println \"before\"] \
       [let a [Slow.fetch 1]] \
       [IO.println [str \"after \" a]] \
       [* a 2]] \
     [fn suspending [thunk] \
       [handle [thunk] [Slow.fetch id] [do [Host.park resume] 0]]] \
     [fn main [] [IO.println [str \"result \" [suspending work]]]]";

#[test]
fn a_program_can_park_and_be_finished_by_its_host() {
    let mut session = Session::new(PARKING, &dir(), Mode::Cpu).expect("prepares");

    // It runs until it needs something the host has to supply.
    let step = session.start().expect("starts");
    assert!(
        matches!(step, Step::Parked { .. }),
        "the program should have parked"
    );
    // "before" ran; so did the code after the handle, with the placeholder —
    // see `code_after_the_parking_handler_runs_before_the_answer_arrives`.
    assert_eq!(session.take_output(), vec!["before", "result 0"]);

    let k = session.pending().expect("a parked continuation");

    // The host does whatever it could not do synchronously, then finishes.
    let answer = loon_lang::eir::value64::Val::int(21);
    let step = session.resume(k, answer).expect("resumes");
    match step {
        Step::Done(_) => {}
        Step::Parked { .. } => panic!("it should have finished this time"),
    }

    // The suspended part picked up mid-expression with the supplied value, and
    // its result comes back through `resume`.
    let out = session.take_output();
    assert!(
        out.iter().any(|l| l == "after 21"),
        "the continuation should have carried the supplied value: {out:?}"
    );
    assert_eq!(session.show(session.value()), "42");
}

#[test]
fn a_program_that_never_parks_just_finishes() {
    // The same API for the ordinary case, so a host does not need two paths.
    let mut session = Session::new(
        "[fn main [] [IO.println \"straight through\"] 7]",
        &dir(),
        Mode::Cpu,
    )
    .expect("prepares");

    match session.start().expect("runs") {
        Step::Done(_) => {}
        Step::Parked { .. } => panic!("nothing here parks"),
    }
    assert_eq!(session.take_output(), vec!["straight through"]);
    assert!(session.pending().is_none());
}

#[test]
fn a_parked_read_can_be_answered_with_data_the_host_fetched() {
    // The placement case the whole thing is for: `Place.read` parks, the host
    // supplies the numbers, and the rest of the computation runs with them.
    //
    // Note where the parking handler sits — outermost, with nothing after it.
    // That is not decoration. Parking unwinds to the `handle`, so anything
    // written after it would run immediately, with the placeholder, before the
    // real answer existed. The value of a deferred computation comes back from
    // `resume`, not from the call that parked.
    let src = "[effect Host [park [a] Unit]] \
         [kernel k [i b] [put b i [* 2.0 [at b i]]]] \
         [fn work [] \
           [let mut b [buf #[1 2 3]]] \
           [Place.run k 3 #[b]] \
           [let got [Place.read b]] \
           [IO.println [str \"read \" got]] \
           [sum got]] \
         [fn deferred [thunk] \
           [handle [thunk] [Place.read b] [do [Host.park resume] #[]]]] \
         [fn main [] [deferred work]]";

    let mut session = Session::new(src, &dir(), Mode::Cpu).expect("prepares");
    let Step::Parked { .. } = session.start().expect("starts") else {
        panic!("the read should have parked");
    };
    let k = session.pending().expect("a continuation");

    // Pretend these came back from a device.
    let data = session.vec_of_floats(&[2.0, 4.0, 6.0]);
    let step = session.resume(k, data).expect("resumes");
    assert!(matches!(step, Step::Done(_)));

    let out = session.take_output();
    assert!(
        out.iter().any(|l| l == "read #[2 4 6]"),
        "the read should return what the host supplied: {out:?}"
    );
    // The rest of the suspended computation returns its value through `resume`.
    assert_eq!(session.show(session.value()), "12");
}

#[test]
fn code_after_the_parking_handler_runs_before_the_answer_arrives() {
    // Worth pinning, because it is the one surprising thing about parking and
    // it decides where a host must put the handler. Unwinding means the caller
    // of `handle` carries on immediately with whatever the clause returned; the
    // suspended part is what waits.
    let src = "[effect Slow [fetch [Int] Int]] \
         [effect Host [park [a] Unit]] \
         [fn work [] [let a [Slow.fetch 1]] [IO.println \"inner: resumed\"] a] \
         [fn suspending [thunk] \
           [handle [thunk] [Slow.fetch id] [do [Host.park resume] 0]]] \
         [fn main [] [let r [suspending work]] [IO.println \"outer: carried on\"] r]";

    let mut session = Session::new(src, &dir(), Mode::Cpu).expect("prepares");
    let Step::Parked { .. } = session.start().expect("starts") else {
        panic!("should park");
    };
    // The outer program already finished, using the placeholder.
    assert_eq!(session.take_output(), vec!["outer: carried on"]);

    let k = session.pending().expect("a continuation");
    session
        .resume(k, loon_lang::eir::value64::Val::int(5))
        .expect("resumes");
    assert_eq!(session.take_output(), vec!["inner: resumed"]);
}
