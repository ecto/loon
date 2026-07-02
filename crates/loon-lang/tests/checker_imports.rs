//! Checker import binding: `[use module]` must bind exported names both
//! qualified (`module.name`) and unqualified (`name`), matching the runtime
//! (lower.rs collect_imports splices all module forms into the program).
//! Regression: `loon check` used to report E0201 unbound for names that
//! resolved fine under `loon run`.

use loon_lang::check::Checker;
use std::path::PathBuf;

/// Check `main_src` against a temp project containing `purelib.oo`; return
/// all diagnostic messages (excluding warnings).
fn check_with_purelib(main_src: &str) -> Vec<String> {
    static COUNTER: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
    let n = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let dir: PathBuf =
        std::env::temp_dir().join(format!("loon_imports_{}_{n}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(
        dir.join("purelib.oo"),
        "[pub fn add [a b] [+ a b]]\n[pub fn double [x] [* x 2]]",
    )
    .unwrap();

    let exprs = loon_lang::parser::parse(main_src).expect("parse");
    let mut checker = Checker::with_base_dir(&dir);
    let errors = checker.check_program(&exprs);
    let out = errors
        .iter()
        .filter(|e| !e.code.is_warning())
        .map(|e| format!("[{}] {}", e.code, e.what))
        .collect();
    let _ = std::fs::remove_dir_all(&dir);
    out
}

#[test]
fn unqualified_import_binds() {
    let errors = check_with_purelib("[use purelib] [fn main [] [IO.println [add 1 2]]]");
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn qualified_import_still_binds() {
    let errors = check_with_purelib("[use purelib] [fn main [] [IO.println [purelib.add 1 2]]]");
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn alias_import_binds_both_forms() {
    let errors = check_with_purelib(
        "[use purelib :as p] [fn main [] [IO.println [+ [p.add 1 2] [add 3 4]]]]",
    );
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn selective_import_stays_selective() {
    // Asking for a name the module doesn't export is still an error.
    let errors = check_with_purelib("[use purelib [nonexistent]] [fn main [] 0]");
    assert!(
        errors.iter().any(|e| e.contains("E0501")),
        "selective import of a missing name should error: {errors:?}"
    );
}

#[test]
fn truly_unbound_symbols_still_error() {
    // The fix must not blanket-suppress E0201.
    let errors = check_with_purelib("[use purelib] [fn main [] [IO.println [subtract 1 2]]]");
    assert!(
        errors.iter().any(|e| e.contains("E0201")),
        "an unexported name must still be unbound: {errors:?}"
    );
}
