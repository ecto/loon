//! pkg.oo capability-grant enforcement (E0404): a dependency whose inferred
//! effect row exceeds its declared `:grant` fails at check time. Static
//! supply-chain security — the checker proves a dep can't touch the network
//! or filesystem unless the manifest says so.

use loon_lang::check::Checker;
use std::path::PathBuf;

/// Build a temp project dir with a pkg.oo, a dep module, and return the
/// E0404 diagnostics from checking `main_src` against it.
fn check_project(manifest: &str, dep_name: &str, dep_src: &str, main_src: &str) -> Vec<String> {
    static COUNTER: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
    let n = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let dir: PathBuf = std::env::temp_dir().join(format!("loon_grants_{}_{n}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("pkg.oo"), manifest).unwrap();
    std::fs::write(dir.join(format!("{dep_name}.oo")), dep_src).unwrap();

    let exprs = loon_lang::parser::parse(main_src).expect("parse");
    let mut checker = Checker::with_base_dir(&dir);
    let errors = checker.check_program(&exprs);
    let out = errors
        .iter()
        .filter(|e| e.code.as_str() == "E0404")
        .map(|e| e.what.clone())
        .collect();
    let _ = std::fs::remove_dir_all(&dir);
    out
}

const MANIFEST_NET_ONLY: &str = r#"{
  :name "p"
  :version "0.1.0"
  :deps { "netlib" {:path "./netlib" :grant #["Net"]} }
}"#;

#[test]
fn ungranted_effect_is_a_check_error() {
    let violations = check_project(
        MANIFEST_NET_ONLY,
        "netlib",
        // claims Net-only, also reads files
        r#"[pub fn fetch [url] [Net.get url]]
           [pub fn sneaky [] [IO.read-file "/etc/passwd"]]"#,
        "[use netlib] [fn main [] 0]",
    );
    assert_eq!(violations.len(), 1, "{violations:?}");
    assert!(
        violations[0].contains("performs effect `IO`") && violations[0].contains("sneaky"),
        "{violations:?}"
    );
}

#[test]
fn granted_effects_pass() {
    let violations = check_project(
        MANIFEST_NET_ONLY,
        "netlib",
        "[pub fn fetch [url] [Net.get url]]",
        "[use netlib] [fn main [] 0]",
    );
    assert!(violations.is_empty(), "{violations:?}");
}

#[test]
fn no_grant_means_pure_default_deny() {
    let manifest = r#"{
  :name "p"
  :version "0.1.0"
  :deps { "lib" {:path "./lib"} }
}"#;
    let violations = check_project(
        manifest,
        "lib",
        r#"[pub fn helper [] [IO.println "hi"]]"#,
        "[use lib] [fn main [] 0]",
    );
    assert_eq!(violations.len(), 1, "{violations:?}");
    assert!(violations[0].contains("declares it pure"), "{violations:?}");
}

#[test]
fn user_declared_effects_are_not_subject_to_grants() {
    // A dep-declared effect needs a caller-supplied handler to do anything —
    // no ambient authority, so no grant required.
    let violations = check_project(
        MANIFEST_NET_ONLY,
        "netlib",
        "[effect Retry [ask [] Int]] [pub fn plan [] [Retry.ask]]",
        "[use netlib] [fn main [] 0]",
    );
    assert!(violations.is_empty(), "{violations:?}");
}

#[test]
fn non_dep_local_modules_are_unrestricted() {
    // A plain project-local module (not in :deps) can use any effect.
    let manifest = r#"{
  :name "p"
  :version "0.1.0"
  :deps {}
}"#;
    let violations = check_project(
        manifest,
        "helper",
        r#"[pub fn log [] [IO.println "x"]]"#,
        "[use helper] [fn main [] 0]",
    );
    assert!(violations.is_empty(), "{violations:?}");
}
