use loon_lang::check::Checker;
use loon_lang::parser::parse;
use std::path::Path;

/// Check a file with module support, returning type errors.
fn check_file(path: &str) -> Vec<String> {
    let full = Path::new(env!("CARGO_MANIFEST_DIR")).join(path);
    let source = std::fs::read_to_string(&full).expect("read test file");
    let exprs = parse(&source).expect("parse test file");
    let base_dir = full.parent().unwrap();
    let mut checker = Checker::with_base_dir(base_dir);
    let errors = checker.check_program(&exprs);
    errors.into_iter().map(|e| e.what).collect()
}

#[test]
fn use_selective_import_type_checks() {
    let errors = check_file("tests/modules/main.loon");
    assert!(
        errors.is_empty(),
        "expected no type errors for valid use, got: {errors:?}"
    );
}

#[test]
fn use_unexported_name_errors() {
    // internal-helper is not pub in math.loon
    let source = r#"[use math [internal-helper]]
[internal-helper 5]"#;
    let full = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/modules/main.loon");
    let base_dir = full.parent().unwrap();
    let exprs = parse(source).expect("parse");
    let mut checker = Checker::with_base_dir(base_dir);
    let errors = checker.check_program(&exprs);
    let msgs: Vec<String> = errors.into_iter().map(|e| e.what).collect();
    assert!(
        msgs.iter().any(|m| m.contains("does not export")),
        "expected 'does not export' error, got: {msgs:?}"
    );
}

#[test]
fn use_qualified_import() {
    let source = r#"[use math]
[math.double 5]"#;
    let full = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/modules/main.loon");
    let base_dir = full.parent().unwrap();
    let exprs = parse(source).expect("parse");
    let mut checker = Checker::with_base_dir(base_dir);
    let errors = checker.check_program(&exprs);
    let msgs: Vec<String> = errors.into_iter().map(|e| e.what).collect();
    assert!(
        msgs.is_empty(),
        "expected no errors for qualified import, got: {msgs:?}"
    );
}

#[test]
fn use_aliased_import() {
    let source = r#"[use math :as m]
[m.double 5]"#;
    let full = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/modules/main.loon");
    let base_dir = full.parent().unwrap();
    let exprs = parse(source).expect("parse");
    let mut checker = Checker::with_base_dir(base_dir);
    let errors = checker.check_program(&exprs);
    let msgs: Vec<String> = errors.into_iter().map(|e| e.what).collect();
    assert!(
        msgs.is_empty(),
        "expected no errors for aliased import, got: {msgs:?}"
    );
}
