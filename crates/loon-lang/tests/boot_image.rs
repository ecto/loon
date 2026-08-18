//! The boot image is an ABI between two crates that never link together.
//!
//! `loon-kernel` decodes these tags by number. Nothing in the type system
//! connects the two sides, so reordering an enum here would silently remap
//! an operator in every image the kernel runs. These tests are the seam.

use loon_lang::eir::image;
use loon_lang::eir::{BinOp, UnOp};

#[test]
fn binop_tags_are_pinned() {
    // Changing any of these means changing `binop()` in
    // crates/loon-kernel/src/eir/decode.rs to match.
    let expected = [
        (BinOp::Add, 0),
        (BinOp::Sub, 1),
        (BinOp::Mul, 2),
        (BinOp::Div, 3),
        (BinOp::Rem, 4),
        (BinOp::Eq, 5),
        (BinOp::Ne, 6),
        (BinOp::Lt, 7),
        (BinOp::Gt, 8),
        (BinOp::Le, 9),
        (BinOp::Ge, 10),
        (BinOp::And, 11),
        (BinOp::Or, 12),
        (BinOp::Concat, 13),
    ];
    for (op, tag) in expected {
        assert_eq!(
            op as u8, tag,
            "{op:?} moved: the unikernel decodes it as {tag}"
        );
    }
}

#[test]
fn unop_tags_are_pinned() {
    assert_eq!(UnOp::Neg as u8, 0);
    assert_eq!(UnOp::Not as u8, 1);
}

/// Compile a source string the way `loon image` does.
fn image_of(src: &str) -> Vec<u8> {
    let exprs = loon_lang::parser::parse(src).expect("parse");
    let mut checker = loon_lang::check::Checker::new();
    checker.check_program(&exprs);
    let module = loon_lang::eir::lower::lower(&checker);
    image::encode(&module)
}

#[test]
fn image_has_a_versioned_header() {
    let img = image_of("[println [+ 1 2]]");
    assert_eq!(&img[..8], image::MAGIC);
    assert_eq!(
        u32::from_le_bytes(img[8..12].try_into().unwrap()),
        image::VERSION
    );
}

#[test]
fn image_names_every_builtin_it_references() {
    // The kernel dispatches intrinsics on these names, so an image that
    // uses `println` must carry the string "Println".
    let img = image_of("[println \"hi\"]");
    let text = String::from_utf8_lossy(&img);
    assert!(
        text.contains("Println"),
        "builtin name table missing from the image"
    );
}

#[test]
fn effect_programs_survive_encoding() {
    // Handlers are the whole point of the exercise; make sure a program
    // with a `handle` encodes at all rather than tripping an unreachable.
    let img = image_of(
        r#"
        [effect Console [write [String] Unit]]
        [fn main []
          [handle
            [fn [] [Console.write "x"]]
            [Console.write s]
            [do [print s] [resume []]]]]
        "#,
    );
    assert!(img.len() > 12, "encoded a suspiciously empty image");
}
