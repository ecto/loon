//! Rust bindings for tree-sitter-loon.
//!
//! This crate provides Rust bindings for the Loon tree-sitter grammar,
//! enabling parsing of Loon source code into concrete syntax trees.

use tree_sitter_language::LanguageFn;

extern "C" {
    fn tree_sitter_loon() -> *const ();
}

/// Returns the tree-sitter [`LanguageFn`] for the Loon grammar.
///
/// # Example
///
/// ```
/// let mut parser = tree_sitter::Parser::new();
/// parser
///     .set_language(&tree_sitter_loon::LANGUAGE.into())
///     .expect("Error loading Loon parser");
///
/// let source = r#"[defn main [] [println "hello"]]"#;
/// let tree = parser.parse(source, None).unwrap();
/// assert!(!tree.root_node().has_error());
/// ```
pub const LANGUAGE: LanguageFn = unsafe { LanguageFn::from_raw(tree_sitter_loon) };

/// The syntax highlighting query for Loon.
pub const HIGHLIGHTS_QUERY: &str = include_str!("../../queries/highlights.scm");

/// The content of the [`node-types.json`][] file for this grammar.
///
/// [`node-types.json`]: https://tree-sitter.github.io/tree-sitter/using-parsers#static-node-types
pub const NODE_TYPES: &str = include_str!("../../src/node-types.json");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_can_load_grammar() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&LANGUAGE.into())
            .expect("Error loading Loon parser");
    }
}
