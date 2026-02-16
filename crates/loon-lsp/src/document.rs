use std::collections::HashMap;

use loon_lang::ast::{Expr, NodeId};
use loon_lang::check::{Checker, DefInfo, RefInfo};
use loon_lang::errors::LoonDiagnostic;
use loon_lang::types::{Scheme, Type};
use ropey::Rope;

/// Per-document state including source text, parsed AST, and checker results.
pub struct DocumentState {
    pub rope: Rope,
    pub version: i32,
    pub exprs: Vec<Expr>,
    pub checker: Option<CheckerState>,
    pub diagnostics: Vec<LoonDiagnostic>,
}

/// Extracted, Send-safe results from a type-check pass.
/// (Checker itself contains Rc<RefCell<..>> which is not Send.)
pub struct CheckerState {
    pub type_of: HashMap<NodeId, Type>,
    pub subst_resolved: HashMap<NodeId, Type>,
    pub definitions: Vec<HashMap<String, DefInfo>>,
    pub references: Vec<RefInfo>,
    pub visible_names: Vec<(String, Scheme)>,
}

impl CheckerState {
    fn from_checker(checker: &Checker) -> Self {
        // Pre-resolve all types so we don't need to keep the Subst around
        let mut subst_resolved = HashMap::new();
        for (id, ty) in &checker.type_of {
            subst_resolved.insert(*id, checker.resolve(ty));
        }

        // Resolve types in visible names
        let visible_names: Vec<(String, Scheme)> = checker
            .env
            .all_visible_names()
            .into_iter()
            .map(|(name, scheme)| {
                let resolved_ty = checker.resolve(&scheme.ty);
                (name, Scheme { bounds: scheme.bounds.clone(), vars: scheme.vars, ty: resolved_ty })
            })
            .collect();

        Self {
            type_of: checker.type_of.clone(),
            subst_resolved,
            definitions: checker.definitions.clone(),
            references: checker.references.clone(),
            visible_names,
        }
    }

    /// Look up the resolved type for a node.
    pub fn get_resolved_type(&self, id: NodeId) -> Option<&Type> {
        self.subst_resolved.get(&id)
    }

    /// Look up definition info for a name across all scopes (innermost first).
    pub fn lookup_definition(&self, name: &str) -> Option<&DefInfo> {
        for scope in self.definitions.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }
}

// Safety: CheckerState contains only owned data (HashMap, Vec, etc.) with no Rc/RefCell.
// The types it stores (Type, Scheme, DefInfo, RefInfo) are all composed of owned data.
unsafe impl Send for CheckerState {}
unsafe impl Sync for CheckerState {}

impl DocumentState {
    pub fn new(text: &str, version: i32) -> Self {
        let mut state = Self {
            rope: Rope::from_str(text),
            version,
            exprs: Vec::new(),
            checker: None,
            diagnostics: Vec::new(),
        };
        state.recheck();
        state
    }

    /// Re-parse and re-check the document. Collects all diagnostics.
    pub fn recheck(&mut self) {
        self.diagnostics.clear();
        self.checker = None;
        self.exprs.clear();

        let source = self.rope.to_string();

        // Parse
        let exprs = match loon_lang::parser::parse(&source) {
            Ok(exprs) => exprs,
            Err(e) => {
                self.diagnostics.push(e.into());
                return;
            }
        };

        // Type check
        let mut checker = Checker::new();
        let type_errors = checker.check_program(&exprs);
        self.diagnostics.extend(type_errors);

        // Ownership check
        let mut ownership =
            loon_lang::check::ownership::OwnershipChecker::with_type_info(
                &checker.type_of,
                &checker.subst,
            )
            .with_derived_copy_types(&checker.derived_copy_types);
        let ownership_errors = ownership.check_program(&exprs);
        self.diagnostics.extend(ownership_errors);

        // Extract Send-safe state from the checker
        let checker_state = CheckerState::from_checker(&checker);

        self.exprs = exprs;
        self.checker = Some(checker_state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_and_check_valid() {
        let state = DocumentState::new("[defn add [x y] [+ x y]]", 1);
        assert!(state.diagnostics.is_empty(), "unexpected diagnostics: {:?}", state.diagnostics);
        assert!(state.checker.is_some());
        assert!(!state.exprs.is_empty());
    }

    #[test]
    fn parse_error_produces_diagnostic() {
        // Unclosed bracket
        let state = DocumentState::new("[defn add [x y", 1);
        assert!(!state.diagnostics.is_empty());
    }

    #[test]
    fn type_error_produces_diagnostic() {
        let state = DocumentState::new("[if 42 1 2]", 1);
        assert!(!state.diagnostics.is_empty());
    }

    #[test]
    fn hover_type_available() {
        let state = DocumentState::new("[let x 42]\nx", 1);
        assert!(state.checker.is_some());
        let checker = state.checker.as_ref().unwrap();
        // The resolved types should be populated
        assert!(!checker.subst_resolved.is_empty());
    }
}
