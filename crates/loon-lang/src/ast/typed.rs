use crate::ast::{Expr, ExprKind, NodeId};
use crate::syntax::Span;
use crate::types::{Subst, Type};

use std::collections::HashMap;

/// A typed expression — every node carries its resolved type.
#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Keyword(String),
    Symbol(String),
    List(Vec<TypedExpr>),
    Vec(Vec<TypedExpr>),
    Map(Vec<(TypedExpr, TypedExpr)>),
    Set(Vec<TypedExpr>),
    Tuple(Vec<TypedExpr>),
}

/// Convert an untyped AST to a typed AST using the type checker's side-table.
///
/// Looks up each node's type from `type_of`, resolves it through `subst`,
/// and defaults to `Type::Unit` for nodes not present in the table.
pub fn to_typed(expr: &Expr, type_of: &HashMap<NodeId, Type>, subst: &Subst) -> TypedExpr {
    let ty = type_of
        .get(&expr.id)
        .map(|t| subst.resolve(t))
        .unwrap_or(Type::Unit);

    let kind = match &expr.kind {
        ExprKind::Int(n) => TypedExprKind::Int(*n),
        ExprKind::Float(n) => TypedExprKind::Float(*n),
        ExprKind::Bool(b) => TypedExprKind::Bool(*b),
        ExprKind::Str(s) => TypedExprKind::Str(s.clone()),
        ExprKind::Keyword(k) => TypedExprKind::Keyword(k.clone()),
        ExprKind::Symbol(s) => TypedExprKind::Symbol(s.clone()),
        ExprKind::List(items) => {
            TypedExprKind::List(items.iter().map(|e| to_typed(e, type_of, subst)).collect())
        }
        ExprKind::Vec(items) => {
            TypedExprKind::Vec(items.iter().map(|e| to_typed(e, type_of, subst)).collect())
        }
        ExprKind::Map(pairs) => TypedExprKind::Map(
            pairs
                .iter()
                .map(|(k, v)| (to_typed(k, type_of, subst), to_typed(v, type_of, subst)))
                .collect(),
        ),
        ExprKind::Set(items) => {
            TypedExprKind::Set(items.iter().map(|e| to_typed(e, type_of, subst)).collect())
        }
        ExprKind::Tuple(items) => {
            TypedExprKind::Tuple(items.iter().map(|e| to_typed(e, type_of, subst)).collect())
        }
        // Quasiquote nodes should be expanded before type checking
        ExprKind::Quote(inner) | ExprKind::Unquote(inner) | ExprKind::UnquoteSplice(inner) => {
            return to_typed(inner, type_of, subst);
        }
    };

    TypedExpr {
        kind,
        ty,
        span: expr.span,
    }
}

/// Convert an entire program (slice of untyped expressions) to typed AST.
pub fn to_typed_program(
    exprs: &[Expr],
    type_of: &HashMap<NodeId, Type>,
    subst: &Subst,
) -> Vec<TypedExpr> {
    exprs.iter().map(|e| to_typed(e, type_of, subst)).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::Checker;
    use crate::parser::parse;

    /// Helper: parse source, run the checker, convert to typed AST.
    fn typed_program(src: &str) -> (Vec<TypedExpr>, Checker) {
        let exprs = parse(src).unwrap();
        let mut checker = Checker::new();
        checker.check_program(&exprs);
        let expanded = checker.expanded_program.clone();
        let typed = to_typed_program(&expanded, &checker.type_of, &checker.subst);
        (typed, checker)
    }

    #[test]
    fn int_literal_has_int_type() {
        let (typed, _) = typed_program("42");
        assert_eq!(typed.len(), 1);
        assert_eq!(typed[0].ty, Type::Int);
        assert!(matches!(typed[0].kind, TypedExprKind::Int(42)));
    }

    #[test]
    fn string_literal_has_str_type() {
        let (typed, _) = typed_program("\"hello\"");
        assert_eq!(typed.len(), 1);
        assert_eq!(typed[0].ty, Type::Str);
    }

    #[test]
    fn bool_literal_has_bool_type() {
        let (typed, _) = typed_program("true");
        assert_eq!(typed.len(), 1);
        assert_eq!(typed[0].ty, Type::Bool);
    }

    #[test]
    fn arithmetic_expr_has_int_type() {
        let (typed, _) = typed_program("[+ 1 2]");
        assert_eq!(typed.len(), 1);
        assert_eq!(typed[0].ty, Type::Int);
    }

    #[test]
    fn fn_def_has_fn_type() {
        let (typed, checker) = typed_program("[fn add [x y] [+ x y]]\nadd");
        assert_eq!(typed.len(), 2);
        // The reference to `add` should carry a Fn type (may contain type vars
        // from let-polymorphism, so resolve through subst).
        let resolved = checker.subst.resolve(&typed[1].ty);
        match &resolved {
            Type::Fn(params, _ret) => {
                assert_eq!(params.len(), 2);
            }
            other => panic!("expected Fn type, got {other:?}"),
        }
    }

    #[test]
    fn if_expr_has_branch_type() {
        let (typed, _) = typed_program("[if true 1 2]");
        assert_eq!(typed.len(), 1);
        assert_eq!(typed[0].ty, Type::Int);
    }

    #[test]
    fn if_expr_with_string_branches() {
        let (typed, _) = typed_program("[if true \"a\" \"b\"]");
        assert_eq!(typed.len(), 1);
        assert_eq!(typed[0].ty, Type::Str);
    }

    #[test]
    fn let_binding_type() {
        let (typed, _) = typed_program("[let x 10]");
        assert_eq!(typed.len(), 1);
        // let returns the value type
        assert_eq!(typed[0].ty, Type::Int);
    }

    #[test]
    fn multiple_exprs_program() {
        let (typed, _) = typed_program("[let x 1]\n[let y true]\nx");
        assert_eq!(typed.len(), 3);
        assert_eq!(typed[0].ty, Type::Int);
        assert_eq!(typed[1].ty, Type::Bool);
        assert_eq!(typed[2].ty, Type::Int);
    }

    #[test]
    fn nested_list_preserves_children_types() {
        let (typed, _) = typed_program("[+ 1 2]");
        if let TypedExprKind::List(items) = &typed[0].kind {
            // the `+` symbol, and the two int literals
            assert_eq!(items.len(), 3);
            assert_eq!(items[1].ty, Type::Int);
            assert_eq!(items[2].ty, Type::Int);
        } else {
            panic!("expected List");
        }
    }
}
