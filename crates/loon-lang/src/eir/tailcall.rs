//! Tail-call marking: rewrite calls in tail position to `End::Tail`.
//!
//! The lowering builds every call the same way — `Op::Call` into a register,
//! then a jump to whatever block merges the surrounding `if`/`match`/clause
//! arms, which returns that register. Tail position is therefore not a
//! property the lowering tracks; it is a *shape* in the finished IR:
//!
//! ```text
//!   b2: ...; Call(r6, odd?, [r5])      b2: ...
//!       Jmp(b3, [r6])            =>        Tail(odd?, [r5])
//!   b3(p7): Ret(p7)                    b3(p7): Ret(p7)   (now unreachable)
//! ```
//!
//! Recognizing the shape after the fact covers every construct at once — `if`,
//! `match`, `do`, `when`, multi-clause arity dispatch — without threading a
//! tail-position flag through all of `lower_expr`.
//!
//! Two rewrites, applied to fixpoint:
//!
//! 1. **Return threading** — a `Jmp` to a block that does nothing but return
//!    one of its own parameters becomes a `Ret` of the corresponding argument.
//! 2. **Tail marking** — a block whose last op is a `Call` producing exactly
//!    the register the block then returns becomes an `End::Tail`, dropping the
//!    op.
//!
//! ## Why only `Op::Call`
//!
//! `Op::Invoke` (closure / function-pointer calls) is deliberately left alone.
//! `End::TailInvoke` is not simply "Invoke in tail position" in the VM: when
//! its callee is a continuation it takes the *tail-resume* path, which reuses
//! the frame below instead of establishing a fresh prompt. `handle` also
//! lowers its body to a thunk called through `Op::Invoke` precisely to create
//! a prompt frame — eliding that frame would move the boundary that delimits
//! captured continuations. A static rewrite cannot tell those cases apart from
//! an ordinary closure call, so it must not try.

use super::{End, Func, Module, Op};

/// Rewrite tail-position calls across every function in the module.
pub fn mark_tail_calls(module: &mut Module) {
    for func in &mut module.funcs {
        // Threading can expose new tail calls and marking can expose new
        // threadable jumps, so alternate until neither fires.
        while thread_returns(func) | mark_calls(func) {}
    }
}

/// Rewrite `Jmp(j, args)` to `Ret(args[i])` when `j` is a pure return block:
/// no ops, and its terminator returns its own `i`th parameter.
///
/// This is what collapses the merge block that `if`/`match` arms jump to. The
/// merge block itself is left in place — other predecessors may still use it,
/// and an unreachable block is harmless.
fn thread_returns(func: &mut Func) -> bool {
    let mut changed = false;

    for idx in 0..func.blocks.len() {
        let End::Jmp(target, ref args) = func.blocks[idx].end else {
            continue;
        };
        let target = target.0 as usize;
        if target == idx || target >= func.blocks.len() {
            continue;
        }

        let dest = &func.blocks[target];
        // Only a block that does nothing but return a parameter is safe to
        // inline into its predecessor. Returning a non-parameter register would
        // move the read to a block where that register may not be defined.
        if !dest.ops.is_empty() || dest.params.len() != args.len() {
            continue;
        }
        let End::Ret(returned) = dest.end else {
            continue;
        };
        let Some(param_idx) = dest.params.iter().position(|p| *p == returned) else {
            continue;
        };

        let arg = args[param_idx];
        func.blocks[idx].end = End::Ret(arg);
        changed = true;
    }

    changed
}

/// Rewrite a trailing `Call(r, f, args)` + `Ret(r)` into `End::Tail(f, args)`.
///
/// The call must be the block's *last* op: anything emitted after it (a
/// `PopHandler`, say) still has to run before the function returns, and a tail
/// call would skip it.
fn mark_calls(func: &mut Func) -> bool {
    let mut changed = false;

    for block in &mut func.blocks {
        let End::Ret(returned) = block.end else {
            continue;
        };
        let Some(Op::Call(dst, callee, args, _)) = block.ops.last() else {
            continue;
        };
        if *dst != returned {
            continue;
        }

        block.end = End::Tail(*callee, args.clone());
        block.ops.pop();
        changed = true;
    }

    changed
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::Checker;
    use crate::eir::{BlockId, FuncId, Reg};
    use crate::parser::parse;

    fn lower_src(src: &str) -> Module {
        let exprs = parse(src).expect("parse failed");
        let mut checker = Checker::new();
        let _ = checker.check_program(&exprs);
        crate::eir::lower::lower(&checker)
    }

    fn func<'a>(module: &'a Module, name: &str) -> &'a Func {
        module
            .funcs
            .iter()
            .find(|f| f.name.as_deref() == Some(name))
            .unwrap_or_else(|| panic!("no function named {name}"))
    }

    fn tail_targets(f: &Func) -> Vec<FuncId> {
        f.blocks
            .iter()
            .filter_map(|b| match &b.end {
                End::Tail(fid, _) => Some(*fid),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn mutual_recursion_becomes_tail_calls() {
        let module = lower_src(
            r#"
            [fn even? [n] [if [= n 0] true [odd? [- n 1]]]]
            [fn odd? [n] [if [= n 0] false [even? [- n 1]]]]
            [even? 10]
        "#,
        );
        let even = func(&module, "even?");
        let odd = func(&module, "odd?");
        assert_eq!(tail_targets(even).len(), 1, "even? should tail-call odd?");
        assert_eq!(tail_targets(odd).len(), 1, "odd? should tail-call even?");
        assert_eq!(tail_targets(even)[0], odd.id);
        assert_eq!(tail_targets(odd)[0], even.id);
    }

    #[test]
    fn tail_call_through_match_is_marked() {
        let module = lower_src(
            r#"
            [fn go [n] [match n 0 :done _ [go [- n 1]]]]
            [go 3]
        "#,
        );
        let go = func(&module, "go");
        assert_eq!(tail_targets(go), vec![go.id]);
    }

    #[test]
    fn tail_call_through_do_is_marked() {
        let module = lower_src(
            r#"
            [fn go [n] [if [= n 0] :done [do [let _ 0] [go [- n 1]]]]]
            [go 3]
        "#,
        );
        assert_eq!(tail_targets(func(&module, "go")).len(), 1);
    }

    /// A call whose result is used is not in tail position and must stay a
    /// `Call` — turning it into a tail call would skip the multiply.
    #[test]
    fn non_tail_call_is_left_alone() {
        let module = lower_src(
            r#"
            [fn fact [n] [if [<= n 1] 1 [* n [fact [- n 1]]]]]
            [fact 5]
        "#,
        );
        let fact = func(&module, "fact");
        assert!(
            tail_targets(fact).is_empty(),
            "a call feeding an arithmetic op is not a tail call"
        );
        assert!(
            fact.blocks
                .iter()
                .any(|b| b.ops.iter().any(|o| matches!(o, Op::Call(..)))),
            "the recursive call should survive as an Op::Call"
        );
    }

    /// `Op::Invoke` is never rewritten — see the module docs.
    #[test]
    fn closure_calls_are_not_marked() {
        let module = lower_src(
            r#"
            [fn apply-it [f x] [f x]]
            [apply-it [fn [y] [+ y 1]] 1]
        "#,
        );
        for f in &module.funcs {
            for b in &f.blocks {
                assert!(
                    !matches!(b.end, End::TailInvoke(..)),
                    "invoke must not be rewritten to a tail invoke"
                );
            }
        }
    }

    /// Ops emitted after the call still have to run, so the call is not in
    /// tail position even though its result is returned.
    #[test]
    fn call_followed_by_another_op_is_not_marked() {
        use crate::eir::{Block, Lit, Ty};
        use crate::syntax::Span;

        let mut module = Module {
            funcs: vec![Func {
                id: FuncId(0),
                name: Some("f".to_string()),
                params: vec![],
                ret: Ty::Any,
                evidence: vec![],
                captures: vec![],
                blocks: vec![Block {
                    id: BlockId(0),
                    params: vec![],
                    ops: vec![
                        Op::Call(Reg(0), FuncId(0), vec![], Span::ZERO),
                        Op::PopHandler(Span::ZERO),
                        Op::Lit(Reg(1), Lit::Unit, Span::ZERO),
                    ],
                    end: End::Ret(Reg(0)),
                }],
                span: Span::ZERO,
                is_closure: false,
            }],
            strings: vec![],
            ctors: vec![],
            entry: FuncId(0),
        };

        mark_tail_calls(&mut module);
        assert!(matches!(module.funcs[0].blocks[0].end, End::Ret(_)));
        assert_eq!(module.funcs[0].blocks[0].ops.len(), 3);
    }
}
