use super::kernel::{self, check_proof, term_eq, Goal, Outcome, Rule};
use super::*;

const DEFS: &str = r#"
[type List T [Cons T (List T)] Nil]
[fn append [xs ys]
  [match xs
    Nil           ys
    [Cons x rest] [Cons x [append rest ys]]]]
[fn reverse [xs]
  [match xs
    Nil           Nil
    [Cons x rest] [append [reverse rest] [Cons x Nil]]]]
[fn spin [xs] [spin xs]]
"#;

fn p(s: &str) -> Expr {
    crate::parser::parse(s).unwrap().pop().unwrap()
}

fn defs() -> Defs {
    Defs::from_exprs(&crate::parser::parse(DEFS).unwrap()).unwrap()
}

fn prove(
    defs: &Defs,
    params: &[&str],
    lhs: &str,
    rhs: &str,
    steps: &str,
    lemmas: &kernel::Lemmas,
) -> Outcome {
    let params: Vec<String> = params.iter().map(|s| s.to_string()).collect();
    let types: HashMap<String, kernel::Ty> = params
        .iter()
        .map(|p| (p.clone(), kernel::Ty::named("List")))
        .collect();
    let (l, r) = (p(lhs), p(rhs));
    let goal = Goal {
        law_name: "t",
        params: &params,
        param_types: &types,
        lhs: &l,
        rhs: &r,
    };
    let steps = crate::parser::parse(steps).unwrap();
    check_proof(&goal, &steps, defs, lemmas)
}

fn none(_: &str) -> Option<(Rule, bool)> {
    None
}

#[test]
fn totality() {
    let d = defs();
    assert!(d.total.contains("append"));
    assert!(d.total.contains("reverse"));
    assert!(!d.total.contains("spin"));
}

#[test]
fn unfold_normalizes_nil_case() {
    let d = defs();
    let mut used = HashSet::new();
    let t = kernel::unfold(&p("[append Nil ys]"), "append", &d, &mut used).unwrap();
    assert!(term_eq(&kernel::normalize(&t, &d), &p("ys")));
    let t = kernel::unfold(&p("[append [Cons a b] ys]"), "append", &d, &mut used).unwrap();
    assert!(term_eq(
        &kernel::normalize(&t, &d),
        &p("[Cons a [append b ys]]")
    ));
}

#[test]
fn unfold_refuses_non_total() {
    let d = defs();
    assert!(kernel::unfold(&p("[spin Nil]"), "spin", &d, &mut HashSet::new()).is_err());
}

#[test]
fn rewrite_with_rule() {
    let d = defs();
    let ih = Rule {
        params: vec![],
        lhs: p("[append rest Nil]"),
        rhs: p("rest"),
    };
    let t = kernel::rewrite(&p("[Cons x [append rest Nil]]"), &ih, 0, &d).unwrap();
    assert!(term_eq(&t, &p("[Cons x rest]")));
    // :at selects the Nth preorder match
    let r = Rule {
        params: vec!["a".into()],
        lhs: p("[f a]"),
        rhs: p("a"),
    };
    let t = kernel::rewrite(&p("[g [f 1] [f 2]]"), &r, 1, &d).unwrap();
    assert!(term_eq(&t, &p("[g [f 1] 2]")));
    assert!(kernel::rewrite(&p("[g 1]"), &r, 0, &d).is_err());
}

#[test]
fn induction_closes_append_nil() {
    let d = defs();
    let o = prove(
        &d,
        &["xs"],
        "[append xs Nil]",
        "xs",
        "[induction xs [Nil [unfold append]] [Cons [unfold append] [rewrite ih]]]",
        &none,
    );
    assert_eq!(o, Outcome::Proven);
}

#[test]
fn stuck_reports_term() {
    let d = defs();
    let o = prove(
        &d,
        &["xs"],
        "[append xs Nil]",
        "xs",
        "[induction xs [Nil [unfold append]] [Cons [unfold append]]]",
        &none,
    );
    match o {
        Outcome::Stuck { case, term, .. } => {
            assert_eq!(case, "Cons");
            assert_eq!(term, "[Cons x [append rest Nil]]");
        }
        o => panic!("expected stuck, got {o:?}"),
    }
}

#[test]
fn missing_case_is_stuck() {
    let d = defs();
    let o = prove(
        &d,
        &["xs"],
        "[append xs Nil]",
        "xs",
        "[induction xs [Nil [unfold append]]]",
        &none,
    );
    assert!(matches!(o, Outcome::Stuck { ref reason, .. } if reason.contains("missing case Cons")));
}

#[test]
fn admit_taints() {
    let d = defs();
    let o = prove(
        &d,
        &["xs"],
        "[append xs Nil]",
        "xs",
        "[induction xs [Nil [unfold append]] [Cons [admit]]]",
        &none,
    );
    assert_eq!(o, Outcome::Admitted);
    // Citing an admitted lemma taints too.
    let lem = |n: &str| {
        (n == "an").then(|| {
            (
                Rule {
                    params: vec!["xs".into()],
                    lhs: p("[append xs Nil]"),
                    rhs: p("xs"),
                },
                true,
            )
        })
    };
    let o = prove(
        &d,
        &["ys"],
        "[append [append ys Nil] Nil]",
        "ys",
        "[rewrite an] [rewrite an]",
        &lem,
    );
    assert_eq!(o, Outcome::Admitted);
}

#[test]
fn ih_only_inside_induction() {
    let d = defs();
    let o = prove(&d, &["xs"], "[append xs Nil]", "xs", "[rewrite ih]", &none);
    assert!(matches!(o, Outcome::Stuck { .. }));
    // `cases` gives no ih
    let o = prove(
        &d,
        &["xs"],
        "[append xs Nil]",
        "xs",
        "[cases xs [Nil] [Cons [unfold append] [rewrite ih]]]",
        &none,
    );
    assert!(matches!(o, Outcome::Stuck { .. }));
}

#[test]
fn induction_refuses_non_total_goal() {
    let d = defs();
    let o = prove(
        &d,
        &["xs"],
        "[spin xs]",
        "[spin xs]",
        "[induction xs [Nil] [Cons]]",
        &none,
    );
    assert!(
        matches!(o, Outcome::Stuck { ref reason, .. } if reason.contains("structurally recursive"))
    );
}

#[test]
fn canary_cannot_be_proven() {
    let d = defs();
    for steps in [
        "",
        "[refl]",
        "[induction xs [Nil] [Cons [rewrite ih]]]",
        "[unfold reverse] [unfold append]",
    ] {
        let o = prove(&d, &["xs"], "[reverse xs]", "xs", steps, &none);
        assert!(
            matches!(o, Outcome::Stuck { .. }),
            "steps {steps:?} gave {o:?}"
        );
    }
    let o = prove(&d, &[], "1", "2", "", &none);
    assert!(matches!(o, Outcome::Stuck { .. }));
}

// ─── Whole-pipeline tests over a temp verify dir ───────────────────────────

fn tmp_dir(tag: &str, files: &[(&str, &str)]) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("loon-verify-{tag}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    for (name, body) in files {
        let path = dir.join(name);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, body).unwrap();
    }
    dir
}

fn status<'a>(r: &'a Report, name: &str) -> &'a LawReport {
    r.laws.iter().find(|l| l.name == name).unwrap()
}

const LAWS: &str = r#"
[law append-nil [xs] [= [append xs Nil] xs]]
[law append-assoc [xs ys zs] [= [append [append xs ys] zs] [append xs [append ys zs]]]]
[law rev-append-wrong [xs ys] [= [reverse [append xs ys]] [append [reverse xs] [reverse ys]]]]
"#;

#[test]
fn pipeline_proves_falsifies_and_orders() {
    let dir = tmp_dir(
        "pipeline",
        &[
            ("defs.oo", DEFS),
            ("canary.oo", "[law canary [] [= 1 2]]"),
            ("laws/l.oo", LAWS),
            (
                "proofs/p.oo",
                r#"
[proof append-assoc [induction xs [Nil [unfold append]] [Cons [unfold append] [unfold append] [rewrite ih]]]]
[proof append-nil [induction xs [Nil [unfold append]] [Cons [unfold append] [rewrite ih]]]]
[proof rev-append-wrong [admit]]
"#,
            ),
        ],
    );
    let r = run_dir(&dir, &Options::default()).unwrap();
    assert!(r.sound);
    assert_eq!(status(&r, "append-nil").status, "proven");
    assert_eq!(status(&r, "append-assoc").status, "proven");
    let w = status(&r, "rev-append-wrong");
    assert_eq!(w.status, "falsified");
    let (asg, _, _) = w.counterexample.as_ref().unwrap();
    assert_eq!(asg.len(), 2);
    assert_eq!(status(&r, "canary").status, "falsified");
    assert_eq!(r.score(), 2);
    let j = r.to_json();
    assert_eq!(j["score"]["value"], 2);
    assert!(j["defs_hash"].as_str().unwrap().contains(':'));
}

#[test]
fn cycle_rejected() {
    let dir = tmp_dir(
        "cycle",
        &[
            ("defs.oo", DEFS),
            (
                "laws/l.oo",
                "[law a [xs] [= [append xs Nil] xs]]\n[law b [xs] [= xs [append xs Nil]]]",
            ),
            (
                "proofs/p.oo",
                "[proof a [rewrite b :rev]]\n[proof b [rewrite a :rev]]",
            ),
        ],
    );
    let r = run_dir(&dir, &Options::default()).unwrap();
    for n in ["a", "b"] {
        let l = status(&r, n);
        assert_eq!(l.status, "unknown");
        assert!(l.stuck.as_ref().unwrap().2.contains("cycle"));
    }
}

#[test]
fn admitted_canary_is_unsound() {
    let dir = tmp_dir(
        "canary",
        &[
            ("defs.oo", DEFS),
            ("canary.oo", "[law canary-list [xs] [= [reverse xs] xs]]"),
            ("laws/l.oo", "[law append-nil [xs] [= [append xs Nil] xs]]"),
            (
                "proofs/p.oo",
                "[proof append-nil [induction xs [Nil [unfold append]] [Cons [unfold append] [rewrite ih]]]]\n[proof canary-list [admit]]",
            ),
        ],
    );
    let r = run_dir(&dir, &Options::default()).unwrap();
    assert!(!r.sound);
    assert_eq!(status(&r, "canary-list").status, "unsound");
    assert_eq!(r.score(), 0);
}

#[test]
fn plus_canonical_form() {
    let d = defs();
    let n = |s: &str| kernel::normalize(&p(s), &d).to_string();
    assert_eq!(n("[+ 1 [+ a b]]"), "[+ 1 [+ a b]]");
    assert_eq!(n("[+ [+ 1 a] b]"), "[+ 1 [+ a b]]");
    assert_eq!(n("[+ b [+ a 1]]"), "[+ 1 [+ a b]]");
    assert_eq!(n("[+ 0 n]"), "n");
    assert_eq!(n("[+ n 0]"), "n");
    assert_eq!(n("[+ 2 [+ 3 x]]"), "[+ 5 x]");
    assert_eq!(n("[+ 0 0]"), "0");
    // not conflated with other arithmetic
    assert_eq!(n("[- a b]"), "[- a b]");
    assert_eq!(n("[* [+ 1 a] b]"), "[* [+ 1 a] b]");
}

#[test]
fn plus_normalization_does_not_prove_falsehoods() {
    let d = defs();
    let r = prove(
        &d,
        &["xs"],
        "[+ 1 [length xs]]",
        "[length xs]",
        "[refl]",
        &none,
    );
    assert!(!matches!(r, Outcome::Proven), "1 + n = n must not close");
    let r = prove(&d, &["xs"], "[+ a b]", "[+ a c]", "[refl]", &none);
    assert!(
        !matches!(r, Outcome::Proven),
        "a + b = a + c must not close"
    );
}

// --- nat agent: bounded generation for non-list ADTs ---
#[test]
fn bounded_enumerates_nat() {
    let defs = r#"
[type Nat [S Nat] Z]
[fn add [m n] [match m Z n [S k] [S [add k n]]]]
[fn mul [m n] [match m Z Z [S k] [add n [mul k n]]]]
"#;
    let dir = tmp_dir(
        "nat",
        &[
            ("defs.oo", defs),
            ("canary.oo", "[law canary [] [= 1 2]]"),
            (
                "laws/n.oo",
                r#"
[law add-zero-right [n] [= [add n Z] n]]
[law mul-dist [a b p] [= [mul [add a b] p] [add [mul a p] [mul b p]]]]
[law mul-is-add [m n] [= [mul m n] [add m n]]]
"#,
            ),
            (
                "proofs/p.oo",
                "[proof add-zero-right [induction n [Z] [S [unfold add] [rewrite ih]]]]",
            ),
        ],
    );
    let r = run_dir(&dir, &Options::default()).unwrap();
    assert!(r.sound);
    assert_eq!(status(&r, "add-zero-right").status, "proven");
    // `p` is only a non-scrutinized argument; it must still be generated as a Nat.
    let d = status(&r, "mul-dist");
    assert!(
        !d.note.as_deref().unwrap_or("").contains("bounded"),
        "{:?}",
        d.note
    );
    assert!(d.checked > 10);
    let w = status(&r, "mul-is-add");
    assert_eq!(w.status, "falsified");
    let (asg, _, _) = w.counterexample.as_ref().unwrap();
    assert_eq!(format!("{}", asg[1].1), "[S Z]");
}

// ─── Compiler correctness: kernel extensions (compiler agent) ──────────────

/// Hutton's razor with `exec` written as in the paper: nested constructor
/// patterns and a `_` fallback. The kernel handles this shape even though the
/// VM cannot run it (verify/defs.oo factors out `step` for that reason).
const COMPILER_DEFS: &str = r#"
[type List T [Cons T (List T)] Nil]
[type Expr [Lit Int] [Add Expr Expr]]
[type Instr [Push Int] IAdd]
[type Rose [Node Int (List Rose)]]
[fn append [xs ys]
  [match xs
    Nil           ys
    [Cons x rest] [Cons x [append rest ys]]]]
[fn eval [e] [match e [Lit n] n [Add a b] [+ [eval a] [eval b]]]]
[fn compile [e]
  [match e
    [Lit n]   [Cons [Push n] Nil]
    [Add a b] [append [compile a] [append [compile b] [Cons IAdd Nil]]]]]
[fn exec [code stack]
  [match code
    Nil stack
    [Cons [Push n] rest] [exec rest [Cons n stack]]
    [Cons IAdd rest] [match stack
                       [Cons y [Cons x s]] [exec rest [Cons [+ x y] s]]
                       _ [exec rest stack]]]]
[fn exec-halt [code stack]
  [match code
    Nil stack
    [Cons [Push n] rest] [exec-halt rest [Cons n stack]]
    [Cons IAdd rest] [match stack
                       [Cons y [Cons x s]] [exec-halt rest [Cons [+ x y] s]]
                       _ stack]]]
"#;

fn cdefs() -> Defs {
    Defs::from_exprs(&crate::parser::parse(COMPILER_DEFS).unwrap()).unwrap()
}

fn ty(s: &str) -> kernel::Ty {
    match p(s).kind {
        ExprKind::Symbol(n) => kernel::Ty::named(&n),
        ExprKind::List(v) => kernel::Ty::app(
            &v[0].to_string(),
            v[1..].iter().map(|a| ty(&a.to_string())).collect(),
        ),
        _ => panic!("bad type {s}"),
    }
}

fn prove_typed(
    defs: &Defs,
    params: &[(&str, &str)],
    lhs: &str,
    rhs: &str,
    steps: &str,
    lemmas: &kernel::Lemmas,
) -> Outcome {
    let names: Vec<String> = params.iter().map(|(n, _)| n.to_string()).collect();
    let types: HashMap<String, kernel::Ty> =
        params.iter().map(|(n, t)| (n.to_string(), ty(t))).collect();
    let (l, r) = (p(lhs), p(rhs));
    let goal = Goal {
        law_name: "t",
        params: &names,
        param_types: &types,
        lhs: &l,
        rhs: &r,
    };
    let steps = crate::parser::parse(steps).unwrap();
    check_proof(&goal, &steps, defs, lemmas)
}

fn exec_append_lemma(n: &str) -> Option<(Rule, bool)> {
    (n == "exec-append").then(|| {
        (
            Rule {
                params: vec!["c1".into(), "c2".into(), "s".into()],
                lhs: p("[exec [append c1 c2] s]"),
                rhs: p("[exec c2 [exec c1 s]]"),
            },
            false,
        )
    })
}

const EXEC_APPEND_PROOF: &str = r#"
[induction c1
  [Nil [unfold append]]
  [Cons [cases x
    [Push [unfold append] [unfold exec] [rewrite ih]]
    [IAdd [unfold append] [unfold exec]
      [cases s
        [Nil [rewrite ih]]
        [Cons [cases rest1
          [Nil [rewrite ih]]
          [Cons [rewrite ih]]]]]]]]]
"#;

#[test]
fn nested_pattern_exec_is_total() {
    let d = cdefs();
    assert!(d.total.contains("exec"), "code shrinks on every call");
    assert!(d.total.contains("exec-halt"));
    assert!(d.total.contains("compile"));
    assert!(d.total.contains("eval"));
}

#[test]
fn reduce_match_nested_patterns_and_wildcard() {
    let d = cdefs();
    let n = |s: &str| kernel::normalize(&p(s), &d).to_string();
    let m = "[match S [Cons y [Cons x s]] [hit x y s] _ [miss]]";
    assert_eq!(n(&m.replace('S', "[Cons 1 [Cons 2 t]]")), "[hit 2 1 t]");
    assert_eq!(n(&m.replace('S', "[Cons 1 Nil]")), "[miss]");
    assert_eq!(n(&m.replace('S', "Nil")), "[miss]");
    // Undecided inner pattern: must not fall through to `_`.
    assert_eq!(
        n(&m.replace('S', "[Cons 1 t]")),
        m.replace('S', "[Cons 1 t]")
    );
}

#[test]
fn field_types_instantiate_type_params() {
    let d = cdefs();
    let cons = d.types["List"].iter().find(|c| c.name == "Cons").unwrap();
    let ft = kernel::field_tys(&d, &ty("[List Instr]"), &cons.fields);
    assert_eq!(ft, vec![Some(ty("Instr")), Some(ty("[List Instr]"))]);
    // Unknown element type: the head field gets no type.
    let ft = kernel::field_tys(&d, &kernel::Ty::named("List"), &cons.fields);
    assert_eq!(ft[0], None);
}

#[test]
fn cases_on_constructor_field_with_inferred_type() {
    // exec-append over the nested-pattern exec needs `cases` on the head
    // instruction `x` (a field, typed Instr via List Instr) and on the stack
    // and its tail `rest1` (List Int).
    let d = cdefs();
    let o = prove_typed(
        &d,
        &[
            ("c1", "[List Instr]"),
            ("c2", "[List Instr]"),
            ("s", "[List Int]"),
        ],
        "[exec [append c1 c2] s]",
        "[exec c2 [exec c1 s]]",
        EXEC_APPEND_PROOF,
        &none,
    );
    assert_eq!(o, Outcome::Proven);
    // Without the element type, `x` cannot be split.
    let o = prove_typed(
        &d,
        &[("c1", "List"), ("c2", "List"), ("s", "List")],
        "[exec [append c1 c2] s]",
        "[exec c2 [exec c1 s]]",
        EXEC_APPEND_PROOF,
        &none,
    );
    assert!(
        matches!(o, Outcome::Stuck { ref reason, .. } if reason.contains("type of `x`")),
        "{o:?}"
    );
    // `cases` on an Int is refused.
    let o = prove_typed(
        &d,
        &[("s", "[List Int]")],
        "s",
        "s",
        "[cases s [Nil] [Cons [cases x [A] [B]]]]",
        &none,
    );
    assert!(matches!(o, Outcome::Stuck { .. }), "{o:?}");
}

#[test]
fn halting_exec_does_not_distribute() {
    // exec-halt stops on stack underflow, so exec-append is false for it;
    // the same proof must get stuck.
    let d = cdefs();
    let o = prove_typed(
        &d,
        &[
            ("c1", "[List Instr]"),
            ("c2", "[List Instr]"),
            ("s", "[List Int]"),
        ],
        "[exec-halt [append c1 c2] s]",
        "[exec-halt c2 [exec-halt c1 s]]",
        &EXEC_APPEND_PROOF.replace("exec", "exec-halt"),
        &none,
    );
    assert!(matches!(o, Outcome::Stuck { .. }), "{o:?}");
}

#[test]
fn two_hypotheses_for_two_recursive_fields() {
    let d = cdefs();
    let goal = |steps: &str| {
        prove_typed(
            &d,
            &[("e", "Expr"), ("stack", "[List Int]")],
            "[exec [compile e] stack]",
            "[Cons [eval e] stack]",
            steps,
            &exec_append_lemma,
        )
    };
    let ok = "[induction e [Lit] [Add [unfold compile] [rewrite exec-append] [rewrite ih-rest] [rewrite exec-append] [rewrite ih-rest1]]]";
    assert_eq!(goal(ok), Outcome::Proven);
    // Plain `ih` is ambiguous with two hypotheses.
    let o = goal("[induction e [Lit] [Add [unfold compile] [rewrite exec-append] [rewrite ih]]]");
    assert!(
        matches!(o, Outcome::Stuck { ref reason, .. } if reason.contains("ambiguous")),
        "{o:?}"
    );
    // The stuck report lists the hypotheses in scope.
    let o = goal("[induction e [Lit] [Add [unfold compile]]]");
    match o {
        Outcome::Stuck { case, reason, .. } => {
            assert_eq!(case, "Add");
            assert!(reason.contains("ih-rest ih-rest1"), "{reason}");
        }
        o => panic!("expected stuck, got {o:?}"),
    }
    // A hypothesis is about its own field: using ih-rest where ih-rest1 is
    // needed does not match.
    let o = goal("[induction e [Lit] [Add [unfold compile] [rewrite exec-append] [rewrite ih-rest] [rewrite exec-append] [rewrite ih-rest]]]");
    assert!(matches!(o, Outcome::Stuck { .. }), "{o:?}");
    // Unknown hypothesis names are refused, not looked up as laws.
    let o = goal("[induction e [Lit] [Add [rewrite ih-zz]]]");
    assert!(
        matches!(o, Outcome::Stuck { ref reason, .. } if reason.contains("ih-zz")),
        "{o:?}"
    );
}

#[test]
fn hypotheses_quantify_the_other_params() {
    // ih-rest1 is used at stack := [Cons [eval rest] stack]. If hypotheses
    // were fixed at the original stack, this proof would get stuck.
    let d = cdefs();
    let o = prove_typed(
        &d,
        &[("e", "Expr"), ("stack", "[List Int]")],
        "[exec [compile e] stack]",
        "[Cons [eval e] stack]",
        "[induction e [Lit] [Add [unfold compile] [rewrite exec-append] [rewrite ih-rest] [rewrite exec-append] [rewrite ih-rest1]]]",
        &exec_append_lemma,
    );
    assert_eq!(o, Outcome::Proven);
    // ...but not over the induction variable: a false law stays unprovable.
    let o = prove_typed(
        &d,
        &[("e", "Expr"), ("stack", "[List Int]")],
        "[exec [compile e] stack]",
        "[Cons [eval e] Nil]",
        "[induction e [Lit] [Add [unfold compile] [rewrite exec-append] [rewrite ih-rest] [rewrite exec-append] [rewrite ih-rest1]]]",
        &exec_append_lemma,
    );
    assert!(matches!(o, Outcome::Stuck { .. }), "{o:?}");
}

#[test]
fn no_hypothesis_for_indirectly_recursive_fields() {
    // Rose's field `(List Rose)` mentions Rose but is a list, not a Rose:
    // induction on Rose must not offer a hypothesis about it.
    let d = cdefs();
    let o = prove_typed(
        &d,
        &[("t", "Rose")],
        "[f t]",
        "[g t]",
        "[induction t [Node [rewrite ih]]]",
        &none,
    );
    assert!(
        matches!(o, Outcome::Stuck { ref reason, .. } if reason.contains("no induction hypothesis")),
        "{o:?}"
    );
}

#[test]
fn infer_law_param_types() {
    let d = cdefs();
    let ts = super::infer::law_param_types(
        &d,
        &d.fn_order,
        &["c1".into(), "c2".into(), "s".into()],
        &p("[exec [append c1 c2] s]"),
        &p("[exec c2 [exec c1 s]]"),
    )
    .unwrap();
    let ts: Vec<String> = ts
        .iter()
        .map(|t| format!("{:?}", super::infer::to_ty(t)))
        .collect();
    assert!(
        ts[0].contains("\"List\"") && ts[0].contains("\"Instr\""),
        "{ts:?}"
    );
    assert!(
        ts[2].contains("\"List\"") && ts[2].contains("\"Int\""),
        "{ts:?}"
    );
    // compile uses append at Instr; a plain list law stays polymorphic.
    let ts = super::infer::law_param_types(
        &d,
        &d.fn_order,
        &["xs".into()],
        &p("[append xs Nil]"),
        &p("xs"),
    )
    .unwrap();
    assert_eq!(
        super::infer::to_ty(&ts[0]),
        kernel::Ty::app("List", vec![kernel::Ty::unknown()])
    );
    // Ill-typed laws do not infer.
    assert!(super::infer::law_param_types(
        &d,
        &d.fn_order,
        &["e".into()],
        &p("[eval e]"),
        &p("[Cons 1 Nil]")
    )
    .is_none());
}

#[test]
fn typed_generator_enumerates_any_adt() {
    use crate::types::Type;
    let d = cdefs();
    let expr = Type::Con("Expr".into(), vec![]);
    assert!(super::bounded::typed_values(&expr, 0, &d, 0).is_empty());
    let one: Vec<String> = super::bounded::typed_values(&expr, 1, &d, 0)
        .iter()
        .map(|e| e.to_string())
        .collect();
    assert_eq!(one, vec!["[Lit 0]", "[Lit 1]", "[Lit 2]"]);
    assert_eq!(super::bounded::typed_values(&expr, 3, &d, 0).len(), 9); // Add of two Lits
    let code = Type::Con("List".into(), vec![Type::Con("Instr".into(), vec![])]);
    let c1: Vec<String> = super::bounded::typed_values(&code, 1, &d, 0)
        .iter()
        .map(|e| e.to_string())
        .collect();
    assert_eq!(c1, vec!["[Cons IAdd Nil]"]);
}

#[test]
fn pipeline_falsifies_false_compiler_law() {
    let dir = tmp_dir(
        "compiler",
        &[
            ("defs.oo", &std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../../verify/defs.oo")).unwrap()),
            (
                "laws/c.oo",
                "[law compile-drops-stack [e stack] [= [exec [compile e] stack] [Cons [eval e] Nil]]]\n\
                 [law exec-append [c1 c2 s] [= [exec [append c1 c2] s] [exec c2 [exec c1 s]]]]",
            ),
        ],
    );
    let r = run_dir(&dir, &Options::default()).unwrap();
    let w = status(&r, "compile-drops-stack");
    assert_eq!(w.status, "falsified");
    let (asg, _, _) = w.counterexample.as_ref().unwrap();
    assert_eq!(format!("{}", asg[0].1), "[Lit 0]");
    // the true law survives the bounded check on instruction lists
    let t = status(&r, "exec-append");
    let bounded_err = t.note.as_deref().is_some_and(|n| n.contains("bounded"));
    assert!(
        t.counterexample.is_none() && !bounded_err && t.checked > 100,
        "{:?} {}",
        t.note,
        t.checked
    );
}

// ─── AC normalization of declared operators ────────────────────────────────

const NAT_DEFS: &str = r#"
[type Nat [S Nat] Z]
[fn add [m n] [match m Z n [S k] [S [add k n]]]]
[fn mul [m n] [match m Z Z [S k] [add n [mul k n]]]]
"#;

fn nat_ac(unit: bool) -> Defs {
    let mut d = Defs::from_exprs(&crate::parser::parse(NAT_DEFS).unwrap()).unwrap();
    d.ac.push(AcOp {
        op: "add".into(),
        unit: unit.then(|| kernel::sym("Z")),
    });
    d
}

fn prove_nat(d: &Defs, params: &[&str], lhs: &str, rhs: &str, steps: &str) -> Outcome {
    let ps: Vec<(&str, &str)> = params.iter().map(|p| (*p, "Nat")).collect();
    prove_typed(d, &ps, lhs, rhs, steps, &none)
}

#[test]
fn ac_closes_permuted_nesting() {
    let d = nat_ac(false);
    let o = prove_nat(
        &d,
        &["a", "b", "c"],
        "[add [add a b] c]",
        "[add c [add b a]]",
        "",
    );
    assert_eq!(o, Outcome::Proven);
    // not without the declaration
    let plain = Defs::from_exprs(&crate::parser::parse(NAT_DEFS).unwrap()).unwrap();
    let o = prove_nat(
        &plain,
        &["a", "b", "c"],
        "[add [add a b] c]",
        "[add c [add b a]]",
        "",
    );
    assert!(matches!(o, Outcome::Stuck { .. }));
}

#[test]
fn ac_does_not_prove_falsehoods() {
    let d = nat_ac(true);
    let o = prove_nat(&d, &["a", "b", "c"], "[add a b]", "[add a c]", "[refl]");
    assert!(
        !matches!(o, Outcome::Proven),
        "a + b = a + c must not close"
    );
    let o = prove_nat(&d, &["a", "b"], "[mul a b]", "[add a b]", "");
    assert!(!matches!(o, Outcome::Proven));
    let o = prove_nat(&d, &["a"], "[add a a]", "a", "");
    assert!(!matches!(o, Outcome::Proven));
}

#[test]
fn ac_unit_dropped_only_when_declared() {
    let n = |d: &Defs, s: &str| kernel::normalize(&p(s), d).to_string();
    let with = nat_ac(true);
    assert_eq!(n(&with, "[add n Z]"), "n");
    assert_eq!(n(&with, "[add Z [add n Z]]"), "n");
    assert_eq!(n(&with, "[add Z Z]"), "Z");
    assert_eq!(n(&with, "[add b [add Z a]]"), "[add a b]");
    let without = nat_ac(false);
    assert_eq!(n(&without, "[add n Z]"), "[add Z n]");
}

const AC_LAWS: &str = r#"
[law add-zero-right [n] [= [add n Z] n]]
[law add-succ-right [m n] [= [add m [S n]] [S [add m n]]]]
[law add-comm [m n] [= [add m n] [add n m]]]
[law add-assoc [m n p] [= [add [add m n] p] [add m [add n p]]]]
[law add-rotate [a b c] [= [add [add a b] c] [add c [add b a]]]]
[ac add add-assoc add-comm :unit Z add-zero-right]
"#;

const AC_PROOFS_GOOD: &str = r#"
[proof add-zero-right [induction n [Z] [S [unfold add] [rewrite ih]]]]
[proof add-succ-right [induction m [Z [unfold add]] [S [unfold add] [rewrite ih]]]]
[proof add-assoc [induction m [Z] [S [unfold add] [unfold add] [rewrite ih]]]]
[proof add-rotate]
"#;

fn ac_dir(tag: &str, laws: &str, proofs: &str) -> Report {
    let dir = tmp_dir(
        tag,
        &[
            ("defs.oo", NAT_DEFS),
            ("canary.oo", "[law canary [] [= 1 2]]"),
            ("laws/n.oo", laws),
            ("proofs/p.oo", proofs),
        ],
    );
    run_dir(&dir, &Options::default()).unwrap()
}

#[test]
fn ac_active_after_its_laws_are_proven() {
    let proofs = format!(
        "{AC_PROOFS_GOOD}\n[proof add-comm [induction m [Z [unfold add] [rewrite add-zero-right :rev]] [S [unfold add] [rewrite ih] [rewrite add-succ-right :rev]]]]"
    );
    let r = ac_dir("ac-on", AC_LAWS, &proofs);
    assert!(r.errors.is_empty(), "{:?}", r.errors);
    assert_eq!(status(&r, "add-comm").status, "proven");
    assert_eq!(
        status(&r, "add-rotate").status,
        "proven",
        "{:?}",
        status(&r, "add-rotate").stuck
    );
}

#[test]
fn ac_not_active_in_proofs_of_its_own_laws() {
    // With AC on, add-comm would close by normalization alone.
    let proofs = format!("{AC_PROOFS_GOOD}\n[proof add-comm]");
    let r = ac_dir("ac-self", AC_LAWS, &proofs);
    assert_eq!(status(&r, "add-comm").status, "unknown");
    // ...and add-rotate, which relies on the decl, is then not proven either.
    assert_eq!(status(&r, "add-rotate").status, "unknown");
    // Same for a lemma a cited law's proof rests on (add-succ-right, via add-comm).
    let proofs = AC_PROOFS_GOOD.replace(
        "[proof add-succ-right [induction m [Z [unfold add]] [S [unfold add] [rewrite ih]]]]",
        "[proof add-succ-right]",
    ) + "\n[proof add-comm [induction m [Z [unfold add] [rewrite add-zero-right :rev]] [S [unfold add] [rewrite ih] [rewrite add-succ-right :rev]]]]";
    let r = ac_dir("ac-self2", AC_LAWS, &proofs);
    assert_eq!(status(&r, "add-succ-right").status, "unknown");
}

#[test]
fn ac_malformed_decl_rejected() {
    for (decl, frag) in [
        (
            "[ac add add-comm add-comm]",
            "add-comm` is not [= [add [add",
        ),
        (
            "[ac add add-assoc add-assoc]",
            "add-assoc` is not [= [add a b]",
        ),
        (
            "[ac add add-assoc add-comm :unit Z add-succ-right]",
            "add-succ-right` is not",
        ),
        (
            "[ac add add-assoc add-comm :unit S add-zero-right]",
            "unit `S`",
        ),
        ("[ac add add-assoc nope]", "`nope` is not a law"),
        ("[ac Z add-assoc add-comm]", "not a two-argument fn"),
        ("[ac add add-assoc]", "malformed"),
    ] {
        let laws = AC_LAWS.replace("[ac add add-assoc add-comm :unit Z add-zero-right]", decl);
        let proofs = format!("{AC_PROOFS_GOOD}\n[proof add-comm [induction m [Z [unfold add] [rewrite add-zero-right :rev]] [S [unfold add] [rewrite ih] [rewrite add-succ-right :rev]]]]");
        let r = ac_dir("ac-bad", &laws, &proofs);
        assert!(
            r.errors.iter().any(|e| e.contains(frag)),
            "{decl}: {:?}",
            r.errors
        );
        assert_eq!(status(&r, "add-rotate").status, "unknown", "{decl}");
    }
    // A comm law with the same shape but over a different op is rejected too.
    let laws = format!(
        "{AC_LAWS}\n[law mul-comm [m n] [= [mul m n] [mul n m]]]\n[ac add add-assoc mul-comm]"
    );
    let r = ac_dir("ac-bad2", &laws, AC_PROOFS_GOOD);
    assert!(
        r.errors.iter().any(|e| e.contains("mul-comm` is not")),
        "{:?}",
        r.errors
    );
}

#[test]
fn ac_citing_admitted_law_taints() {
    let proofs = format!("{AC_PROOFS_GOOD}\n[proof add-comm [admit]]");
    let r = ac_dir("ac-adm", AC_LAWS, &proofs);
    assert_eq!(status(&r, "add-comm").status, "admitted");
    assert_eq!(status(&r, "add-rotate").status, "admitted");
}
