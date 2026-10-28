//! `loon prove`: laws, proof objects, a rewrite kernel, bounded falsification.
//!
//! See docs/plans/2026-10-28-verify-autoresearch.md. Layout of a verify dir:
//!
//! ```text
//! defs.oo       frozen   reference prelude (ADTs + fns the kernel can unfold)
//! builtins.oo   frozen   bridge laws, bounded-only (status `bounded`)
//! canary.oo     frozen   false laws; proving one makes the run unsound
//! laws/*.oo     agent    conjectures
//! proofs/*.oo   agent    proof objects
//! ```

pub mod bounded;
pub mod infer;
pub mod kernel;

use crate::ast::{Expr, ExprKind};
use kernel::{head_sym, Outcome, Rule};
use serde_json::{json, Value as J};
use std::collections::{HashMap, HashSet};
use std::path::Path;

// ─── Definitions ───────────────────────────────────────────────────────────

#[derive(Clone, Debug)]
pub struct FnDef {
    pub params: Vec<String>,
    pub body: Expr,
    /// Index of the param the body's top-level `match` scrutinizes.
    pub scrutinee: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct CtorInfo {
    pub name: String,
    /// One entry per field: does the field mention the ADT itself?
    pub recursive: Vec<bool>,
    /// Declared field types, verbatim (`T`, `(List T)`, `Int`, ...).
    pub fields: Vec<Expr>,
}

#[derive(Clone, Debug, Default)]
pub struct Defs {
    pub fns: HashMap<String, FnDef>,
    pub types: HashMap<String, Vec<CtorInfo>>,
    /// Reference fn names in definition order.
    pub fn_order: Vec<String>,
    /// Type parameters of each ADT, in declaration order.
    pub type_params: HashMap<String, Vec<String>>,
    pub ctors: HashSet<String>,
    /// Structurally recursive (total) fns.
    pub total: HashSet<String>,
    /// User operators normalized modulo associativity/commutativity (and a
    /// unit, if declared). Empty in the frozen defs; `run_dir` fills a clone
    /// per proof with the `ac` declarations active for that proof.
    pub ac: Vec<AcOp>,
}

/// An active AC operator: a two-argument fn of defs.oo.
#[derive(Clone, Debug)]
pub struct AcOp {
    pub op: String,
    pub unit: Option<Expr>,
}

impl Defs {
    pub fn is_ctor(&self, s: &str) -> bool {
        self.ctors.contains(s)
    }

    pub fn type_of_ctor(&self, c: &str) -> Option<String> {
        self.types
            .iter()
            .find(|(_, cs)| cs.iter().any(|k| k.name == c))
            .map(|(t, _)| t.clone())
    }

    /// Build from parsed top-level forms (`type` and `fn`); other forms are ignored.
    pub fn from_exprs(exprs: &[Expr]) -> Result<Defs, String> {
        let mut d = Defs::default();
        for e in exprs {
            let ExprKind::List(items) = &e.kind else { continue };
            match head_sym(e) {
                Some("type") => d.add_type(items)?,
                Some("fn") => d.add_fn(items)?,
                _ => {}
            }
        }
        // Scrutinee needs ctor knowledge; compute after all types are in.
        d.total = kernel::total_fns(&d);
        Ok(d)
    }

    fn add_type(&mut self, items: &[Expr]) -> Result<(), String> {
        let Some(ExprKind::Symbol(name)) = items.get(1).map(|e| &e.kind) else {
            return Err("type: expected a name".into());
        };
        let mut fields_syms = HashSet::new();
        for it in &items[2..] {
            if let ExprKind::List(v) = &it.kind {
                for f in &v[1..] {
                    kernel::symbols(f, &mut fields_syms);
                }
            }
        }
        let mut params: Vec<String> = Vec::new();
        for it in &items[2..] {
            if let ExprKind::Symbol(s) = &it.kind {
                if fields_syms.contains(s) && !params.contains(s) {
                    params.push(s.clone());
                }
            }
        }
        let mut ctors = Vec::new();
        for it in &items[2..] {
            match &it.kind {
                // A bare symbol used in some field is a type param, else a nullary ctor.
                ExprKind::Symbol(s) if fields_syms.contains(s) => {}
                ExprKind::Symbol(s) => ctors.push(CtorInfo { name: s.clone(), recursive: vec![], fields: vec![] }),
                ExprKind::List(v) => {
                    let Some(ExprKind::Symbol(c)) = v.first().map(|e| &e.kind) else {
                        return Err(format!("type {name}: malformed constructor `{it}`"));
                    };
                    let recursive = v[1..]
                        .iter()
                        .map(|f| {
                            let mut s = HashSet::new();
                            kernel::symbols(f, &mut s);
                            s.contains(name)
                        })
                        .collect();
                    ctors.push(CtorInfo { name: c.clone(), recursive, fields: v[1..].to_vec() });
                }
                _ => return Err(format!("type {name}: unexpected `{it}`")),
            }
        }
        for c in &ctors {
            self.ctors.insert(c.name.clone());
        }
        self.types.insert(name.clone(), ctors);
        self.type_params.insert(name.clone(), params);
        Ok(())
    }

    fn add_fn(&mut self, items: &[Expr]) -> Result<(), String> {
        let (Some(ExprKind::Symbol(name)), Some(ExprKind::List(ps))) =
            (items.get(1).map(|e| &e.kind), items.get(2).map(|e| &e.kind))
        else {
            return Ok(()); // anonymous or multi-clause: not unfoldable
        };
        if items.len() != 4 {
            return Err(format!("fn {name}: reference definitions must have a single body expression"));
        }
        let params: Vec<String> = ps
            .iter()
            .map(|p| match &p.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(format!("fn {name}: params must be plain symbols")),
            })
            .collect::<Result<_, _>>()?;
        let body = items[3].clone();
        let scrutinee = match &body.kind {
            ExprKind::List(v) if head_sym(&body) == Some("match") && v.len() >= 2 => match &v[1].kind {
                ExprKind::Symbol(s) => params.iter().position(|p| p == s),
                _ => None,
            },
            _ => None,
        };
        self.fns.insert(name.clone(), FnDef { params, body, scrutinee });
        self.fn_order.push(name.clone());
        Ok(())
    }
}

// ─── Laws and proofs ───────────────────────────────────────────────────────

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Origin {
    Laws,
    Builtins,
    Canary,
}

#[derive(Clone, Debug)]
pub struct Law {
    pub name: String,
    pub params: Vec<String>,
    pub lhs: Expr,
    pub rhs: Expr,
    pub origin: Origin,
    pub file: String,
}

/// Parse `[law NAME [PARAMS...] [= LHS RHS]]`.
pub fn parse_law(e: &Expr, origin: Origin, file: &str) -> Option<Result<Law, String>> {
    let ExprKind::List(items) = &e.kind else { return None };
    if head_sym(e) != Some("law") {
        return None;
    }
    Some((|| {
        let Some(ExprKind::Symbol(name)) = items.get(1).map(|e| &e.kind) else {
            return Err(format!("{file}: law needs a name"));
        };
        let Some(ExprKind::List(ps)) = items.get(2).map(|e| &e.kind) else {
            return Err(format!("{file}: law {name}: expected a param list"));
        };
        let params = ps
            .iter()
            .map(|p| match &p.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(format!("{file}: law {name}: params must be symbols")),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let eq = items.get(3).filter(|_| items.len() == 4);
        let Some(ExprKind::List(eqv)) = eq.map(|e| &e.kind) else {
            return Err(format!("{file}: law {name}: expected [= LHS RHS]"));
        };
        if eqv.len() != 3 || head_sym(eq.unwrap()) != Some("=") {
            return Err(format!("{file}: law {name}: expected [= LHS RHS]"));
        }
        Ok(Law {
            name: name.clone(),
            params,
            lhs: eqv[1].clone(),
            rhs: eqv[2].clone(),
            origin,
            file: file.to_string(),
        })
    })())
}

/// Rule names a proof cites (excluding `ih`).
pub fn cited(steps: &[Expr], out: &mut Vec<String>) {
    for s in steps {
        let ExprKind::List(items) = &s.kind else { continue };
        match head_sym(s) {
            Some("rewrite") => {
                if let Some(ExprKind::Symbol(n)) = items.get(1).map(|e| &e.kind) {
                    if n != "ih" && !out.contains(n) {
                        out.push(n.clone());
                    }
                }
            }
            Some("induction") | Some("cases") => {
                for c in items.iter().skip(2) {
                    if let ExprKind::List(cs) = &c.kind {
                        cited(&cs[1..], out);
                    }
                }
            }
            _ => {}
        }
    }
}

// ─── AC declarations ───────────────────────────────────────────────────────

/// `[ac OP ASSOC-LAW COMM-LAW]` or `[ac OP ASSOC-LAW COMM-LAW :unit UNIT UNIT-LAW]`,
/// with the cited laws' statements checked for shape.
#[derive(Clone, Debug)]
pub struct AcDecl {
    pub op: String,
    pub assoc: String,
    pub comm: String,
    pub unit: Option<Expr>,
    pub unit_law: Option<String>,
}

impl AcDecl {
    pub fn laws(&self) -> Vec<String> {
        let mut v = vec![self.assoc.clone(), self.comm.clone()];
        v.extend(self.unit_law.clone());
        v
    }
}

const META: [&str; 3] = ["\0a", "\0b", "\0c"];

/// Does `t` match `pat`, where the `META` symbols of `pat` stand for distinct
/// law params (alpha-equivalence) and everything else must be equal?
fn alpha_match(pat: &Expr, t: &Expr, params: &[String], b: &mut HashMap<String, String>) -> bool {
    if let ExprKind::Symbol(m) = &pat.kind {
        if META.contains(&m.as_str()) {
            let ExprKind::Symbol(s) = &t.kind else { return false };
            if !params.contains(s) {
                return false;
            }
            return match b.get(m) {
                Some(prev) => prev == s,
                None if b.values().any(|v| v == s) => false,
                None => {
                    b.insert(m.clone(), s.clone());
                    true
                }
            };
        }
    }
    match (&pat.kind, &t.kind) {
        (ExprKind::List(ps), ExprKind::List(ts)) => {
            ps.len() == ts.len() && ps.iter().zip(ts).all(|(p, x)| alpha_match(p, x, params, b))
        }
        _ => kernel::term_eq(pat, t),
    }
}

/// Is `law`'s statement `[= L R]` an instance of one of `shapes` (either
/// orientation)?
fn law_has_shape(law: &Law, shapes: &[(Expr, Expr)]) -> bool {
    shapes.iter().any(|(l, r)| {
        [(&law.lhs, &law.rhs), (&law.rhs, &law.lhs)].iter().any(|(x, y)| {
            let mut b = HashMap::new();
            alpha_match(l, x, &law.params, &mut b) && alpha_match(r, y, &law.params, &mut b)
        })
    })
}

pub fn parse_ac(e: &Expr, laws: &[Law], defs: &Defs) -> Result<AcDecl, String> {
    use kernel::{list, sym};
    let ExprKind::List(items) = &e.kind else { unreachable!() };
    let names: Vec<Option<&str>> = items
        .iter()
        .map(|x| match &x.kind {
            ExprKind::Symbol(s) => Some(s.as_str()),
            _ => None,
        })
        .collect();
    let bad = || format!("malformed `{e}`; expected [ac OP ASSOC-LAW COMM-LAW] or [ac OP ASSOC-LAW COMM-LAW :unit UNIT UNIT-LAW]");
    let with_unit = items.len() == 7 && matches!(&items[4].kind, ExprKind::Keyword(k) if k == "unit");
    if !(items.len() == 4 || with_unit) {
        return Err(bad());
    }
    let (Some(op), Some(assoc), Some(comm)) = (names[1], names[2], names[3]) else {
        return Err(bad());
    };
    if defs.fns.get(op).map(|f| f.params.len()) != Some(2) {
        return Err(format!("ac {op}: `{op}` is not a two-argument fn of defs.oo"));
    }
    let law = |n: &str| {
        laws.iter()
            .find(|l| l.name == n && l.origin == Origin::Laws)
            .ok_or_else(|| format!("ac {op}: `{n}` is not a law"))
    };
    let app = |a: Expr, b: Expr| list(vec![sym(op), a, b]);
    let [a, b, c] = META.map(sym);
    let assoc_shape = (app(app(a.clone(), b.clone()), c.clone()), app(a.clone(), app(b.clone(), c)));
    if !law_has_shape(law(assoc)?, &[assoc_shape]) {
        return Err(format!("ac {op}: `{assoc}` is not [= [{op} [{op} a b] c] [{op} a [{op} b c]]]"));
    }
    let comm_shape = (app(a.clone(), b.clone()), app(b, a.clone()));
    if !law_has_shape(law(comm)?, &[comm_shape]) {
        return Err(format!("ac {op}: `{comm}` is not [= [{op} a b] [{op} b a]]"));
    }
    let (mut unit, mut unit_law) = (None, None);
    if with_unit {
        let u = items[5].clone();
        let ok = match &u.kind {
            ExprKind::Int(_) => true,
            ExprKind::Symbol(s) => defs.types.values().flatten().any(|k| &k.name == s && k.fields.is_empty()),
            _ => false,
        };
        if !ok {
            return Err(format!("ac {op}: unit `{u}` is not a nullary constructor or literal"));
        }
        let Some(ul) = names[6] else { return Err(bad()) };
        let shapes = [(app(a.clone(), u.clone()), a.clone()), (app(u.clone(), a.clone()), a.clone())];
        if !law_has_shape(law(ul)?, &shapes) {
            return Err(format!("ac {op}: `{ul}` is not [= [{op} n {u}] n] or [= [{op} {u} n] n]"));
        }
        unit = Some(u);
        unit_law = Some(ul.to_string());
    }
    Ok(AcDecl { op: op.into(), assoc: assoc.into(), comm: comm.into(), unit, unit_law })
}

/// `op` plus every fn of defs.oo whose body transitively mentions it: a goal
/// mentioning none of these can never contain an `op` application.
fn reaching(op: &str, defs: &Defs) -> HashSet<String> {
    let mut r: HashSet<String> = HashSet::from([op.to_string()]);
    loop {
        let more: Vec<String> = defs
            .fns
            .iter()
            .filter(|(n, f)| {
                !r.contains(*n) && {
                    let mut s = HashSet::new();
                    kernel::symbols(&f.body, &mut s);
                    s.iter().any(|x| r.contains(x))
                }
            })
            .map(|(n, _)| n.clone())
            .collect();
        if more.is_empty() {
            return r;
        }
        r.extend(more);
    }
}

// ─── Run ───────────────────────────────────────────────────────────────────

pub struct Options {
    pub bound: usize,
    pub seed: u64,
}

impl Default for Options {
    fn default() -> Self {
        Options { bound: 6, seed: 0x5EED }
    }
}

#[derive(Clone, Debug)]
pub struct LawReport {
    pub name: String,
    pub origin: Origin,
    pub status: &'static str,
    pub counterexample: Option<(Vec<(String, Expr)>, String, String)>,
    pub stuck: Option<(String, String, String)>,
    pub note: Option<String>,
    pub checked: usize,
}

pub struct Report {
    pub run: String,
    pub defs_hash: String,
    pub sound: bool,
    pub laws: Vec<LawReport>,
    pub errors: Vec<String>,
    pub defs: Defs,
}

fn read_forms(path: &Path, required: bool) -> Result<Vec<Expr>, String> {
    let src = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(_) if !required => return Ok(vec![]),
        Err(e) => return Err(format!("{}: {e}", path.display())),
    };
    crate::parser::parse(&src).map_err(|e| format!("{}: parse error: {}", path.display(), e.message))
}

fn oo_files(dir: &Path) -> Vec<std::path::PathBuf> {
    let mut v: Vec<_> = std::fs::read_dir(dir)
        .map(|rd| {
            rd.filter_map(|e| e.ok().map(|e| e.path()))
                .filter(|p| p.extension().is_some_and(|x| x == "oo" || x == "loon"))
                .collect()
        })
        .unwrap_or_default();
    v.sort();
    v
}

fn hash_hex(data: &[u8]) -> String {
    #[cfg(feature = "pkg-fetch")]
    {
        format!("blake3:{}", blake3::hash(data).to_hex())
    }
    #[cfg(not(feature = "pkg-fetch"))]
    {
        // FNV-1a 64; only used when blake3 is not compiled in.
        let mut h: u64 = 0xcbf29ce484222325;
        for b in data {
            h ^= *b as u64;
            h = h.wrapping_mul(0x100000001b3);
        }
        format!("fnv1a64:{h:016x}")
    }
}

fn now_rfc3339() -> String {
    let secs = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0);
    let (days, rem) = (secs.div_euclid(86400), secs.rem_euclid(86400));
    // Howard Hinnant's civil_from_days.
    let z = days + 719468;
    let era = z.div_euclid(146097);
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = yoe + era * 400 + if m <= 2 { 1 } else { 0 };
    format!(
        "{y:04}-{m:02}-{d:02}T{:02}:{:02}:{:02}Z",
        rem / 3600,
        (rem % 3600) / 60,
        rem % 60
    )
}

/// The kernel's view of a checker type.
fn checker_ty(t: &crate::types::Type) -> kernel::Ty {
    use crate::types::Type;
    match t {
        Type::Int => kernel::Ty::named("Int"),
        Type::Bool => kernel::Ty::named("Bool"),
        Type::Con(n, args) => kernel::Ty::app(n, args.iter().map(checker_ty).collect()),
        _ => kernel::Ty::unknown(),
    }
}

/// Run the whole pipeline over a verify directory.
pub fn run_dir(dir: &Path, opts: &Options) -> Result<Report, String> {
    let read = |name: &str| std::fs::read(dir.join(name)).unwrap_or_default();
    let mut hashed = Vec::new();
    for f in ["defs.oo", "builtins.oo", "canary.oo"] {
        hashed.extend(read(f));
        hashed.push(0);
    }
    let defs_hash = hash_hex(&hashed);

    let defs_src = std::fs::read_to_string(dir.join("defs.oo"))
        .map_err(|e| format!("{}: {e}", dir.join("defs.oo").display()))?;
    let defs_forms = read_forms(&dir.join("defs.oo"), true)?;
    let defs = Defs::from_exprs(&defs_forms)?;

    let mut errors = Vec::new();
    let mut laws: Vec<Law> = Vec::new();
    let mut proofs: HashMap<String, Vec<Expr>> = HashMap::new();
    let mut extra_fns: Vec<Expr> = Vec::new();
    let mut ac_forms: Vec<(Expr, String)> = Vec::new();

    let mut sources: Vec<(std::path::PathBuf, Origin, bool)> = vec![
        (dir.join("builtins.oo"), Origin::Builtins, false),
        (dir.join("canary.oo"), Origin::Canary, false),
    ];
    for p in oo_files(&dir.join("laws")) {
        sources.push((p, Origin::Laws, true));
    }
    for (path, origin, required) in &sources {
        let file = path.display().to_string();
        for e in read_forms(path, *required)? {
            if let Some(l) = parse_law(&e, *origin, &file) {
                match l {
                    Ok(l) if laws.iter().any(|x| x.name == l.name) => {
                        errors.push(format!("{file}: duplicate law `{}`", l.name))
                    }
                    Ok(l) => laws.push(l),
                    Err(m) => errors.push(m),
                }
            } else if *origin == Origin::Laws && head_sym(&e) == Some("ac") {
                ac_forms.push((e.clone(), file.clone()));
            } else if *origin == Origin::Builtins && head_sym(&e) == Some("fn") {
                extra_fns.push(e);
            } else if head_sym(&e) == Some("proof") {
                errors.push(format!("{file}: proofs belong in proofs/"));
            }
        }
    }
    for path in oo_files(&dir.join("proofs")) {
        let file = path.display().to_string();
        for e in read_forms(&path, true)? {
            let ExprKind::List(items) = &e.kind else { continue };
            if head_sym(&e) != Some("proof") {
                continue;
            }
            let Some(ExprKind::Symbol(name)) = items.get(1).map(|x| &x.kind) else {
                errors.push(format!("{file}: proof needs a law name"));
                continue;
            };
            match laws.iter().find(|l| &l.name == name) {
                None => errors.push(format!("{file}: proof for unknown law `{name}`")),
                Some(l) if l.origin == Origin::Builtins => {
                    errors.push(format!("{file}: `{name}` is a bridge law; bridge laws are bounded-only"))
                }
                Some(_) if proofs.contains_key(name) => errors.push(format!("{file}: duplicate proof `{name}`")),
                Some(_) => {
                    proofs.insert(name.clone(), items[2..].to_vec());
                }
            }
        }
    }

    let ac_decls: Vec<AcDecl> = ac_forms
        .iter()
        .filter_map(|(e, file)| match parse_ac(e, &laws, &defs) {
            Ok(d) => Some(d),
            Err(m) => {
                errors.push(format!("{file}: {m}"));
                None
            }
        })
        .collect();

    // Evaluation prelude: type decls verbatim, fns renamed.
    let mut prelude = String::new();
    for e in &defs_forms {
        match head_sym(e) {
            Some("type") => {
                prelude.push_str(&defs_src[e.span.start..e.span.end]);
                prelude.push('\n');
            }
            Some("fn") => {
                prelude.push_str(&format!("{}\n", bounded::mangle(e, &defs)));
            }
            _ => {}
        }
    }
    for e in &extra_fns {
        prelude.push_str(&format!("{}\n", bounded::mangle(e, &defs)));
    }

    // Bounded evaluation and param types.
    let mut reports: Vec<LawReport> = Vec::new();
    let mut param_types: HashMap<String, HashMap<String, kernel::Ty>> = HashMap::new();
    for law in &laws {
        let kinds = bounded::param_kinds(&prelude, &law.params, &law.lhs, &law.rhs, &defs);
        let inferred = infer::law_param_types(&defs, &defs.fn_order, &law.params, &law.lhs, &law.rhs);
        let types = law
            .params
            .iter()
            .zip(&kinds)
            .filter_map(|(p, k)| match k {
                bounded::Kind::List(t, _, _) => Some((p.clone(), kernel::Ty::app(t, vec![kernel::Ty::named("Int")]))),
                bounded::Kind::Adt(t, _) => {
                    let n = defs.type_params.get(t).map_or(0, |v| v.len());
                    Some((p.clone(), kernel::Ty::app(t, vec![kernel::Ty::named("Int"); n])))
                }
                bounded::Kind::Typed(t, _) => Some((p.clone(), checker_ty(t))),
                _ => None,
            })
            .collect();
        let types = match &inferred {
            Some(ts) => law
                .params
                .iter()
                .zip(ts)
                .map(|(p, t)| (p.clone(), infer::to_ty(t)))
                .filter(|(_, t)| defs.types.contains_key(&t.name))
                .collect(),
            None => types,
        };
        param_types.insert(law.name.clone(), types);
        let b = bounded::check_law(&prelude, &law.params, &kinds, &law.lhs, &law.rhs, &defs, opts.bound, opts.seed);
        let status = match (&b.counterexample, law.origin) {
            (Some(_), _) => "falsified",
            (None, Origin::Builtins) if b.error.is_none() => "bounded",
            _ => "unknown",
        };
        reports.push(LawReport {
            name: law.name.clone(),
            origin: law.origin,
            status,
            counterexample: b.counterexample,
            stuck: None,
            note: b.error.map(|e| format!("bounded: {e}")),
            checked: b.checked,
        });
    }

    // Kernel, in dependency order.
    let mut deps: HashMap<String, Vec<String>> = proofs
        .iter()
        .map(|(n, steps)| {
            let mut c = Vec::new();
            cited(steps, &mut c);
            (n.clone(), c)
        })
        .collect();
    // An `ac` declaration counts as citing its laws, for every proof it may
    // affect except the proofs its own laws (transitively) rest on: those
    // must not use the normalization they justify.
    let mut ac_for: HashMap<String, Vec<usize>> = HashMap::new();
    for (i, d) in ac_decls.iter().enumerate() {
        let mut excl: HashSet<String> = HashSet::new();
        let mut todo = d.laws();
        while let Some(n) = todo.pop() {
            if excl.insert(n.clone()) {
                todo.extend(deps.get(&n).cloned().unwrap_or_default());
            }
        }
        let reach = reaching(&d.op, &defs);
        let names: Vec<String> = proofs.keys().filter(|n| !excl.contains(*n)).cloned().collect();
        for n in names {
            let mut syms = HashSet::new();
            for l in laws.iter().filter(|l| l.name == n || deps[&n].contains(&l.name)) {
                kernel::symbols(&l.lhs, &mut syms);
                kernel::symbols(&l.rhs, &mut syms);
            }
            if syms.iter().any(|s| reach.contains(s)) {
                ac_for.entry(n.clone()).or_default().push(i);
                let dn = deps.get_mut(&n).unwrap();
                for l in d.laws() {
                    if !dn.contains(&l) {
                        dn.push(l);
                    }
                }
            }
        }
    }
    let mut order = Vec::new();
    let mut state: HashMap<String, u8> = HashMap::new();
    let mut cyclic: HashSet<String> = HashSet::new();
    fn dfs(
        n: &str,
        deps: &HashMap<String, Vec<String>>,
        state: &mut HashMap<String, u8>,
        stack: &mut Vec<String>,
        cyclic: &mut HashSet<String>,
        order: &mut Vec<String>,
    ) {
        state.insert(n.to_string(), 1);
        stack.push(n.to_string());
        for d in deps.get(n).into_iter().flatten() {
            if !deps.contains_key(d) {
                continue;
            }
            match state.get(d.as_str()) {
                Some(1) => {
                    let at = stack.iter().position(|s| s == d).unwrap_or(0);
                    cyclic.extend(stack[at..].iter().cloned());
                }
                Some(_) => {}
                None => dfs(d, deps, state, stack, cyclic, order),
            }
        }
        stack.pop();
        state.insert(n.to_string(), 2);
        order.push(n.to_string());
    }
    let mut names: Vec<&String> = proofs.keys().collect();
    names.sort_by_key(|n| laws.iter().position(|l| &l.name == *n));
    for n in names {
        if !state.contains_key(n.as_str()) {
            dfs(n, &deps, &mut state, &mut Vec::new(), &mut cyclic, &mut order);
        }
    }

    let mut outcome: HashMap<String, Outcome> = HashMap::new();
    for name in &order {
        let law = laws.iter().find(|l| &l.name == name).unwrap();
        let rep = reports.iter().find(|r| &r.name == name).unwrap();
        if rep.status == "falsified" && law.origin != Origin::Canary {
            continue; // falsified laws are not sent to the kernel
        }
        if cyclic.contains(name) {
            outcome.insert(
                name.clone(),
                Outcome::Stuck {
                    case: "root".into(),
                    term: format!("{}", law.lhs),
                    reason: "dependency cycle between proofs".into(),
                },
            );
            continue;
        }
        let lemmas = |cite: &str| -> Option<(Rule, bool)> {
            let adm = match outcome.get(cite)? {
                Outcome::Proven => false,
                Outcome::Admitted => true,
                Outcome::Stuck { .. } => return None,
            };
            let l = laws.iter().find(|l| l.name == cite)?;
            Some((Rule { params: l.params.clone(), lhs: l.lhs.clone(), rhs: l.rhs.clone() }, adm))
        };
        let goal = kernel::Goal {
            law_name: name,
            params: &law.params,
            param_types: &param_types[name],
            lhs: &law.lhs,
            rhs: &law.rhs,
        };
        // Activate each relevant `ac` decl whose laws are proven (or admitted,
        // which taints this proof).
        let mut pdefs = defs.clone();
        let mut ac_tainted = false;
        for &i in ac_for.get(name).into_iter().flatten() {
            let d = &ac_decls[i];
            let st: Vec<Option<&Outcome>> = d.laws().iter().map(|l| outcome.get(l)).collect();
            if st.iter().all(|o| matches!(o, Some(Outcome::Proven | Outcome::Admitted))) {
                ac_tainted |= st.iter().any(|o| matches!(o, Some(Outcome::Admitted)));
                pdefs.ac.push(AcOp { op: d.op.clone(), unit: d.unit.clone() });
            }
        }
        let o = match kernel::check_proof(&goal, &proofs[name], &pdefs, &lemmas) {
            Outcome::Proven if ac_tainted => Outcome::Admitted,
            o => o,
        };
        outcome.insert(name.clone(), o);
    }

    let mut sound = true;
    for rep in reports.iter_mut() {
        let law = laws.iter().find(|l| l.name == rep.name).unwrap();
        match (outcome.get(&rep.name), law.origin) {
            (Some(Outcome::Proven | Outcome::Admitted), Origin::Canary) => {
                sound = false;
                rep.status = "unsound";
                rep.note = Some("canary law was accepted by the kernel".into());
            }
            (Some(Outcome::Proven), _) => rep.status = "proven",
            (Some(Outcome::Admitted), _) => rep.status = "admitted",
            (Some(Outcome::Stuck { case, term, reason }), _) => {
                if rep.status != "falsified" {
                    rep.status = "unknown";
                }
                rep.stuck = Some((case.clone(), term.clone(), reason.clone()));
            }
            (None, Origin::Laws) if rep.status == "unknown" && !proofs.contains_key(&rep.name) => {
                rep.note = Some(rep.note.take().map_or("no proof".into(), |n| format!("no proof; {n}")));
            }
            _ => {}
        }
    }

    Ok(Report { run: now_rfc3339(), defs_hash, sound, laws: reports, errors, defs })
}

// ─── Reporting ─────────────────────────────────────────────────────────────

impl Report {
    fn count(&self, status: &str) -> usize {
        self.laws
            .iter()
            .filter(|l| l.origin == Origin::Laws && l.status == status)
            .count()
    }

    pub fn score(&self) -> i64 {
        if !self.sound {
            return 0;
        }
        self.count("proven") as i64 - 2 * self.count("admitted") as i64
    }

    pub fn to_json(&self) -> J {
        let laws: Vec<J> = self
            .laws
            .iter()
            .map(|l| {
                let mut o = json!({ "name": l.name, "status": l.status });
                let m = o.as_object_mut().unwrap();
                match l.origin {
                    Origin::Builtins => {
                        m.insert("bridge".into(), J::Bool(true));
                    }
                    Origin::Canary => {
                        m.insert("canary".into(), J::Bool(true));
                    }
                    Origin::Laws => {}
                }
                if let Some((asg, lv, rv)) = &l.counterexample {
                    let ce: serde_json::Map<String, J> = asg
                        .iter()
                        .map(|(k, v)| (k.clone(), bounded::to_json(v, &self.defs)))
                        .collect();
                    m.insert("counterexample".into(), J::Object(ce));
                    m.insert("lhs".into(), bounded::value_json(lv, &self.defs));
                    m.insert("rhs".into(), bounded::value_json(rv, &self.defs));
                }
                if let Some((case, term, reason)) = &l.stuck {
                    m.insert("stuck".into(), json!({ "case": case, "term": term, "reason": reason }));
                }
                if l.checked > 0 {
                    m.insert("inputs".into(), J::from(l.checked));
                }
                if let Some(n) = &l.note {
                    m.insert("note".into(), J::String(n.clone()));
                }
                o
            })
            .collect();
        json!({
            "run": self.run,
            "defs_hash": self.defs_hash,
            "sound": self.sound,
            "laws": laws,
            "errors": self.errors,
            "score": {
                "proven": self.count("proven"),
                "admitted": self.count("admitted"),
                "falsified": self.count("falsified"),
                "unknown": self.count("unknown"),
                "value": self.score(),
            }
        })
    }

    pub fn to_text(&self) -> String {
        let mut s = String::new();
        for (title, origin) in [("laws", Origin::Laws), ("bridge (bounded only)", Origin::Builtins), ("canary", Origin::Canary)] {
            let ls: Vec<_> = self.laws.iter().filter(|l| l.origin == origin).collect();
            if ls.is_empty() {
                continue;
            }
            s.push_str(&format!("{title}:\n"));
            for l in ls {
                s.push_str(&format!("  {:<10} {}", l.status, l.name));
                if let Some((asg, lv, rv)) = &l.counterexample {
                    let a: Vec<String> = asg.iter().map(|(k, v)| format!("{k}={v}")).collect();
                    s.push_str(&format!("\n    counterexample {}\n    lhs = {lv}\n    rhs = {rv}", a.join(" ")));
                }
                if let Some((case, term, reason)) = &l.stuck {
                    s.push_str(&format!("  case {case}: stuck at\n    L = {term}\n    {reason}"));
                }
                if let Some(n) = &l.note {
                    s.push_str(&format!("  ({n})"));
                }
                s.push('\n');
            }
        }
        for e in &self.errors {
            s.push_str(&format!("error: {e}\n"));
        }
        s.push_str(&format!(
            "\nsound: {}  score: {}  (proven {}, admitted {}, falsified {}, unknown {})\ndefs_hash: {}\n",
            self.sound,
            self.score(),
            self.count("proven"),
            self.count("admitted"),
            self.count("falsified"),
            self.count("unknown"),
            self.defs_hash
        ));
        s
    }
}

#[cfg(test)]
mod tests;
