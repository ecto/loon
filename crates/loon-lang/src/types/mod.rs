use std::collections::{BTreeSet, HashMap};
use std::fmt;

use crate::syntax::Span;

// ── Dimensional Analysis ─────────────────────────────────────────────

/// SI dimension exponents for compile-time dimensional analysis.
/// Represents mass^m · length^l · time^t · current^i · temperature^θ.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Dimension {
    pub mass: i8,
    pub length: i8,
    pub time: i8,
    pub current: i8,
    pub temperature: i8,
}

impl Dimension {
    /// Scalar: all exponents zero (NOT called "dimensionless" — we don't have that concept)
    pub const SCALAR: Dimension = Dimension {
        mass: 0,
        length: 0,
        time: 0,
        current: 0,
        temperature: 0,
    };

    pub fn is_scalar(&self) -> bool {
        self.mass == 0
            && self.length == 0
            && self.time == 0
            && self.current == 0
            && self.temperature == 0
    }

    pub fn length() -> Dimension {
        Dimension {
            length: 1,
            ..Self::SCALAR
        }
    }
    pub fn time() -> Dimension {
        Dimension {
            time: 1,
            ..Self::SCALAR
        }
    }
    pub fn mass() -> Dimension {
        Dimension {
            mass: 1,
            ..Self::SCALAR
        }
    }
    pub fn current() -> Dimension {
        Dimension {
            current: 1,
            ..Self::SCALAR
        }
    }
    pub fn temperature() -> Dimension {
        Dimension {
            temperature: 1,
            ..Self::SCALAR
        }
    }

    /// Multiply dimensions (add exponents)
    pub fn mul(&self, other: &Dimension) -> Dimension {
        Dimension {
            mass: self.mass + other.mass,
            length: self.length + other.length,
            time: self.time + other.time,
            current: self.current + other.current,
            temperature: self.temperature + other.temperature,
        }
    }

    /// Divide dimensions (subtract exponents)
    pub fn div(&self, other: &Dimension) -> Dimension {
        Dimension {
            mass: self.mass - other.mass,
            length: self.length - other.length,
            time: self.time - other.time,
            current: self.current - other.current,
            temperature: self.temperature - other.temperature,
        }
    }

    /// Raise dimension to a power (multiply exponents)
    pub fn pow(&self, n: i8) -> Dimension {
        Dimension {
            mass: self.mass * n,
            length: self.length * n,
            time: self.time * n,
            current: self.current * n,
            temperature: self.temperature * n,
        }
    }

    /// ALWAYS returns a name — no Option, no escape hatch
    pub fn name(&self) -> &'static str {
        match (
            self.mass,
            self.length,
            self.time,
            self.current,
            self.temperature,
        ) {
            (0, 0, 0, 0, 0) => "Scalar",
            (0, 1, 0, 0, 0) => "Length",
            (0, 0, 1, 0, 0) => "Time",
            (1, 0, 0, 0, 0) => "Mass",
            (0, 0, 0, 1, 0) => "Current",
            (0, 0, 0, 0, 1) => "Temperature",
            (0, 1, -1, 0, 0) => "Velocity",
            (0, 1, -2, 0, 0) => "Acceleration",
            (1, 1, -2, 0, 0) => "Force",
            (1, -1, -2, 0, 0) => "Pressure",
            (1, 2, -2, 0, 0) => "Energy",
            (1, 2, -3, 0, 0) => "Power",
            (0, 0, -1, 0, 0) => "Frequency",
            (0, 2, 0, 0, 0) => "Area",
            (0, 3, 0, 0, 0) => "Volume",
            (1, -3, 0, 0, 0) => "Density",
            (1, 1, -1, 0, 0) => "Momentum",
            (0, 0, 1, 1, 0) => "Charge",
            (1, 2, -3, -1, 0) => "Voltage",
            (1, 2, -3, -2, 0) => "Resistance",
            (1, 1, -3, 0, -1) => "ThermalConductivity",
            _ => "Dim", // fallback — Display will show computed notation
        }
    }
}

impl fmt::Display for Dimension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n = self.name();
        if n != "Dim" {
            return write!(f, "{n}");
        }
        // Computed notation with Unicode superscripts for unknown combinations
        let mut parts = Vec::new();
        let bases = [
            (self.mass, "kg"),
            (self.length, "m"),
            (self.time, "s"),
            (self.current, "A"),
            (self.temperature, "K"),
        ];
        for (exp, unit) in bases {
            if exp == 1 {
                parts.push(unit.to_string());
            } else if exp != 0 {
                let sup = format_superscript(exp);
                parts.push(format!("{unit}{sup}"));
            }
        }
        if parts.is_empty() {
            write!(f, "Scalar")
        } else {
            write!(f, "{}", parts.join("\u{22c5}"))
        }
    }
}

fn format_superscript(n: i8) -> String {
    const SUPER_DIGITS: &[char] = &[
        '\u{2070}', '\u{00b9}', '\u{00b2}', '\u{00b3}', '\u{2074}', '\u{2075}', '\u{2076}',
        '\u{2077}', '\u{2078}', '\u{2079}',
    ];
    let mut s = String::new();
    if n < 0 {
        s.push('\u{207b}'); // superscript minus
        for c in n.unsigned_abs().to_string().chars() {
            s.push(SUPER_DIGITS[c.to_digit(10).unwrap() as usize]);
        }
    } else {
        for c in n.to_string().chars() {
            s.push(SUPER_DIGITS[c.to_digit(10).unwrap() as usize]);
        }
    }
    s
}

/// Unique type variable identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub u32);

/// Type representation for Loon.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Str,
    Keyword,
    Unit,
    /// Unification variable
    Var(TypeVar),
    /// Function type: params -> return, with an effect row describing the
    /// effects performed when the function is called.
    Fn(Vec<Type>, Box<Type>, EffectRow),
    /// Type constructor: name + type args (e.g., Vec<Int>, Option<T>)
    Con(String, Vec<Type>),
    /// Tuple
    Tuple(Vec<Type>),
    /// A bare effect row. Only used as a substitution binding target for
    /// effect-row tail variables (mirroring how record row `rest` variables
    /// are bound to `Type::Row`). Never appears as the type of an expression.
    Effects(EffectRow),
    /// Row type for structural records: maps field names to types with an optional extension variable
    /// Row(fields, rest) where rest is None (closed) or Some(TypeVar) (open/extensible)
    Row(Vec<(String, Type)>, Option<TypeVar>),
    /// Record type: a map with a row type
    Record(Box<Type>),
    /// Dimensional type for physics (e.g., Length, Velocity, Force)
    Dim(Dimension),
}

/// A row of effects: a set of concrete effect labels plus an optional
/// polymorphic tail variable.
///
/// This mirrors record row polymorphism (`Type::Row(fields, rest)`): the
/// `tail` is a unification variable that can absorb further labels, which is
/// what lets higher-order functions generalize over the effects of their
/// arguments. `{IO | e}` means "performs IO, plus whatever `e` turns out to
/// be"; `{IO}` (closed) means "performs exactly IO".
///
/// Effect rows are inference-only — there is no user-facing syntax for tail
/// variables. Tail variables share the `TypeVar` namespace with ordinary
/// type variables and are bound in the same `Subst` (to `Type::Effects`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectRow {
    pub labels: BTreeSet<String>,
    pub tail: Option<TypeVar>,
}

impl EffectRow {
    /// The closed empty row: performs no effects.
    pub fn pure() -> Self {
        Self {
            labels: BTreeSet::new(),
            tail: None,
        }
    }

    /// An open row with no concrete labels: `{ | e}`.
    pub fn open(tail: TypeVar) -> Self {
        Self {
            labels: BTreeSet::new(),
            tail: Some(tail),
        }
    }

    /// A closed row with the given labels.
    pub fn closed(labels: BTreeSet<String>) -> Self {
        Self { labels, tail: None }
    }

    pub fn is_pure(&self) -> bool {
        self.labels.is_empty() && self.tail.is_none()
    }

    pub fn is_empty(&self) -> bool {
        self.labels.is_empty()
    }

    pub fn contains(&self, name: &str) -> bool {
        self.labels.contains(name)
    }

    pub fn insert(&mut self, name: String) {
        self.labels.insert(name);
    }

    /// Render the row for diagnostics: "IO", "IO + Fail", "IO + e", "pure".
    /// The tail is rendered as a plain `e` — internal variable ids are never
    /// shown to users.
    pub fn render(&self) -> String {
        let mut parts: Vec<String> = self.labels.iter().cloned().collect();
        if self.tail.is_some() {
            parts.push("e".to_string());
        }
        if parts.is_empty() {
            "pure".to_string()
        } else {
            parts.join(" + ")
        }
    }
}

/// Set of effect names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectSet(pub BTreeSet<String>);

impl EffectSet {
    pub fn empty() -> Self {
        Self(BTreeSet::new())
    }

    pub fn singleton(name: &str) -> Self {
        let mut s = BTreeSet::new();
        s.insert(name.to_string());
        Self(s)
    }

    pub fn union(&self, other: &EffectSet) -> Self {
        Self(self.0.union(&other.0).cloned().collect())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn contains(&self, name: &str) -> bool {
        self.0.contains(name)
    }

    pub fn subtract(&self, other: &EffectSet) -> Self {
        Self(self.0.difference(&other.0).cloned().collect())
    }

    pub fn is_subset_of(&self, other: &EffectSet) -> bool {
        self.0.is_subset(&other.0)
    }

    pub fn insert(&mut self, name: String) {
        self.0.insert(name);
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
            Type::Str => write!(f, "String"),
            Type::Keyword => write!(f, "Keyword"),
            Type::Unit => write!(f, "()"),
            Type::Var(v) => write!(f, "t{}", v.0),
            Type::Fn(params, ret, effects) => {
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, " \u{2192} ")?;
                    }
                    write!(f, "{p}")?;
                }
                if !params.is_empty() {
                    write!(f, " \u{2192} ")?;
                }
                write!(f, "{ret}")?;
                // Only render the effect row when it has concrete labels; a
                // pure or merely-open row is the common case and rendering it
                // (or its internal tail variable) would be noise.
                if !effects.labels.is_empty() {
                    write!(f, " / {}", effects.render())?;
                }
                Ok(())
            }
            Type::Con(name, args) if args.is_empty() => write!(f, "{name}"),
            Type::Con(name, args) => {
                write!(f, "{name}")?;
                for a in args {
                    write!(f, " {a}")?;
                }
                Ok(())
            }
            Type::Tuple(items) => {
                write!(f, "(")?;
                for (i, t) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{t}")?;
                }
                write!(f, ")")
            }
            Type::Effects(row) => {
                write!(f, "{{{}}}", row.render())
            }
            Type::Row(fields, rest) => {
                write!(f, "{{")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{name}: {ty}")?;
                }
                if let Some(r) = rest {
                    if !fields.is_empty() {
                        write!(f, " | ")?;
                    }
                    write!(f, "t{}", r.0)?;
                }
                write!(f, "}}")
            }
            Type::Record(row) => {
                write!(f, "Record{row}")
            }
            Type::Dim(d) => write!(f, "{d}"),
        }
    }
}

/// Mutable substitution: maps TypeVar -> Type
pub struct Subst {
    bindings: Vec<Option<Type>>,
    next_var: u32,
    pub constraints: HashMap<TypeVar, Vec<TraitBound>>,
}

impl Subst {
    pub fn new() -> Self {
        Self {
            bindings: Vec::new(),
            next_var: 0,
            constraints: HashMap::new(),
        }
    }

    pub fn add_constraint(&mut self, v: TypeVar, bound: TraitBound) {
        self.constraints.entry(v).or_default().push(bound);
    }

    pub fn fresh(&mut self) -> Type {
        let v = TypeVar(self.next_var);
        self.next_var += 1;
        self.bindings.push(None);
        Type::Var(v)
    }

    /// Create a fresh TypeVar and return just the variable (not wrapped in Type).
    pub fn fresh_var(&mut self) -> TypeVar {
        let v = TypeVar(self.next_var);
        self.next_var += 1;
        self.bindings.push(None);
        v
    }

    pub fn bind(&mut self, v: TypeVar, ty: Type) {
        let idx = v.0 as usize;
        if idx < self.bindings.len() {
            self.bindings[idx] = Some(ty);
        }
    }

    /// Walk a type, resolving any bound type variables.
    pub fn resolve(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(v) => {
                let idx = v.0 as usize;
                if idx < self.bindings.len() {
                    if let Some(ref bound) = self.bindings[idx] {
                        return self.resolve(bound);
                    }
                }
                ty.clone()
            }
            Type::Fn(params, ret, effects) => Type::Fn(
                params.iter().map(|p| self.resolve(p)).collect(),
                Box::new(self.resolve(ret)),
                self.resolve_effect_row(effects),
            ),
            Type::Con(name, args) => {
                Type::Con(name.clone(), args.iter().map(|a| self.resolve(a)).collect())
            }
            Type::Tuple(items) => Type::Tuple(items.iter().map(|t| self.resolve(t)).collect()),
            Type::Effects(row) => Type::Effects(self.resolve_effect_row(row)),
            Type::Row(fields, rest) => {
                let resolved_fields: Vec<(String, Type)> = fields
                    .iter()
                    .map(|(n, t)| (n.clone(), self.resolve(t)))
                    .collect();
                if let Some(rv) = rest {
                    let idx = rv.0 as usize;
                    if idx < self.bindings.len() {
                        if let Some(ref bound) = self.bindings[idx] {
                            let resolved_rest = self.resolve(bound);
                            if let Type::Row(extra_fields, new_rest) = resolved_rest {
                                let mut all_fields = resolved_fields;
                                all_fields.extend(extra_fields);
                                return Type::Row(all_fields, new_rest);
                            }
                            if let Type::Var(new_rv) = resolved_rest {
                                return Type::Row(resolved_fields, Some(new_rv));
                            }
                        }
                    }
                }
                Type::Row(resolved_fields, *rest)
            }
            Type::Record(inner) => Type::Record(Box::new(self.resolve(inner))),
            Type::Dim(_) => ty.clone(),
            _ => ty.clone(),
        }
    }

    /// Walk an effect row, resolving its tail through the substitution and
    /// flattening any label sets found along the way. Mirrors how record row
    /// `rest` variables are resolved through `Type::Row` bindings.
    pub fn resolve_effect_row(&self, row: &EffectRow) -> EffectRow {
        let mut labels = row.labels.clone();
        let mut tail = row.tail;
        while let Some(v) = tail {
            let idx = v.0 as usize;
            let bound = if idx < self.bindings.len() {
                self.bindings[idx].as_ref()
            } else {
                None
            };
            match bound {
                Some(Type::Effects(inner)) => {
                    labels.extend(inner.labels.iter().cloned());
                    tail = inner.tail;
                }
                Some(Type::Var(u)) => {
                    tail = Some(*u);
                }
                // Any other binding is a namespace mix-up; treat as open.
                Some(_) | None => break,
            }
        }
        EffectRow { labels, tail }
    }

    /// Occurs check: does TypeVar v occur in type ty?
    fn occurs_in(&self, v: TypeVar, ty: &Type) -> bool {
        match self.resolve(ty) {
            Type::Var(u) => u == v,
            Type::Fn(params, ret, effects) => {
                params.iter().any(|p| self.occurs_in(v, p))
                    || self.occurs_in(v, &ret)
                    || self.resolve_effect_row(&effects).tail == Some(v)
            }
            Type::Con(_, args) => args.iter().any(|a| self.occurs_in(v, a)),
            Type::Tuple(items) => items.iter().any(|t| self.occurs_in(v, t)),
            Type::Effects(row) => self.resolve_effect_row(&row).tail == Some(v),
            Type::Row(fields, rest) => {
                fields.iter().any(|(_, t)| self.occurs_in(v, t)) || rest.is_some_and(|r| r == v)
            }
            Type::Record(inner) => self.occurs_in(v, &inner),
            Type::Dim(_) => false,
            _ => false,
        }
    }
}

impl Default for Subst {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct TypeError {
    pub message: String,
    pub span: Option<Span>,
}

impl TypeError {
    pub fn at(msg: impl Into<String>, span: Span) -> Self {
        Self {
            message: msg.into(),
            span: Some(span),
        }
    }

    pub fn bare(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
            span: None,
        }
    }

    /// Add a span to this error if it doesn't already have one.
    pub fn with_span(mut self, span: Span) -> Self {
        if self.span.is_none() {
            self.span = Some(span);
        }
        self
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type error: {}", self.message)
    }
}

impl std::error::Error for TypeError {}

/// Trait declaration
#[derive(Debug, Clone)]
pub struct TraitDecl {
    pub name: String,
    pub type_params: Vec<String>,
    pub methods: Vec<TraitMethod>,
}

/// Method signature in a trait
#[derive(Debug, Clone)]
pub struct TraitMethod {
    pub name: String,
    pub param_types: Vec<Type>,
    pub ret_type: Type,
}

/// A trait bound on a type variable
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitBound {
    pub trait_name: String,
}

/// Unify two types under the given substitution.
pub fn unify(subst: &mut Subst, a: &Type, b: &Type) -> Result<(), TypeError> {
    let a = subst.resolve(a);
    let b = subst.resolve(b);

    match (&a, &b) {
        _ if a == b => Ok(()),
        (Type::Var(v), _) => {
            if subst.occurs_in(*v, &b) {
                return Err(TypeError::bare(format!("infinite type: {a} ~ {b}")));
            }
            // Propagate constraints when binding a variable to another variable
            if let Type::Var(u) = &b {
                if let Some(bounds) = subst.constraints.get(v).cloned() {
                    for bound in bounds {
                        subst.add_constraint(*u, bound);
                    }
                }
            }
            subst.bind(*v, b);
            Ok(())
        }
        (_, Type::Var(v)) => {
            if subst.occurs_in(*v, &a) {
                return Err(TypeError::bare(format!("infinite type: {a} ~ {b}")));
            }
            if let Type::Var(u) = &a {
                if let Some(bounds) = subst.constraints.get(v).cloned() {
                    for bound in bounds {
                        subst.add_constraint(*u, bound);
                    }
                }
            }
            subst.bind(*v, a);
            Ok(())
        }
        (Type::Fn(ap, ar, ae), Type::Fn(bp, br, be)) => {
            if ap.len() != bp.len() {
                return Err(TypeError::bare(format!(
                    "function arity mismatch: expected {}, got {}",
                    ap.len(),
                    bp.len()
                )));
            }
            for (p1, p2) in ap.iter().zip(bp.iter()) {
                unify(subst, p1, p2)?;
            }
            unify(subst, ar, br)?;
            unify_effect_rows(subst, ae, be)
        }
        (Type::Effects(r1), Type::Effects(r2)) => unify_effect_rows(subst, r1, r2),
        (Type::Con(n1, a1), Type::Con(n2, a2)) if n1 == n2 && a1.len() == a2.len() => {
            for (t1, t2) in a1.iter().zip(a2.iter()) {
                unify(subst, t1, t2)?;
            }
            Ok(())
        }
        (Type::Tuple(a1), Type::Tuple(a2)) if a1.len() == a2.len() => {
            for (t1, t2) in a1.iter().zip(a2.iter()) {
                unify(subst, t1, t2)?;
            }
            Ok(())
        }
        (Type::Record(r1), Type::Record(r2)) => unify(subst, r1, r2),
        (Type::Row(fields_a, rest_a), Type::Row(fields_b, rest_b)) => {
            unify_rows(subst, fields_a, *rest_a, fields_b, *rest_b)
        }
        (Type::Dim(d1), Type::Dim(d2)) => {
            if d1 == d2 {
                Ok(())
            } else {
                Err(TypeError::bare(format!(
                    "dimension mismatch: {} vs {}",
                    d1, d2
                )))
            }
        }
        _ => Err(TypeError::bare(format!("cannot unify {a} with {b}"))),
    }
}

/// Effect-row unification: label-set merge + tail unification + occurs check.
///
/// Structured exactly like `unify_rows` below, with labels playing the role
/// of fields. Labels missing on one side are absorbed by the other side's
/// tail variable; two open rows are linked through a shared fresh tail.
/// Unifying two rows with the SAME tail but different labels would require
/// the tail to contain itself (`e ~ {IO | e}`), so it fails the occurs check.
pub fn unify_effect_rows(subst: &mut Subst, a: &EffectRow, b: &EffectRow) -> Result<(), TypeError> {
    let a = subst.resolve_effect_row(a);
    let b = subst.resolve_effect_row(b);

    let only_a: BTreeSet<String> = a.labels.difference(&b.labels).cloned().collect();
    let only_b: BTreeSet<String> = b.labels.difference(&a.labels).cloned().collect();

    match (a.tail, b.tail) {
        (None, None) => {
            if !only_a.is_empty() || !only_b.is_empty() {
                return Err(TypeError::bare(format!(
                    "effect mismatch: `{}` vs `{}`",
                    a.render(),
                    b.render()
                )));
            }
            Ok(())
        }
        (Some(ra), None) => {
            if !only_a.is_empty() {
                return Err(TypeError::bare(format!(
                    "effect mismatch: `{}` is not allowed to perform `{}`",
                    b.render(),
                    only_a.into_iter().collect::<Vec<_>>().join(" + ")
                )));
            }
            subst.bind(ra, Type::Effects(EffectRow::closed(only_b)));
            Ok(())
        }
        (None, Some(rb)) => {
            if !only_b.is_empty() {
                return Err(TypeError::bare(format!(
                    "effect mismatch: `{}` is not allowed to perform `{}`",
                    a.render(),
                    only_b.into_iter().collect::<Vec<_>>().join(" + ")
                )));
            }
            subst.bind(rb, Type::Effects(EffectRow::closed(only_a)));
            Ok(())
        }
        (Some(ra), Some(rb)) => {
            if ra == rb {
                // Same tail on both sides: the label parts must already
                // agree, otherwise the tail would have to absorb a label
                // into itself (infinite effect row).
                if !only_a.is_empty() || !only_b.is_empty() {
                    return Err(TypeError::bare(format!(
                        "infinite effect row: `{}` ~ `{}`",
                        a.render(),
                        b.render()
                    )));
                }
                return Ok(());
            }
            let fresh_tail = subst.fresh_var();
            subst.bind(
                ra,
                Type::Effects(EffectRow {
                    labels: only_b,
                    tail: Some(fresh_tail),
                }),
            );
            subst.bind(
                rb,
                Type::Effects(EffectRow {
                    labels: only_a,
                    tail: Some(fresh_tail),
                }),
            );
            Ok(())
        }
    }
}

/// Row unification: unify two row types by matching fields by name.
fn unify_rows(
    subst: &mut Subst,
    fields_a: &[(String, Type)],
    rest_a: Option<TypeVar>,
    fields_b: &[(String, Type)],
    rest_b: Option<TypeVar>,
) -> Result<(), TypeError> {
    let map_a: HashMap<&str, &Type> = fields_a.iter().map(|(n, t)| (n.as_str(), t)).collect();
    let map_b: HashMap<&str, &Type> = fields_b.iter().map(|(n, t)| (n.as_str(), t)).collect();

    // Unify fields present in both rows
    for (name, ty_a) in &map_a {
        if let Some(ty_b) = map_b.get(name) {
            unify(subst, ty_a, ty_b)?;
        }
    }

    // Fields only in A (not in B)
    let only_a: Vec<(String, Type)> = fields_a
        .iter()
        .filter(|(n, _)| !map_b.contains_key(n.as_str()))
        .cloned()
        .collect();

    // Fields only in B (not in A)
    let only_b: Vec<(String, Type)> = fields_b
        .iter()
        .filter(|(n, _)| !map_a.contains_key(n.as_str()))
        .cloned()
        .collect();

    match (rest_a, rest_b) {
        (None, None) => {
            if !only_a.is_empty() || !only_b.is_empty() {
                return Err(TypeError::bare(format!(
                    "record field mismatch: extra fields {:?} / {:?}",
                    only_a.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>(),
                    only_b.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>(),
                )));
            }
            Ok(())
        }
        (Some(ra), None) => {
            if !only_a.is_empty() {
                return Err(TypeError::bare(format!(
                    "closed record missing fields: {:?}",
                    only_a.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>(),
                )));
            }
            subst.bind(ra, Type::Row(only_b, None));
            Ok(())
        }
        (None, Some(rb)) => {
            if !only_b.is_empty() {
                return Err(TypeError::bare(format!(
                    "closed record missing fields: {:?}",
                    only_b.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>(),
                )));
            }
            subst.bind(rb, Type::Row(only_a, None));
            Ok(())
        }
        (Some(ra), Some(rb)) => {
            let fresh_rest = subst.fresh_var();
            subst.bind(ra, Type::Row(only_b, Some(fresh_rest)));
            subst.bind(rb, Type::Row(only_a, Some(fresh_rest)));
            Ok(())
        }
    }
}

/// Type scheme: forall vars . type (for let-polymorphism)
#[derive(Debug, Clone)]
pub struct Scheme {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
    /// Trait bounds on quantified type variables (e.g., Add a => ...)
    pub bounds: Vec<(TypeVar, Vec<TraitBound>)>,
}

impl Scheme {
    pub fn mono(ty: Type) -> Self {
        Self {
            vars: vec![],
            ty,
            bounds: vec![],
        }
    }

    pub fn poly(vars: Vec<TypeVar>, ty: Type) -> Self {
        Self {
            vars,
            ty,
            bounds: vec![],
        }
    }
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.bounds.is_empty() {
            let bound_strs: Vec<String> = self
                .bounds
                .iter()
                .flat_map(|(tv, bounds)| {
                    bounds
                        .iter()
                        .map(move |b| format!("{} t{}", b.trait_name, tv.0))
                })
                .collect();
            write!(f, "{} => ", bound_strs.join(", "))?;
        }
        write!(f, "{}", self.ty)
    }
}

/// Type environment: maps names to type schemes.
#[derive(Debug, Clone)]
pub struct TypeEnv {
    bindings: Vec<HashMap<String, Scheme>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            bindings: vec![HashMap::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.bindings.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.bindings.len() > 1 {
            self.bindings.pop();
        }
    }

    pub fn get(&self, name: &str) -> Option<&Scheme> {
        for scope in self.bindings.iter().rev() {
            if let Some(s) = scope.get(name) {
                return Some(s);
            }
        }
        None
    }

    pub fn set(&mut self, name: String, scheme: Scheme) {
        if let Some(scope) = self.bindings.last_mut() {
            scope.insert(name, scheme);
        }
    }

    pub fn set_global(&mut self, name: String, scheme: Scheme) {
        self.bindings[0].insert(name, scheme);
    }

    /// Access the global (bottom) scope.
    pub fn global_scope(&self) -> Option<&HashMap<String, Scheme>> {
        self.bindings.first()
    }

    /// Returns all in-scope (name, Scheme) pairs. Inner scopes shadow outer.
    pub fn all_visible_names(&self) -> Vec<(String, Scheme)> {
        let mut seen = HashMap::new();
        // Iterate from outermost to innermost; later inserts overwrite earlier.
        for scope in &self.bindings {
            for (name, scheme) in scope {
                seen.insert(name.clone(), scheme.clone());
            }
        }
        seen.into_iter().collect()
    }

    /// Free type variables in the environment.
    pub fn free_vars(&self, subst: &Subst) -> BTreeSet<TypeVar> {
        let mut fvs = BTreeSet::new();
        for scope in &self.bindings {
            for scheme in scope.values() {
                let ty = subst.resolve(&scheme.ty);
                free_vars_ty(&ty, &mut fvs);
                for v in &scheme.vars {
                    fvs.remove(v);
                }
            }
        }
        fvs
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

fn free_vars_ty(ty: &Type, out: &mut BTreeSet<TypeVar>) {
    match ty {
        Type::Var(v) => {
            out.insert(*v);
        }
        Type::Fn(params, ret, effects) => {
            for p in params {
                free_vars_ty(p, out);
            }
            free_vars_ty(ret, out);
            if let Some(t) = effects.tail {
                out.insert(t);
            }
        }
        Type::Con(_, args) => {
            for a in args {
                free_vars_ty(a, out);
            }
        }
        Type::Tuple(items) => {
            for t in items {
                free_vars_ty(t, out);
            }
        }
        Type::Effects(row) => {
            if let Some(t) = row.tail {
                out.insert(t);
            }
        }
        Type::Row(fields, rest) => {
            for (_, t) in fields {
                free_vars_ty(t, out);
            }
            if let Some(r) = rest {
                out.insert(*r);
            }
        }
        Type::Record(inner) => free_vars_ty(inner, out),
        Type::Dim(_) => {}
        _ => {}
    }
}

/// Collect free type variables in left-to-right appearance order (for pretty printing).
fn free_vars_ordered(ty: &Type, out: &mut Vec<TypeVar>) {
    match ty {
        Type::Var(v) => {
            if !out.contains(v) {
                out.push(*v);
            }
        }
        // NOTE: effect-row tail variables are deliberately NOT collected
        // here. This function feeds pretty-printing (∀ prefixes and letter
        // names); effect tails are an inference-internal detail and would
        // only add noise ("invisible types" applies to effects too).
        Type::Fn(params, ret, _effects) => {
            for p in params {
                free_vars_ordered(p, out);
            }
            free_vars_ordered(ret, out);
        }
        Type::Con(_, args) => {
            for a in args {
                free_vars_ordered(a, out);
            }
        }
        Type::Tuple(items) => {
            for t in items {
                free_vars_ordered(t, out);
            }
        }
        Type::Effects(_) => {}
        Type::Row(fields, rest) => {
            for (_, t) in fields {
                free_vars_ordered(t, out);
            }
            if let Some(r) = rest {
                if !out.contains(r) {
                    out.push(*r);
                }
            }
        }
        Type::Record(inner) => free_vars_ordered(inner, out),
        Type::Dim(_) => {}
        _ => {}
    }
}

/// Pretty-print a type using nice variable names, with parenthesization for nested fn types.
fn pretty_type(ty: &Type, var_names: &HashMap<TypeVar, String>, nested: bool) -> String {
    match ty {
        Type::Int => "Num".to_string(),
        Type::Float => "Float".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Str => "String".to_string(),
        Type::Keyword => "Keyword".to_string(),
        Type::Unit => "()".to_string(),
        Type::Var(v) => var_names
            .get(v)
            .cloned()
            .unwrap_or_else(|| format!("t{}", v.0)),
        Type::Fn(params, ret, effects) => {
            let mut parts = Vec::new();
            for p in params {
                // Parenthesize fn-typed params
                parts.push(pretty_type(p, var_names, true));
            }
            parts.push(pretty_type(ret, var_names, false));
            let mut s = parts.join(" \u{2192} ");
            // Render the effect row only when it has concrete labels — a
            // pure or merely-open tail is the common case and stays quiet.
            if !effects.labels.is_empty() {
                s = format!("{s} / {}", effects.render());
            }
            if nested {
                format!("({s})")
            } else {
                s
            }
        }
        Type::Con(name, args) if args.is_empty() => name.clone(),
        Type::Con(name, args) => {
            let arg_strs: Vec<String> = args
                .iter()
                .map(|a| pretty_type(a, var_names, true))
                .collect();
            format!("{} {}", name, arg_strs.join(" "))
        }
        Type::Tuple(items) => {
            let inner: Vec<String> = items
                .iter()
                .map(|t| pretty_type(t, var_names, false))
                .collect();
            format!("({})", inner.join(", "))
        }
        Type::Record(row) => pretty_type(row, var_names, false),
        Type::Row(fields, rest) => {
            let mut parts: Vec<String> = fields
                .iter()
                .map(|(n, t)| format!("{}: {}", n, pretty_type(t, var_names, false)))
                .collect();
            if let Some(r) = rest {
                parts.push(
                    var_names
                        .get(r)
                        .cloned()
                        .unwrap_or_else(|| format!("t{}", r.0)),
                );
            }
            format!("{{{}}}", parts.join(", "))
        }
        Type::Effects(row) => format!("{{{}}}", row.render()),
        Type::Dim(d) => d.to_string(),
    }
}

/// Pretty-print a type scheme with nice variable names (a, b, c, ...).
///
/// - Resolves all type vars through substitution
/// - Maps remaining free vars to letters
/// - Adds `∀a b.` prefix for polymorphic types
/// - Adds `Add a =>` prefix for constrained vars
/// - Parenthesizes fn types when nested as arguments
pub fn pretty_scheme(scheme: &Scheme, subst: &Subst) -> String {
    let resolved = subst.resolve(&scheme.ty);

    // Collect free vars in appearance order
    let mut ordered_vars = Vec::new();
    free_vars_ordered(&resolved, &mut ordered_vars);

    // Only keep vars that are quantified in this scheme
    let quantified: BTreeSet<TypeVar> = scheme.vars.iter().copied().collect();
    ordered_vars.retain(|v| quantified.contains(v));

    // Map to nice letter names
    let mut var_names: HashMap<TypeVar, String> = HashMap::new();
    let mut letter = b'a';
    for v in &ordered_vars {
        if !var_names.contains_key(v) {
            var_names.insert(*v, String::from(letter as char));
            if letter < b'z' {
                letter += 1;
            }
        }
    }

    let mut result = String::new();

    // Collect constraint strings (deduplicated)
    let mut constraint_parts = Vec::new();
    let mut seen_constraints = BTreeSet::new();
    for (tv, bounds) in &scheme.bounds {
        if let Some(name) = var_names.get(tv) {
            for b in bounds {
                let key = format!("{} {}", b.trait_name, name);
                if seen_constraints.insert(key.clone()) {
                    constraint_parts.push(key);
                }
            }
        }
    }

    // ∀ prefix for polymorphic vars (only if there are quantified free vars)
    if !ordered_vars.is_empty() {
        let var_list: Vec<&String> = ordered_vars
            .iter()
            .filter_map(|v| var_names.get(v))
            .collect();
        if !var_list.is_empty() {
            result.push_str(&format!(
                "\u{2200}{}",
                var_list
                    .iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join(" ")
            ));
            result.push_str(". ");
        }
    }

    // Constraint prefix
    if !constraint_parts.is_empty() {
        result.push_str(&constraint_parts.join(", "));
        result.push_str(" => ");
    }

    result.push_str(&pretty_type(&resolved, &var_names, false));
    result
}

/// Generalize a type into a scheme by quantifying over variables not free in the env.
pub fn generalize(env: &TypeEnv, subst: &Subst, ty: &Type) -> Scheme {
    let resolved = subst.resolve(ty);
    let env_fvs = env.free_vars(subst);
    let mut ty_fvs = BTreeSet::new();
    free_vars_ty(&resolved, &mut ty_fvs);
    let vars: Vec<TypeVar> = ty_fvs.difference(&env_fvs).copied().collect();
    // Collect trait bounds for quantified variables
    let bounds: Vec<(TypeVar, Vec<TraitBound>)> = vars
        .iter()
        .filter_map(|v| subst.constraints.get(v).map(|bs| (*v, bs.clone())))
        .filter(|(_, bs)| !bs.is_empty())
        .collect();
    Scheme {
        vars,
        ty: resolved,
        bounds,
    }
}

/// Instantiate a scheme with fresh type variables, propagating constraints.
pub fn instantiate(subst: &mut Subst, scheme: &Scheme) -> Type {
    let mapping: HashMap<TypeVar, Type> = scheme.vars.iter().map(|v| (*v, subst.fresh())).collect();
    // Propagate constraints from old vars to fresh vars
    for (old_var, new_ty) in &mapping {
        if let Type::Var(new_var) = new_ty {
            if let Some(bounds) = subst.constraints.get(old_var).cloned() {
                for bound in bounds {
                    subst.add_constraint(*new_var, bound);
                }
            }
        }
    }
    // Also propagate bounds stored in the scheme itself
    for (old_var, bounds) in &scheme.bounds {
        if let Some(Type::Var(new_var)) = mapping.get(old_var) {
            for bound in bounds {
                subst.add_constraint(*new_var, bound.clone());
            }
        }
    }
    substitute(&scheme.ty, &mapping)
}

/// Substitute an effect row's tail variable through the mapping (used by
/// scheme instantiation so generalized tails are freshened per use).
fn substitute_effect_row(row: &EffectRow, mapping: &HashMap<TypeVar, Type>) -> EffectRow {
    let tail = row.tail.map(|t| {
        if let Some(Type::Var(v)) = mapping.get(&t) {
            *v
        } else {
            t
        }
    });
    EffectRow {
        labels: row.labels.clone(),
        tail,
    }
}

fn substitute(ty: &Type, mapping: &HashMap<TypeVar, Type>) -> Type {
    match ty {
        Type::Var(v) => mapping.get(v).cloned().unwrap_or(ty.clone()),
        Type::Fn(params, ret, effects) => Type::Fn(
            params.iter().map(|p| substitute(p, mapping)).collect(),
            Box::new(substitute(ret, mapping)),
            substitute_effect_row(effects, mapping),
        ),
        Type::Con(name, args) => Type::Con(
            name.clone(),
            args.iter().map(|a| substitute(a, mapping)).collect(),
        ),
        Type::Tuple(items) => Type::Tuple(items.iter().map(|t| substitute(t, mapping)).collect()),
        Type::Effects(row) => Type::Effects(substitute_effect_row(row, mapping)),
        Type::Row(fields, rest) => {
            let new_fields = fields
                .iter()
                .map(|(n, t)| (n.clone(), substitute(t, mapping)))
                .collect();
            let new_rest = rest.map(|r| {
                if let Some(Type::Var(v)) = mapping.get(&r) {
                    *v
                } else {
                    r
                }
            });
            Type::Row(new_fields, new_rest)
        }
        Type::Record(inner) => Type::Record(Box::new(substitute(inner, mapping))),
        Type::Dim(_) => ty.clone(),
        _ => ty.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_visible_names_basic() {
        let mut env = TypeEnv::new();
        env.set("x".to_string(), Scheme::mono(Type::Int));
        env.set("y".to_string(), Scheme::mono(Type::Bool));

        let names = env.all_visible_names();
        let name_strs: Vec<&str> = names.iter().map(|(n, _)| n.as_str()).collect();
        assert!(name_strs.contains(&"x"));
        assert!(name_strs.contains(&"y"));
    }

    #[test]
    fn all_visible_names_shadowing() {
        let mut env = TypeEnv::new();
        env.set("x".to_string(), Scheme::mono(Type::Int));
        env.push_scope();
        env.set("x".to_string(), Scheme::mono(Type::Bool));

        let names = env.all_visible_names();
        // Should have exactly one "x"
        let x_entries: Vec<_> = names.iter().filter(|(n, _)| n == "x").collect();
        assert_eq!(x_entries.len(), 1);
        // The shadowing entry should be Bool
        assert_eq!(x_entries[0].1.ty, Type::Bool);
    }

    #[test]
    fn all_visible_names_includes_all_scopes() {
        let mut env = TypeEnv::new();
        env.set("a".to_string(), Scheme::mono(Type::Int));
        env.push_scope();
        env.set("b".to_string(), Scheme::mono(Type::Bool));

        let names = env.all_visible_names();
        let name_strs: Vec<&str> = names.iter().map(|(n, _)| n.as_str()).collect();
        assert!(name_strs.contains(&"a"));
        assert!(name_strs.contains(&"b"));
    }

    fn infer_and_pretty(source: &str, name: &str) -> String {
        let exprs = crate::parser::parse(source).unwrap();
        let mut checker = crate::check::Checker::new();
        let errors = checker.check_program(&exprs);
        assert!(errors.is_empty(), "type errors: {:?}", errors);
        let scheme = checker.env.get(name).expect("binding not found");
        pretty_scheme(scheme, &checker.subst)
    }

    #[test]
    fn pretty_scheme_id() {
        let result = infer_and_pretty("[fn id [x] x]", "id");
        assert_eq!(result, "\u{2200}a. a \u{2192} a");
    }

    #[test]
    fn pretty_scheme_apply() {
        let result = infer_and_pretty("[fn apply [f x] [f x]]", "apply");
        assert_eq!(result, "\u{2200}a b. (a \u{2192} b) \u{2192} a \u{2192} b");
    }

    #[test]
    fn pretty_scheme_add() {
        let result = infer_and_pretty("[fn add [a b] [+ a b]]", "add");
        assert_eq!(result, "\u{2200}a. Add a => a \u{2192} a \u{2192} a");
    }

    #[test]
    fn pretty_scheme_double() {
        let result = infer_and_pretty("[fn double [x] [+ x x]]", "double");
        assert_eq!(result, "\u{2200}a. Add a => a \u{2192} a");
    }

    #[test]
    fn pretty_scheme_concrete() {
        // Monomorphic: no ∀ prefix
        let result = infer_and_pretty("[fn not2 [x] [not x]]", "not2");
        assert_eq!(result, "Bool \u{2192} Bool");
    }
}
