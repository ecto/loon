use std::collections::{BTreeSet, HashMap};
use std::fmt;

use crate::syntax::Span;

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
    /// Function type: params -> return
    Fn(Vec<Type>, Box<Type>),
    /// Type constructor: name + type args (e.g., Vec<Int>, Option<T>)
    Con(String, Vec<Type>),
    /// Tuple
    Tuple(Vec<Type>),
    /// Effect set on a function type
    Effect(Box<Type>, EffectSet),
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
            Type::Fn(params, ret) => {
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, " \u{2192} ")?;
                    }
                    write!(f, "{p}")?;
                }
                if !params.is_empty() {
                    write!(f, " \u{2192} ")?;
                }
                write!(f, "{ret}")
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
            Type::Effect(inner, effects) => {
                write!(f, "{inner} / {{")?;
                for (i, e) in effects.0.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, "}}")
            }
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
            Type::Fn(params, ret) => Type::Fn(
                params.iter().map(|p| self.resolve(p)).collect(),
                Box::new(self.resolve(ret)),
            ),
            Type::Con(name, args) => {
                Type::Con(name.clone(), args.iter().map(|a| self.resolve(a)).collect())
            }
            Type::Tuple(items) => {
                Type::Tuple(items.iter().map(|t| self.resolve(t)).collect())
            }
            Type::Effect(inner, effects) => {
                Type::Effect(Box::new(self.resolve(inner)), effects.clone())
            }
            _ => ty.clone(),
        }
    }

    /// Occurs check: does TypeVar v occur in type ty?
    fn occurs_in(&self, v: TypeVar, ty: &Type) -> bool {
        match self.resolve(ty) {
            Type::Var(u) => u == v,
            Type::Fn(params, ret) => {
                params.iter().any(|p| self.occurs_in(v, p)) || self.occurs_in(v, &ret)
            }
            Type::Con(_, args) => args.iter().any(|a| self.occurs_in(v, a)),
            Type::Tuple(items) => items.iter().any(|t| self.occurs_in(v, t)),
            Type::Effect(inner, _) => self.occurs_in(v, &inner),
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
        (Type::Fn(ap, ar), Type::Fn(bp, br)) => {
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
            unify(subst, ar, br)
        }
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
        _ => Err(TypeError::bare(format!("cannot unify {a} with {b}"))),
    }
}

/// Type scheme: forall vars . type (for let-polymorphism)
#[derive(Debug, Clone)]
pub struct Scheme {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
}

impl Scheme {
    pub fn mono(ty: Type) -> Self {
        Self { vars: vec![], ty }
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
        Type::Fn(params, ret) => {
            for p in params {
                free_vars_ty(p, out);
            }
            free_vars_ty(ret, out);
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
        Type::Effect(inner, _) => free_vars_ty(inner, out),
        _ => {}
    }
}

/// Generalize a type into a scheme by quantifying over variables not free in the env.
pub fn generalize(env: &TypeEnv, subst: &Subst, ty: &Type) -> Scheme {
    let resolved = subst.resolve(ty);
    let env_fvs = env.free_vars(subst);
    let mut ty_fvs = BTreeSet::new();
    free_vars_ty(&resolved, &mut ty_fvs);
    let vars: Vec<TypeVar> = ty_fvs.difference(&env_fvs).copied().collect();
    Scheme {
        vars,
        ty: resolved,
    }
}

/// Instantiate a scheme with fresh type variables, propagating constraints.
pub fn instantiate(subst: &mut Subst, scheme: &Scheme) -> Type {
    let mapping: HashMap<TypeVar, Type> = scheme
        .vars
        .iter()
        .map(|v| (*v, subst.fresh()))
        .collect();
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
    substitute(&scheme.ty, &mapping)
}

fn substitute(ty: &Type, mapping: &HashMap<TypeVar, Type>) -> Type {
    match ty {
        Type::Var(v) => mapping.get(v).cloned().unwrap_or(ty.clone()),
        Type::Fn(params, ret) => Type::Fn(
            params.iter().map(|p| substitute(p, mapping)).collect(),
            Box::new(substitute(ret, mapping)),
        ),
        Type::Con(name, args) => {
            Type::Con(name.clone(), args.iter().map(|a| substitute(a, mapping)).collect())
        }
        Type::Tuple(items) => {
            Type::Tuple(items.iter().map(|t| substitute(t, mapping)).collect())
        }
        Type::Effect(inner, effects) => {
            Type::Effect(Box::new(substitute(inner, mapping)), effects.clone())
        }
        _ => ty.clone(),
    }
}
