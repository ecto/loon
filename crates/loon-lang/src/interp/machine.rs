//! SICP-style effect machine for Loon.
//!
//! Three ideas from SICP:
//! 1. **Analyze** (4.1.7) — transform AST to Proc IR once, execute many times
//! 2. **Eval/Apply** (4.1.1) — two fundamental operations, everything else is a continuation
//! 3. **Continuations** (5.4) — effects, TCO, errors all emerge from one mechanism

use crate::ast::{Expr, ExprKind};
use crate::interp::env::Env;
use crate::interp::value::{LoonFn, Param, Value};
use crate::interp::{
    access_field, bind_param, err, err_at, eval_impl_def, eval_test_def, eval_type_def,
    extract_param, extract_params, pattern_matches, perform_effect, try_builtin_handler, IResult,
    InterpError, PerformedEffect, StackFrame,
};
use crate::syntax::Span;
use std::collections::HashMap;
use std::rc::Rc;

// ─── Proc IR ───────────────────────────────────────────────────────────────

/// Pre-analyzed expression. Produced once by `analyze`, executed many times
/// by the machine. No string comparisons for special forms at runtime.
#[derive(Debug, Clone)]
pub enum Proc {
    Const(Value, Span),
    Lookup(String, Span),
    If {
        cond: Box<Proc>,
        then: Box<Proc>,
        else_: Option<Box<Proc>>,
    },
    Seq(Vec<Proc>),
    Lambda {
        name: Option<String>,
        clauses: Vec<(Vec<Param>, Rc<[Expr]>)>,
        span: Span,
    },
    Apply {
        func: Box<Proc>,
        args: Vec<Proc>,
        span: Span,
    },
    Let {
        binding: Param,
        value: Box<Proc>,
        span: Span,
    },
    Match {
        scrutinee: Box<Proc>,
        arms: Vec<MatchArm>,
    },
    Perform {
        effect: String,
        op: String,
        args: Vec<Proc>,
        span: Span,
    },
    Handle {
        body: Box<Proc>,
        clauses: Vec<HandlerClause>,
    },
    Loop {
        bindings: Vec<(String, Proc)>,
        body: Vec<Proc>,
    },
    Recur(Vec<Proc>, Span),
    DotAccess {
        expr: Box<Proc>,
        field: String,
        full_path: Option<String>,
        span: Span,
    },
    Mut(Box<Proc>),
    Set {
        name: String,
        value: Box<Proc>,
    },
    MakeVec(Vec<Proc>, Span),
    MakeSet(Vec<Proc>, Span),
    MakeMap(Vec<(Proc, Proc)>, Span),
    MakeTuple(Vec<Proc>, Span),
    DefType(Vec<Expr>),
    DefImpl(Vec<Expr>),
    DefTest(Vec<Expr>),
    Inspect {
        expr: Box<Proc>,
        source: String,
    },
    CatchErrors(Box<Proc>),
    Pub {
        inner: Box<Proc>,
        name: Option<String>,
    },
    Derive {
        inner: Box<Proc>,
    },
    Noop,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Expr,
    pub guard: Option<Proc>,
    pub body: Proc,
}

#[derive(Debug, Clone)]
pub struct HandlerClause {
    pub effect: String,
    pub op: String,
    pub params: Vec<String>,
    pub body: Proc,
}

// ─── Analyze ───────────────────────────────────────────────────────────────

/// Transform an AST expression into a Proc. Called once per program.
pub fn analyze(expr: &Expr) -> Proc {
    match &expr.kind {
        ExprKind::Int(n) => Proc::Const(Value::Int(*n), expr.span),
        ExprKind::Float(n) => Proc::Const(Value::Float(*n), expr.span),
        ExprKind::Bool(b) => Proc::Const(Value::Bool(*b), expr.span),
        ExprKind::Str(s) => Proc::Const(Value::Str(s.clone()), expr.span),
        ExprKind::Keyword(k) => Proc::Const(Value::Keyword(k.clone()), expr.span),
        ExprKind::Symbol(s) => Proc::Lookup(s.clone(), expr.span),

        ExprKind::DotAccess(inner, field) => {
            let full_path = expr.as_dotted_path();
            Proc::DotAccess {
                expr: Box::new(analyze(inner)),
                field: field.clone(),
                full_path,
                span: expr.span,
            }
        }

        ExprKind::Vec(items) => Proc::MakeVec(items.iter().map(analyze).collect(), expr.span),
        ExprKind::Set(items) => Proc::MakeSet(items.iter().map(analyze).collect(), expr.span),
        ExprKind::Map(pairs) => Proc::MakeMap(
            pairs
                .iter()
                .map(|(k, v)| (analyze(k), analyze(v)))
                .collect(),
            expr.span,
        ),
        ExprKind::Tuple(items) => Proc::MakeTuple(items.iter().map(analyze).collect(), expr.span),

        ExprKind::Quote(_) | ExprKind::Unquote(_) | ExprKind::UnquoteSplice(_) => Proc::Noop,

        ExprKind::List(items) if items.is_empty() => Proc::Const(Value::Unit, expr.span),
        ExprKind::List(items) => analyze_list(items, expr.span),
    }
}

fn analyze_list(items: &[Expr], span: Span) -> Proc {
    let head = &items[0];

    if let ExprKind::Symbol(s) = &head.kind {
        match s.as_str() {
            "fn" => return analyze_fn(&items[1..], span),
            "let" => return analyze_let(&items[1..], span),
            "if" => return analyze_if(&items[1..]),
            "when" => return analyze_when(&items[1..]),
            "do" => return Proc::Seq(items[1..].iter().map(analyze).collect()),
            "match" => return analyze_match(&items[1..]),
            "pipe" => return analyze_pipe(&items[1..]),
            "handle" => return analyze_handle(&items[1..]),
            "try" => return analyze_try(&items[1..]),
            "loop" => return analyze_loop(&items[1..]),
            "recur" => return Proc::Recur(items[1..].iter().map(analyze).collect(), span),
            "mut" => {
                if items.len() >= 2 {
                    return Proc::Mut(Box::new(analyze(&items[1])));
                }
                return Proc::Noop;
            }
            "set!" => {
                if items.len() >= 3 {
                    if let ExprKind::Symbol(name) = &items[1].kind {
                        return Proc::Set {
                            name: name.clone(),
                            value: Box::new(analyze(&items[2])),
                        };
                    }
                }
                return Proc::Noop;
            }
            "type" => return Proc::DefType(items[1..].to_vec()),
            "test" => return Proc::DefTest(items[1..].to_vec()),
            "impl" => return Proc::DefImpl(items[1..].to_vec()),
            "effect" | "trait" | "sig" | "macro" | "macro+" | "macroexpand" => return Proc::Noop,
            "derive" => {
                if items.len() >= 3 {
                    return Proc::Derive {
                        inner: Box::new(analyze(&items[2])),
                    };
                }
                return Proc::Noop;
            }
            "catch-errors" => {
                if items.len() >= 2 {
                    return Proc::CatchErrors(Box::new(analyze(&items[1])));
                }
                return Proc::Const(Value::Vec(imbl::Vector::new()), span);
            }
            "inspect" => {
                if items.len() >= 2 {
                    let source = format!("{}", &items[1]);
                    return Proc::Inspect {
                        expr: Box::new(analyze(&items[1])),
                        source,
                    };
                }
                return Proc::Noop;
            }
            "pub" => {
                if items.len() > 1 {
                    let name = extract_pub_name(&items[1..]);
                    let inner_expr = Expr::new(ExprKind::List(items[1..].to_vec()), span);
                    return Proc::Pub {
                        inner: Box::new(analyze(&inner_expr)),
                        name,
                    };
                }
                return Proc::Noop;
            }
            _ => {}
        }
    }

    // Effect operation: Effect.op pattern
    if let ExprKind::DotAccess(obj, op) = &head.kind {
        if let ExprKind::Symbol(effect) = &obj.kind {
            if effect.starts_with(char::is_uppercase) {
                return Proc::Perform {
                    effect: effect.clone(),
                    op: op.clone(),
                    args: items[1..].iter().map(analyze).collect(),
                    span,
                };
            }
        }
    }

    // Regular function application
    Proc::Apply {
        func: Box::new(analyze(head)),
        args: items[1..].iter().map(analyze).collect(),
        span,
    }
}

fn extract_pub_name(items: &[Expr]) -> Option<String> {
    if items.len() >= 2 {
        if let ExprKind::Symbol(ref kind) = items[0].kind {
            if kind == "fn" || kind == "let" {
                if let ExprKind::Symbol(ref name) = items[1].kind {
                    return Some(name.clone());
                }
            }
        }
    }
    None
}

fn analyze_fn(args: &[Expr], span: Span) -> Proc {
    if args.is_empty() {
        return Proc::Noop;
    }

    // Named function: [fn name [params] body...] or [fn name (clause) (clause) ...]
    if let ExprKind::Symbol(name) = &args[0].kind {
        let name = name.clone();
        if args.len() < 2 {
            return Proc::Noop;
        }

        // Multi-arity: [fn name (params body) (params body) ...]
        if matches!(args[1].kind, ExprKind::Tuple(_)) {
            let mut clauses = Vec::new();
            for clause_expr in &args[1..] {
                if let ExprKind::Tuple(clause_items) = &clause_expr.kind {
                    if clause_items.len() >= 2 {
                        if let Ok(params) = extract_params(&clause_items[0]) {
                            let body: Rc<[Expr]> = clause_items[1..].to_vec().into();
                            clauses.push((params, body));
                        }
                    }
                }
            }
            return Proc::Lambda {
                name: Some(name),
                clauses,
                span,
            };
        }

        // Single-arity: [fn name [params] #{effects}? body...]
        if let Ok(params) = extract_params(&args[1]) {
            let mut body_start = 2;
            if body_start < args.len() && matches!(&args[body_start].kind, ExprKind::Set(_)) {
                body_start += 1; // skip effect annotation
            }
            let body: Rc<[Expr]> = args[body_start..].to_vec().into();
            return Proc::Lambda {
                name: Some(name),
                clauses: vec![(params, body)],
                span,
            };
        }
        return Proc::Noop;
    }

    // Anonymous lambda: [fn [params] body...]
    if let Ok(params) = extract_params(&args[0]) {
        let body: Rc<[Expr]> = args[1..].to_vec().into();
        return Proc::Lambda {
            name: None,
            clauses: vec![(params, body)],
            span,
        };
    }
    Proc::Noop
}

fn analyze_let(args: &[Expr], span: Span) -> Proc {
    if args.len() < 2 {
        return Proc::Noop;
    }
    // Handle [let mut name val]
    let (binding_expr, val_expr) = if matches!(&args[0].kind, ExprKind::Symbol(s) if s == "mut") {
        if args.len() < 3 {
            return Proc::Noop;
        }
        (&args[1], &args[2])
    } else {
        (&args[0], &args[1])
    };

    if let Ok(param) = extract_param(binding_expr) {
        Proc::Let {
            binding: param,
            value: Box::new(analyze(val_expr)),
            span,
        }
    } else {
        Proc::Noop
    }
}

fn analyze_if(args: &[Expr]) -> Proc {
    if args.len() < 2 {
        return Proc::Noop;
    }
    Proc::If {
        cond: Box::new(analyze(&args[0])),
        then: Box::new(analyze(&args[1])),
        else_: args.get(2).map(|e| Box::new(analyze(e))),
    }
}

fn analyze_when(args: &[Expr]) -> Proc {
    if args.len() < 2 {
        return Proc::Noop;
    }
    // Desugar: [when cond body...] → [if cond [do body...]]
    let body = if args.len() == 2 {
        analyze(&args[1])
    } else {
        Proc::Seq(args[1..].iter().map(analyze).collect())
    };
    Proc::If {
        cond: Box::new(analyze(&args[0])),
        then: Box::new(body),
        else_: None,
    }
}

fn analyze_match(args: &[Expr]) -> Proc {
    if args.is_empty() {
        return Proc::Noop;
    }
    let scrutinee = Box::new(analyze(&args[0]));
    let raw_arms = &args[1..];
    let mut arms = Vec::new();
    let mut i = 0;
    while i < raw_arms.len() {
        let pattern = raw_arms[i].clone();

        // Check for guard: pattern [when guard] body
        if i + 2 < raw_arms.len() {
            if let ExprKind::List(guard_form) = &raw_arms[i + 1].kind {
                if !guard_form.is_empty() {
                    if let ExprKind::Symbol(s) = &guard_form[0].kind {
                        if s == "when" && guard_form.len() > 1 {
                            arms.push(MatchArm {
                                pattern,
                                guard: Some(analyze(&guard_form[1])),
                                body: analyze(&raw_arms[i + 2]),
                            });
                            i += 3;
                            continue;
                        }
                    }
                }
            }
        }

        // pattern body
        if i + 1 < raw_arms.len() {
            arms.push(MatchArm {
                pattern,
                guard: None,
                body: analyze(&raw_arms[i + 1]),
            });
            i += 2;
        } else {
            break;
        }
    }
    Proc::Match { scrutinee, arms }
}

fn analyze_pipe(args: &[Expr]) -> Proc {
    if args.is_empty() {
        return Proc::Noop;
    }
    // Desugar: [pipe x [f a] g] → [g [f a x]]
    let mut current = analyze(&args[0]);
    for step in &args[1..] {
        match &step.kind {
            ExprKind::List(items) if !items.is_empty() => {
                let func = analyze(&items[0]);
                let mut call_args: Vec<Proc> = items[1..].iter().map(analyze).collect();
                call_args.push(current);
                current = Proc::Apply {
                    func: Box::new(func),
                    args: call_args,
                    span: step.span,
                };
            }
            ExprKind::Symbol(_) => {
                let func = analyze(step);
                current = Proc::Apply {
                    func: Box::new(func),
                    args: vec![current],
                    span: step.span,
                };
            }
            _ => {}
        }
    }
    current
}

fn analyze_handle(args: &[Expr]) -> Proc {
    if args.is_empty() {
        return Proc::Noop;
    }
    let body = Box::new(analyze(&args[0]));
    let mut clauses = Vec::new();
    let handler_args = &args[1..];
    let mut i = 0;
    while i < handler_args.len() {
        // [Effect.op params...] body
        if let ExprKind::List(pattern) = &handler_args[i].kind {
            if !pattern.is_empty() {
                if let ExprKind::DotAccess(obj, op) = &pattern[0].kind {
                    if let ExprKind::Symbol(effect) = &obj.kind {
                        let params: Vec<String> = pattern[1..]
                            .iter()
                            .filter_map(|e| {
                                if let ExprKind::Symbol(s) = &e.kind {
                                    Some(s.clone())
                                } else {
                                    None
                                }
                            })
                            .collect();
                        if i + 1 < handler_args.len() {
                            clauses.push(HandlerClause {
                                effect: effect.clone(),
                                op: op.clone(),
                                params,
                                body: analyze(&handler_args[i + 1]),
                            });
                            i += 2;
                            continue;
                        }
                    }
                }
            }
        }
        i += 1;
    }
    Proc::Handle { body, clauses }
}

fn analyze_try(args: &[Expr]) -> Proc {
    if args.is_empty() {
        return Proc::Noop;
    }
    // [try body on-fail] → Handle with Fail.fail handler
    let body = Box::new(analyze(&args[0]));
    let handler_body = if args.len() > 1 {
        // Build: [on-fail msg]
        let on_fail = analyze(&args[args.len() - 1]);
        Proc::Apply {
            func: Box::new(on_fail),
            args: vec![Proc::Lookup("__fail_msg".to_string(), Span::ZERO)],
            span: Span::ZERO,
        }
    } else {
        Proc::Const(Value::Unit, Span::ZERO)
    };
    Proc::Handle {
        body,
        clauses: vec![HandlerClause {
            effect: "Fail".to_string(),
            op: "fail".to_string(),
            params: vec!["__fail_msg".to_string()],
            body: handler_body,
        }],
    }
}

fn analyze_loop(args: &[Expr]) -> Proc {
    if args.is_empty() {
        return Proc::Noop;
    }
    // [loop [name1 init1 name2 init2 ...] body...]
    let mut bindings = Vec::new();
    if let ExprKind::List(binding_list) = &args[0].kind {
        let mut i = 0;
        while i + 1 < binding_list.len() {
            if let ExprKind::Symbol(name) = &binding_list[i].kind {
                bindings.push((name.clone(), analyze(&binding_list[i + 1])));
            }
            i += 2;
        }
    }
    let body = args[1..].iter().map(analyze).collect();
    Proc::Loop { bindings, body }
}

// ─── Machine ───────────────────────────────────────────────────────────────

/// The three states of computation.
enum Focus {
    /// Reduce this expression in the current environment.
    Eval(Proc),
    /// Enter this function with these arguments.
    Apply(Value, Vec<Value>, Span),
    /// Deliver this value to the top continuation.
    Return(Value),
}

/// A continuation — a suspended computation waiting for a value.
#[derive(Clone)]
enum Kont {
    /// Evaluating function + args left-to-right.
    EvalArgs {
        func: Option<Value>,
        evaluated: Vec<Value>,
        remaining: Vec<Proc>,
        span: Span,
    },
    /// Received condition value — pick a branch.
    If { then_: Proc, else_: Option<Proc> },
    /// Received binding value — bind it.
    Bind(Param),
    /// Sequential evaluation — evaluate remaining, return last.
    Seq(Vec<Proc>),
    /// Pattern match — received scrutinee.
    Match(Vec<MatchArm>),
    /// Restore env after function return.
    RestoreEnv(Env),
    /// Collect values for a vector.
    CollectVec {
        evaluated: Vec<Value>,
        remaining: Vec<Proc>,
        span: Span,
    },
    /// Collect values for a set.
    CollectSet {
        evaluated: Vec<Value>,
        remaining: Vec<Proc>,
        span: Span,
    },
    /// Collect values for a tuple.
    CollectTuple {
        evaluated: Vec<Value>,
        remaining: Vec<Proc>,
        span: Span,
    },
    /// Collect key-value pairs for a map (alternating key, value, key, value...).
    CollectMap {
        pairs: Vec<(Value, Value)>,
        pending_key: Option<Value>,
        remaining: Vec<Proc>,
        span: Span,
    },
    /// Collect effect args then perform.
    CollectPerform {
        effect: String,
        op: String,
        evaluated: Vec<Value>,
        remaining: Vec<Proc>,
        span: Span,
    },
    /// Function body boundary — supports fn/recur.
    FnBody {
        params: Vec<Param>,
        body: Rc<[Expr]>,
    },
    /// Effect handler boundary.
    Handler {
        effect: String,
        clauses: Vec<HandlerClause>,
    },
    /// After handler body completes — splice captured continuation back.
    AfterHandler {
        captured: Vec<Kont>,
        handler_effect: String,
        handler_clauses: Vec<HandlerClause>,
    },
    /// Loop iteration boundary.
    LoopBody { names: Vec<String>, body: Vec<Proc> },
    /// Collect recur args (for subsequent iterations).
    CollectRecur {
        evaluated: Vec<Value>,
        remaining: Vec<Proc>,
        span: Span,
    },
    /// Collect initial loop binding values.
    InitLoop {
        names: Vec<String>,
        evaluated: Vec<Value>,
        remaining: Vec<Proc>,
        body: Vec<Proc>,
    },
    /// Set! — received value.
    SetVar(String),
    /// Dot access — received value, access field.
    AccessField {
        field: String,
        #[allow(dead_code)]
        full_path: Option<String>,
        span: Span,
    },
    /// Inspect — print and return value.
    InspectVal(String),
    /// Pub — mark name and return value (reserved for future use).
    #[allow(dead_code)]
    MarkPub(Option<String>),
}

/// The SICP machine: eval/apply with explicit continuations.
pub struct Machine {
    focus: Focus,
    kont: Vec<Kont>,
    env: Env,
    call_stack: Vec<StackFrame>,
}

impl Machine {
    pub fn new(env: Env) -> Self {
        Self {
            focus: Focus::Return(Value::Unit),
            kont: Vec::new(),
            env,
            call_stack: Vec::new(),
        }
    }

    /// Run a single Proc to completion.
    pub fn run_proc(&mut self, proc: Proc) -> IResult {
        self.focus = Focus::Eval(proc);
        self.run()
    }

    /// The main loop — three transitions, that's the whole machine.
    fn run(&mut self) -> IResult {
        loop {
            match std::mem::replace(&mut self.focus, Focus::Return(Value::Unit)) {
                Focus::Eval(proc) => self.step_eval(proc)?,
                Focus::Apply(func, args, span) => self.step_apply(func, args, span)?,
                Focus::Return(val) => match self.kont.pop() {
                    None => return Ok(val),
                    Some(k) => self.step_deliver(val, k)?,
                },
            }
        }
    }

    // ── Eval: decompose a Proc into continuations ──────────────────────

    fn step_eval(&mut self, proc: Proc) -> Result<(), InterpError> {
        match proc {
            Proc::Const(val, _) => {
                self.focus = Focus::Return(val);
            }

            Proc::Lookup(name, span) => {
                let val = self
                    .env
                    .get(&name)
                    .ok_or_else(|| err_at(format!("unbound symbol '{name}'"), span))?;
                self.focus = Focus::Return(val);
            }

            Proc::If { cond, then, else_ } => {
                self.kont.push(Kont::If {
                    then_: *then,
                    else_: else_.map(|b| *b),
                });
                self.focus = Focus::Eval(*cond);
            }

            Proc::Seq(procs) => {
                if procs.is_empty() {
                    self.focus = Focus::Return(Value::Unit);
                } else {
                    let mut procs = procs;
                    let first = procs.remove(0);
                    if !procs.is_empty() {
                        self.kont.push(Kont::Seq(procs));
                    }
                    self.focus = Focus::Eval(first);
                }
            }

            Proc::Lambda {
                name,
                clauses,
                span: _,
            } => {
                let lf = LoonFn {
                    name: name.clone(),
                    clauses,
                    captured_env: Some(self.env.clone()),
                };
                if let Some(ref name) = lf.name {
                    self.env.set_global(name.clone(), Value::Fn(lf));
                    self.focus = Focus::Return(Value::Unit);
                } else {
                    self.focus = Focus::Return(Value::Fn(lf));
                }
            }

            Proc::Apply { func, args, span } => {
                // Push EvalArgs: first evaluate the function, then args
                self.kont.push(Kont::EvalArgs {
                    func: None,
                    evaluated: Vec::new(),
                    remaining: args,
                    span,
                });
                self.focus = Focus::Eval(*func);
            }

            Proc::Let {
                binding,
                value,
                span: _,
            } => {
                self.kont.push(Kont::Bind(binding));
                self.focus = Focus::Eval(*value);
            }

            Proc::Match { scrutinee, arms } => {
                self.kont.push(Kont::Match(arms));
                self.focus = Focus::Eval(*scrutinee);
            }

            Proc::Perform {
                effect,
                op,
                args,
                span,
            } => {
                if args.is_empty() {
                    self.do_perform(&effect, &op, vec![], span)?;
                } else {
                    let mut remaining = args;
                    let first = remaining.remove(0);
                    self.kont.push(Kont::CollectPerform {
                        effect,
                        op,
                        evaluated: Vec::new(),
                        remaining,
                        span,
                    });
                    self.focus = Focus::Eval(first);
                }
            }

            Proc::Handle { body, clauses } => {
                // Get the effect name(s) from clauses
                if let Some(first) = clauses.first() {
                    self.kont.push(Kont::Handler {
                        effect: first.effect.clone(),
                        clauses,
                    });
                }
                self.focus = Focus::Eval(*body);
            }

            Proc::Loop { bindings, body } => {
                let names: Vec<String> = bindings.iter().map(|(n, _)| n.clone()).collect();
                let init_procs: Vec<Proc> = bindings.into_iter().map(|(_, p)| p).collect();
                if init_procs.is_empty() {
                    // No bindings — push loop body and evaluate directly
                    self.kont.push(Kont::LoopBody {
                        names: Vec::new(),
                        body: body.clone(),
                    });
                    self.env.push_scope();
                    self.eval_seq(body);
                } else {
                    // Collect initial values via InitLoop (NOT CollectRecur)
                    let mut remaining = init_procs;
                    let first = remaining.remove(0);
                    self.kont.push(Kont::InitLoop {
                        names,
                        evaluated: Vec::new(),
                        remaining,
                        body,
                    });
                    self.focus = Focus::Eval(first);
                }
            }

            Proc::Recur(args, span) => {
                if args.is_empty() {
                    // Signal recur with empty args
                    self.do_recur(vec![], span)?;
                } else {
                    let mut remaining = args;
                    let first = remaining.remove(0);
                    self.kont.push(Kont::CollectRecur {
                        evaluated: Vec::new(),
                        remaining,
                        span,
                    });
                    self.focus = Focus::Eval(first);
                }
            }

            Proc::DotAccess {
                expr,
                field,
                full_path,
                span,
            } => {
                // Try qualified name lookup first
                if let Some(ref path) = full_path {
                    if let Some(v) = self.env.get(path) {
                        self.focus = Focus::Return(v);
                        return Ok(());
                    }
                }
                // Fall back to evaluating expr and accessing field
                self.kont.push(Kont::AccessField {
                    field,
                    full_path,
                    span,
                });
                self.focus = Focus::Eval(*expr);
            }

            Proc::Mut(inner) => {
                self.focus = Focus::Eval(*inner);
            }

            Proc::Set { name, value } => {
                self.kont.push(Kont::SetVar(name));
                self.focus = Focus::Eval(*value);
            }

            Proc::MakeVec(procs, span) => {
                if procs.is_empty() {
                    self.focus = Focus::Return(Value::Vec(imbl::Vector::new()));
                } else {
                    let mut remaining = procs;
                    let first = remaining.remove(0);
                    self.kont.push(Kont::CollectVec {
                        evaluated: Vec::new(),
                        remaining,
                        span,
                    });
                    self.focus = Focus::Eval(first);
                }
            }

            Proc::MakeSet(procs, span) => {
                if procs.is_empty() {
                    self.focus = Focus::Return(Value::Set(imbl::HashSet::new()));
                } else {
                    let mut remaining = procs;
                    let first = remaining.remove(0);
                    self.kont.push(Kont::CollectSet {
                        evaluated: Vec::new(),
                        remaining,
                        span,
                    });
                    self.focus = Focus::Eval(first);
                }
            }

            Proc::MakeTuple(procs, span) => {
                if procs.is_empty() {
                    self.focus = Focus::Return(Value::Tuple(Vec::new()));
                } else {
                    let mut remaining = procs;
                    let first = remaining.remove(0);
                    self.kont.push(Kont::CollectTuple {
                        evaluated: Vec::new(),
                        remaining,
                        span,
                    });
                    self.focus = Focus::Eval(first);
                }
            }

            Proc::MakeMap(pairs, span) => {
                if pairs.is_empty() {
                    self.focus = Focus::Return(Value::Map(imbl::HashMap::new()));
                } else {
                    // Flatten pairs into alternating key, value, key, value...
                    let mut flat: Vec<Proc> = Vec::with_capacity(pairs.len() * 2);
                    for (k, v) in pairs {
                        flat.push(k);
                        flat.push(v);
                    }
                    let first = flat.remove(0);
                    self.kont.push(Kont::CollectMap {
                        pairs: Vec::new(),
                        pending_key: None,
                        remaining: flat,
                        span,
                    });
                    self.focus = Focus::Eval(first);
                }
            }

            Proc::DefType(args) => {
                eval_type_def(&args, &mut self.env)?;
                self.focus = Focus::Return(Value::Unit);
            }

            Proc::DefImpl(args) => {
                eval_impl_def(&args, &mut self.env)?;
                self.focus = Focus::Return(Value::Unit);
            }

            Proc::DefTest(args) => {
                eval_test_def(&args, &mut self.env)?;
                self.focus = Focus::Return(Value::Unit);
            }

            Proc::Inspect { expr, source } => {
                self.kont.push(Kont::InspectVal(source));
                self.focus = Focus::Eval(*expr);
            }

            Proc::CatchErrors(inner) => {
                // Evaluate inner as string, then run catch-errors
                self.kont
                    .push(Kont::Bind(Param::Simple("__catch_src".to_string())));
                self.focus = Focus::Eval(*inner);
                // After getting the string value, we need to call eval_catch_errors
                // This is a bit awkward — let's handle it inline
                // Actually, re-think: CatchErrors expects a string source
                // The inner proc should evaluate to a string
                // For now, handle in deliver for Bind
            }

            Proc::Pub { inner, name } => {
                if let Some(ref n) = name {
                    self.env.pub_names.insert(n.clone());
                }
                self.focus = Focus::Eval(*inner);
            }

            Proc::Derive { inner } => {
                self.focus = Focus::Eval(*inner);
            }

            Proc::Noop => {
                self.focus = Focus::Return(Value::Unit);
            }
        }
        Ok(())
    }

    // ── Apply: enter a function body ───────────────────────────────────

    fn step_apply(&mut self, func: Value, args: Vec<Value>, span: Span) -> Result<(), InterpError> {
        match func {
            Value::Fn(ref lf) => {
                let fn_name = lf.name.as_deref().unwrap_or("anonymous").to_string();
                self.call_stack.push(StackFrame {
                    fn_name,
                    call_site: span,
                });

                // Find matching clause
                for (params, body) in &lf.clauses {
                    let has_rest = params.last().is_some_and(|p| matches!(p, Param::Rest(_)));
                    let required = if has_rest {
                        params.len() - 1
                    } else {
                        params.len()
                    };
                    let matches = if has_rest {
                        args.len() >= required
                    } else {
                        args.len() == required
                    };

                    if matches {
                        // Save current env
                        self.kont.push(Kont::RestoreEnv(self.env.clone()));

                        // Set up function env
                        if let Some(ref captured) = lf.captured_env {
                            let mut fn_env = captured.clone();
                            fn_env.merge_globals(&self.env);
                            self.env = fn_env;
                        }
                        self.env.push_scope();

                        // Bind params
                        for (param, val) in params[..required].iter().zip(args[..required].iter()) {
                            bind_param(param, val, &mut self.env)?;
                        }
                        if has_rest {
                            if let Some(Param::Rest(name)) = params.last() {
                                let rest_vals: imbl::Vector<Value> =
                                    args[required..].iter().cloned().collect();
                                self.env.set(name.clone(), Value::Vec(rest_vals));
                            }
                        }

                        // Push FnBody for recur support
                        self.kont.push(Kont::FnBody {
                            params: params.clone(),
                            body: body.clone(),
                        });

                        // Eval body as sequence
                        let body_procs: Vec<Proc> = body.iter().map(analyze).collect();
                        self.eval_seq(body_procs);
                        return Ok(());
                    }
                }

                Err(err(format!(
                    "no matching clause for {} with {} args",
                    lf.name.as_deref().unwrap_or("anonymous"),
                    args.len()
                )))
            }

            Value::Builtin(name, f) => {
                self.call_stack.push(StackFrame {
                    fn_name: format!("<builtin: {name}>"),
                    call_site: span,
                });
                let result = f(&name, &args);
                self.call_stack.pop();
                match result {
                    Ok(val) => {
                        self.focus = Focus::Return(val);
                        Ok(())
                    }
                    Err(e) => Err(e),
                }
            }

            other => Err(err_at(format!("not callable: {other}"), span)),
        }
    }

    // ── Deliver: give a value to a waiting continuation ────────────────

    fn step_deliver(&mut self, val: Value, kont: Kont) -> Result<(), InterpError> {
        match kont {
            Kont::EvalArgs {
                func,
                mut evaluated,
                mut remaining,
                span,
            } => {
                if func.is_none() {
                    // We just evaluated the function expression
                    if remaining.is_empty() {
                        // No args — call immediately
                        self.focus = Focus::Apply(val, Vec::new(), span);
                    } else {
                        let next = remaining.remove(0);
                        self.kont.push(Kont::EvalArgs {
                            func: Some(val),
                            evaluated: Vec::new(),
                            remaining,
                            span,
                        });
                        self.focus = Focus::Eval(next);
                    }
                } else {
                    // We evaluated an argument
                    evaluated.push(val);
                    if remaining.is_empty() {
                        // All args evaluated — apply
                        self.focus = Focus::Apply(func.unwrap(), evaluated, span);
                    } else {
                        let next = remaining.remove(0);
                        self.kont.push(Kont::EvalArgs {
                            func,
                            evaluated,
                            remaining,
                            span,
                        });
                        self.focus = Focus::Eval(next);
                    }
                }
            }

            Kont::If { then_, else_ } => {
                if val.is_truthy() {
                    self.focus = Focus::Eval(then_);
                } else if let Some(else_proc) = else_ {
                    self.focus = Focus::Eval(else_proc);
                } else {
                    self.focus = Focus::Return(Value::Unit);
                }
            }

            Kont::Bind(param) => {
                bind_param(&param, &val, &mut self.env)?;
                self.focus = Focus::Return(val);
            }

            Kont::Seq(mut remaining) => {
                if remaining.is_empty() {
                    self.focus = Focus::Return(val);
                } else {
                    let next = remaining.remove(0);
                    if !remaining.is_empty() {
                        self.kont.push(Kont::Seq(remaining));
                    }
                    // Discard val (intermediate result in sequence)
                    self.focus = Focus::Eval(next);
                }
            }

            Kont::Match(arms) => {
                self.do_match(val, arms)?;
            }

            Kont::RestoreEnv(saved) => {
                self.env = saved;
                self.call_stack.pop();
                self.focus = Focus::Return(val);
            }

            Kont::CollectVec {
                mut evaluated,
                mut remaining,
                span,
            } => {
                evaluated.push(val);
                if remaining.is_empty() {
                    self.focus = Focus::Return(Value::Vec(evaluated.into_iter().collect()));
                } else {
                    let next = remaining.remove(0);
                    self.kont.push(Kont::CollectVec {
                        evaluated,
                        remaining,
                        span,
                    });
                    self.focus = Focus::Eval(next);
                }
            }

            Kont::CollectSet {
                mut evaluated,
                mut remaining,
                span,
            } => {
                evaluated.push(val);
                if remaining.is_empty() {
                    self.focus = Focus::Return(Value::Set(evaluated.into_iter().collect()));
                } else {
                    let next = remaining.remove(0);
                    self.kont.push(Kont::CollectSet {
                        evaluated,
                        remaining,
                        span,
                    });
                    self.focus = Focus::Eval(next);
                }
            }

            Kont::CollectTuple {
                mut evaluated,
                mut remaining,
                span,
            } => {
                evaluated.push(val);
                if remaining.is_empty() {
                    self.focus = Focus::Return(Value::Tuple(evaluated));
                } else {
                    let next = remaining.remove(0);
                    self.kont.push(Kont::CollectTuple {
                        evaluated,
                        remaining,
                        span,
                    });
                    self.focus = Focus::Eval(next);
                }
            }

            Kont::CollectMap {
                mut pairs,
                pending_key,
                mut remaining,
                span,
            } => {
                if pending_key.is_none() {
                    // We just evaluated a key — next evaluate its value
                    if remaining.is_empty() {
                        let map: imbl::HashMap<Value, Value> = pairs.into_iter().collect();
                        self.focus = Focus::Return(Value::Map(map));
                    } else {
                        let next_v = remaining.remove(0);
                        self.kont.push(Kont::CollectMap {
                            pairs,
                            pending_key: Some(val),
                            remaining,
                            span,
                        });
                        self.focus = Focus::Eval(next_v);
                    }
                } else {
                    // We just evaluated a value — store the pair
                    pairs.push((pending_key.unwrap(), val));
                    if remaining.is_empty() {
                        let map: imbl::HashMap<Value, Value> = pairs.into_iter().collect();
                        self.focus = Focus::Return(Value::Map(map));
                    } else {
                        let next_k = remaining.remove(0);
                        self.kont.push(Kont::CollectMap {
                            pairs,
                            pending_key: None,
                            remaining,
                            span,
                        });
                        self.focus = Focus::Eval(next_k);
                    }
                }
            }

            Kont::CollectPerform {
                effect,
                op,
                mut evaluated,
                mut remaining,
                span,
            } => {
                evaluated.push(val);
                if remaining.is_empty() {
                    self.do_perform(&effect, &op, evaluated, span)?;
                } else {
                    let next = remaining.remove(0);
                    self.kont.push(Kont::CollectPerform {
                        effect,
                        op,
                        evaluated,
                        remaining,
                        span,
                    });
                    self.focus = Focus::Eval(next);
                }
            }

            Kont::FnBody { .. } => {
                // Function completed normally — pass value through
                self.focus = Focus::Return(val);
            }

            Kont::Handler { .. } => {
                // Body completed normally — handler not needed, pass value through
                self.focus = Focus::Return(val);
            }

            Kont::AfterHandler {
                captured,
                handler_effect,
                handler_clauses,
            } => {
                // Handler body completed with resume value.
                // Pop the handler scope, splice captured continuation back,
                // re-install the handler for subsequent effects, and resume.
                self.env.pop_scope();

                // Re-install handler (for subsequent effects from the same body)
                self.kont.push(Kont::Handler {
                    effect: handler_effect,
                    clauses: handler_clauses,
                });
                // Splice captured continuation back — execution resumes
                // from where the effect was performed
                self.kont.extend(captured);
                // The resume value becomes the result of the effect call
                self.focus = Focus::Return(val);
            }

            Kont::LoopBody { .. } => {
                // Body completed without recur — exit loop
                self.env.pop_scope();
                self.focus = Focus::Return(val);
            }

            Kont::CollectRecur {
                mut evaluated,
                mut remaining,
                span,
            } => {
                evaluated.push(val);
                if remaining.is_empty() {
                    self.do_recur(evaluated, span)?;
                } else {
                    let next = remaining.remove(0);
                    self.kont.push(Kont::CollectRecur {
                        evaluated,
                        remaining,
                        span,
                    });
                    self.focus = Focus::Eval(next);
                }
            }

            Kont::InitLoop {
                names,
                mut evaluated,
                mut remaining,
                body,
            } => {
                evaluated.push(val);
                if remaining.is_empty() {
                    // All initial values collected — push scope, bind, enter loop
                    self.kont.push(Kont::LoopBody {
                        names: names.clone(),
                        body: body.clone(),
                    });
                    self.env.push_scope();
                    for (name, val) in names.iter().zip(evaluated.iter()) {
                        self.env.set(name.clone(), val.clone());
                    }
                    self.eval_seq(body);
                } else {
                    let next = remaining.remove(0);
                    self.kont.push(Kont::InitLoop {
                        names,
                        evaluated,
                        remaining,
                        body,
                    });
                    self.focus = Focus::Eval(next);
                }
            }

            Kont::SetVar(name) => {
                self.env.set(name, val.clone());
                self.focus = Focus::Return(val);
            }

            Kont::AccessField {
                field,
                full_path: _,
                span,
            } => {
                let result = access_field(&val, &field, span)?;
                self.focus = Focus::Return(result);
            }

            Kont::InspectVal(source) => {
                println!("[inspect] {} = {}", source, val);
                self.focus = Focus::Return(val);
            }

            Kont::MarkPub(name) => {
                if let Some(n) = name {
                    self.env.pub_names.insert(n);
                }
                self.focus = Focus::Return(val);
            }
        }
        Ok(())
    }

    // ── Helpers ────────────────────────────────────────────────────────

    fn eval_seq(&mut self, procs: Vec<Proc>) {
        if procs.is_empty() {
            self.focus = Focus::Return(Value::Unit);
        } else {
            let mut procs = procs;
            let first = procs.remove(0);
            if !procs.is_empty() {
                self.kont.push(Kont::Seq(procs));
            }
            self.focus = Focus::Eval(first);
        }
    }

    fn do_match(&mut self, scrutinee: Value, arms: Vec<MatchArm>) -> Result<(), InterpError> {
        for arm in &arms {
            let mut bindings = HashMap::new();
            if pattern_matches(&arm.pattern, &scrutinee, &mut bindings, &mut self.env)? {
                // Check guard if present
                if let Some(ref guard) = arm.guard {
                    self.env.push_scope();
                    for (k, v) in &bindings {
                        self.env.set(k.clone(), v.clone());
                    }
                    // Need to evaluate guard — but we're in the middle of deliver.
                    // For now, evaluate guard synchronously using the old eval.
                    // TODO: make guard evaluation use the machine too
                    let guard_val = self.run_sub_proc(guard.clone())?;
                    if !guard_val.is_truthy() {
                        self.env.pop_scope();
                        continue;
                    }
                    // Guard passed — evaluate body in this scope
                    // But we need to pop scope after body completes
                    // Push a PopScope-like kont... but we don't have one for match
                    // Actually, match scope cleanup is done by the body eval
                    // Let's just eval the body in the current (extended) scope
                    self.focus = Focus::Eval(arm.body.clone());
                    return Ok(());
                }

                // No guard — bind and eval body
                self.env.push_scope();
                for (k, v) in &bindings {
                    self.env.set(k.clone(), v.clone());
                }
                self.focus = Focus::Eval(arm.body.clone());
                return Ok(());
            }
        }
        // No match — return Unit (Loon convention)
        self.focus = Focus::Return(Value::Unit);
        Ok(())
    }

    fn do_perform(
        &mut self,
        effect: &str,
        op: &str,
        args: Vec<Value>,
        _span: Span,
    ) -> Result<(), InterpError> {
        // Check for a Handler on the kont stack
        let handler_pos = self
            .kont
            .iter()
            .rposition(|k| matches!(k, Kont::Handler { effect: e, .. } if e == effect));

        if let Some(pos) = handler_pos {
            // Capture the continuation between here and the handler
            let captured: Vec<Kont> = self.kont.drain(pos + 1..).collect();
            let handler = self.kont.pop().unwrap();

            if let Kont::Handler {
                effect: handler_effect,
                clauses,
            } = handler
            {
                // Find matching op
                for clause in &clauses {
                    if clause.op == op {
                        // resume is just an identity function — the real magic happens
                        // in AfterHandler which splices the captured continuation back
                        let resume_fn = Value::Builtin(
                            "resume".to_string(),
                            std::sync::Arc::new(|_name, args| {
                                Ok(args.first().cloned().unwrap_or(Value::Unit))
                            }),
                        );

                        // Push AfterHandler — when handler body completes, this
                        // splices the captured continuation and resumes execution
                        self.kont.push(Kont::AfterHandler {
                            captured,
                            handler_effect: handler_effect.clone(),
                            handler_clauses: clauses.clone(),
                        });

                        // Bind handler params and resume, eval handler body
                        self.env.push_scope();
                        for (i, param) in clause.params.iter().enumerate() {
                            let val = args.get(i).cloned().unwrap_or(Value::Unit);
                            self.env.set(param.clone(), val);
                        }
                        self.env.set("resume".to_string(), resume_fn);

                        self.focus = Focus::Eval(clause.body.clone());
                        return Ok(());
                    }
                }
            }
        }

        // No handler — try builtin
        let performed = PerformedEffect {
            effect: effect.to_string(),
            operation: op.to_string(),
            args,
        };
        if let Some(result) = try_builtin_handler(&performed) {
            self.focus = Focus::Return(result?);
            Ok(())
        } else {
            Err(perform_effect(effect, op, performed.args))
        }
    }

    fn do_recur(&mut self, args: Vec<Value>, span: Span) -> Result<(), InterpError> {
        // Walk kont stack to find the nearest LoopBody or FnBody
        let recur_pos = self
            .kont
            .iter()
            .rposition(|k| matches!(k, Kont::LoopBody { .. } | Kont::FnBody { .. }));

        if let Some(pos) = recur_pos {
            let target = self.kont[pos].clone();
            match target {
                Kont::LoopBody { names, body } => {
                    // Discard everything between here and the loop boundary
                    self.kont.truncate(pos + 1);
                    // Pop old scope, push new one with updated bindings
                    self.env.pop_scope();
                    self.env.push_scope();
                    for (name, val) in names.iter().zip(args.iter()) {
                        self.env.set(name.clone(), val.clone());
                    }
                    self.eval_seq(body);
                    Ok(())
                }
                Kont::FnBody { params, body } => {
                    // fn/recur: rebind params, re-evaluate body
                    self.kont.truncate(pos + 1);
                    // Pop old scope, push new one with rebound params
                    self.env.pop_scope();
                    self.env.push_scope();
                    let has_rest = params.last().is_some_and(|p| matches!(p, Param::Rest(_)));
                    let required = if has_rest {
                        params.len() - 1
                    } else {
                        params.len()
                    };
                    for (param, val) in params[..required].iter().zip(args[..required].iter()) {
                        bind_param(param, val, &mut self.env)?;
                    }
                    if has_rest {
                        if let Some(Param::Rest(name)) = params.last() {
                            let rest_vals: imbl::Vector<Value> =
                                args[required..].iter().cloned().collect();
                            self.env.set(name.clone(), Value::Vec(rest_vals));
                        }
                    }
                    let body_procs: Vec<Proc> = body.iter().map(analyze).collect();
                    self.eval_seq(body_procs);
                    Ok(())
                }
                _ => unreachable!(),
            }
        } else {
            Err(err_at("recur outside of loop or fn".to_string(), span))
        }
    }

    /// Run a sub-computation (used for match guards, etc.)
    fn run_sub_proc(&mut self, proc: Proc) -> IResult {
        let saved_kont = std::mem::take(&mut self.kont);
        let saved_focus = std::mem::replace(&mut self.focus, Focus::Eval(proc));
        let result = self.run();
        self.kont = saved_kont;
        self.focus = saved_focus;
        result
    }
}

// ─── Entry point ───────────────────────────────────────────────────────────

/// Evaluate a program using the SICP-style effect machine.
pub fn eval_program_vm(exprs: &[Expr]) -> IResult {
    eval_program_vm_with_base_dir(exprs, None)
}

pub fn eval_program_vm_with_base_dir(
    exprs: &[Expr],
    base_dir: Option<&std::path::Path>,
) -> IResult {
    use crate::interp::register_builtins;

    // Macro expansion phase (same as old interpreter)
    let mut expander = crate::macros::MacroExpander::new();
    let exprs = expander.expand_program(exprs).map_err(err)?;

    let mut env = Env::new();
    register_builtins(&mut env);

    // Load prelude
    if let Ok(prelude_exprs) = crate::parser::parse(crate::prelude::PRELUDE) {
        let mut machine = Machine::new(env.clone());
        for expr in &prelude_exprs {
            let proc = analyze(expr);
            let _ = machine.run_proc(proc);
        }
        env = machine.env;
    }

    // Load manifest for grants/modules
    let default_base = std::path::PathBuf::from(".");
    let base = base_dir.unwrap_or(&default_base);
    let _cache = match crate::pkg::Manifest::load(base) {
        Ok(Some(manifest)) => {
            let grants = crate::pkg::capability::grants_from_manifest(&manifest.deps);
            crate::interp::set_effect_grants(grants);
            let lockfile = crate::pkg::lockfile::Lockfile::load(base).ok().flatten();
            crate::module::ModuleCache::with_manifest_and_lockfile(
                manifest,
                lockfile,
                base.to_path_buf(),
            )
        }
        _ => crate::module::ModuleCache::new(),
    };
    crate::interp::set_current_module(None);
    crate::interp::sync_global_env(&env);

    // Analyze all expressions
    let procs: Vec<Proc> = exprs.iter().map(|e| analyze(e)).collect();

    // Evaluate
    let mut machine = Machine::new(env);
    let mut last = Value::Unit;

    for proc in procs {
        match machine.run_proc(proc) {
            Ok(val) => last = val,
            Err(e) => {
                if let Some(ref performed) = e.performed_effect {
                    if let Some(result) = try_builtin_handler(performed) {
                        last = result?;
                    } else {
                        return Err(e);
                    }
                } else {
                    return Err(e);
                }
            }
        }
    }

    crate::interp::sync_global_env(&machine.env);

    // Call main if it exists
    if let Some(Value::Fn(main_fn)) = machine.env.get("main") {
        let proc = Proc::Apply {
            func: Box::new(Proc::Const(Value::Fn(main_fn), Span::ZERO)),
            args: Vec::new(),
            span: Span::ZERO,
        };
        match machine.run_proc(proc) {
            Ok(val) => return Ok(val),
            Err(e) => {
                if let Some(ref performed) = e.performed_effect {
                    if let Some(result) = try_builtin_handler(performed) {
                        return result;
                    }
                }
                return Err(e);
            }
        }
    }

    Ok(last)
}
