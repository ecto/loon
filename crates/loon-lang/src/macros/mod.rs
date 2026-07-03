pub mod compile_builtins;

use crate::ast::{Expr, ExprKind, NodeId};
use crate::interp;
use crate::syntax::Span;

use std::collections::{HashMap, HashSet};

// ── Data Structures ──────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct MacroDef {
    pub name: String,
    pub params: Vec<MacroParam>,
    pub body: Expr,
    pub style: MacroStyle,
    pub is_type_aware: bool,
    pub compile_effects: HashSet<CompileEffect>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum MacroParam {
    Named(String),
    Rest(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MacroStyle {
    Template,
    Procedural,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompileEffect {
    IO,
    Net,
    Env,
    Print,
}

impl CompileEffect {
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "IO" => Some(CompileEffect::IO),
            "Net" => Some(CompileEffect::Net),
            "Env" => Some(CompileEffect::Env),
            "Print" => Some(CompileEffect::Print),
            _ => None,
        }
    }
}

// ── Expansion Trace ──────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct ExpansionTrace {
    pub steps: Vec<ExpansionStep>,
}

#[derive(Debug, Clone)]
pub struct ExpansionStep {
    pub macro_name: String,
    pub invocation_span: Span,
    pub definition_span: Span,
}

// ── Macro Expander ───────────────────────────────────────────────────

pub struct MacroExpander {
    macros: HashMap<String, MacroDef>,
    type_aware_macros: HashMap<String, MacroDef>,
    gensym_counter: u32,
    pub expansion_traces: HashMap<u32, ExpansionTrace>,
}

impl Default for MacroExpander {
    fn default() -> Self {
        Self::new()
    }
}

impl MacroExpander {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            type_aware_macros: HashMap::new(),
            gensym_counter: 0,
            expansion_traces: HashMap::new(),
        }
    }

    fn gensym(&mut self, name: &str) -> String {
        self.gensym_counter += 1;
        format!("__gensym_{name}_{}", self.gensym_counter)
    }

    /// Register a macro definition.
    fn register_macro(&mut self, def: MacroDef) {
        if def.is_type_aware {
            self.type_aware_macros.insert(def.name.clone(), def);
        } else {
            self.macros.insert(def.name.clone(), def);
        }
    }

    /// Expand all macros in a program (first phase: regular macros only).
    pub fn expand_program(&mut self, exprs: &[Expr]) -> Result<Vec<Expr>, String> {
        let mut result = Vec::new();
        for expr in exprs {
            if let Some(expanded) = self.expand_toplevel(expr)? {
                result.push(expanded);
            }
        }
        Ok(result)
    }

    /// Expand type-aware macros (second phase, after type checking).
    pub fn expand_type_aware(&mut self, exprs: &[Expr]) -> Result<Vec<Expr>, String> {
        let mut result = Vec::new();
        for expr in exprs {
            result.push(self.expand_type_aware_expr(expr)?);
        }
        Ok(result)
    }

    fn expand_toplevel(&mut self, expr: &Expr) -> Result<Option<Expr>, String> {
        // Check for macro / macro+ forms
        if let ExprKind::List(items) = &expr.kind {
            if !items.is_empty() {
                if let ExprKind::Symbol(s) = &items[0].kind {
                    if s == "macro" || s == "macro+" {
                        let is_type_aware = s == "macro+";
                        self.parse_and_register_macro(&items[1..], is_type_aware, expr.span)?;
                        return Ok(None); // consumed
                    }
                    if s == "macroexpand" {
                        // [macroexpand expr] — expand and return as string
                        if items.len() >= 2 {
                            let expanded = self.expand_expr(&items[1])?;
                            return Ok(Some(Expr::new(
                                ExprKind::Str(format!("{expanded}")),
                                expr.span,
                            )));
                        }
                    }
                }
            }
        }
        let expanded = self.expand_expr(expr)?;
        Ok(Some(expanded))
    }

    fn expand_expr(&mut self, expr: &Expr) -> Result<Expr, String> {
        match &expr.kind {
            ExprKind::List(items) if !items.is_empty() => {
                // Check if head is a known macro
                if let ExprKind::Symbol(name) = &items[0].kind {
                    // Skip macro forms (already handled at toplevel)
                    if name == "macro" || name == "macro+" {
                        return Ok(expr.clone());
                    }

                    if let Some(mac) = self.macros.get(name).cloned() {
                        let expanded = self.expand_invocation(&mac, &items[1..], expr.span)?;
                        let trace = ExpansionTrace {
                            steps: vec![ExpansionStep {
                                macro_name: mac.name.clone(),
                                invocation_span: expr.span,
                                definition_span: mac.span,
                            }],
                        };
                        // Record on intermediate node
                        self.expansion_traces.insert(expanded.id.0, trace.clone());
                        // Recursively expand (macros may produce macro calls)
                        let result = self.expand_expr(&expanded)?;
                        // Record on final result too
                        self.expansion_traces.insert(result.id.0, trace);
                        return Ok(result);
                    }

                    // Core desugar: `[and ...]`/`[or ...]` in call position become
                    // nested `[if ...]`, so they SHORT-CIRCUIT on every backend
                    // (interp, EIR VM, wasm codegen all consume expanded programs).
                    // Value semantics match the eager builtins exactly: return the
                    // first falsy (and) / truthy (or) operand, else the last one;
                    // `[and]` is true, `[or]` is false. Each operand is evaluated
                    // at most once (bound to a gensym temp).
                    //
                    // Consequences (documented in ARCHITECTURE.md):
                    // - Call-position `and`/`or` are effectively SPECIAL FORMS
                    //   now: the rewrite happens before any scope information
                    //   exists, so a local binding named `and`/`or` cannot
                    //   shadow them at call sites.
                    // - A bare `and`/`or` in value position is untouched. On
                    //   the interpreter it resolves to the eager variadic env
                    //   builtin; the EIR VM and wasm have no callable builtin
                    //   values (pre-existing), so it fails there — pinned by
                    //   the `and-or-value.oo` conformance program.
                    if name == "and" || name == "or" {
                        let is_and = name == "and";
                        let operands: Result<Vec<_>, _> =
                            items[1..].iter().map(|e| self.expand_expr(e)).collect();
                        return Ok(self.desugar_and_or(is_and, &operands?, expr.span));
                    }

                    // `pipe` steps get special treatment: an `[and ...]`/`[or ...]`
                    // STEP is thread-last partial application ([pipe v [or 7]] ≡
                    // [or 7 v]), so desugaring it in place would corrupt the step
                    // shape (a one-operand step collapses to a bare value, a
                    // multi-operand one becomes a [do ...] the pipe would try to
                    // call). Rewrite such steps into a unary lambda wrapping the
                    // short-circuit desugar of [op operands... piped-value].
                    if name == "pipe" {
                        return self.expand_pipe(items, expr.span);
                    }

                    // Core desugar: `[if-let [x expr] then else?]` and
                    // `[when-let [x expr] body...]`. Like and/or these are
                    // expanded here (before any backend sees them) so all
                    // backends agree; a user macro of the same name takes
                    // precedence via the lookup above.
                    if name == "if-let" || name == "when-let" {
                        return self.desugar_if_let(name == "if-let", &items[1..], expr.span);
                    }
                }
                // Not a macro call — recursively expand children
                let expanded_items: Result<Vec<_>, _> =
                    items.iter().map(|e| self.expand_expr(e)).collect();
                Ok(Expr::new(ExprKind::List(expanded_items?), expr.span))
            }
            ExprKind::Vec(items) => {
                let expanded: Result<Vec<_>, _> =
                    items.iter().map(|e| self.expand_expr(e)).collect();
                Ok(Expr::new(ExprKind::Vec(expanded?), expr.span))
            }
            ExprKind::Set(items) => {
                let expanded: Result<Vec<_>, _> =
                    items.iter().map(|e| self.expand_expr(e)).collect();
                Ok(Expr::new(ExprKind::Set(expanded?), expr.span))
            }
            ExprKind::Map(pairs) => {
                let expanded: Result<Vec<(Expr, Expr)>, String> = pairs
                    .iter()
                    .map(|(k, v)| Ok((self.expand_expr(k)?, self.expand_expr(v)?)))
                    .collect();
                Ok(Expr::new(ExprKind::Map(expanded?), expr.span))
            }
            ExprKind::Tuple(items) => {
                let expanded: Result<Vec<_>, _> =
                    items.iter().map(|e| self.expand_expr(e)).collect();
                Ok(Expr::new(ExprKind::Tuple(expanded?), expr.span))
            }
            // Atoms and other nodes pass through
            _ => Ok(expr.clone()),
        }
    }

    /// Desugar `[and a b ...]` / `[or a b ...]` (operands already expanded)
    /// into nested `[do [let g a] [if g ... ...]]` so evaluation stops at the
    /// deciding operand. The temp binding guarantees single evaluation while
    /// still returning the operand's VALUE (not a coerced bool).
    fn desugar_and_or(&mut self, is_and: bool, operands: &[Expr], span: Span) -> Expr {
        match operands {
            // [and] → true, [or] → false (identity elements, as the builtins).
            [] => Expr::new(ExprKind::Bool(is_and), span),
            [only] => only.clone(),
            [first, rest @ ..] => {
                let tmp = self.gensym(if is_and { "and" } else { "or" });
                let sym = |s: &str| Expr::new(ExprKind::Symbol(s.to_string()), span);
                let rest_expr = self.desugar_and_or(is_and, rest, span);
                let let_form = Expr::new(
                    ExprKind::List(vec![sym("let"), sym(&tmp), first.clone()]),
                    span,
                );
                let (then_e, else_e) = if is_and {
                    (rest_expr, sym(&tmp))
                } else {
                    (sym(&tmp), rest_expr)
                };
                let if_form = Expr::new(
                    ExprKind::List(vec![sym("if"), sym(&tmp), then_e, else_e]),
                    span,
                );
                Expr::new(ExprKind::List(vec![sym("do"), let_form, if_form]), span)
            }
        }
    }

    /// Desugar `[if-let [x expr] then else?]` (and `[when-let [x expr]
    /// body...]`, which is if-let with an implicit-do body and no else).
    ///
    /// Semantics: `expr` is evaluated ONCE. If it yields `[Some v]`, `x` is
    /// bound to the payload `v` and `then` runs. If it yields a falsy value
    /// (`None`, `false`, `()` — the whole falsy set), `else` runs (or nothing
    /// for when-let). Any OTHER truthy value binds `x` to the value itself
    /// and runs `then` — so if-let works over both Option-returning and
    /// plain-truthy expressions. Note the payload's truthiness is irrelevant:
    /// `[Some false]` takes the then-branch with `x` bound to `false`.
    ///
    /// Expansion (g, v are gensyms; hygienic against user code):
    ///   [do [let g expr]
    ///       [if g
    ///           [do [let x [match g [Some v] v _ g]] then]
    ///           else]]
    fn desugar_if_let(&mut self, is_if_let: bool, args: &[Expr], span: Span) -> Result<Expr, String> {
        let form = if is_if_let { "if-let" } else { "when-let" };
        let binding = match args.first().map(|b| &b.kind) {
            Some(ExprKind::List(pair)) if pair.len() == 2 => pair,
            _ => {
                return Err(format!(
                    "{form} expects a binding pair: [{form} [x expr] ...] — \
                     the first argument must be a two-element [name expr] form"
                ))
            }
        };
        let name = match &binding[0].kind {
            ExprKind::Symbol(s) if !s.starts_with(char::is_uppercase) && !s.starts_with(':') => {
                s.clone()
            }
            _ => {
                return Err(format!(
                    "{form} binding must start with a lowercase variable name: [{form} [x expr] ...]"
                ))
            }
        };
        if is_if_let && !(2..=3).contains(&args.len()) {
            return Err("if-let expects [if-let [x expr] then else?]".to_string());
        }
        if !is_if_let && args.len() < 2 {
            return Err("when-let expects [when-let [x expr] body...]".to_string());
        }

        let value = self.expand_expr(&binding[1])?;
        let then_e = if is_if_let {
            self.expand_expr(&args[1])?
        } else {
            // when-let: implicit do over the body forms
            let mut body = vec![Expr::new(ExprKind::Symbol("do".to_string()), span)];
            for b in &args[1..] {
                body.push(self.expand_expr(b)?);
            }
            Expr::new(ExprKind::List(body), span)
        };
        let else_e = if is_if_let { args.get(2).cloned() } else { None };
        let else_e = match else_e {
            Some(e) => Some(self.expand_expr(&e)?),
            None => None,
        };

        let sym = |s: &str| Expr::new(ExprKind::Symbol(s.to_string()), span);
        let g = self.gensym(form);
        let v = self.gensym("payload");
        // [match g [Some v] v _ g] — unwrap a Some, pass anything else through.
        let unwrap = Expr::new(
            ExprKind::List(vec![
                sym("match"),
                sym(&g),
                Expr::new(ExprKind::List(vec![sym("Some"), sym(&v)]), span),
                sym(&v),
                sym("_"),
                sym(&g),
            ]),
            span,
        );
        let bind_x = Expr::new(ExprKind::List(vec![sym("let"), sym(&name), unwrap]), span);
        let then_do = Expr::new(ExprKind::List(vec![sym("do"), bind_x, then_e]), span);
        let mut if_form = vec![sym("if"), sym(&g), then_do];
        if let Some(e) = else_e {
            if_form.push(e);
        }
        let if_form = Expr::new(ExprKind::List(if_form), span);
        let bind_g = Expr::new(ExprKind::List(vec![sym("let"), sym(&g), value]), span);
        Ok(Expr::new(
            ExprKind::List(vec![sym("do"), bind_g, if_form]),
            span,
        ))
    }

    /// Expand a `[pipe seed step...]` form. Steps expand normally except
    /// `[and ...]`/`[or ...]` steps, which become `[[fn [g] <desugar of
    /// [op operands... g]>]]` — a single-element list whose head is a unary
    /// lambda, the pipe-step shape every backend calls with the piped value.
    /// This preserves both the thread-last value semantics the eager builtins
    /// had ([pipe v [or a]] ≡ [or a v]) and short-circuit evaluation.
    fn expand_pipe(&mut self, items: &[Expr], span: Span) -> Result<Expr, String> {
        let mut out = Vec::with_capacity(items.len());
        out.push(items[0].clone()); // the `pipe` symbol itself
        for (i, arg) in items[1..].iter().enumerate() {
            let is_step = i > 0; // items[1] is the seed value, not a step
            let and_or = match &arg.kind {
                ExprKind::List(sub) if is_step && !sub.is_empty() => match &sub[0].kind {
                    ExprKind::Symbol(s) if s == "and" || s == "or" => Some((s == "and", sub)),
                    _ => None,
                },
                _ => None,
            };
            match and_or {
                Some((is_and, sub)) => {
                    let operands: Result<Vec<_>, _> =
                        sub[1..].iter().map(|e| self.expand_expr(e)).collect();
                    let mut operands = operands?;
                    let g = self.gensym(if is_and { "and" } else { "or" });
                    let gsym = Expr::new(ExprKind::Symbol(g), arg.span);
                    operands.push(gsym.clone());
                    let body = self.desugar_and_or(is_and, &operands, arg.span);
                    let lambda = Expr::new(
                        ExprKind::List(vec![
                            Expr::new(ExprKind::Symbol("fn".to_string()), arg.span),
                            Expr::new(ExprKind::List(vec![gsym]), arg.span),
                            body,
                        ]),
                        arg.span,
                    );
                    out.push(Expr::new(ExprKind::List(vec![lambda]), arg.span));
                }
                None => out.push(self.expand_expr(arg)?),
            }
        }
        Ok(Expr::new(ExprKind::List(out), span))
    }

    fn expand_type_aware_expr(&mut self, expr: &Expr) -> Result<Expr, String> {
        match &expr.kind {
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(name) = &items[0].kind {
                    if let Some(mac) = self.type_aware_macros.get(name).cloned() {
                        let expanded = self.expand_invocation(&mac, &items[1..], expr.span)?;
                        self.expansion_traces.insert(
                            expanded.id.0,
                            ExpansionTrace {
                                steps: vec![ExpansionStep {
                                    macro_name: mac.name.clone(),
                                    invocation_span: expr.span,
                                    definition_span: mac.span,
                                }],
                            },
                        );
                        return self.expand_type_aware_expr(&expanded);
                    }
                }
                let expanded_items: Result<Vec<_>, _> = items
                    .iter()
                    .map(|e| self.expand_type_aware_expr(e))
                    .collect();
                Ok(Expr::new(ExprKind::List(expanded_items?), expr.span))
            }
            _ => Ok(expr.clone()),
        }
    }

    // ── Macro Parsing ────────────────────────────────────────────────

    fn parse_and_register_macro(
        &mut self,
        args: &[Expr],
        is_type_aware: bool,
        span: Span,
    ) -> Result<(), String> {
        // [macro name [params] body]
        // [macro name [params] #{Effects} body]
        if args.len() < 2 {
            return Err("macro requires a name and body".to_string());
        }
        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err("macro name must be a symbol".to_string()),
        };
        let params = self.parse_macro_params(&args[1])?;

        // Check for effect annotation: #{Effects}
        let mut compile_effects = HashSet::new();
        let mut body_start = 2;
        if body_start < args.len() {
            if matches!(&args[body_start].kind, ExprKind::Set(_) | ExprKind::Map(_)) {
                self.parse_compile_effects(&args[body_start], &mut compile_effects)?;
                body_start += 1;
            }
        }

        if body_start >= args.len() {
            return Err(format!("macro '{name}' missing body"));
        }
        let body = args[body_start].clone();

        let style = if matches!(body.kind, ExprKind::Quote(_)) {
            MacroStyle::Template
        } else {
            MacroStyle::Procedural
        };

        self.register_macro(MacroDef {
            name,
            params,
            body,
            style,
            is_type_aware,
            compile_effects,
            span,
        });
        Ok(())
    }

    fn parse_macro_params(&self, expr: &Expr) -> Result<Vec<MacroParam>, String> {
        match &expr.kind {
            ExprKind::List(items) => {
                let mut params = Vec::new();
                let mut i = 0;
                while i < items.len() {
                    if let ExprKind::Symbol(s) = &items[i].kind {
                        if s == "&" {
                            if i + 1 < items.len() {
                                if let ExprKind::Symbol(rest) = &items[i + 1].kind {
                                    params.push(MacroParam::Rest(rest.clone()));
                                    i += 2;
                                    continue;
                                }
                            }
                            return Err("& must be followed by a parameter name".to_string());
                        }
                        params.push(MacroParam::Named(s.clone()));
                    } else {
                        return Err("macro parameter must be a symbol".to_string());
                    }
                    i += 1;
                }
                Ok(params)
            }
            _ => Err("macro params must be a list".to_string()),
        }
    }

    fn parse_compile_effects(
        &self,
        expr: &Expr,
        effects: &mut HashSet<CompileEffect>,
    ) -> Result<(), String> {
        match &expr.kind {
            ExprKind::Set(items) => {
                for item in items {
                    if let ExprKind::Symbol(s) = &item.kind {
                        if let Some(eff) = CompileEffect::from_name(s) {
                            effects.insert(eff);
                        } else {
                            return Err(format!("unknown compile-time effect: {s}"));
                        }
                    }
                }
                Ok(())
            }
            _ => Err("compile-time effects must be a set like #{IO Net}".to_string()),
        }
    }

    // ── Macro Invocation ─────────────────────────────────────────────

    fn expand_invocation(
        &mut self,
        mac: &MacroDef,
        args: &[Expr],
        call_span: Span,
    ) -> Result<Expr, String> {
        // Bind arguments to parameters
        let bindings = self.bind_args(&mac.params, args, &mac.name)?;

        match mac.style {
            MacroStyle::Template => self.expand_template(&mac.body, &bindings, call_span),
            MacroStyle::Procedural => self.expand_procedural(mac, &bindings, call_span),
        }
    }

    fn bind_args(
        &self,
        params: &[MacroParam],
        args: &[Expr],
        macro_name: &str,
    ) -> Result<HashMap<String, Vec<Expr>>, String> {
        let mut bindings = HashMap::new();
        let mut arg_idx = 0;

        for param in params {
            match param {
                MacroParam::Named(name) => {
                    if arg_idx >= args.len() {
                        return Err(format!(
                            "macro '{macro_name}' expected argument '{name}', got {} args",
                            args.len()
                        ));
                    }
                    bindings.insert(name.clone(), vec![args[arg_idx].clone()]);
                    arg_idx += 1;
                }
                MacroParam::Rest(name) => {
                    bindings.insert(name.clone(), args[arg_idx..].to_vec());
                    arg_idx = args.len();
                }
            }
        }
        Ok(bindings)
    }

    // ── Template Expansion ───────────────────────────────────────────

    fn expand_template(
        &mut self,
        body: &Expr,
        bindings: &HashMap<String, Vec<Expr>>,
        call_span: Span,
    ) -> Result<Expr, String> {
        match &body.kind {
            ExprKind::Quote(inner) => {
                // Hygiene: binders the TEMPLATE ITSELF introduces (`let` names,
                // `fn`/`loop` parameters) are renamed to fresh gensyms, one per
                // expansion, so they can neither capture a user binding passed
                // in as an argument nor clobber a caller binding of the same
                // name. Names bound to macro parameters are skipped (they are
                // caller code), as are named-fn definition names (a macro that
                // expands to [fn helper ...] INTENDS to introduce `helper`).
                let mut binders = Vec::new();
                collect_template_binders(inner, bindings, &mut binders);
                let renames: HashMap<String, String> = binders
                    .into_iter()
                    .map(|b| {
                        let g = self.gensym(&b);
                        (b, g)
                    })
                    .collect();
                self.substitute(inner, bindings, &renames, call_span)
            }
            _ => Err("template macro body must be a quasiquoted expression".to_string()),
        }
    }

    fn substitute(
        &mut self,
        expr: &Expr,
        bindings: &HashMap<String, Vec<Expr>>,
        renames: &HashMap<String, String>,
        span: Span,
    ) -> Result<Expr, String> {
        match &expr.kind {
            ExprKind::Unquote(inner) => {
                // ~name → substitute the binding
                if let ExprKind::Symbol(name) = &inner.kind {
                    if let Some(vals) = bindings.get(name) {
                        if vals.len() == 1 {
                            return Ok(vals[0].clone());
                        }
                        // Multiple values — wrap in a list (shouldn't happen for Named params)
                        return Ok(Expr::new(ExprKind::List(vals.clone()), span));
                    }
                }
                // Not a bound name — evaluate as-is (could be a computed unquote)
                Ok((**inner).clone())
            }
            ExprKind::UnquoteSplice(_) => {
                Err("~@ (unquote-splice) can only appear inside a list".to_string())
            }
            ExprKind::List(items) => {
                let mut result = Vec::new();
                for item in items {
                    if let ExprKind::UnquoteSplice(inner) = &item.kind {
                        // ~@name → splice all elements
                        if let ExprKind::Symbol(name) = &inner.kind {
                            if let Some(vals) = bindings.get(name) {
                                result.extend(vals.iter().cloned());
                                continue;
                            }
                        }
                        return Err("~@ requires a bound rest parameter".to_string());
                    }
                    result.push(self.substitute(item, bindings, renames, span)?);
                }
                Ok(Expr::new(ExprKind::List(result), span))
            }
            ExprKind::Vec(items) => {
                let mut result = Vec::new();
                for item in items {
                    if let ExprKind::UnquoteSplice(inner) = &item.kind {
                        if let ExprKind::Symbol(name) = &inner.kind {
                            if let Some(vals) = bindings.get(name) {
                                result.extend(vals.iter().cloned());
                                continue;
                            }
                        }
                        return Err("~@ requires a bound rest parameter".to_string());
                    }
                    result.push(self.substitute(item, bindings, renames, span)?);
                }
                Ok(Expr::new(ExprKind::Vec(result), span))
            }
            ExprKind::Symbol(name) => {
                // Bare symbol inside template — check if it's a binding
                if let Some(vals) = bindings.get(name) {
                    if vals.len() == 1 {
                        return Ok(vals[0].clone());
                    }
                }
                // Hygiene: a template-introduced binder is renamed to its
                // gensym everywhere in the template (binder and references).
                if let Some(renamed) = renames.get(name) {
                    return Ok(Expr::new(ExprKind::Symbol(renamed.clone()), span));
                }
                Ok(expr.clone())
            }
            ExprKind::Map(pairs) => {
                let expanded: Result<Vec<(Expr, Expr)>, String> = pairs
                    .iter()
                    .map(|(k, v)| {
                        Ok((
                            self.substitute(k, bindings, renames, span)?,
                            self.substitute(v, bindings, renames, span)?,
                        ))
                    })
                    .collect();
                Ok(Expr::new(ExprKind::Map(expanded?), span))
            }
            ExprKind::Tuple(items) => {
                let expanded: Result<Vec<_>, _> = items
                    .iter()
                    .map(|e| self.substitute(e, bindings, renames, span))
                    .collect();
                Ok(Expr::new(ExprKind::Tuple(expanded?), span))
            }
            ExprKind::Set(items) => {
                let expanded: Result<Vec<_>, _> = items
                    .iter()
                    .map(|e| self.substitute(e, bindings, renames, span))
                    .collect();
                Ok(Expr::new(ExprKind::Set(expanded?), span))
            }
            // Nested quotes — don't substitute inside
            ExprKind::Quote(_) => Ok(expr.clone()),
            // Literals pass through
            _ => Ok(expr.clone()),
        }
    }

    // ── Procedural Expansion ─────────────────────────────────────────

    fn expand_procedural(
        &mut self,
        mac: &MacroDef,
        bindings: &HashMap<String, Vec<Expr>>,
        call_span: Span,
    ) -> Result<Expr, String> {
        // Build a mini program that evaluates the macro body with bindings
        // The approach: create let bindings for each macro param, then evaluate body
        let mut program = Vec::new();

        // Register AST builtins
        program.push(make_ast_builtins_prelude(call_span));

        // Bind macro arguments as AST values
        for (name, vals) in bindings {
            let ast_val = if vals.len() == 1 {
                expr_to_ast_value(&vals[0], call_span)
            } else {
                // Rest param → vector of AST values
                let items: Vec<Expr> = vals
                    .iter()
                    .map(|v| expr_to_ast_value(v, call_span))
                    .collect();
                Expr::new(ExprKind::Vec(items), call_span)
            };
            program.push(Expr::new(
                ExprKind::List(vec![
                    Expr::new(ExprKind::Symbol("let".to_string()), call_span),
                    Expr::new(ExprKind::Symbol(name.clone()), call_span),
                    ast_val,
                ]),
                call_span,
            ));
        }

        // Add the macro body
        program.push(mac.body.clone());

        // Enter the compile-time effect sandbox. A procedural macro body runs
        // arbitrary Loon code via the interpreter at *build time*; without this
        // a malicious dependency macro could read ~/.ssh, spawn processes, or
        // exfiltrate over the network during `loon build`. Default-deny: only
        // the effect categories the macro explicitly declared via `#{...}` are
        // permitted. Save/restore the prior value so nested procedural macro
        // expansion composes correctly.
        let allowed: std::collections::HashSet<String> = mac
            .compile_effects
            .iter()
            .map(|e| {
                match e {
                    CompileEffect::IO => "IO",
                    CompileEffect::Net => "Net",
                    CompileEffect::Env => "Env",
                    CompileEffect::Print => "Print",
                }
                .to_string()
            })
            .collect();
        let prev_sandbox = interp::swap_compile_sandbox(Some(allowed));

        // Evaluate the program using the interpreter
        let eval_result = interp::eval_program(&program);

        interp::swap_compile_sandbox(prev_sandbox);

        let result = eval_result
            .map_err(|e| format!("procedural macro '{}' failed: {}", mac.name, e.message))?;

        // Convert the result value back to an AST Expr
        ast_value_to_expr(&result, call_span)
    }

    /// Check if a name is a registered macro.
    pub fn is_macro(&self, name: &str) -> bool {
        self.macros.contains_key(name)
    }

    /// Check if a name is a registered type-aware macro.
    pub fn is_type_aware_macro(&self, name: &str) -> bool {
        self.type_aware_macros.contains_key(name)
    }

    /// Check if any type-aware macros were registered.
    pub fn has_type_aware_macros(&self) -> bool {
        !self.type_aware_macros.is_empty()
    }

    /// Get expansion trace for a node, if any.
    pub fn get_trace(&self, node_id: NodeId) -> Option<&ExpansionTrace> {
        self.expansion_traces.get(&node_id.0)
    }
}

// ── AST Value Representation ─────────────────────────────────────────
//
// Procedural macros work with AST-as-values. An AST node is represented as:
// - {:kind :symbol :name "foo"}
// - {:kind :int :value 42}
// - {:kind :list :items #[...]}
// - etc.

/// Collect names a quasiquoted TEMPLATE binds itself: `[let name ...]`
/// binding names, `fn` parameter names (anonymous and named), and `loop`
/// binding names. These are the hygiene-sensitive binders that get gensym'd
/// per expansion. Skipped on purpose:
/// - names bound to macro PARAMETERS (they substitute to caller code),
/// - unquoted binders like `[let ~name ...]` (caller-chosen names),
/// - named-fn definition names (`[fn helper ...]` deliberately introduces
///   `helper` at the expansion site),
/// - `match` pattern variables (arm-scoped; left alone for now).
fn collect_template_binders(
    expr: &Expr,
    params: &HashMap<String, Vec<Expr>>,
    out: &mut Vec<String>,
) {
    let push = |name: &str, params: &HashMap<String, Vec<Expr>>, out: &mut Vec<String>| {
        if !params.contains_key(name) && !out.iter().any(|n| n == name) {
            out.push(name.to_string());
        }
    };
    match &expr.kind {
        ExprKind::List(items) if !items.is_empty() => {
            if let ExprKind::Symbol(head) = &items[0].kind {
                match head.as_str() {
                    "let" => {
                        // [let name value] / [let mut name value]. A malformed
                        // bare `[let]` has no binder — items.get keeps this
                        // total (it used to index out of bounds and panic).
                        let ni = if matches!(items.get(1).map(|e| &e.kind), Some(ExprKind::Symbol(s)) if s == "mut")
                        {
                            2
                        } else {
                            1
                        };
                        if let Some(ExprKind::Symbol(name)) = items.get(ni).map(|e| &e.kind) {
                            push(name, params, out);
                        }
                    }
                    "fn" => {
                        // [fn [params] body...] or [fn name [params] body...]
                        let pi = if matches!(
                            &items.get(1).map(|e| &e.kind),
                            Some(ExprKind::Symbol(_))
                        ) {
                            2
                        } else {
                            1
                        };
                        if let Some(ExprKind::List(ps)) = items.get(pi).map(|e| &e.kind) {
                            for p in ps {
                                if let ExprKind::Symbol(pn) = &p.kind {
                                    if pn != "&" {
                                        push(pn, params, out);
                                    }
                                }
                            }
                        }
                    }
                    "loop" => {
                        // [loop [n1 v1 n2 v2 ...] body...]
                        if let Some(ExprKind::List(bs)) = items.get(1).map(|e| &e.kind) {
                            for pair in bs.chunks(2) {
                                if let Some(ExprKind::Symbol(bn)) = pair.first().map(|e| &e.kind) {
                                    push(bn, params, out);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
            for item in items {
                collect_template_binders(item, params, out);
            }
        }
        ExprKind::Vec(items) | ExprKind::Set(items) | ExprKind::Tuple(items) => {
            for item in items {
                collect_template_binders(item, params, out);
            }
        }
        ExprKind::Map(pairs) => {
            for (k, v) in pairs {
                collect_template_binders(k, params, out);
                collect_template_binders(v, params, out);
            }
        }
        // Nested quotes are not substituted, so don't rename inside them.
        _ => {}
    }
}

fn expr_to_ast_value(expr: &Expr, span: Span) -> Expr {
    match &expr.kind {
        ExprKind::Symbol(s) => Expr::new(
            ExprKind::Map(vec![
                (
                    Expr::new(ExprKind::Keyword("kind".to_string()), span),
                    Expr::new(ExprKind::Keyword("symbol".to_string()), span),
                ),
                (
                    Expr::new(ExprKind::Keyword("name".to_string()), span),
                    Expr::new(ExprKind::Str(s.clone()), span),
                ),
            ]),
            span,
        ),
        ExprKind::Int(n) => Expr::new(
            ExprKind::Map(vec![
                (
                    Expr::new(ExprKind::Keyword("kind".to_string()), span),
                    Expr::new(ExprKind::Keyword("int".to_string()), span),
                ),
                (
                    Expr::new(ExprKind::Keyword("value".to_string()), span),
                    Expr::new(ExprKind::Int(*n), span),
                ),
            ]),
            span,
        ),
        ExprKind::Str(s) => Expr::new(
            ExprKind::Map(vec![
                (
                    Expr::new(ExprKind::Keyword("kind".to_string()), span),
                    Expr::new(ExprKind::Keyword("str".to_string()), span),
                ),
                (
                    Expr::new(ExprKind::Keyword("value".to_string()), span),
                    Expr::new(ExprKind::Str(s.clone()), span),
                ),
            ]),
            span,
        ),
        ExprKind::Bool(b) => Expr::new(
            ExprKind::Map(vec![
                (
                    Expr::new(ExprKind::Keyword("kind".to_string()), span),
                    Expr::new(ExprKind::Keyword("bool".to_string()), span),
                ),
                (
                    Expr::new(ExprKind::Keyword("value".to_string()), span),
                    Expr::new(ExprKind::Bool(*b), span),
                ),
            ]),
            span,
        ),
        ExprKind::Keyword(k) => Expr::new(
            ExprKind::Map(vec![
                (
                    Expr::new(ExprKind::Keyword("kind".to_string()), span),
                    Expr::new(ExprKind::Keyword("keyword".to_string()), span),
                ),
                (
                    Expr::new(ExprKind::Keyword("value".to_string()), span),
                    Expr::new(ExprKind::Keyword(k.clone()), span),
                ),
            ]),
            span,
        ),
        ExprKind::List(items) => {
            let ast_items: Vec<Expr> = items.iter().map(|e| expr_to_ast_value(e, span)).collect();
            Expr::new(
                ExprKind::Map(vec![
                    (
                        Expr::new(ExprKind::Keyword("kind".to_string()), span),
                        Expr::new(ExprKind::Keyword("list".to_string()), span),
                    ),
                    (
                        Expr::new(ExprKind::Keyword("items".to_string()), span),
                        Expr::new(ExprKind::Vec(ast_items), span),
                    ),
                ]),
                span,
            )
        }
        ExprKind::Vec(items) => {
            let ast_items: Vec<Expr> = items.iter().map(|e| expr_to_ast_value(e, span)).collect();
            Expr::new(
                ExprKind::Map(vec![
                    (
                        Expr::new(ExprKind::Keyword("kind".to_string()), span),
                        Expr::new(ExprKind::Keyword("vec".to_string()), span),
                    ),
                    (
                        Expr::new(ExprKind::Keyword("items".to_string()), span),
                        Expr::new(ExprKind::Vec(ast_items), span),
                    ),
                ]),
                span,
            )
        }
        _ => {
            // Fallback: represent as string
            Expr::new(
                ExprKind::Map(vec![
                    (
                        Expr::new(ExprKind::Keyword("kind".to_string()), span),
                        Expr::new(ExprKind::Keyword("str".to_string()), span),
                    ),
                    (
                        Expr::new(ExprKind::Keyword("value".to_string()), span),
                        Expr::new(ExprKind::Str(format!("{expr}")), span),
                    ),
                ]),
                span,
            )
        }
    }
}

/// Convert a runtime Value (from procedural macro execution) back to an AST Expr.
fn ast_value_to_expr(val: &interp::Value, span: Span) -> Result<Expr, String> {
    match val {
        interp::Value::Map(m) => {
            // Look up :kind
            let kind = m.get(&interp::Value::Keyword("kind".into()));

            match kind {
                Some(interp::Value::Keyword(k)) => match &**k {
                    "symbol" => {
                        let name = get_str_field(m, "name")?;
                        Ok(Expr::new(ExprKind::Symbol(name), span))
                    }
                    "int" => {
                        let val = get_int_field(m, "value")?;
                        Ok(Expr::new(ExprKind::Int(val), span))
                    }
                    "str" => {
                        let val = get_str_field(m, "value")?;
                        Ok(Expr::new(ExprKind::Str(val), span))
                    }
                    "bool" => {
                        let val = get_bool_field(m, "value")?;
                        Ok(Expr::new(ExprKind::Bool(val), span))
                    }
                    "keyword" => {
                        let val = get_keyword_field(m, "value")?;
                        Ok(Expr::new(ExprKind::Keyword(val), span))
                    }
                    "list" => {
                        let items = get_vec_field(m, "items")?;
                        let exprs: Result<Vec<_>, _> =
                            items.iter().map(|v| ast_value_to_expr(v, span)).collect();
                        Ok(Expr::new(ExprKind::List(exprs?), span))
                    }
                    "vec" => {
                        let items = get_vec_field(m, "items")?;
                        let exprs: Result<Vec<_>, _> =
                            items.iter().map(|v| ast_value_to_expr(v, span)).collect();
                        Ok(Expr::new(ExprKind::Vec(exprs?), span))
                    }
                    other => Err(format!("unknown AST node kind: {other}")),
                },
                _ => Err("procedural macro must return a map with :kind".to_string()),
            }
        }
        // Allow direct value returns for simple cases
        interp::Value::Int(n) => Ok(Expr::new(ExprKind::Int(*n), span)),
        interp::Value::Str(s) => Ok(Expr::new(ExprKind::Str(s.to_string()), span)),
        interp::Value::Bool(b) => Ok(Expr::new(ExprKind::Bool(*b), span)),
        interp::Value::Keyword(k) => Ok(Expr::new(ExprKind::Keyword(k.to_string()), span)),
        other => Err(format!(
            "procedural macro returned unexpected value: {other}"
        )),
    }
}

// Helper functions for extracting fields from Value::Map

fn get_str_field(m: &interp::OrdMap, field: &str) -> Result<String, String> {
    m.get(&interp::Value::Keyword(field.into()))
        .and_then(|v| match v {
            interp::Value::Str(s) => Some(s.to_string()),
            _ => None,
        })
        .ok_or_else(|| format!("AST node missing string field :{field}"))
}

fn get_int_field(m: &interp::OrdMap, field: &str) -> Result<i64, String> {
    m.get(&interp::Value::Keyword(field.into()))
        .and_then(|v| match v {
            interp::Value::Int(n) => Some(*n),
            _ => None,
        })
        .ok_or_else(|| format!("AST node missing int field :{field}"))
}

fn get_bool_field(m: &interp::OrdMap, field: &str) -> Result<bool, String> {
    m.get(&interp::Value::Keyword(field.into()))
        .and_then(|v| match v {
            interp::Value::Bool(b) => Some(*b),
            _ => None,
        })
        .ok_or_else(|| format!("AST node missing bool field :{field}"))
}

fn get_keyword_field(m: &interp::OrdMap, field: &str) -> Result<String, String> {
    m.get(&interp::Value::Keyword(field.into()))
        .and_then(|v| match v {
            interp::Value::Keyword(k) => Some(k.to_string()),
            _ => None,
        })
        .ok_or_else(|| format!("AST node missing keyword field :{field}"))
}

fn get_vec_field(m: &interp::OrdMap, field: &str) -> Result<imbl::Vector<interp::Value>, String> {
    m.get(&interp::Value::Keyword(field.into()))
        .and_then(|v| match v {
            interp::Value::Vec(items) => Some(items.clone()),
            _ => None,
        })
        .ok_or_else(|| format!("AST node missing vec field :{field}"))
}

/// Generate a prelude that registers AST-building builtins for procedural macros.
fn make_ast_builtins_prelude(_span: Span) -> Expr {
    // We don't need an actual prelude — the procedural macro body uses
    // plain Loon map/vec literals which are already available.
    // The ast/symbol, ast/list etc. builtins will be registered as interpreter builtins.
    let span = Span::new(0, 0);
    Expr::new(ExprKind::List(vec![]), span) // no-op: evaluates to Unit
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn template_macro_when() {
        let src = r#"
            [macro when [cond body]
              `[if ~cond ~body None]]
            [when true 42]
        "#;
        let exprs = parse(src).unwrap();
        let mut expander = MacroExpander::new();
        let expanded = expander.expand_program(&exprs).unwrap();
        // Should have one expression: [if true 42 None]
        assert_eq!(expanded.len(), 1);
        let s = format!("{}", expanded[0]);
        assert!(s.contains("if"), "expected 'if' in: {s}");
        assert!(s.contains("true"), "expected 'true' in: {s}");
        assert!(s.contains("42"), "expected '42' in: {s}");
        assert!(s.contains("None"), "expected 'None' in: {s}");
    }

    #[test]
    fn template_macro_rest_params() {
        let src = r#"
            [macro unless [cond & body]
              `[if ~cond None [do ~@body]]]
            [unless false 1 2 3]
        "#;
        let exprs = parse(src).unwrap();
        let mut expander = MacroExpander::new();
        let expanded = expander.expand_program(&exprs).unwrap();
        assert_eq!(expanded.len(), 1);
        let s = format!("{}", expanded[0]);
        assert!(s.contains("if"), "expected 'if' in: {s}");
        assert!(s.contains("do"), "expected 'do' in: {s}");
        // The body elements should be spliced in
        assert!(s.contains("1"), "expected '1' in: {s}");
        assert!(s.contains("2"), "expected '2' in: {s}");
        assert!(s.contains("3"), "expected '3' in: {s}");
    }

    #[test]
    fn macroexpand_returns_string() {
        let src = r#"
            [macro when [cond body]
              `[if ~cond ~body None]]
            [macroexpand [when true 42]]
        "#;
        let exprs = parse(src).unwrap();
        let mut expander = MacroExpander::new();
        let expanded = expander.expand_program(&exprs).unwrap();
        assert_eq!(expanded.len(), 1);
        // macroexpand produces a string literal
        if let ExprKind::Str(s) = &expanded[0].kind {
            assert!(s.contains("if"), "expected 'if' in macroexpand output: {s}");
        } else {
            panic!("expected string from macroexpand, got: {}", expanded[0]);
        }
    }

    #[test]
    fn nested_macro_expansion() {
        let src = r#"
            [macro when [cond body]
              `[if ~cond ~body None]]
            [macro when2 [a b]
              `[when ~a ~b]]
            [when2 true 99]
        "#;
        let exprs = parse(src).unwrap();
        let mut expander = MacroExpander::new();
        let expanded = expander.expand_program(&exprs).unwrap();
        assert_eq!(expanded.len(), 1);
        let s = format!("{}", expanded[0]);
        // when2 expands to when, which expands to if
        assert!(s.contains("if"), "expected 'if' in: {s}");
    }

    #[test]
    fn procedural_macro_undeclared_effect_denied() {
        // A procedural macro that performs an effect at expansion time without
        // declaring it must be hard-denied — this is the supply-chain guard.
        let src = r#"
            [macro snitch [x]
              [do
                [Process.env "HOME"]
                {:kind :int :value 1}]]
            [snitch 0]
        "#;
        let exprs = parse(src).unwrap();
        let mut expander = MacroExpander::new();
        let e = expander.expand_program(&exprs).unwrap_err();
        assert!(e.contains("compile-time effect `Env`"), "got: {e}");
        assert!(e.contains("did not declare it"), "got: {e}");
    }

    #[test]
    fn procedural_macro_declared_effect_allowed() {
        // Same macro, but it declares `#{Env}` — expansion is permitted.
        let src = r#"
            [macro envy [x] #{Env}
              [do
                [Process.env "HOME"]
                {:kind :int :value 7}]]
            [envy 0]
        "#;
        let exprs = parse(src).unwrap();
        let mut expander = MacroExpander::new();
        let expanded = expander.expand_program(&exprs).unwrap();
        assert_eq!(expanded.len(), 1);
        assert!(
            matches!(expanded[0].kind, ExprKind::Int(7)),
            "expected Int(7), got: {}",
            expanded[0]
        );
    }

    #[test]
    fn procedural_macro_undeclared_when_other_effect_declared() {
        // Declaring `#{Print}` must not implicitly grant `Env` — categories
        // are independent (default-deny per category).
        let src = r#"
            [macro sneaky [x] #{Print}
              [do
                [Process.env "HOME"]
                {:kind :int :value 1}]]
            [sneaky 0]
        "#;
        let exprs = parse(src).unwrap();
        let mut expander = MacroExpander::new();
        let e = expander.expand_program(&exprs).unwrap_err();
        assert!(e.contains("compile-time effect `Env`"), "got: {e}");
    }

    #[test]
    fn procedural_macro_thread_spawn_always_denied() {
        // Async.spawn is a sandbox-escape vector and is denied even though the
        // macro declares IO.
        let src = r#"
            [macro forker [x] #{IO}
              [do
                [Async.spawn [fn [] 1]]
                {:kind :int :value 1}]]
            [forker 0]
        "#;
        let exprs = parse(src).unwrap();
        let mut expander = MacroExpander::new();
        let e = expander.expand_program(&exprs).unwrap_err();
        assert!(e.contains("Async.spawn"), "got: {e}");
        assert!(e.contains("not permitted"), "got: {e}");
    }

    #[test]
    fn runtime_effects_not_sandboxed() {
        // Regression guard: the compile-time sandbox must not leak into normal
        // runtime execution. A plain program performing an effect is fine.
        let exprs = parse(r#"[Process.env "HOME"]"#).unwrap();
        let mut expander = MacroExpander::new();
        let expanded = expander.expand_program(&exprs).unwrap();
        let r = crate::interp::eval_program(&expanded);
        assert!(
            r.is_ok(),
            "runtime effect wrongly sandboxed: {:?}",
            r.err().map(|e| e.message)
        );
    }

    #[test]
    fn expansion_trace_recorded() {
        let src = r#"
            [macro when [cond body]
              `[if ~cond ~body None]]
            [when true 42]
        "#;
        let exprs = parse(src).unwrap();
        let mut expander = MacroExpander::new();
        let expanded = expander.expand_program(&exprs).unwrap();
        // The expanded node should have a trace
        let trace = expander.get_trace(expanded[0].id);
        assert!(trace.is_some(), "expected expansion trace");
        assert_eq!(trace.unwrap().steps[0].macro_name, "when");
    }
}
