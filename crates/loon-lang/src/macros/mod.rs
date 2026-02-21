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
    #[allow(dead_code)]
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

    #[allow(dead_code)]
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
                let expanded_items: Result<Vec<_>, _> =
                    items.iter().map(|e| self.expand_type_aware_expr(e)).collect();
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
            MacroStyle::Procedural => {
                self.expand_procedural(mac, &bindings, call_span)
            }
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
            ExprKind::Quote(inner) => self.substitute(inner, bindings, call_span),
            _ => Err("template macro body must be a quasiquoted expression".to_string()),
        }
    }

    fn substitute(
        &mut self,
        expr: &Expr,
        bindings: &HashMap<String, Vec<Expr>>,
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
                    result.push(self.substitute(item, bindings, span)?);
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
                    result.push(self.substitute(item, bindings, span)?);
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
                Ok(expr.clone())
            }
            ExprKind::Map(pairs) => {
                let expanded: Result<Vec<(Expr, Expr)>, String> = pairs
                    .iter()
                    .map(|(k, v)| {
                        Ok((
                            self.substitute(k, bindings, span)?,
                            self.substitute(v, bindings, span)?,
                        ))
                    })
                    .collect();
                Ok(Expr::new(ExprKind::Map(expanded?), span))
            }
            ExprKind::Tuple(items) => {
                let expanded: Result<Vec<_>, _> =
                    items.iter().map(|e| self.substitute(e, bindings, span)).collect();
                Ok(Expr::new(ExprKind::Tuple(expanded?), span))
            }
            ExprKind::Set(items) => {
                let expanded: Result<Vec<_>, _> =
                    items.iter().map(|e| self.substitute(e, bindings, span)).collect();
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
        // Check compile-time effects
        self.check_compile_effects(mac)?;

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
                let items: Vec<Expr> = vals.iter().map(|v| expr_to_ast_value(v, call_span)).collect();
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

        // Evaluate the program using the interpreter
        let result = interp::eval_program(&program)
            .map_err(|e| format!("procedural macro '{}' failed: {}", mac.name, e.message))?;

        // Convert the result value back to an AST Expr
        ast_value_to_expr(&result, call_span)
    }

    fn check_compile_effects(&self, _mac: &MacroDef) -> Result<(), String> {
        // For now, we just record what's declared. Full enforcement happens
        // when compile-time builtins are called.
        // Pure macros (no declared effects) should not call IO/Net/etc builtins.
        Ok(())
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
        interp::Value::Map(pairs) => {
            // Look up :kind
            let kind = pairs
                .iter()
                .find(|(k, _)| matches!(k, interp::Value::Keyword(k) if k == "kind"))
                .map(|(_, v)| v);

            match kind {
                Some(interp::Value::Keyword(k)) => match k.as_str() {
                    "symbol" => {
                        let name = get_str_field(pairs, "name")?;
                        Ok(Expr::new(ExprKind::Symbol(name), span))
                    }
                    "int" => {
                        let val = get_int_field(pairs, "value")?;
                        Ok(Expr::new(ExprKind::Int(val), span))
                    }
                    "str" => {
                        let val = get_str_field(pairs, "value")?;
                        Ok(Expr::new(ExprKind::Str(val), span))
                    }
                    "bool" => {
                        let val = get_bool_field(pairs, "value")?;
                        Ok(Expr::new(ExprKind::Bool(val), span))
                    }
                    "keyword" => {
                        let val = get_keyword_field(pairs, "value")?;
                        Ok(Expr::new(ExprKind::Keyword(val), span))
                    }
                    "list" => {
                        let items = get_vec_field(pairs, "items")?;
                        let exprs: Result<Vec<_>, _> =
                            items.iter().map(|v| ast_value_to_expr(v, span)).collect();
                        Ok(Expr::new(ExprKind::List(exprs?), span))
                    }
                    "vec" => {
                        let items = get_vec_field(pairs, "items")?;
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
        interp::Value::Str(s) => Ok(Expr::new(ExprKind::Str(s.clone()), span)),
        interp::Value::Bool(b) => Ok(Expr::new(ExprKind::Bool(*b), span)),
        interp::Value::Keyword(k) => Ok(Expr::new(ExprKind::Keyword(k.clone()), span)),
        other => Err(format!(
            "procedural macro returned unexpected value: {other}"
        )),
    }
}

// Helper functions for extracting fields from Value::Map

fn get_str_field(pairs: &[(interp::Value, interp::Value)], field: &str) -> Result<String, String> {
    pairs
        .iter()
        .find(|(k, _)| matches!(k, interp::Value::Keyword(k) if k == field))
        .and_then(|(_, v)| match v {
            interp::Value::Str(s) => Some(s.clone()),
            _ => None,
        })
        .ok_or_else(|| format!("AST node missing string field :{field}"))
}

fn get_int_field(pairs: &[(interp::Value, interp::Value)], field: &str) -> Result<i64, String> {
    pairs
        .iter()
        .find(|(k, _)| matches!(k, interp::Value::Keyword(k) if k == field))
        .and_then(|(_, v)| match v {
            interp::Value::Int(n) => Some(*n),
            _ => None,
        })
        .ok_or_else(|| format!("AST node missing int field :{field}"))
}

fn get_bool_field(pairs: &[(interp::Value, interp::Value)], field: &str) -> Result<bool, String> {
    pairs
        .iter()
        .find(|(k, _)| matches!(k, interp::Value::Keyword(k) if k == field))
        .and_then(|(_, v)| match v {
            interp::Value::Bool(b) => Some(*b),
            _ => None,
        })
        .ok_or_else(|| format!("AST node missing bool field :{field}"))
}

fn get_keyword_field(
    pairs: &[(interp::Value, interp::Value)],
    field: &str,
) -> Result<String, String> {
    pairs
        .iter()
        .find(|(k, _)| matches!(k, interp::Value::Keyword(k) if k == field))
        .and_then(|(_, v)| match v {
            interp::Value::Keyword(k) => Some(k.clone()),
            _ => None,
        })
        .ok_or_else(|| format!("AST node missing keyword field :{field}"))
}

fn get_vec_field(
    pairs: &[(interp::Value, interp::Value)],
    field: &str,
) -> Result<Vec<interp::Value>, String> {
    pairs
        .iter()
        .find(|(k, _)| matches!(k, interp::Value::Keyword(k) if k == field))
        .and_then(|(_, v)| match v {
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
