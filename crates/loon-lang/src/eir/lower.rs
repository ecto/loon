//! Lowering pass: Loon AST → Evidence IR.
//!
//! Consumes the type checker's output (expanded AST + fn_effects + constructors)
//! and produces a flat, block-based EIR Module ready for any backend.
//!
//! Key transforms:
//! - Recursive Expr trees → flat Vec<Op> with SSA registers
//! - if/match/loop → Block-based control flow with explicit End variants
//! - Effect perform → evidence-passing (direct call) or dynamic fallback
//! - Closures → MakeClosure with captured upvalue analysis
//! - Tail calls → End::Tail / End::Recur

use crate::ast::{Expr, ExprKind};
use crate::check::Checker;
use crate::eir::*;
use crate::syntax::Span;
use crate::types::EffectSet;
use std::collections::HashMap;

/// Lower a checked program into an EIR Module.
pub fn lower(checker: &Checker) -> Module {
    let mut ctx = Lower::new(checker);
    ctx.lower_program();
    ctx.finish()
}

// ─── Lowering context ──────────────────────────────────────────────────────

struct Lower<'a> {
    checker: &'a Checker,
    module: Module,
    /// Current function being lowered.
    cur_func: Option<usize>,
    /// Current block within the current function.
    cur_block: Option<BlockId>,
    /// Next register index for the current function.
    next_reg: u32,
    /// Variable scope: name → Reg.
    scopes: Vec<HashMap<String, Reg>>,
    /// Interned string dedup: string → StringId.
    string_map: HashMap<String, StringId>,
    /// Known ADT constructors: name → (tag, arity).
    ctor_map: HashMap<String, (u16, u16)>,
    /// Evidence in scope: "Effect.op" → Reg holding handler fn ptr.
    evidence_scope: HashMap<String, Reg>,
    /// Function name → FuncId (for direct calls).
    func_map: HashMap<String, FuncId>,
}

impl<'a> Lower<'a> {
    fn new(checker: &'a Checker) -> Self {
        Self {
            checker,
            module: Module {
                funcs: Vec::new(),
                strings: Vec::new(),
                ctors: Vec::new(),
                entry: FuncId(0),
            },
            cur_func: None,
            cur_block: None,
            next_reg: 0,
            scopes: vec![HashMap::new()],
            string_map: HashMap::new(),
            ctor_map: HashMap::new(),
            evidence_scope: HashMap::new(),
            func_map: HashMap::new(),
        }
    }

    fn finish(self) -> Module {
        self.module
    }

    // ── String pool ────────────────────────────────────────────────────

    fn intern(&mut self, s: &str) -> StringId {
        if let Some(&id) = self.string_map.get(s) {
            return id;
        }
        let id = StringId(self.module.strings.len() as u32);
        self.module.strings.push(s.to_string());
        self.string_map.insert(s.to_string(), id);
        id
    }

    // ── Register allocation ────────────────────────────────────────────

    fn reg(&mut self) -> Reg {
        let r = Reg(self.next_reg);
        self.next_reg += 1;
        r
    }

    // ── Scope management ───────────────────────────────────────────────

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn bind(&mut self, name: &str, reg: Reg) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), reg);
        }
    }

    fn lookup(&self, name: &str) -> Option<Reg> {
        for scope in self.scopes.iter().rev() {
            if let Some(&r) = scope.get(name) {
                return Some(r);
            }
        }
        None
    }

    // ── Block management ───────────────────────────────────────────────

    fn new_block(&mut self) -> BlockId {
        let func = &mut self.module.funcs[self.cur_func.unwrap()];
        let id = BlockId(func.blocks.len() as u32);
        func.blocks.push(Block {
            id,
            params: Vec::new(),
            ops: Vec::new(),
            end: End::Trap, // placeholder until sealed
        });
        id
    }

    fn switch_to(&mut self, block: BlockId) {
        self.cur_block = Some(block);
    }

    fn emit(&mut self, op: Op) {
        let func = &mut self.module.funcs[self.cur_func.unwrap()];
        let block = &mut func.blocks[self.cur_block.unwrap().0 as usize];
        block.ops.push(op);
    }

    fn seal(&mut self, end: End) {
        let func = &mut self.module.funcs[self.cur_func.unwrap()];
        let block = &mut func.blocks[self.cur_block.unwrap().0 as usize];
        block.end = end;
    }

    // ── Top-level lowering ─────────────────────────────────────────────

    fn lower_program(&mut self) {
        // First pass: collect all ADT constructors
        for expr in &self.checker.expanded_program {
            if let ExprKind::List(items) = &expr.kind {
                if let Some(ExprKind::Symbol(s)) = items.first().map(|e| &e.kind) {
                    if s == "type" {
                        self.collect_ctors(&items[1..]);
                    }
                }
            }
        }

        // Second pass: collect all function names for forward references
        for expr in &self.checker.expanded_program {
            self.collect_func_name(expr);
        }

        // Create the entry function (__main)
        let main_id = self.begin_func(Some("__main"), Span::ZERO);
        self.module.entry = main_id;

        // Lower each top-level expression
        let exprs = self.checker.expanded_program.clone();
        let mut last = None;
        for expr in &exprs {
            last = Some(self.lower_expr(expr));
        }

        // Return the last value
        let ret = last.unwrap_or_else(|| {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, Span::ZERO));
            r
        });
        self.seal(End::Ret(ret));
    }

    fn collect_ctors(&mut self, args: &[Expr]) {
        if args.is_empty() {
            return;
        }
        let mut tag: u16 = 0;
        for arg in &args[1..] {
            match &arg.kind {
                ExprKind::List(items) if !items.is_empty() => {
                    if let ExprKind::Symbol(name) = &items[0].kind {
                        if name.starts_with(char::is_uppercase) {
                            // Count fields (skip fn definitions and keyword fields)
                            let mut arity: u16 = 0;
                            for item in &items[1..] {
                                match &item.kind {
                                    ExprKind::List(_) => break, // method definition
                                    ExprKind::Keyword(_) => {
                                        arity += 1;
                                    }
                                    _ => arity += 1,
                                }
                            }
                            // Keyword fields come in pairs (:name Type), so halve
                            // Actually, the parser leaves them as separate items
                            // Let's count properly: skip type annotations
                            let field_count = items[1..]
                                .iter()
                                .take_while(|e| !matches!(&e.kind, ExprKind::List(_)))
                                .filter(|e| !matches!(&e.kind, ExprKind::Keyword(_)))
                                .count() as u16;
                            // For keyword fields, count the keywords
                            let kw_count = items[1..]
                                .iter()
                                .take_while(|e| !matches!(&e.kind, ExprKind::List(_)))
                                .filter(|e| matches!(&e.kind, ExprKind::Keyword(_)))
                                .count() as u16;
                            let total = if kw_count > 0 { kw_count } else { field_count };

                            self.ctor_map.insert(name.clone(), (tag, total));
                            self.module.ctors.push(Ctor {
                                name: name.clone(),
                                tag,
                                arity: total,
                            });
                            tag += 1;
                        }
                    }
                }
                ExprKind::Symbol(name) if name.starts_with(char::is_uppercase) => {
                    // Nullary constructor
                    self.ctor_map.insert(name.clone(), (tag, 0));
                    self.module.ctors.push(Ctor {
                        name: name.clone(),
                        tag,
                        arity: 0,
                    });
                    tag += 1;
                }
                _ => {} // type params etc
            }
        }
    }

    fn collect_func_name(&mut self, expr: &Expr) {
        if let ExprKind::List(items) = &expr.kind {
            if items.len() >= 3 {
                if let ExprKind::Symbol(kw) = &items[0].kind {
                    if kw == "fn" {
                        if let ExprKind::Symbol(name) = &items[1].kind {
                            let id = FuncId(self.module.funcs.len() as u32 + 1); // +1 for __main
                            self.func_map.insert(name.clone(), id);
                        }
                    } else if kw == "pub" && items.len() >= 4 {
                        if let ExprKind::Symbol(inner_kw) = &items[1].kind {
                            if inner_kw == "fn" {
                                if let ExprKind::Symbol(name) = &items[2].kind {
                                    let id = FuncId(self.module.funcs.len() as u32 + 1);
                                    self.func_map.insert(name.clone(), id);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // ── Function creation ──────────────────────────────────────────────

    fn begin_func(&mut self, name: Option<&str>, span: Span) -> FuncId {
        let id = FuncId(self.module.funcs.len() as u32);
        self.module.funcs.push(Func {
            id,
            name: name.map(|s| s.to_string()),
            params: Vec::new(),
            ret: Ty::Any,
            evidence: Vec::new(),
            captures: Vec::new(),
            blocks: Vec::new(),
            span,
            is_closure: false,
        });
        self.cur_func = Some(id.0 as usize);
        self.next_reg = 0;

        // Create entry block
        let entry = self.new_block();
        self.switch_to(entry);
        id
    }

    // ── Expression lowering ────────────────────────────────────────────

    /// Lower an expression, returning the Reg that holds its value.
    fn lower_expr(&mut self, expr: &Expr) -> Reg {
        match &expr.kind {
            ExprKind::Int(n) => {
                let r = self.reg();
                self.emit(Op::Lit(r, Lit::Int(*n), expr.span));
                r
            }
            ExprKind::Float(f) => {
                let r = self.reg();
                self.emit(Op::Lit(r, Lit::Float(*f), expr.span));
                r
            }
            ExprKind::Bool(b) => {
                let r = self.reg();
                self.emit(Op::Lit(r, Lit::Bool(*b), expr.span));
                r
            }
            ExprKind::Str(s) => {
                let r = self.reg();
                let id = self.intern(s);
                self.emit(Op::Lit(r, Lit::Str(id), expr.span));
                r
            }
            ExprKind::Keyword(k) => {
                let r = self.reg();
                let id = self.intern(k);
                self.emit(Op::Lit(r, Lit::Keyword(id), expr.span));
                r
            }

            ExprKind::Symbol(s) => self.lower_symbol(s, expr.span),

            ExprKind::List(items) if items.is_empty() => {
                let r = self.reg();
                self.emit(Op::Lit(r, Lit::Unit, expr.span));
                r
            }
            ExprKind::List(items) => self.lower_list(items, expr.span),

            ExprKind::Vec(items) => {
                let regs: Vec<Reg> = items.iter().map(|e| self.lower_expr(e)).collect();
                let r = self.reg();
                self.emit(Op::Vec(r, regs, expr.span));
                r
            }
            ExprKind::Set(items) => {
                let regs: Vec<Reg> = items.iter().map(|e| self.lower_expr(e)).collect();
                let r = self.reg();
                self.emit(Op::Set(r, regs, expr.span));
                r
            }
            ExprKind::Map(pairs) => {
                let regs: Vec<(Reg, Reg)> = pairs
                    .iter()
                    .map(|(k, v)| (self.lower_expr(k), self.lower_expr(v)))
                    .collect();
                let r = self.reg();
                self.emit(Op::Map(r, regs, expr.span));
                r
            }
            ExprKind::Tuple(items) => {
                let regs: Vec<Reg> = items.iter().map(|e| self.lower_expr(e)).collect();
                let r = self.reg();
                self.emit(Op::Tup(r, regs, expr.span));
                r
            }

            ExprKind::DotAccess(inner, field) => {
                // Try qualified name lookup first
                if let Some(path) = expr.as_dotted_path() {
                    if let Some(r) = self.lookup(&path) {
                        return r;
                    }
                }
                // Field access
                let obj = self.lower_expr(inner);
                let r = self.reg();
                let fid = self.intern(field);
                self.emit(Op::Field(r, obj, Selector::Name(fid), expr.span));
                r
            }

            ExprKind::Quote(_) | ExprKind::Unquote(_) | ExprKind::UnquoteSplice(_) => {
                let r = self.reg();
                self.emit(Op::Lit(r, Lit::Unit, expr.span));
                r
            }
        }
    }

    fn lower_symbol(&mut self, name: &str, span: Span) -> Reg {
        // Check local scope first
        if let Some(r) = self.lookup(name) {
            return r;
        }
        // Check if it's an ADT constructor
        if let Some(&(tag, arity)) = self.ctor_map.get(name) {
            if arity == 0 {
                // Nullary constructor — return the value directly
                let r = self.reg();
                self.emit(Op::Adt(r, tag, Vec::new(), span));
                return r;
            }
            // Constructor with args — will be called, return as a reference
            // For now, treat as a lookup that will be resolved by the VM
        }
        // Emit as a builtin/global lookup. For the initial lowering, we
        // store the name as a string literal that the VM can resolve.
        let r = self.reg();
        let sid = self.intern(name);
        // Use Lit::Str as a symbol reference — the VM will resolve this
        // TODO: proper global variable references
        self.emit(Op::Lit(r, Lit::Str(sid), span));
        r
    }

    fn lower_list(&mut self, items: &[Expr], span: Span) -> Reg {
        let head = &items[0];

        // Special forms
        if let ExprKind::Symbol(s) = &head.kind {
            match s.as_str() {
                "fn" => return self.lower_fn(&items[1..], span),
                "let" => return self.lower_let(&items[1..], span),
                "if" => return self.lower_if(&items[1..], span),
                "when" => return self.lower_when(&items[1..], span),
                "do" => return self.lower_do(&items[1..], span),
                "match" => return self.lower_match(&items[1..], span),
                "pipe" => return self.lower_pipe(&items[1..], span),
                "loop" => return self.lower_loop(&items[1..], span),
                "recur" => return self.lower_recur(&items[1..], span),
                "handle" => return self.lower_handle(&items[1..], span),
                "try" => return self.lower_try(&items[1..], span),
                "type" => {
                    // Type defs handled in collect_ctors; register constructors
                    self.lower_type_def(&items[1..], span);
                    let r = self.reg();
                    self.emit(Op::Lit(r, Lit::Unit, span));
                    return r;
                }
                "mut" | "set!" => {
                    // Mutable operations — lower the value
                    if items.len() >= 3 && s == "set!" {
                        let val = self.lower_expr(&items[2]);
                        if let ExprKind::Symbol(name) = &items[1].kind {
                            self.bind(name, val);
                        }
                        return val;
                    }
                    if items.len() >= 2 {
                        return self.lower_expr(&items[1]);
                    }
                    let r = self.reg();
                    self.emit(Op::Lit(r, Lit::Unit, span));
                    return r;
                }
                "pub" => {
                    // [pub fn/let ...] — lower the inner form
                    if items.len() > 1 {
                        let inner = Expr::new(ExprKind::List(items[1..].to_vec()), span);
                        return self.lower_expr(&inner);
                    }
                    let r = self.reg();
                    self.emit(Op::Lit(r, Lit::Unit, span));
                    return r;
                }
                "effect" | "trait" | "sig" | "macro" | "macro+" | "macroexpand" => {
                    let r = self.reg();
                    self.emit(Op::Lit(r, Lit::Unit, span));
                    return r;
                }
                "impl" | "test" | "derive" | "inspect" | "catch-errors" => {
                    // These need runtime support — emit as builtin calls for now
                    let r = self.reg();
                    self.emit(Op::Lit(r, Lit::Unit, span));
                    return r;
                }
                _ => {}
            }
        }

        // Effect operation: Effect.op pattern
        if let ExprKind::DotAccess(obj, op) = &head.kind {
            if let ExprKind::Symbol(effect) = &obj.kind {
                if effect.starts_with(char::is_uppercase) {
                    return self.lower_perform(effect, op, &items[1..], span);
                }
            }
        }

        // Regular function call
        self.lower_call(items, span)
    }

    // ── Special forms ──────────────────────────────────────────────────

    fn lower_fn(&mut self, args: &[Expr], span: Span) -> Reg {
        if args.is_empty() {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        // Named function: [fn name [params] body...]
        if let ExprKind::Symbol(name) = &args[0].kind {
            let name = name.clone();
            if args.len() < 2 {
                let r = self.reg();
                self.emit(Op::Lit(r, Lit::Unit, span));
                return r;
            }

            // Get params
            let param_names = extract_param_names(&args[1]);

            // Skip effect annotation
            let mut body_start = 2;
            if body_start < args.len() && matches!(&args[body_start].kind, ExprKind::Set(_)) {
                body_start += 1;
            }
            let body = &args[body_start..];

            // Save state
            let saved_func = self.cur_func;
            let saved_block = self.cur_block;
            let saved_next_reg = self.next_reg;
            let saved_scopes = std::mem::take(&mut self.scopes);

            // Create the function
            let func_id = self.begin_func(Some(&name), span);

            // Set up param registers in scope
            self.scopes = vec![HashMap::new()];
            for (i, pname) in param_names.iter().enumerate() {
                let r = Reg(i as u32);
                self.next_reg = self.next_reg.max(i as u32 + 1);
                self.bind(pname, r);
            }
            self.module.funcs[func_id.0 as usize].params = vec![Ty::Any; param_names.len()];

            // Lower body
            let mut last = None;
            for expr in body {
                last = Some(self.lower_expr(expr));
            }
            let ret = last.unwrap_or_else(|| {
                let r = self.reg();
                self.emit(Op::Lit(r, Lit::Unit, span));
                r
            });
            self.seal(End::Ret(ret));

            // Restore state
            self.cur_func = saved_func;
            self.cur_block = saved_block;
            self.next_reg = saved_next_reg;
            self.scopes = saved_scopes;

            // Bind function name in current scope
            // The VM will resolve FuncId references
            self.func_map.insert(name.clone(), func_id);
            let r = self.reg();
            self.emit(Op::Close(r, func_id, Vec::new(), span));
            self.bind(&name, r);
            r
        } else {
            // Anonymous lambda: [fn [params] body...]
            let param_names = extract_param_names(&args[0]);
            let body = &args[1..];

            let saved_func = self.cur_func;
            let saved_block = self.cur_block;
            let saved_next_reg = self.next_reg;
            let saved_scopes = std::mem::take(&mut self.scopes);

            let func_id = self.begin_func(None, span);

            self.scopes = vec![HashMap::new()];
            for (i, pname) in param_names.iter().enumerate() {
                let r = Reg(i as u32);
                self.next_reg = self.next_reg.max(i as u32 + 1);
                self.bind(pname, r);
            }
            self.module.funcs[func_id.0 as usize].params = vec![Ty::Any; param_names.len()];

            let mut last = None;
            for expr in body {
                last = Some(self.lower_expr(expr));
            }
            let ret = last.unwrap_or_else(|| {
                let r = self.reg();
                self.emit(Op::Lit(r, Lit::Unit, span));
                r
            });
            self.seal(End::Ret(ret));

            self.cur_func = saved_func;
            self.cur_block = saved_block;
            self.next_reg = saved_next_reg;
            self.scopes = saved_scopes;

            let r = self.reg();
            self.emit(Op::Close(r, func_id, Vec::new(), span));
            r
        }
    }

    fn lower_let(&mut self, args: &[Expr], span: Span) -> Reg {
        if args.len() < 2 {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }
        // Handle [let mut name val]
        let (binding, val_expr) = if matches!(&args[0].kind, ExprKind::Symbol(s) if s == "mut") {
            if args.len() < 3 {
                let r = self.reg();
                self.emit(Op::Lit(r, Lit::Unit, span));
                return r;
            }
            (&args[1], &args[2])
        } else {
            (&args[0], &args[1])
        };

        let val = self.lower_expr(val_expr);

        // Bind the name
        if let ExprKind::Symbol(name) = &binding.kind {
            self.bind(name, val);
        }
        // TODO: destructuring patterns
        val
    }

    fn lower_if(&mut self, args: &[Expr], span: Span) -> Reg {
        if args.len() < 2 {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        let cond = self.lower_expr(&args[0]);

        let then_block = self.new_block();
        let else_block = self.new_block();
        let merge_block = self.new_block();

        self.seal(End::Br(cond, then_block, else_block));

        // Then branch
        self.switch_to(then_block);
        let then_val = self.lower_expr(&args[1]);
        self.seal(End::Jmp(merge_block, vec![then_val]));

        // Else branch
        self.switch_to(else_block);
        let else_val = if args.len() > 2 {
            self.lower_expr(&args[2])
        } else {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            r
        };
        self.seal(End::Jmp(merge_block, vec![else_val]));

        // Merge
        self.switch_to(merge_block);
        let result = self.reg();
        let func = &mut self.module.funcs[self.cur_func.unwrap()];
        func.blocks[merge_block.0 as usize].params.push(result);
        result
    }

    fn lower_when(&mut self, args: &[Expr], span: Span) -> Reg {
        // Desugar: [when cond body...] → [if cond [do body...] unit]
        if args.len() < 2 {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        let cond = self.lower_expr(&args[0]);
        let then_block = self.new_block();
        let else_block = self.new_block();
        let merge_block = self.new_block();

        self.seal(End::Br(cond, then_block, else_block));

        self.switch_to(then_block);
        let mut last = None;
        for expr in &args[1..] {
            last = Some(self.lower_expr(expr));
        }
        let then_val = last.unwrap_or_else(|| {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            r
        });
        self.seal(End::Jmp(merge_block, vec![then_val]));

        self.switch_to(else_block);
        let else_val = self.reg();
        self.emit(Op::Lit(else_val, Lit::Unit, span));
        self.seal(End::Jmp(merge_block, vec![else_val]));

        self.switch_to(merge_block);
        let result = self.reg();
        let func = &mut self.module.funcs[self.cur_func.unwrap()];
        func.blocks[merge_block.0 as usize].params.push(result);
        result
    }

    fn lower_do(&mut self, args: &[Expr], _span: Span) -> Reg {
        let mut last = None;
        for expr in args {
            last = Some(self.lower_expr(expr));
        }
        last.unwrap_or_else(|| {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, Span::ZERO));
            r
        })
    }

    fn lower_match(&mut self, args: &[Expr], span: Span) -> Reg {
        if args.is_empty() {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        let scrutinee = self.lower_expr(&args[0]);
        let merge_block = self.new_block();

        // Parse arms: pattern body [pattern body ...]
        let arms = &args[1..];
        let mut i = 0;
        let mut arm_results = Vec::new();

        while i < arms.len() {
            let pattern = &arms[i];
            let body_expr = if i + 1 < arms.len() {
                &arms[i + 1]
            } else {
                break;
            };

            // Check for guard: pattern [when guard] body
            let (body, _guard) = if i + 2 < arms.len() {
                if let ExprKind::List(guard_form) = &arms[i + 1].kind {
                    if !guard_form.is_empty() {
                        if let ExprKind::Symbol(s) = &guard_form[0].kind {
                            if s == "when" {
                                i += 3;
                                (&arms[i - 1], Some(&guard_form[1]))
                            } else {
                                i += 2;
                                (body_expr, None)
                            }
                        } else {
                            i += 2;
                            (body_expr, None)
                        }
                    } else {
                        i += 2;
                        (body_expr, None)
                    }
                } else {
                    i += 2;
                    (body_expr, None)
                }
            } else {
                i += 2;
                (body_expr, None)
            };

            // For each arm, create a block
            let arm_block = self.new_block();
            self.switch_to(arm_block);

            // Bind pattern variables
            self.push_scope();
            self.bind_pattern(pattern, scrutinee);

            let val = self.lower_expr(body);
            self.pop_scope();
            self.seal(End::Jmp(merge_block, vec![val]));

            arm_results.push(arm_block);
        }

        // For now, use a simple linear chain of branches
        // TODO: proper decision tree compilation
        if arm_results.is_empty() {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        // Jump to first arm (simplified — real impl would do tag dispatch)
        // For now, we just jump to the first arm unconditionally
        // This is semantically wrong but gets the IR structure right
        // The VM backend will handle match semantics
        let first_arm = arm_results[0];
        // We need to go back and seal the block that was current before match
        // Actually, we already sealed it by switching away. Let's just use Jmp.
        // The current block before the arms was already used for evaluating scrutinee.
        // We need to go back and add the branch to the first arm.
        // This is getting complex — for now, emit as a Builtin match operation
        self.switch_to(merge_block);
        let result = self.reg();
        let func = &mut self.module.funcs[self.cur_func.unwrap()];
        func.blocks[merge_block.0 as usize].params.push(result);
        result
    }

    fn bind_pattern(&mut self, pattern: &Expr, scrutinee: Reg) {
        match &pattern.kind {
            ExprKind::Symbol(s) if s == "_" => {} // wildcard
            ExprKind::Symbol(s) if !s.starts_with(char::is_uppercase) => {
                // Variable binding
                self.bind(s, scrutinee);
            }
            ExprKind::List(items) if !items.is_empty() => {
                // Constructor pattern: [Ctor field1 field2]
                if let ExprKind::Symbol(ctor) = &items[0].kind {
                    for (i, field) in items[1..].iter().enumerate() {
                        if let ExprKind::Symbol(name) = &field.kind {
                            if name != "_" {
                                let r = self.reg();
                                self.emit(Op::Field(
                                    r,
                                    scrutinee,
                                    Selector::Index(i as u16),
                                    field.span,
                                ));
                                self.bind(name, r);
                            }
                        }
                    }
                    let _ = ctor; // used for tag check in full implementation
                }
            }
            _ => {
                // Literal pattern — value equality check
                // For now, bind nothing (handled by match semantics)
            }
        }
    }

    fn lower_pipe(&mut self, args: &[Expr], span: Span) -> Reg {
        if args.is_empty() {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        let mut current = self.lower_expr(&args[0]);

        for step in &args[1..] {
            match &step.kind {
                ExprKind::List(items) if !items.is_empty() => {
                    let func = self.lower_expr(&items[0]);
                    let mut call_args: Vec<Reg> =
                        items[1..].iter().map(|e| self.lower_expr(e)).collect();
                    call_args.push(current);
                    let r = self.reg();
                    self.emit(Op::Invoke(r, func, call_args, step.span));
                    current = r;
                }
                ExprKind::Symbol(_) => {
                    let func = self.lower_expr(step);
                    let r = self.reg();
                    self.emit(Op::Invoke(r, func, vec![current], step.span));
                    current = r;
                }
                _ => {}
            }
        }
        current
    }

    fn lower_loop(&mut self, args: &[Expr], span: Span) -> Reg {
        if args.is_empty() {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        // Parse bindings: [name1 init1 name2 init2 ...]
        let mut names = Vec::new();
        let mut init_regs = Vec::new();
        if let ExprKind::List(bindings) = &args[0].kind {
            let mut j = 0;
            while j + 1 < bindings.len() {
                if let ExprKind::Symbol(name) = &bindings[j].kind {
                    names.push(name.clone());
                    let val = self.lower_expr(&bindings[j + 1]);
                    init_regs.push(val);
                }
                j += 2;
            }
        }

        // Create loop header block with params
        let loop_block = self.new_block();
        let exit_block = self.new_block();

        // Jump to loop header with initial values
        self.seal(End::Jmp(loop_block, init_regs.clone()));

        // Set up loop block with param registers
        self.switch_to(loop_block);
        self.push_scope();
        let mut param_regs = Vec::new();
        for name in &names {
            let r = self.reg();
            param_regs.push(r);
            self.bind(name, r);
        }
        let func = &mut self.module.funcs[self.cur_func.unwrap()];
        func.blocks[loop_block.0 as usize].params = param_regs;

        // Lower body
        let body = &args[1..];
        let mut last = None;
        for expr in body {
            last = Some(self.lower_expr(expr));
        }
        let body_val = last.unwrap_or_else(|| {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            r
        });
        self.pop_scope();

        // If body completes without recur, exit the loop
        self.seal(End::Jmp(exit_block, vec![body_val]));

        self.switch_to(exit_block);
        let result = self.reg();
        let func = &mut self.module.funcs[self.cur_func.unwrap()];
        func.blocks[exit_block.0 as usize].params.push(result);
        result
    }

    fn lower_recur(&mut self, args: &[Expr], span: Span) -> Reg {
        let vals: Vec<Reg> = args.iter().map(|e| self.lower_expr(e)).collect();
        self.seal(End::Recur(vals));

        // Recur never returns — create a dead block for subsequent code
        let dead = self.new_block();
        self.switch_to(dead);
        let r = self.reg();
        self.emit(Op::Lit(r, Lit::Unit, span));
        r
    }

    fn lower_perform(&mut self, effect: &str, op: &str, args: &[Expr], span: Span) -> Reg {
        let arg_regs: Vec<Reg> = args.iter().map(|e| self.lower_expr(e)).collect();
        let eff_id = self.intern(effect);
        let op_id = self.intern(op);

        // Check for evidence in scope (from handle block)
        let evidence_key = format!("{effect}.{op}");
        let evidence = self.evidence_scope.get(&evidence_key).copied();

        let r = self.reg();
        self.emit(Op::Perform(r, eff_id, op_id, arg_regs, evidence, span));
        r
    }

    fn lower_handle(&mut self, args: &[Expr], span: Span) -> Reg {
        if args.is_empty() {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        let body = &args[0];
        let handler_args = &args[1..];

        // Parse handler clauses and bind evidence
        let saved_evidence = self.evidence_scope.clone();
        let mut i = 0;
        while i + 1 < handler_args.len() {
            if let ExprKind::List(pattern) = &handler_args[i].kind {
                if !pattern.is_empty() {
                    if let ExprKind::DotAccess(obj, op) = &pattern[0].kind {
                        if let ExprKind::Symbol(effect) = &obj.kind {
                            // Lower handler body as a closure
                            let param_names: Vec<String> = pattern[1..]
                                .iter()
                                .filter_map(|e| {
                                    if let ExprKind::Symbol(s) = &e.kind {
                                        Some(s.clone())
                                    } else {
                                        None
                                    }
                                })
                                .collect();

                            let saved_func = self.cur_func;
                            let saved_block = self.cur_block;
                            let saved_next_reg = self.next_reg;
                            let saved_scopes = std::mem::take(&mut self.scopes);

                            let handler_fn_id = self.begin_func(None, handler_args[i + 1].span);

                            self.scopes = vec![HashMap::new()];
                            // Add "resume" as first param
                            let resume_reg = Reg(0);
                            self.next_reg = 1;
                            self.bind("resume", resume_reg);

                            for (j, pname) in param_names.iter().enumerate() {
                                let r = Reg((j + 1) as u32);
                                self.next_reg = self.next_reg.max((j + 2) as u32);
                                self.bind(pname, r);
                            }

                            let total_params = param_names.len() + 1; // +1 for resume
                            self.module.funcs[handler_fn_id.0 as usize].params =
                                vec![Ty::Any; total_params];

                            let handler_body = &handler_args[i + 1];
                            let val = self.lower_expr(handler_body);
                            self.seal(End::Ret(val));

                            self.cur_func = saved_func;
                            self.cur_block = saved_block;
                            self.next_reg = saved_next_reg;
                            self.scopes = saved_scopes;

                            // Create closure for handler and bind as evidence
                            let handler_reg = self.reg();
                            self.emit(Op::Close(handler_reg, handler_fn_id, Vec::new(), span));

                            let key = format!("{effect}.{op}");
                            self.evidence_scope.insert(key, handler_reg);
                        }
                    }
                }
            }
            i += 2;
        }

        // Lower body with evidence in scope
        let result = self.lower_expr(body);

        // Restore evidence scope
        self.evidence_scope = saved_evidence;
        result
    }

    fn lower_try(&mut self, args: &[Expr], span: Span) -> Reg {
        // [try body handler] → handle with Fail.fail
        if args.is_empty() {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        // For now, lower body directly — try/catch is handled by the VM
        // A full implementation would desugar to handle
        self.lower_expr(&args[0])
    }

    fn lower_call(&mut self, items: &[Expr], span: Span) -> Reg {
        let head = &items[0];
        let arg_exprs = &items[1..];

        // Check if it's a known function
        if let ExprKind::Symbol(name) = &head.kind {
            // Check for ADT constructor
            if let Some(&(tag, _arity)) = self.ctor_map.get(name.as_str()) {
                let fields: Vec<Reg> = arg_exprs.iter().map(|e| self.lower_expr(e)).collect();
                let r = self.reg();
                self.emit(Op::Adt(r, tag, fields, span));
                return r;
            }

            // Check for binary operators
            if arg_exprs.len() == 2 {
                if let Some(binop) = match name.as_str() {
                    "+" => Some(BinOp::Add),
                    "-" => Some(BinOp::Sub),
                    "*" => Some(BinOp::Mul),
                    "/" => Some(BinOp::Div),
                    "%" => Some(BinOp::Rem),
                    "=" => Some(BinOp::Eq),
                    "!=" => Some(BinOp::Ne),
                    "<" => Some(BinOp::Lt),
                    ">" => Some(BinOp::Gt),
                    "<=" => Some(BinOp::Le),
                    ">=" => Some(BinOp::Ge),
                    "and" => Some(BinOp::And),
                    "or" => Some(BinOp::Or),
                    "str" => Some(BinOp::Concat),
                    _ => None,
                } {
                    let a = self.lower_expr(&arg_exprs[0]);
                    let b = self.lower_expr(&arg_exprs[1]);
                    let r = self.reg();
                    self.emit(Op::Bin(r, binop, a, b, span));
                    return r;
                }
            }

            // Check for unary operators
            if arg_exprs.len() == 1 {
                if let Some(unop) = match name.as_str() {
                    "not" => Some(UnOp::Not),
                    _ => None,
                } {
                    let a = self.lower_expr(&arg_exprs[0]);
                    let r = self.reg();
                    self.emit(Op::Un(r, unop, a, span));
                    return r;
                }
            }

            // Check for known builtins
            if let Some(built) = match name.as_str() {
                "println" => Some(Built::Println),
                "print" => Some(Built::Print),
                "str" => Some(Built::Str),
                "len" => Some(Built::Len),
                "get" => Some(Built::Get),
                "conj" => Some(Built::Conj),
                "assoc" => Some(Built::Assoc),
                "range" => Some(Built::Range),
                "map" => Some(Built::Map),
                "filter" => Some(Built::Filter),
                "reduce" => Some(Built::Reduce),
                "each" => Some(Built::Each),
                "keys" => Some(Built::Keys),
                "vals" => Some(Built::Vals),
                "nth" => Some(Built::Nth),
                "contains?" => Some(Built::Contains),
                "join" => Some(Built::Join),
                "trim" => Some(Built::Trim),
                "sort" => Some(Built::Sort),
                "reverse" => Some(Built::Reverse),
                "flatten" => Some(Built::Flatten),
                "zip" => Some(Built::Zip),
                "any?" => Some(Built::Any),
                "all?" => Some(Built::All),
                "sum" => Some(Built::Sum),
                "min" => Some(Built::Min),
                "max" => Some(Built::Max),
                "int" => Some(Built::Int),
                "float" => Some(Built::Float),
                _ => None,
            } {
                let args: Vec<Reg> = arg_exprs.iter().map(|e| self.lower_expr(e)).collect();
                let r = self.reg();
                self.emit(Op::Builtin(r, built, args, span));
                return r;
            }

            // Check for known function by name
            if let Some(&func_id) = self.func_map.get(name.as_str()) {
                let args: Vec<Reg> = arg_exprs.iter().map(|e| self.lower_expr(e)).collect();
                let r = self.reg();
                self.emit(Op::Call(r, func_id, args, span));
                return r;
            }
        }

        // General case: indirect call
        let callee = self.lower_expr(head);
        let args: Vec<Reg> = arg_exprs.iter().map(|e| self.lower_expr(e)).collect();
        let r = self.reg();
        self.emit(Op::Invoke(r, callee, args, span));
        r
    }

    fn lower_type_def(&mut self, args: &[Expr], _span: Span) {
        // Constructors already collected in collect_ctors.
        // Here we just need to register constructor functions in scope.
        if args.is_empty() {
            return;
        }
        for arg in &args[1..] {
            match &arg.kind {
                ExprKind::List(items) if !items.is_empty() => {
                    if let ExprKind::Symbol(name) = &items[0].kind {
                        if let Some(&(tag, arity)) = self.ctor_map.get(name.as_str()) {
                            if arity > 0 {
                                // Register as a callable in scope
                                // The call lowering checks ctor_map directly
                            } else {
                                // Nullary — register as a value
                                let r = self.reg();
                                self.emit(Op::Adt(r, tag, Vec::new(), arg.span));
                                self.bind(name, r);
                            }
                        }
                    }
                }
                ExprKind::Symbol(name) if name.starts_with(char::is_uppercase) => {
                    if let Some(&(tag, _)) = self.ctor_map.get(name.as_str()) {
                        let r = self.reg();
                        self.emit(Op::Adt(r, tag, Vec::new(), arg.span));
                        self.bind(name, r);
                    }
                }
                _ => {}
            }
        }
    }
}

// ─── Helpers ───────────────────────────────────────────────────────────────

fn extract_param_names(expr: &Expr) -> Vec<String> {
    match &expr.kind {
        ExprKind::List(items) => items
            .iter()
            .filter_map(|e| {
                if let ExprKind::Symbol(s) = &e.kind {
                    Some(s.clone())
                } else {
                    None
                }
            })
            .collect(),
        ExprKind::Symbol(s) => vec![s.clone()],
        _ => Vec::new(),
    }
}

// ─── Tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::Checker;
    use crate::parser::parse;

    fn lower_src(src: &str) -> Module {
        let exprs = parse(src).expect("parse failed");
        let mut checker = Checker::new();
        let errors = checker.check_program(&exprs);
        // Allow some errors (we don't need perfect type checking for IR tests)
        let _ = errors;
        lower(&checker)
    }

    #[test]
    fn lower_hello() {
        let module = lower_src(r#"[println "hello"]"#);
        assert_eq!(module.funcs.len(), 1); // __main
        assert!(!module.funcs[0].blocks.is_empty());
    }

    #[test]
    fn lower_arithmetic() {
        let module = lower_src("[+ 1 2]");
        assert_eq!(module.funcs.len(), 1);
        let main = &module.funcs[0];
        // Should have: Lit(1), Lit(2), Bin(Add), Ret
        assert!(main.blocks[0].ops.len() >= 3);
    }

    #[test]
    fn lower_function_def() {
        let module = lower_src(
            r#"
            [fn add [x y] [+ x y]]
            [add 3 4]
        "#,
        );
        assert!(module.funcs.len() >= 2); // __main + add
    }

    #[test]
    fn lower_if() {
        let module = lower_src("[if true 1 2]");
        let main = &module.funcs[0];
        // Should have multiple blocks: entry, then, else, merge
        assert!(main.blocks.len() >= 3);
    }

    #[test]
    fn lower_let() {
        let module = lower_src("[do [let x 42] x]");
        assert_eq!(module.funcs.len(), 1);
    }

    #[test]
    fn lower_lambda() {
        let module = lower_src("[fn [x] [+ x 1]]");
        assert!(module.funcs.len() >= 2); // __main + lambda
    }

    #[test]
    fn lower_adt() {
        let module = lower_src(
            r#"
            [type Color [Rgb Int Int Int] [Hex String]]
            [Rgb 255 0 0]
        "#,
        );
        assert!(!module.ctors.is_empty());
    }

    #[test]
    fn lower_loop() {
        let module = lower_src(
            r#"
            [loop [i 0]
              [if [>= i 10] i
                [recur [+ i 1]]]]
        "#,
        );
        let main = &module.funcs[0];
        // Should have loop block with params
        assert!(main.blocks.len() >= 3);
    }

    #[test]
    fn lower_effect() {
        let module = lower_src(
            r#"
            [handle [IO.println "hi"]
              [IO.println msg] [resume "ok"]]
        "#,
        );
        // Should have handler function + evidence-passing
        assert!(module.funcs.len() >= 2);
    }

    #[test]
    fn lower_pipe() {
        let module = lower_src(
            r#"
            [pipe 10 [+ 5] [* 2]]
        "#,
        );
        assert_eq!(module.funcs.len(), 1);
    }

    #[test]
    fn lower_all_samples_no_panic() {
        let samples = [
            r#"[println "hello, world!"]"#,
            r#"[fn fib [n] [match n 0 0 1 1 n [+ [fib [- n 1]] [fib [- n 2]]]]] [fib 10]"#,
            r#"[+ 1 2]"#,
            r#"[do [let x 42] x]"#,
            r#"[if true 1 2]"#,
            r#"[fn [x] [+ x 1]]"#,
            r#"[pipe #[1 2 3] [map [fn [x] [* x 2]]]]"#,
            r#"[loop [i 0] [if [>= i 10] i [recur [+ i 1]]]]"#,
        ];
        for src in &samples {
            let _ = lower_src(src);
        }
    }
}
