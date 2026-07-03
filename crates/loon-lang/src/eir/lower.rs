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
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

/// Lower a checked program into an EIR Module.
pub fn lower(checker: &Checker) -> Module {
    let mut ctx = Lower::new(checker);
    ctx.lower_program();
    ctx.finish()
}

// ─── Lowering context ──────────────────────────────────────────────────────

/// How a name entered scope — decides whether it SHADOWS at call sites.
#[derive(Clone, Copy, PartialEq, Eq)]
enum BindKind {
    /// let / fn param / pattern / set! — shadows builtins and functions.
    Local,
    /// A named fn's self-binding or a nullary-ctor value — value form only;
    /// calls still resolve through func_map/ctor_map (direct Call).
    Fn,
}

struct Lower<'a> {
    checker: &'a Checker,
    module: Module,
    /// Current function being lowered.
    cur_func: Option<usize>,
    /// Current block within the current function.
    cur_block: Option<BlockId>,
    /// Next register index for the current function.
    next_reg: u32,
    /// Variable scope: name → (Reg, how it was bound). The kind decides
    /// shadowing at CALL sites: a `Local` binding (let / param / pattern)
    /// shadows operators, builtins, and top-level functions; a `Fn` binding
    /// (a named fn's self-binding, a nullary-ctor value) is just the value
    /// form of something that still resolves through func_map/ctor_map for
    /// direct calls.
    scopes: Vec<HashMap<String, (Reg, BindKind)>>,
    /// Interned string dedup: string → StringId.
    string_map: HashMap<String, StringId>,
    /// Known ADT constructors: name → (tag, arity).
    ctor_map: HashMap<String, (u16, u16)>,
    /// Next constructor tag. Tags are GLOBALLY unique across all type
    /// declarations (not reset per type), so a pattern of one type cannot match
    /// a value of another type that happens to share a per-type ordinal.
    next_tag: u16,
    /// Evidence in scope: "Effect.op" → Reg holding handler fn ptr.
    evidence_scope: HashMap<String, Reg>,
    /// Function name → FuncId (for direct calls).
    func_map: HashMap<String, FuncId>,
    /// Current loop header block (for recur → Jmp back to loop).
    recur_target: Option<BlockId>,
    /// Monotonic counter for compiler-synthesized (gensym) binding names.
    gensym_counter: u32,
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
            next_tag: 0,
            evidence_scope: HashMap::new(),
            func_map: HashMap::new(),
            recur_target: None,
            gensym_counter: 0,
        }
    }

    /// A binding name that cannot collide with any user symbol. The tokenizer
    /// splits on whitespace, so no user identifier can contain a space; a name
    /// with one is safe to use as a synthesized (gensym) binding key.
    fn fresh_name(&mut self, prefix: &str) -> String {
        let n = self.gensym_counter;
        self.gensym_counter += 1;
        format!("{prefix} {n}")
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
        // Use the global intern table for deduplication so the backing
        // Rc<str> is shared across the entire process.
        let sym = crate::intern::intern(s);
        self.module.strings.push(sym.as_str().to_string());
        self.string_map.insert(sym.as_str().to_string(), id);
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
            scope.insert(name.to_string(), (reg, BindKind::Local));
        }
    }

    /// Bind a named fn's self-binding / nullary ctor value: usable as a
    /// value, but NOT a shadow at call sites (calls keep resolving through
    /// func_map/ctor_map to a direct Call).
    fn bind_fn(&mut self, name: &str, reg: Reg) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), (reg, BindKind::Fn));
        }
    }

    fn lookup(&self, name: &str) -> Option<Reg> {
        for scope in self.scopes.iter().rev() {
            if let Some(&(r, _)) = scope.get(name) {
                return Some(r);
            }
        }
        None
    }

    /// The innermost binding of `name`, only if it is a genuine LOCAL
    /// (let / param / pattern) — the kind of binding that shadows
    /// builtins/operators/functions at call sites.
    fn lookup_local(&self, name: &str) -> Option<Reg> {
        for scope in self.scopes.iter().rev() {
            if let Some(&(r, kind)) = scope.get(name) {
                return (kind == BindKind::Local).then_some(r);
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
        // Resolve `[use ...]` modules by inlining their (macro-expanded) top-level
        // definitions ahead of the main program, so a multi-file program runs on
        // the EIR VM. Imported functions are registered under their bare name (so
        // their internal references resolve) and additionally under a qualified
        // `alias.name` (so `[module.fn ...]` resolves). See LIM-5 in
        // src/eff/NOTES.md for the supported subset.
        let main_forms = self.checker.expanded_program.clone();
        let mut imported: Vec<Expr> = Vec::new();
        let mut qualified: Vec<(String, String)> = Vec::new();
        if let Some(base) = self.checker.base_dir() {
            let base = base.to_path_buf();
            let mut visited: HashSet<PathBuf> = HashSet::new();
            self.collect_imports(
                &main_forms,
                &base,
                &mut visited,
                &mut imported,
                &mut qualified,
            );
        }
        let mut all_forms = imported;
        all_forms.extend(main_forms);

        // Register the prelude's Option/Result constructors first, so
        // Some/None/Ok/Err work on the EIR VM exactly as they do on the
        // interpreter (which evals the prelude at startup). A program that
        // (re)defines these types simply overwrites the ctor_map entries
        // below with its own tags.
        if let Ok(prelude_forms) = crate::parser::parse(crate::prelude::PRELUDE) {
            for expr in &prelude_forms {
                if let ExprKind::List(items) = &expr.kind {
                    if let Some(ExprKind::Symbol(s)) = items.first().map(|e| &e.kind) {
                        if s == "type" {
                            self.collect_ctors(&items[1..]);
                        }
                    }
                }
            }
        }

        // First pass: collect all ADT constructors
        for expr in &all_forms {
            if let ExprKind::List(items) = &expr.kind {
                if let Some(ExprKind::Symbol(s)) = items.first().map(|e| &e.kind) {
                    if s == "type" {
                        self.collect_ctors(&items[1..]);
                    }
                }
            }
        }

        // Second pass: pre-create ALL top-level named functions as stubs.
        // This assigns real FuncIds (via begin_func) before any body is lowered,
        // enabling mutual recursion without ID mismatch from anonymous lambdas.
        for expr in &all_forms.clone() {
            if let ExprKind::List(items) = &expr.kind {
                if items.len() >= 3 {
                    if let Some(ExprKind::Symbol(kw)) = items.first().map(|e| &e.kind) {
                        if kw == "fn" {
                            if let ExprKind::Symbol(name) = &items[1].kind {
                                let fid = self.begin_func(Some(name), expr.span);
                                self.func_map.insert(name.clone(), fid);
                            }
                        } else if kw == "pub" && items.len() >= 4 {
                            if let Some(ExprKind::Symbol(inner)) = items.get(1).map(|e| &e.kind) {
                                if inner == "fn" {
                                    if let ExprKind::Symbol(name) = &items[2].kind {
                                        let fid = self.begin_func(Some(name), expr.span);
                                        self.func_map.insert(name.clone(), fid);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Expose imported functions under their qualified `alias.name` too, so
        // `[module.fn ...]` resolves to the same FuncId as the bare name.
        for (alias, name) in &qualified {
            if let Some(&fid) = self.func_map.get(name) {
                self.func_map.insert(format!("{alias}.{name}"), fid);
            }
        }

        // Create the entry function (__main)
        let main_id = self.begin_func(Some("__main"), Span::ZERO);
        self.module.entry = main_id;

        // Lower each top-level expression (imported defs first, then main).
        let mut last = None;
        for expr in &all_forms {
            last = Some(self.lower_expr(expr));
        }

        // If there's a `main` function, call it
        if let Some(main_reg) = self.lookup("main") {
            let call_result = self.reg();
            self.emit(Op::Invoke(call_result, main_reg, Vec::new(), Span::ZERO));
            last = Some(call_result);
        }

        // Return the last value
        let ret = last.unwrap_or_else(|| {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, Span::ZERO));
            r
        });
        self.seal(End::Ret(ret));
    }

    /// Walk `forms` for `[use ...]` declarations, recursively load each module's
    /// macro-expanded definitions, and accumulate them in `imported` (with
    /// `qualified` recording each imported pub fn as `(alias, name)`). `visited`
    /// dedups by canonical path and breaks cycles.
    fn collect_imports(
        &self,
        forms: &[Expr],
        base: &std::path::Path,
        visited: &mut HashSet<PathBuf>,
        imported: &mut Vec<Expr>,
        qualified: &mut Vec<(String, String)>,
    ) {
        for form in forms {
            let ExprKind::List(items) = &form.kind else {
                continue;
            };
            let Some(ExprKind::Symbol(head)) = items.first().map(|e| &e.kind) else {
                continue;
            };
            if head != "use" || items.len() < 2 {
                continue;
            }
            let Some(modpath) = items[1].as_dotted_path() else {
                continue;
            };
            // Alias: `[use a/b as c]` → c, else the last path segment.
            let alias = if items.len() >= 4 {
                if let (ExprKind::Symbol(kw), ExprKind::Symbol(a)) =
                    (&items[2].kind, &items[3].kind)
                {
                    if kw == "as" {
                        a.clone()
                    } else {
                        modpath.rsplit('/').next().unwrap_or(&modpath).to_string()
                    }
                } else {
                    modpath.rsplit('/').next().unwrap_or(&modpath).to_string()
                }
            } else {
                modpath.rsplit('/').next().unwrap_or(&modpath).to_string()
            };

            let file = crate::module::ModuleCache::resolve_path(&modpath, base);
            let canonical = file.canonicalize().unwrap_or_else(|_| file.clone());
            if !visited.insert(canonical) {
                continue; // already imported (or cycle)
            }
            let Ok(src) = std::fs::read_to_string(&file) else {
                continue;
            };
            let Ok(exprs) = crate::parser::parse(&src) else {
                continue;
            };
            let dir = file
                .parent()
                .map(|p| p.to_path_buf())
                .unwrap_or_else(|| base.to_path_buf());
            // Run a sub-checker to macro-expand (and type) the module.
            let mut sub = Checker::with_base_dir(&dir);
            let _ = sub.check_program(&exprs);
            let module_forms = sub.expanded_program.clone();
            // Recurse first so transitive imports land before this module.
            self.collect_imports(&module_forms, &dir, visited, imported, qualified);
            for mf in &module_forms {
                if let ExprKind::List(mitems) = &mf.kind {
                    match mitems.first().map(|e| &e.kind) {
                        // Skip the module's own `use` lines (handled by recursion).
                        Some(ExprKind::Symbol(s)) if s == "use" => continue,
                        _ => {}
                    }
                    // Record qualified names for `pub fn` exports.
                    if let Some(ExprKind::Symbol(s)) = mitems.first().map(|e| &e.kind) {
                        if s == "pub" && mitems.len() >= 3 {
                            if let (Some(ExprKind::Symbol(inner)), Some(ExprKind::Symbol(name))) = (
                                mitems.get(1).map(|e| &e.kind),
                                mitems.get(2).map(|e| &e.kind),
                            ) {
                                if inner == "fn" {
                                    qualified.push((alias.clone(), name.clone()));
                                }
                            }
                        }
                    }
                }
                imported.push(mf.clone());
            }
        }
    }

    fn collect_ctors(&mut self, args: &[Expr]) {
        if args.is_empty() {
            return;
        }
        for arg in &args[1..] {
            match &arg.kind {
                ExprKind::List(items) if !items.is_empty() => {
                    if let ExprKind::Symbol(name) = &items[0].kind {
                        if name.starts_with(char::is_uppercase) {
                            // Count fields (skip fn definitions and keyword fields)
                            // (arity is computed below via field_count)
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

                            let tag = self.next_tag;
                            self.next_tag += 1;
                            self.ctor_map.insert(name.clone(), (tag, total));
                            self.module.ctors.push(Ctor {
                                name: name.clone(),
                                tag,
                                arity: total,
                            });
                        }
                    }
                }
                ExprKind::Symbol(name) if name.starts_with(char::is_uppercase) => {
                    // Nullary constructor
                    let tag = self.next_tag;
                    self.next_tag += 1;
                    self.ctor_map.insert(name.clone(), (tag, 0));
                    self.module.ctors.push(Ctor {
                        name: name.clone(),
                        tag,
                        arity: 0,
                    });
                }
                _ => {} // type params etc
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
                    // A qualified imported function, e.g. `math.add` from
                    // `[use math]` — materialize it as a closure value.
                    if let Some(&fid) = self.func_map.get(&path) {
                        let r = self.reg();
                        self.emit(Op::Close(r, fid, Vec::new(), expr.span));
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
        // Check if it's a known function (enables cross-function references)
        if let Some(&fid) = self.func_map.get(name) {
            let r = self.reg();
            self.emit(Op::Close(r, fid, Vec::new(), span));
            return r;
        }
        // Check if it's an ADT constructor
        if let Some(&(tag, arity)) = self.ctor_map.get(name) {
            if arity == 0 {
                let r = self.reg();
                self.emit(Op::Adt(r, tag, Vec::new(), span));
                return r;
            }
        }
        // Check if it's a builtin — wrap as a closure for first-class use
        if let Some(built) = self.resolve_builtin(name) {
            // Create a wrapper function: [fn [x] [builtin x]]
            let saved_func = self.cur_func;
            let saved_block = self.cur_block;
            let saved_next_reg = self.next_reg;
            let saved_scopes = std::mem::take(&mut self.scopes);

            let func_id = self.begin_func(None, span);
            self.scopes = vec![HashMap::new()];
            // Single param
            let param = Reg(0);
            self.next_reg = 1;
            self.bind("__x", param);
            self.module.funcs[func_id.0 as usize].params = vec![Ty::Any];

            let result = self.reg();
            self.emit(Op::Builtin(result, built, vec![param], span));
            self.seal(End::Ret(result));

            self.cur_func = saved_func;
            self.cur_block = saved_block;
            self.next_reg = saved_next_reg;
            self.scopes = saved_scopes;

            let r = self.reg();
            self.emit(Op::Close(r, func_id, Vec::new(), span));
            return r;
        }

        // Fallback: emit as a string literal (will be resolved by the VM)
        let r = self.reg();
        let sid = self.intern(name);
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
                "use" => {
                    // Module loading: [use module] — stub for now
                    // TODO: resolve module, parse, type check, lower, bind exports
                    let r = self.reg();
                    self.emit(Op::Lit(r, Lit::Unit, span));
                    return r;
                }
                "effect" | "trait" | "sig" | "macro" | "macro+" | "macroexpand" => {
                    let r = self.reg();
                    self.emit(Op::Lit(r, Lit::Unit, span));
                    return r;
                }
                "test" => {
                    // [test name [params] body...] → treat as named fn
                    return self.lower_fn(&items[1..], span);
                }
                "impl" | "derive" | "inspect" | "catch-errors" => {
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

            // Multi-arity: [fn name (params1 body1) (params2 body2) ...]
            if matches!(args[1].kind, ExprKind::Tuple(_)) {
                return self.lower_multi_arity_fn(&name, &args[1..], span);
            }

            // Single-arity: get params
            let param_names = extract_param_names(&args[1]);
            let destructuring = extract_destructuring(&args[1]);

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

            // Use pre-created function stub (from forward ref pass) or create new
            let func_id = if let Some(&existing) = self.func_map.get(&name) {
                // Reuse pre-created stub — just set cur_func and reset state
                self.cur_func = Some(existing.0 as usize);
                self.next_reg = 0;
                // Create entry block if the stub doesn't have one
                let func = &self.module.funcs[existing.0 as usize];
                if func.blocks.is_empty() {
                    let entry = self.new_block();
                    self.switch_to(entry);
                } else {
                    self.switch_to(BlockId(0));
                }
                existing
            } else {
                let fid = self.begin_func(Some(&name), span);
                self.func_map.insert(name.clone(), fid);
                fid
            };

            // Set up param registers in scope
            self.scopes = vec![HashMap::new()];
            for (i, pname) in param_names.iter().enumerate() {
                let r = Reg(i as u32);
                self.next_reg = self.next_reg.max(i as u32 + 1);
                self.bind(pname, r);
            }
            self.module.funcs[func_id.0 as usize].params = vec![Ty::Any; param_names.len()];

            // Set block0 params for fn/recur support
            let param_regs: Vec<Reg> = (0..param_names.len()).map(|i| Reg(i as u32)).collect();
            let func_mut = &mut self.module.funcs[func_id.0 as usize];
            func_mut.blocks[0].params = param_regs;

            // Emit field extraction for destructured params
            for destr in &destructuring {
                let base = Reg(destr.param_idx as u32);
                for (j, field_name) in destr.fields.iter().enumerate() {
                    if field_name != "_" {
                        let field_reg = self.reg();
                        self.emit(Op::Field(field_reg, base, Selector::Index(j as u16), span));
                        self.bind(field_name, field_reg);
                    }
                }
            }

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

            // Bind function name in current scope (value form; calls still
            // resolve through func_map to a direct Call)
            let r = self.reg();
            self.emit(Op::Close(r, func_id, Vec::new(), span));
            self.bind_fn(&name, r);
            r
        } else {
            // Anonymous lambda: [fn [params] body...]
            let param_names = extract_param_names(&args[0]);
            let destructuring = extract_destructuring(&args[0]);
            let body = &args[1..];

            // Collect free variables before switching scopes
            let mut locals = std::collections::HashSet::new();
            for p in &param_names {
                locals.insert(p.clone());
            }
            // Also add destructured field names as locals
            for destr in &destructuring {
                for field_name in &destr.fields {
                    locals.insert(field_name.clone());
                }
            }
            let ctor_map_ref = &self.ctor_map;
            let func_map_ref = &self.func_map;
            // A name counts as a builtin/operator only when it is NOT shadowed
            // by a lexical binding in scope: a shadowed name must be collected
            // as a free variable so the closure captures the local, not the
            // builtin.
            let resolve = |name: &str| -> bool {
                self.lookup_local(name).is_none()
                    && (self.resolve_builtin(name).is_some() || is_operator(name))
            };
            let mut free_vars = Vec::new();
            let mut seen = std::collections::HashSet::new();
            for expr in body {
                collect_free_vars(
                    expr,
                    &locals,
                    &resolve,
                    ctor_map_ref,
                    func_map_ref,
                    &mut free_vars,
                    &mut seen,
                );
            }

            // Resolve free vars to registers in the outer scope
            let mut capture_regs = Vec::new();
            let mut capture_names = Vec::new();
            for name in &free_vars {
                if let Some(r) = self.lookup(name) {
                    capture_regs.push(r);
                    capture_names.push(name.clone());
                }
            }

            let saved_func = self.cur_func;
            let saved_block = self.cur_block;
            let saved_next_reg = self.next_reg;
            let saved_scopes = std::mem::take(&mut self.scopes);

            let func_id = self.begin_func(None, span);
            self.module.funcs[func_id.0 as usize].is_closure = !capture_names.is_empty();

            self.scopes = vec![HashMap::new()];
            for (i, pname) in param_names.iter().enumerate() {
                let r = Reg(i as u32);
                self.next_reg = self.next_reg.max(i as u32 + 1);
                self.bind(pname, r);
            }
            self.module.funcs[func_id.0 as usize].params = vec![Ty::Any; param_names.len()];

            // Emit upvalue loads for captured variables
            for (idx, cap_name) in capture_names.iter().enumerate() {
                let r = self.reg();
                self.emit(Op::Upval(r, idx as u16, span));
                self.bind(cap_name, r);
            }

            // Emit field extraction for destructured params
            for destr in &destructuring {
                let base = Reg(destr.param_idx as u32);
                for (j, field_name) in destr.fields.iter().enumerate() {
                    if field_name != "_" {
                        let field_reg = self.reg();
                        self.emit(Op::Field(field_reg, base, Selector::Index(j as u16), span));
                        self.bind(field_name, field_reg);
                    }
                }
            }

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
            self.emit(Op::Close(r, func_id, capture_regs, span));
            r
        }
    }

    /// Multi-arity: [fn name (params1 body1) (params2 body2) ...]
    /// Compiled as a single function that checks arg count and dispatches.
    fn lower_multi_arity_fn(&mut self, name: &str, clauses: &[Expr], span: Span) -> Reg {
        // For simplicity, use the max arity and dispatch on arg count.
        // Extract all clauses.
        let mut parsed: Vec<(Vec<String>, Vec<&Expr>)> = Vec::new();
        for clause in clauses {
            if let ExprKind::Tuple(items) = &clause.kind {
                if items.len() >= 2 {
                    let params = extract_param_names(&items[0]);
                    let body: Vec<&Expr> = items[1..].iter().collect();
                    parsed.push((params, body));
                }
            }
        }
        if parsed.is_empty() {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        let max_arity = parsed.iter().map(|(p, _)| p.len()).max().unwrap_or(0);

        // Save state
        let saved_func = self.cur_func;
        let saved_block = self.cur_block;
        let saved_next_reg = self.next_reg;
        let saved_scopes = std::mem::take(&mut self.scopes);

        let func_id = self.begin_func(Some(name), span);
        self.func_map.insert(name.to_string(), func_id);

        self.scopes = vec![HashMap::new()];
        // All params accessible by position
        for i in 0..max_arity {
            let _r = Reg(i as u32);
            self.next_reg = self.next_reg.max(i as u32 + 1);
        }
        self.module.funcs[func_id.0 as usize].params = vec![Ty::Any; max_arity];
        let param_regs: Vec<Reg> = (0..max_arity).map(|i| Reg(i as u32)).collect();
        self.module.funcs[func_id.0 as usize].blocks[0].params = param_regs;

        // For now, just use the first matching clause based on a simple heuristic.
        // A full impl would check a special "argc" register.
        // Since Loon dispatches on arity, we lower each clause as an if-else chain.
        // Use the LAST clause as default (usually the most-args variant).
        // For 2 clauses (common case), just pick based on whether arg[1] is Unit.
        if parsed.len() == 1 {
            let (params, body) = &parsed[0];
            for (i, pname) in params.iter().enumerate() {
                self.bind(pname, Reg(i as u32));
            }
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
        } else {
            // Multi-clause: dispatch on arity (ascending order).
            // Each clause gets a body block. Arity checks are a linear chain.
            let mut sorted = parsed.clone();
            sorted.sort_by_key(|(p, _)| p.len());

            let merge_block = self.new_block();
            let mut body_blocks = Vec::new();
            for _ in &sorted {
                body_blocks.push(self.new_block());
            }

            // Emit arity dispatch chain in entry block (block 0)
            for i in 0..sorted.len() {
                if i < sorted.len() - 1 {
                    let arity = sorted[i].0.len();
                    let check_reg = Reg(arity as u32);
                    let is_unit = self.reg();
                    let unit_val = self.reg();
                    self.emit(Op::Lit(unit_val, Lit::Unit, span));
                    self.emit(Op::Bin(is_unit, BinOp::Eq, check_reg, unit_val, span));
                    let next_check = self.new_block();
                    self.seal(End::Br(is_unit, body_blocks[i], next_check));
                    self.switch_to(next_check);
                } else {
                    // Last clause: unconditional
                    self.seal(End::Jmp(body_blocks[i], Vec::new()));
                }
            }

            // Lower each clause body in its own block
            for (i, (params, body)) in sorted.iter().enumerate() {
                self.switch_to(body_blocks[i]);
                self.push_scope();
                for (j, pname) in params.iter().enumerate() {
                    self.bind(pname, Reg(j as u32));
                }
                let mut last = None;
                for expr in body {
                    last = Some(self.lower_expr(expr));
                }
                let ret = last.unwrap_or_else(|| {
                    let r = self.reg();
                    self.emit(Op::Lit(r, Lit::Unit, span));
                    r
                });
                self.pop_scope();
                self.seal(End::Jmp(merge_block, vec![ret]));
            }

            self.switch_to(merge_block);
            let result = self.reg();
            self.module.funcs[func_id.0 as usize].blocks[merge_block.0 as usize]
                .params
                .push(result);
            self.seal(End::Ret(result));
        }

        // Restore state
        self.cur_func = saved_func;
        self.cur_block = saved_block;
        self.next_reg = saved_next_reg;
        self.scopes = saved_scopes;

        let r = self.reg();
        self.emit(Op::Close(r, func_id, Vec::new(), span));
        self.bind_fn(name, r);
        r
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

        // Parse arms into (pattern, guard, body) triples
        struct Arm<'a> {
            pattern: &'a Expr,
            guard: Option<&'a Expr>,
            body: &'a Expr,
        }
        let raw = &args[1..];
        let mut parsed_arms: Vec<Arm> = Vec::new();
        let mut i = 0;
        while i < raw.len() {
            if i + 1 >= raw.len() {
                break;
            }
            // Check for guard: pattern [when guard] body
            if i + 2 < raw.len() {
                if let ExprKind::List(gf) = &raw[i + 1].kind {
                    if !gf.is_empty() {
                        if let ExprKind::Symbol(s) = &gf[0].kind {
                            if s == "when" && gf.len() > 1 {
                                parsed_arms.push(Arm {
                                    pattern: &raw[i],
                                    guard: Some(&gf[1]),
                                    body: &raw[i + 2],
                                });
                                i += 3;
                                continue;
                            }
                        }
                    }
                }
            }
            parsed_arms.push(Arm {
                pattern: &raw[i],
                guard: None,
                body: &raw[i + 1],
            });
            i += 2;
        }

        if parsed_arms.is_empty() {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        // Linear scan: for each arm, generate a test + body block.
        // test_i: if pattern matches → body_i, else → test_{i+1}
        let mut test_blocks = Vec::new();
        let mut body_blocks = Vec::new();
        for _ in &parsed_arms {
            test_blocks.push(self.new_block());
            body_blocks.push(self.new_block());
        }
        // Default: jump to merge with Unit
        let default_block = self.new_block();

        // Jump to first test
        self.seal(End::Jmp(test_blocks[0], Vec::new()));

        for (idx, arm) in parsed_arms.iter().enumerate() {
            let test_b = test_blocks[idx];
            let body_b = body_blocks[idx];
            let next = if idx + 1 < test_blocks.len() {
                test_blocks[idx + 1]
            } else {
                default_block
            };

            // Test block: check if pattern matches
            self.switch_to(test_b);
            let matches = self.compile_pattern_test(arm.pattern, scrutinee, span);

            if let Some(cond) = matches {
                // Check guard if present
                if let Some(guard_expr) = arm.guard {
                    // pattern matches → check guard
                    let guard_block = self.new_block();
                    self.seal(End::Br(cond, guard_block, next));
                    self.switch_to(guard_block);
                    self.push_scope();
                    self.bind_pattern(arm.pattern, scrutinee);
                    let guard_val = self.lower_expr(guard_expr);
                    self.pop_scope();
                    self.seal(End::Br(guard_val, body_b, next));
                } else {
                    self.seal(End::Br(cond, body_b, next));
                }
            } else {
                // Always matches (variable/wildcard pattern)
                self.seal(End::Jmp(body_b, Vec::new()));
            }

            // Body block: bind pattern vars, evaluate body, jump to merge
            self.switch_to(body_b);
            self.push_scope();
            self.bind_pattern(arm.pattern, scrutinee);
            let val = self.lower_expr(arm.body);
            self.pop_scope();
            self.seal(End::Jmp(merge_block, vec![val]));
        }

        // Default block
        self.switch_to(default_block);
        let unit = self.reg();
        self.emit(Op::Lit(unit, Lit::Unit, span));
        self.seal(End::Jmp(merge_block, vec![unit]));

        // Merge block
        self.switch_to(merge_block);
        let result = self.reg();
        let func = &mut self.module.funcs[self.cur_func.unwrap()];
        func.blocks[merge_block.0 as usize].params.push(result);
        result
    }

    /// Compile a pattern test — returns Some(cond_reg) if the pattern
    /// needs a runtime check, None if it always matches.
    fn compile_pattern_test(&mut self, pattern: &Expr, scrutinee: Reg, span: Span) -> Option<Reg> {
        match &pattern.kind {
            // Wildcard — always matches
            ExprKind::Symbol(s) if s == "_" => None,

            // Variable — always matches (binds in body)
            ExprKind::Symbol(s) if !s.starts_with(char::is_uppercase) => {
                // Check if it's a literal keyword like :done
                if s.starts_with(':') {
                    let sid = self.intern(s);
                    let lit = self.reg();
                    self.emit(Op::Lit(lit, Lit::Keyword(sid), span));
                    let cond = self.reg();
                    self.emit(Op::Bin(cond, BinOp::Eq, scrutinee, lit, span));
                    Some(cond)
                } else {
                    None // variable binding — always matches
                }
            }

            // Nullary constructor (e.g., Point, None)
            ExprKind::Symbol(s) if s.starts_with(char::is_uppercase) => {
                if let Some(&(tag, _)) = self.ctor_map.get(s.as_str()) {
                    let stag = self.reg();
                    self.emit(Op::Tag(stag, scrutinee, span));
                    let expected = self.reg();
                    self.emit(Op::Lit(expected, Lit::Int(tag as i64), span));
                    let cond = self.reg();
                    self.emit(Op::Bin(cond, BinOp::Eq, stag, expected, span));
                    Some(cond)
                } else {
                    None
                }
            }

            // Int literal
            ExprKind::Int(n) => {
                let lit = self.reg();
                self.emit(Op::Lit(lit, Lit::Int(*n), span));
                let cond = self.reg();
                self.emit(Op::Bin(cond, BinOp::Eq, scrutinee, lit, span));
                Some(cond)
            }

            // Float literal
            ExprKind::Float(f) => {
                let lit = self.reg();
                self.emit(Op::Lit(lit, Lit::Float(*f), span));
                let cond = self.reg();
                self.emit(Op::Bin(cond, BinOp::Eq, scrutinee, lit, span));
                Some(cond)
            }

            // Bool literal
            ExprKind::Bool(b) => {
                let lit = self.reg();
                self.emit(Op::Lit(lit, Lit::Bool(*b), span));
                let cond = self.reg();
                self.emit(Op::Bin(cond, BinOp::Eq, scrutinee, lit, span));
                Some(cond)
            }

            // String literal
            ExprKind::Str(s) => {
                let sid = self.intern(s);
                let lit = self.reg();
                self.emit(Op::Lit(lit, Lit::Str(sid), span));
                let cond = self.reg();
                self.emit(Op::Bin(cond, BinOp::Eq, scrutinee, lit, span));
                Some(cond)
            }

            // Keyword literal
            ExprKind::Keyword(k) => {
                let sid = self.intern(k);
                let lit = self.reg();
                self.emit(Op::Lit(lit, Lit::Keyword(sid), span));
                let cond = self.reg();
                self.emit(Op::Bin(cond, BinOp::Eq, scrutinee, lit, span));
                Some(cond)
            }

            // Constructor pattern [Ctor field1 field2 ...]
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(ctor) = &items[0].kind {
                    if let Some(&(tag, _)) = self.ctor_map.get(ctor.as_str()) {
                        let stag = self.reg();
                        self.emit(Op::Tag(stag, scrutinee, span));
                        let expected = self.reg();
                        self.emit(Op::Lit(expected, Lit::Int(tag as i64), span));
                        let cond = self.reg();
                        self.emit(Op::Bin(cond, BinOp::Eq, stag, expected, span));
                        return Some(cond);
                    }
                }
                // Expression-based guard (e.g., [> x 0])
                None
            }

            _ => None,
        }
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
            // Build a synthetic call expression: [step_fn step_args... current_val]
            // and lower it through lower_call which handles builtins.
            // We already have `current` as a Reg, so we pass it directly.
            match &step.kind {
                ExprKind::List(items) if !items.is_empty() => {
                    // Check if head is a known builtin or function
                    let head = &items[0];
                    let explicit_args: Vec<Reg> =
                        items[1..].iter().map(|e| self.lower_expr(e)).collect();

                    // Try builtin recognition (thread-last: append current as last arg)
                    if let ExprKind::Symbol(name) = &head.kind {
                        // Local bindings shadow builtins here too.
                        if let Some(local) = self.lookup_local(name) {
                            let mut all_args = explicit_args;
                            all_args.push(current);
                            let r = self.reg();
                            self.emit(Op::Invoke(r, local, all_args, step.span));
                            current = r;
                            continue;
                        }
                        if let Some(built) = self.resolve_builtin(name) {
                            let mut all_args = explicit_args;
                            all_args.push(current);
                            let r = self.reg();
                            self.emit(Op::Builtin(r, built, all_args, step.span));
                            current = r;
                            continue;
                        }
                        // Operator partial step, e.g. [pipe 5 [+ 1]] → [+ 1 5]
                        // (thread-last). The interpreter supports this because
                        // operators are ordinary env functions there, and its
                        // semantics are what we mirror:
                        // - `+`/`*` are VARIADIC left folds: [pipe 5 [+ 1 2]]
                        //   is [+ 1 2 5] = 8.
                        // - the rest are strictly BINARY and ignore extra
                        //   arguments: [pipe 5 [- 10 3]] is [- 10 3 5] = 7
                        //   (the piped value is arg 3 and is dropped).
                        if !explicit_args.is_empty() {
                            let variadic = match name.as_str() {
                                "+" => Some(BinOp::Add),
                                "*" => Some(BinOp::Mul),
                                _ => None,
                            };
                            if let Some(binop) = variadic {
                                let mut acc = explicit_args[0];
                                for &a in explicit_args[1..].iter().chain([&current]) {
                                    let r = self.reg();
                                    self.emit(Op::Bin(r, binop, acc, a, step.span));
                                    acc = r;
                                }
                                current = acc;
                                continue;
                            }
                            if let Some(binop) = match name.as_str() {
                                "-" => Some(BinOp::Sub),
                                "/" => Some(BinOp::Div),
                                "%" => Some(BinOp::Rem),
                                "=" => Some(BinOp::Eq),
                                "!=" => Some(BinOp::Ne),
                                "<" => Some(BinOp::Lt),
                                ">" => Some(BinOp::Gt),
                                "<=" => Some(BinOp::Le),
                                ">=" => Some(BinOp::Ge),
                                _ => None,
                            } {
                                // Binary: [op e0 (e1 | current)] — any further
                                // args were still evaluated above, as interp.
                                let rhs = explicit_args.get(1).copied().unwrap_or(current);
                                let r = self.reg();
                                self.emit(Op::Bin(r, binop, explicit_args[0], rhs, step.span));
                                current = r;
                                continue;
                            }
                        }
                    }

                    // General case: indirect call
                    let func = self.lower_expr(head);
                    let mut call_args = explicit_args;
                    call_args.push(current);
                    let r = self.reg();
                    self.emit(Op::Invoke(r, func, call_args, step.span));
                    current = r;
                }
                ExprKind::Symbol(name) => {
                    // Single symbol step: [pipe x f] → [f x]
                    if let Some(local) = self.lookup_local(name) {
                        let r = self.reg();
                        self.emit(Op::Invoke(r, local, vec![current], step.span));
                        current = r;
                    } else if let Some(built) = self.resolve_builtin(name) {
                        let r = self.reg();
                        self.emit(Op::Builtin(r, built, vec![current], step.span));
                        current = r;
                    } else if let Some(&fid) = self.func_map.get(name.as_str()) {
                        let r = self.reg();
                        self.emit(Op::Call(r, fid, vec![current], step.span));
                        current = r;
                    } else {
                        let func = self.lower_expr(step);
                        let r = self.reg();
                        self.emit(Op::Invoke(r, func, vec![current], step.span));
                        current = r;
                    }
                }
                _ => {
                    // A step that is neither a list nor a symbol (e.g. a bare
                    // literal). The interpreter rejects these at runtime
                    // ("pipe step must be a list or symbol"); lowering it as
                    // an invocation of a non-callable value gives the same
                    // error CLASS at the same point instead of silently
                    // dropping the step (which produced a wrong value).
                    let func = self.lower_expr(step);
                    let r = self.reg();
                    self.emit(Op::Invoke(r, func, vec![current], step.span));
                    current = r;
                }
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

        // Set recur target so [recur ...] jumps back to loop_block
        let saved_recur = self.recur_target;
        self.recur_target = Some(loop_block);

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
        self.recur_target = saved_recur;

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

        if let Some(target) = self.recur_target {
            // Loop recur: jump back to loop header
            self.seal(End::Jmp(target, vals));
        } else {
            // Function recur: self-call to block 0
            self.seal(End::Recur(vals));
        }

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

        // Optional return clause: [return x] expr — a transformer applied to the
        // body's NORMAL-completion value (an aborting handler bypasses it). It's
        // baked into the body thunk below (bind x = body, then evaluate expr) so
        // that an abort, which never finishes the body, never runs it. The
        // handler loop ignores this clause (not a DotAccess). This is what makes
        // the function-passing State encoding expressible.
        let mut return_clause: Option<(String, Expr)> = None;
        {
            let mut j = 0;
            while j + 1 < handler_args.len() {
                if let ExprKind::List(p) = &handler_args[j].kind {
                    if let Some(ExprKind::Symbol(s)) = p.first().map(|e| &e.kind) {
                        if s == "return" {
                            let xname = match p.get(1).map(|e| &e.kind) {
                                Some(ExprKind::Symbol(n)) => n.clone(),
                                _ => "_".to_string(),
                            };
                            return_clause = Some((xname, handler_args[j + 1].clone()));
                        }
                    }
                }
                j += 2;
            }
        }

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

                            // Collect free variables of the clause body so the
                            // handler closure captures enclosing locals (e.g. a
                            // parameterized handler that uses an outer binding).
                            // Locals are the op params plus the implicit `resume`.
                            let mut locals = std::collections::HashSet::new();
                            for p in &param_names {
                                locals.insert(p.clone());
                            }
                            locals.insert("resume".to_string());
                            let mut capture_regs = Vec::new();
                            let mut capture_names = Vec::new();
                            {
                                let ctor_map_ref = &self.ctor_map;
                                let func_map_ref = &self.func_map;
                                // As in lower_fn: a lexically shadowed name is
                                // a free var to capture, not a builtin.
                                let resolve = |name: &str| -> bool {
                                    self.lookup_local(name).is_none()
                                        && (self.resolve_builtin(name).is_some()
                                            || is_operator(name))
                                };
                                let mut free_vars = Vec::new();
                                let mut seen = std::collections::HashSet::new();
                                collect_free_vars(
                                    &handler_args[i + 1],
                                    &locals,
                                    &resolve,
                                    ctor_map_ref,
                                    func_map_ref,
                                    &mut free_vars,
                                    &mut seen,
                                );
                                for name in &free_vars {
                                    if let Some(r) = self.lookup(name) {
                                        capture_regs.push(r);
                                        capture_names.push(name.clone());
                                    }
                                }
                            }

                            let saved_func = self.cur_func;
                            let saved_block = self.cur_block;
                            let saved_next_reg = self.next_reg;
                            let saved_scopes = std::mem::take(&mut self.scopes);

                            let handler_fn_id = self.begin_func(None, handler_args[i + 1].span);
                            self.module.funcs[handler_fn_id.0 as usize].is_closure =
                                !capture_names.is_empty();

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

                            // Load captured upvalues into scope.
                            for (idx, cap_name) in capture_names.iter().enumerate() {
                                let r = self.reg();
                                self.emit(Op::Upval(r, idx as u16, span));
                                self.bind(cap_name, r);
                            }

                            let handler_body = &handler_args[i + 1];
                            let val = self.lower_expr(handler_body);
                            // Tail-resume peephole (the one-shot fast path): a
                            // clause that ends by returning `[resume ...]`'s
                            // result is a tail resume. Sealing it as TailInvoke
                            // lets the VM splice the continuation WITHOUT
                            // pushing the handler frame as a fresh prompt, so a
                            // perform/resume loop runs in O(1) stack instead of
                            // leaking a frame per iteration (O(N^2) time — the
                            // 10GB blowups the first os/ demos hit). Scoped to
                            // `resume` only: End::TailInvoke is correct on the
                            // VM but not on the native backend, and
                            // handler-using programs never compile natively.
                            let resume_binding = self.lookup("resume");
                            let mut sealed_tail = false;
                            if let (Some(resume_reg), Some(cf), Some(cb)) =
                                (resume_binding, self.cur_func, self.cur_block)
                            {
                                let block = &mut self.module.funcs[cf].blocks[cb.0 as usize];
                                if let Some(Op::Invoke(dst, callee, args, _)) = block.ops.last() {
                                    if *dst == val && *callee == resume_reg {
                                        let (callee, args) = (*callee, args.clone());
                                        block.ops.pop();
                                        block.end = End::TailInvoke(callee, args);
                                        sealed_tail = true;
                                    }
                                }
                            }
                            if !sealed_tail {
                                self.seal(End::Ret(val));
                            }

                            self.cur_func = saved_func;
                            self.cur_block = saved_block;
                            self.next_reg = saved_next_reg;
                            self.scopes = saved_scopes;

                            // Create closure for handler (capturing enclosing locals)
                            let handler_reg = self.reg();
                            self.emit(Op::Close(handler_reg, handler_fn_id, capture_regs, span));

                            // Install as dynamic handler (accessible to called functions)
                            let eff_id = self.intern(effect);
                            let op_id = self.intern(op);
                            self.emit(Op::PushHandler(handler_reg, eff_id, op_id, span));

                            // Also bind as evidence for direct use in body
                            let key = format!("{effect}.{op}");
                            self.evidence_scope.insert(key, handler_reg);
                        }
                    }
                }
            }
            i += 2;
        }

        // Count handlers to pop
        let handler_count = self.evidence_scope.len() - saved_evidence.len();

        // Lower the body as a zero-arg thunk and invoke it, so the handler
        // delimits a clean frame boundary (a "prompt"): the continuation the VM
        // captures at a `perform` is exactly the frames between that perform and
        // this invoke. Evidence (direct-call handler regs) lives in THIS frame
        // and can't cross into the thunk, so clear it for the body — performs in
        // the body go through the dynamic handler stack (PushHandler above is
        // VM-global and works across frames + the thunk boundary).
        let body_evidence = std::mem::take(&mut self.evidence_scope);
        // The thunk's body is the handle body, with the return clause (if any)
        // baked into its normal-completion path: [fn [] [let x BODY] RETURN].
        let thunk_body: Vec<Expr> = match &return_clause {
            Some((xname, rexpr)) => vec![
                Expr::new(
                    ExprKind::List(vec![
                        Expr::new(ExprKind::Symbol("let".to_string()), span),
                        Expr::new(ExprKind::Symbol(xname.clone()), span),
                        body.clone(),
                    ]),
                    span,
                ),
                rexpr.clone(),
            ],
            None => vec![body.clone()],
        };
        let mut thunk_items = vec![
            Expr::new(ExprKind::Symbol("fn".to_string()), span),
            Expr::new(ExprKind::List(Vec::new()), span),
        ];
        thunk_items.extend(thunk_body);
        let thunk_expr = Expr::new(ExprKind::List(thunk_items), span);
        let thunk_reg = self.lower_expr(&thunk_expr);
        let result = self.reg();
        self.emit(Op::Invoke(result, thunk_reg, Vec::new(), span));
        self.evidence_scope = body_evidence;

        // Pop dynamic handlers
        for _ in 0..handler_count {
            self.emit(Op::PopHandler(span));
        }

        // Restore evidence scope
        self.evidence_scope = saved_evidence;
        result
    }

    fn lower_try(&mut self, args: &[Expr], span: Span) -> Reg {
        // [try body on-fail] → desugar to:
        //   [handle body [Fail.fail msg] [on-fail msg]]
        if args.is_empty() {
            let r = self.reg();
            self.emit(Op::Lit(r, Lit::Unit, span));
            return r;
        }

        let body = &args[0];
        // The on-fail handler is the SECOND arg, matching the tree-walking
        // interpreter (which reads args[1]). For the canonical two-arg form
        // this is also the last arg; taking args[1] specifically keeps the two
        // backends in agreement even when extra args are present.
        let handler_expr = args.get(1);

        if let Some(on_fail) = handler_expr {
            // Desugar through lower_handle so the Fail clause gets the same
            // machinery as any handler clause — in particular free-variable
            // capture (a hand-rolled lowering forgot captures, so on-fail
            // closures over enclosing locals — the supervision retry pattern —
            // mis-resolved them).
            //
            //   [try BODY ON-FAIL] → [handle BODY [Fail.fail <msg>] [<fn> <msg>]]
            //
            // ON-FAIL is lowered EAGERLY here in the enclosing scope and bound
            // to a gensym, then applied inside the clause. This matters for
            // hygiene and evaluation order:
            //   - lowering it out here (not inside the clause) means the
            //     handler's implicit `resume` and the message binding are NOT
            //     in scope while the user's ON-FAIL is compiled, so an ON-FAIL
            //     that references an enclosing `resume` (or the gensym) resolves
            //     to the user's binding, not the injected one;
            //   - it evaluates once, before the body, like the interpreter —
            //     so a side-effecting handler-producing expression runs on the
            //     success path too;
            //   - putting a gensym VALUE (not ON-FAIL itself) in head position
            //     means a bare-symbol or computed handler is applied correctly.
            let on_fail_reg = self.lower_expr(on_fail);
            let fn_name = self.fresh_name("try-onfail");
            let msg_name = self.fresh_name("try-msg");
            self.bind(&fn_name, on_fail_reg);
            let pattern = Expr::new(
                ExprKind::List(vec![
                    Expr::new(
                        ExprKind::DotAccess(
                            Box::new(Expr::new(ExprKind::Symbol("Fail".to_string()), span)),
                            "fail".to_string(),
                        ),
                        span,
                    ),
                    Expr::new(ExprKind::Symbol(msg_name.clone()), span),
                ]),
                span,
            );
            let clause_body = Expr::new(
                ExprKind::List(vec![
                    Expr::new(ExprKind::Symbol(fn_name), span),
                    Expr::new(ExprKind::Symbol(msg_name), span),
                ]),
                span,
            );
            self.lower_handle(&[body.clone(), pattern, clause_body], span)
        } else {
            self.lower_expr(body)
        }
    }

    fn lower_call(&mut self, items: &[Expr], span: Span) -> Reg {
        let head = &items[0];
        let arg_exprs = &items[1..];

        // Check if it's a known function
        if let ExprKind::Symbol(name) = &head.kind {
            // A LOCAL binding (let / fn param / pattern binding) shadows
            // operators, builtins, constructors, and top-level functions at
            // call sites, just like the interpreter (where builtins live in
            // the same env as locals and inner scopes win). Without this
            // check `[let + my-fn] [+ 3 4]` silently called the builtin `+`,
            // and `[let Some inc] [Some 5]` constructed the ADT. This must
            // run BEFORE the ctor check: the prelude Option/Result ctors are
            // always registered, so ctor-first would make `Some`/`None`/`Ok`/
            // `Err` unshadowable on this backend only.
            if let Some(local) = self.lookup_local(name) {
                let args: Vec<Reg> = arg_exprs.iter().map(|e| self.lower_expr(e)).collect();
                let r = self.reg();
                self.emit(Op::Invoke(r, local, args, span));
                return r;
            }

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
                    // "str" is handled as Built::Str (variadic), not BinOp
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

            // Check for a user-defined function FIRST, so a [fn name …] shadows
            // a builtin of the same name (e.g. defining `sum` overrides the
            // builtin `sum`) rather than being silently ignored.
            if let Some(&func_id) = self.func_map.get(name.as_str()) {
                let args: Vec<Reg> = arg_exprs.iter().map(|e| self.lower_expr(e)).collect();
                let r = self.reg();
                self.emit(Op::Call(r, func_id, args, span));
                return r;
            }

            // Check for known builtins
            if let Some(built) = self.resolve_builtin(name) {
                let args: Vec<Reg> = arg_exprs.iter().map(|e| self.lower_expr(e)).collect();
                let r = self.reg();
                self.emit(Op::Builtin(r, built, args, span));
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
                                self.bind_fn(name, r);
                            }
                        }
                    }
                }
                ExprKind::Symbol(name) if name.starts_with(char::is_uppercase) => {
                    if let Some(&(tag, _)) = self.ctor_map.get(name.as_str()) {
                        let r = self.reg();
                        self.emit(Op::Adt(r, tag, Vec::new(), arg.span));
                        self.bind_fn(name, r);
                    }
                }
                _ => {}
            }
        }
    }
}

// ─── Helpers ───────────────────────────────────────────────────────────────

impl Lower<'_> {
    fn resolve_builtin(&self, name: &str) -> Option<Built> {
        match name {
            "println" => Some(Built::Println),
            "print" => Some(Built::Print),
            "str" => Some(Built::Str),
            "len" => Some(Built::Len),
            "get" => Some(Built::Get),
            "conj" => Some(Built::Conj),
            "cons" => Some(Built::Cons),
            "assoc" => Some(Built::Assoc),
            "merge" => Some(Built::Merge),
            "range" => Some(Built::Range),
            "map" => Some(Built::Map),
            "filter" => Some(Built::Filter),
            "reduce" => Some(Built::Reduce),
            "each" => Some(Built::Each),
            "flat-map" => Some(Built::FlatMap),
            "keys" => Some(Built::Keys),
            "vals" => Some(Built::Vals),
            "nth" => Some(Built::Nth),
            "take" => Some(Built::Take),
            "drop" => Some(Built::Drop),
            "contains?" => Some(Built::Contains),
            "join" => Some(Built::Join),
            "trim" => Some(Built::Trim),
            "split" => Some(Built::Split),
            "sort" => Some(Built::Sort),
            "reverse" => Some(Built::Reverse),
            "flatten" => Some(Built::Flatten),
            "zip" => Some(Built::Zip),
            "chunk" => Some(Built::Chunk),
            "any?" => Some(Built::Any),
            "all?" => Some(Built::All),
            "sum" => Some(Built::Sum),
            "min" => Some(Built::Min),
            "max" => Some(Built::Max),
            "int" => Some(Built::Int),
            "float" => Some(Built::Float),
            "into-map" => Some(Built::IntoMap),
            "group-by" => Some(Built::GroupBy),
            "collect" => Some(Built::Collect),
            "starts-with?" => Some(Built::StartsWith),
            "ends-with?" => Some(Built::EndsWith),
            "replace" => Some(Built::Replace),
            "uppercase" => Some(Built::Uppercase),
            "lowercase" => Some(Built::Lowercase),
            "index-of" => Some(Built::IndexOf),
            "char-at" => Some(Built::CharAt),
            "substring" => Some(Built::Substring),
            "not" => Some(Built::Not),
            "empty?" => Some(Built::Empty),
            "fold" => Some(Built::Fold),
            "update" => Some(Built::Update),
            "entries" => Some(Built::Entries),
            "sort-by" => Some(Built::SortBy),
            "unit" => Some(Built::Unit),
            "magnitude" => Some(Built::Magnitude),
            "or" => Some(Built::Or),
            "abs" => Some(Built::Abs),
            "first" => Some(Built::First),
            "last" => Some(Built::Last),
            "find" => Some(Built::Find),
            "keyword" => Some(Built::Keyword),
            "keywordize-keys" => Some(Built::KeywordizeKeys),
            "assert-eq" => Some(Built::AssertEq),
            "concat" => Some(Built::Concat),
            "slice" => Some(Built::Slice),
            "some?" => Some(Built::SomeP),
            "none?" => Some(Built::NoneP),
            "nil?" => Some(Built::NoneP),
            _ => None,
        }
    }
}

/// Destructuring info: (param_index, field_names).
struct Destr {
    param_idx: usize,
    fields: Vec<String>,
}

fn extract_param_names(expr: &Expr) -> Vec<String> {
    match &expr.kind {
        ExprKind::List(items) => items
            .iter()
            .enumerate()
            .map(|(i, e)| match &e.kind {
                ExprKind::Symbol(s) => s.clone(),
                ExprKind::List(_) => format!("__destr_{i}"),
                _ => format!("__param_{i}"),
            })
            .collect(),
        ExprKind::Symbol(s) => vec![s.clone()],
        _ => Vec::new(),
    }
}

/// Extract destructuring info from a param list expression.
fn extract_destructuring(expr: &Expr) -> Vec<Destr> {
    match &expr.kind {
        ExprKind::List(items) => items
            .iter()
            .enumerate()
            .filter_map(|(i, e)| {
                if let ExprKind::List(inner) = &e.kind {
                    let fields: Vec<String> = inner
                        .iter()
                        .filter_map(|f| {
                            if let ExprKind::Symbol(s) = &f.kind {
                                Some(s.clone())
                            } else {
                                None
                            }
                        })
                        .collect();
                    Some(Destr {
                        param_idx: i,
                        fields,
                    })
                } else {
                    None
                }
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// Collect free variable names referenced in an expression that are not
/// in the given `locals` set, builtins, or constructors.
fn collect_free_vars(
    expr: &Expr,
    locals: &std::collections::HashSet<String>,
    builtins: &dyn Fn(&str) -> bool,
    ctors: &HashMap<String, (u16, u16)>,
    func_map: &HashMap<String, FuncId>,
    out: &mut Vec<String>,
    seen: &mut std::collections::HashSet<String>,
) {
    match &expr.kind {
        ExprKind::Symbol(s) => {
            // Skip keywords, builtins, operators, ctors, known functions, wildcards
            if !s.starts_with(':')
                && !s.starts_with(char::is_uppercase)
                && s != "_"
                && s != "true"
                && s != "false"
                && !locals.contains(s)
                && !builtins(s)
                && !ctors.contains_key(s)
                && !func_map.contains_key(s)
                && !is_special_form(s)
                && !seen.contains(s)
            {
                seen.insert(s.clone());
                out.push(s.clone());
            }
        }
        ExprKind::List(items) => {
            if items.is_empty() {
                return;
            }
            // Check for special forms that introduce bindings
            if let ExprKind::Symbol(head) = &items[0].kind {
                match head.as_str() {
                    "fn" => {
                        // [fn name [params] body] or [fn [params] body]
                        // params are local, body is scanned with extended locals
                        if items.len() >= 3 {
                            let (params_expr, body_start) =
                                if matches!(&items[1].kind, ExprKind::Symbol(_)) {
                                    // named fn — name + params + body
                                    let mut locs = locals.clone();
                                    locs.insert(if let ExprKind::Symbol(n) = &items[1].kind {
                                        n.clone()
                                    } else {
                                        String::new()
                                    });
                                    if items.len() >= 4 {
                                        let pnames = extract_param_names(&items[2]);
                                        for p in &pnames {
                                            locs.insert(p.clone());
                                        }
                                        for e in &items[3..] {
                                            collect_free_vars(
                                                e, &locs, builtins, ctors, func_map, out, seen,
                                            );
                                        }
                                    }
                                    return;
                                } else {
                                    (&items[1], 2)
                                };
                            let pnames = extract_param_names(params_expr);
                            let mut locs = locals.clone();
                            for p in &pnames {
                                locs.insert(p.clone());
                            }
                            for e in &items[body_start..] {
                                collect_free_vars(e, &locs, builtins, ctors, func_map, out, seen);
                            }
                            return;
                        }
                    }
                    "let" => {
                        // [let name val body...] — name is bound after val
                        if items.len() >= 3 {
                            let start = if matches!(&items[1].kind, ExprKind::Symbol(s) if s == "mut")
                            {
                                // [let mut name val]
                                if items.len() >= 4 {
                                    collect_free_vars(
                                        &items[3], locals, builtins, ctors, func_map, out, seen,
                                    );
                                    let mut locs = locals.clone();
                                    if let ExprKind::Symbol(n) = &items[2].kind {
                                        locs.insert(n.clone());
                                    }
                                    return;
                                }
                                return;
                            } else {
                                1
                            };
                            collect_free_vars(
                                &items[start + 1],
                                locals,
                                builtins,
                                ctors,
                                func_map,
                                out,
                                seen,
                            );
                            let mut locs = locals.clone();
                            if let ExprKind::Symbol(n) = &items[start].kind {
                                locs.insert(n.clone());
                            }
                            // Scan remaining body with name in scope
                            for e in &items[(start + 2)..] {
                                collect_free_vars(e, &locs, builtins, ctors, func_map, out, seen);
                            }
                            return;
                        }
                    }
                    "loop" => {
                        // [loop [bindings...] body...]
                        if items.len() >= 2 {
                            let mut locs = locals.clone();
                            if let ExprKind::List(bindings) = &items[1].kind {
                                let mut j = 0;
                                while j + 1 < bindings.len() {
                                    collect_free_vars(
                                        &bindings[j + 1],
                                        &locs,
                                        builtins,
                                        ctors,
                                        func_map,
                                        out,
                                        seen,
                                    );
                                    if let ExprKind::Symbol(n) = &bindings[j].kind {
                                        locs.insert(n.clone());
                                    }
                                    j += 2;
                                }
                            }
                            for e in &items[2..] {
                                collect_free_vars(e, &locs, builtins, ctors, func_map, out, seen);
                            }
                            return;
                        }
                    }
                    _ => {}
                }
            }
            // Generic list: scan all children
            for item in items {
                collect_free_vars(item, locals, builtins, ctors, func_map, out, seen);
            }
        }
        ExprKind::Vec(items) | ExprKind::Set(items) | ExprKind::Tuple(items) => {
            for item in items {
                collect_free_vars(item, locals, builtins, ctors, func_map, out, seen);
            }
        }
        ExprKind::Map(pairs) => {
            for (k, v) in pairs {
                collect_free_vars(k, locals, builtins, ctors, func_map, out, seen);
                collect_free_vars(v, locals, builtins, ctors, func_map, out, seen);
            }
        }
        ExprKind::DotAccess(inner, _) => {
            collect_free_vars(inner, locals, builtins, ctors, func_map, out, seen);
        }
        _ => {} // literals, keywords, etc.
    }
}

pub(crate) fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "fn" | "let"
            | "if"
            | "when"
            | "do"
            | "match"
            | "pipe"
            | "loop"
            | "recur"
            | "handle"
            | "try"
            | "type"
            | "mut"
            | "set!"
            | "pub"
            | "effect"
            | "trait"
            | "sig"
            | "macro"
            | "macro+"
            | "macroexpand"
            | "impl"
            | "test"
            | "derive"
            | "inspect"
            | "catch-errors"
            | "use"
    )
}

pub(crate) fn is_operator(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "%" | "=" | "!=" | "<" | ">" | "<=" | ">=" | "and" | "or" | "not"
    )
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
