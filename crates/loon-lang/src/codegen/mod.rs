mod capture;
#[allow(clippy::vec_init_then_push)]
pub mod collections;
#[allow(clippy::vec_init_then_push)]
pub mod maps;
#[allow(clippy::vec_init_then_push)]
pub mod strings;

use crate::ast::{Expr, ExprKind};
use collections::CollectionsRuntime;
use maps::MapsRuntime;
use std::collections::HashMap;
use strings::StringRuntime;
use wasm_encoder::*;

/// Compile a Loon program to WASM bytes.
pub fn compile(exprs: &[Expr]) -> Result<Vec<u8>, String> {
    // Macro expansion phase
    let mut expander = crate::macros::MacroExpander::new();
    let expanded = expander.expand_program(exprs)?;

    let mut compiler = Compiler::new();
    compiler.compile_program(&expanded)?;
    compiler.tree_shake();
    Ok(compiler.finish())
}

/// Compile a Loon program with multi-file support.
pub fn compile_with_imports(exprs: &[Expr], base_dir: &std::path::Path) -> Result<Vec<u8>, String> {
    // Macro expansion phase
    let mut expander = crate::macros::MacroExpander::new();
    let expanded = expander.expand_program(exprs)?;

    let mut compiler = Compiler::new();
    compiler.base_dir = Some(base_dir.to_path_buf());
    compiler.compile_program(&expanded)?;
    compiler.tree_shake();
    Ok(compiler.finish())
}

#[derive(Clone)]
struct FnDef {
    func_idx: u32,
    #[allow(dead_code)]
    arity: usize,
    is_closure: bool,
}

#[derive(Clone, Debug)]
struct AdtInfo {
    #[allow(dead_code)]
    type_name: String,
    #[allow(dead_code)]
    constructors: Vec<(String, u32, usize)>,
}

const WASI_IMPORT_COUNT: u32 = 6;
const PRE_ALLOC_TYPES: u32 = 7;

struct Compiler {
    functions: Vec<FunctionBody>,
    /// Parallel to `functions`: the provisional func index assigned to each
    /// body at push time. Push order need not match index order (runtime
    /// helpers and closures are pushed lazily while a body is being compiled,
    /// but top-level fns are assigned lower indices up front), so `tree_shake`
    /// relocates everything to its final index using this mapping rather than
    /// assuming `position == index - import_count`.
    fn_indices: Vec<u32>,
    fn_map: HashMap<String, FnDef>,
    strings: Vec<(String, u32)>,
    string_offset: u32,
    next_fn_idx: u32,
    #[allow(dead_code)]
    import_count: u32,
    lambda_counter: u32,
    adt_constructors: HashMap<String, (u32, usize)>,
    #[allow(dead_code)]
    adt_types: Vec<AdtInfo>,
    table_entries: Vec<u32>,
    table_map: HashMap<u32, u32>,
    indirect_type_cache: HashMap<usize, u32>,
    type_count: u32,
    string_runtime: Option<StringRuntime>,
    collections_runtime: Option<CollectionsRuntime>,
    maps_runtime: Option<MapsRuntime>,
    split_idx: Option<u32>,
    base_dir: Option<std::path::PathBuf>,
    compiled_modules: std::collections::HashSet<std::path::PathBuf>,
    force_heap: bool,
    used_wasi_imports: Option<Vec<u32>>,
    /// Effect imports: "Effect.op" → import function index
    effect_imports: HashMap<String, u32>,
    /// Effect import definitions in order: (module_namespace, func_name, arity)
    effect_import_defs: Vec<(String, String, usize)>,
    /// Effect registry (populated from [effect ...] declarations)
    effect_registry: crate::effects::EffectRegistry,
    /// Function keys (`name` or `name#arity`) whose result is statically a
    /// string. Lets `println` pick the string-printing path for calls, since
    /// the untagged value model offers no runtime check.
    string_fns: std::collections::HashSet<String>,
    /// Distinct keyword literals interned to unique i64 ids (for `=` and use as
    /// enum-like tags). Ids start high to avoid colliding with small ints.
    keywords: HashMap<String, i64>,
}

struct FunctionBody {
    params: Vec<ValType>,
    results: Vec<ValType>,
    locals: Vec<ValType>,
    instructions: Vec<WasmInstruction>,
}

/// How a higher-order-function argument is invoked per element.
enum FnRepr {
    /// A named top-level function — called directly by index.
    Named(u32),
    /// A closure value held in a local — called via the table (env + args).
    Closure(u32),
}

/// A closure parameter: a plain binding or a positional destructuring pattern
/// (`[k v]`), where `None` entries (`_`) ignore that slot.
enum ClosureParam {
    Simple(String),
    Destructure(Vec<Option<String>>),
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
enum WasmInstruction {
    I64Const(i64),
    F64Const(f64),
    I64Add,
    I64Sub,
    I64Mul,
    I64GtS,
    I64LtS,
    I64LeS,
    I64GeS,
    I64Eqz,
    I64Eq,
    I64Ne,
    I64DivS,
    I64RemS,
    F64Add,
    F64Sub,
    F64Mul,
    LocalGet(u32),
    LocalSet(u32),
    LocalTee(u32),
    Call(u32),
    If(BlockType),
    Else,
    End,
    Drop,
    Return,
    I32Const(i32),
    I32Store(u32, u32),
    I32Store16(u32, u32),
    CallIndirect(u32),
    I64Store(u32, u32),
    I64Load(u32, u32),
    I32Load(u32, u32),
    GlobalGet(u32),
    GlobalSet(u32),
    I32WrapI64,
    I64ExtendI32U,
    I64ShrU,
    I64DivU,
    I64RemU,
    I64And,
    I64Or,
    I64Shl,
    I32Add,
    I32Load8U(u32, u32),
    I32Store8(u32, u32),
    I32Eq,
    I32Eqz,
    Block(BlockType),
    Loop(BlockType),
    Br(u32),
    BrIf(u32),
    BrTable(Vec<u32>, u32),
}

fn emit_instruction(f: &mut Function, instr: &WasmInstruction) {
    match instr {
        WasmInstruction::I64Const(n) => {
            f.instruction(&Instruction::I64Const(*n));
        }
        WasmInstruction::F64Const(n) => {
            f.instruction(&Instruction::F64Const(*n));
        }
        WasmInstruction::I64Add => {
            f.instruction(&Instruction::I64Add);
        }
        WasmInstruction::I64Sub => {
            f.instruction(&Instruction::I64Sub);
        }
        WasmInstruction::I64Mul => {
            f.instruction(&Instruction::I64Mul);
        }
        WasmInstruction::I64GtS => {
            f.instruction(&Instruction::I64GtS);
        }
        WasmInstruction::I64LtS => {
            f.instruction(&Instruction::I64LtS);
        }
        WasmInstruction::I64Eqz => {
            f.instruction(&Instruction::I64Eqz);
        }
        WasmInstruction::I64Eq => {
            f.instruction(&Instruction::I64Eq);
        }
        WasmInstruction::I64Ne => {
            f.instruction(&Instruction::I64Ne);
        }
        WasmInstruction::I64LeS => {
            f.instruction(&Instruction::I64LeS);
        }
        WasmInstruction::I64GeS => {
            f.instruction(&Instruction::I64GeS);
        }
        WasmInstruction::I64DivS => {
            f.instruction(&Instruction::I64DivS);
        }
        WasmInstruction::I64RemS => {
            f.instruction(&Instruction::I64RemS);
        }
        WasmInstruction::F64Add => {
            f.instruction(&Instruction::F64Add);
        }
        WasmInstruction::F64Sub => {
            f.instruction(&Instruction::F64Sub);
        }
        WasmInstruction::F64Mul => {
            f.instruction(&Instruction::F64Mul);
        }
        WasmInstruction::LocalGet(i) => {
            f.instruction(&Instruction::LocalGet(*i));
        }
        WasmInstruction::LocalSet(i) => {
            f.instruction(&Instruction::LocalSet(*i));
        }
        WasmInstruction::LocalTee(i) => {
            f.instruction(&Instruction::LocalTee(*i));
        }
        WasmInstruction::Call(i) => {
            f.instruction(&Instruction::Call(*i));
        }
        WasmInstruction::CallIndirect(ty) => {
            f.instruction(&Instruction::CallIndirect {
                type_index: *ty,
                table_index: 0,
            });
        }
        WasmInstruction::If(bt) => {
            f.instruction(&Instruction::If(*bt));
        }
        WasmInstruction::Else => {
            f.instruction(&Instruction::Else);
        }
        WasmInstruction::End => {
            f.instruction(&Instruction::End);
        }
        WasmInstruction::Drop => {
            f.instruction(&Instruction::Drop);
        }
        WasmInstruction::Return => {
            f.instruction(&Instruction::Return);
        }
        WasmInstruction::I32Const(n) => {
            f.instruction(&Instruction::I32Const(*n));
        }
        WasmInstruction::I32Store(a, o) => {
            f.instruction(&Instruction::I32Store(MemArg {
                offset: *o as u64,
                align: *a,
                memory_index: 0,
            }));
        }
        WasmInstruction::I32Store16(a, o) => {
            f.instruction(&Instruction::I32Store16(MemArg {
                offset: *o as u64,
                align: *a,
                memory_index: 0,
            }));
        }
        WasmInstruction::I64Store(a, o) => {
            f.instruction(&Instruction::I64Store(MemArg {
                offset: *o as u64,
                align: *a,
                memory_index: 0,
            }));
        }
        WasmInstruction::I64Load(a, o) => {
            f.instruction(&Instruction::I64Load(MemArg {
                offset: *o as u64,
                align: *a,
                memory_index: 0,
            }));
        }
        WasmInstruction::I32Load(a, o) => {
            f.instruction(&Instruction::I32Load(MemArg {
                offset: *o as u64,
                align: *a,
                memory_index: 0,
            }));
        }
        WasmInstruction::I32Load8U(a, o) => {
            f.instruction(&Instruction::I32Load8U(MemArg {
                offset: *o as u64,
                align: *a,
                memory_index: 0,
            }));
        }
        WasmInstruction::I32Store8(a, o) => {
            f.instruction(&Instruction::I32Store8(MemArg {
                offset: *o as u64,
                align: *a,
                memory_index: 0,
            }));
        }
        WasmInstruction::GlobalGet(i) => {
            f.instruction(&Instruction::GlobalGet(*i));
        }
        WasmInstruction::GlobalSet(i) => {
            f.instruction(&Instruction::GlobalSet(*i));
        }
        WasmInstruction::I32WrapI64 => {
            f.instruction(&Instruction::I32WrapI64);
        }
        WasmInstruction::I64ExtendI32U => {
            f.instruction(&Instruction::I64ExtendI32U);
        }
        WasmInstruction::I64ShrU => {
            f.instruction(&Instruction::I64ShrU);
        }
        WasmInstruction::I64DivU => {
            f.instruction(&Instruction::I64DivU);
        }
        WasmInstruction::I64RemU => {
            f.instruction(&Instruction::I64RemU);
        }
        WasmInstruction::I64And => {
            f.instruction(&Instruction::I64And);
        }
        WasmInstruction::I64Or => {
            f.instruction(&Instruction::I64Or);
        }
        WasmInstruction::I64Shl => {
            f.instruction(&Instruction::I64Shl);
        }
        WasmInstruction::I32Add => {
            f.instruction(&Instruction::I32Add);
        }
        WasmInstruction::I32Eq => {
            f.instruction(&Instruction::I32Eq);
        }
        WasmInstruction::I32Eqz => {
            f.instruction(&Instruction::I32Eqz);
        }
        WasmInstruction::Block(bt) => {
            f.instruction(&Instruction::Block(*bt));
        }
        WasmInstruction::Loop(bt) => {
            f.instruction(&Instruction::Loop(*bt));
        }
        WasmInstruction::Br(l) => {
            f.instruction(&Instruction::Br(*l));
        }
        WasmInstruction::BrIf(l) => {
            f.instruction(&Instruction::BrIf(*l));
        }
        WasmInstruction::BrTable(labels, default) => {
            f.instruction(&Instruction::BrTable(
                std::borrow::Cow::Borrowed(labels),
                *default,
            ));
        }
    }
}

impl Compiler {
    fn new() -> Self {
        Self {
            functions: Vec::new(),
            fn_indices: Vec::new(),
            fn_map: HashMap::new(),
            strings: Vec::new(),
            string_offset: 1024,
            next_fn_idx: WASI_IMPORT_COUNT,
            import_count: WASI_IMPORT_COUNT,
            lambda_counter: 0,
            adt_constructors: HashMap::new(),
            adt_types: Vec::new(),
            table_entries: Vec::new(),
            table_map: HashMap::new(),
            indirect_type_cache: HashMap::new(),
            type_count: PRE_ALLOC_TYPES,
            string_runtime: None,
            collections_runtime: None,
            maps_runtime: None,
            split_idx: None,
            base_dir: None,
            compiled_modules: std::collections::HashSet::new(),
            force_heap: false,
            used_wasi_imports: None,
            effect_imports: HashMap::new(),
            effect_import_defs: Vec::new(),
            effect_registry: crate::effects::EffectRegistry::new(),
            string_fns: std::collections::HashSet::new(),
            keywords: HashMap::new(),
        }
    }
    /// Intern a keyword to a stable, unique i64 id (high range to avoid
    /// colliding with ordinary integer values under structural `=`).
    fn intern_keyword(&mut self, kw: &str) -> i64 {
        if let Some(&id) = self.keywords.get(kw) {
            return id;
        }
        let id = 0x4000_0000_0000_0000_i64 + self.keywords.len() as i64;
        self.keywords.insert(kw.to_string(), id);
        id
    }
    fn ensure_in_table(&mut self, func_idx: u32) -> u32 {
        if let Some(&ti) = self.table_map.get(&func_idx) {
            return ti;
        }
        let ti = self.table_entries.len() as u32;
        self.table_entries.push(func_idx);
        self.table_map.insert(func_idx, ti);
        ti
    }
    /// Push a compiled function body, recording the provisional index it was
    /// assigned. Keeps `functions` and `fn_indices` in lockstep so `tree_shake`
    /// can relocate bodies to their final indices regardless of push order.
    fn push_function(&mut self, func_idx: u32, body: FunctionBody) {
        self.functions.push(body);
        self.fn_indices.push(func_idx);
    }
    fn ensure_string_runtime(&mut self) {
        if self.string_runtime.is_some() {
            return;
        }
        self.force_heap = true;
        let c = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(c, StringRuntime::gen_str_concat());
        let l = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(l, StringRuntime::gen_str_len());
        let e = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(e, StringRuntime::gen_str_eq());
        let sub = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(sub, StringRuntime::gen_str_substring());
        let its = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(its, StringRuntime::gen_int_to_str());
        let ts = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(ts, StringRuntime::gen_to_str(its));
        self.string_runtime = Some(StringRuntime {
            str_concat_idx: c,
            str_len_idx: l,
            str_eq_idx: e,
            str_substring_idx: sub,
            int_to_str_idx: its,
            to_str_idx: ts,
        });
    }
    fn ensure_collections_runtime(&mut self) {
        if self.collections_runtime.is_some() {
            return;
        }
        self.force_heap = true;
        let n = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(n, CollectionsRuntime::gen_vec_new());
        let p = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(p, CollectionsRuntime::gen_vec_push());
        let g = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(g, CollectionsRuntime::gen_vec_get());
        self.collections_runtime = Some(CollectionsRuntime {
            vec_new_idx: n,
            vec_push_idx: p,
            vec_get_idx: g,
        });
    }
    fn ensure_maps_runtime(&mut self) {
        if self.maps_runtime.is_some() {
            return;
        }
        self.ensure_collections_runtime();
        self.ensure_string_runtime();
        let cr = self.collections_runtime.clone().unwrap();
        let sr = self.string_runtime.clone().unwrap();
        let val_eq = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(val_eq, MapsRuntime::gen_val_eq(sr.str_eq_idx));
        let pair = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(pair, MapsRuntime::gen_pair(cr.vec_new_idx, cr.vec_push_idx));
        let map_get = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(map_get, MapsRuntime::gen_map_get(cr.vec_get_idx, val_eq));
        let map_assoc = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(
            map_assoc,
            MapsRuntime::gen_map_assoc(cr.vec_new_idx, cr.vec_push_idx, cr.vec_get_idx, val_eq, pair),
        );
        self.maps_runtime = Some(MapsRuntime {
            val_eq_idx: val_eq,
            pair_idx: pair,
            map_get_idx: map_get,
            map_assoc_idx: map_assoc,
        });
    }
    fn ensure_split_runtime(&mut self) -> u32 {
        if let Some(idx) = self.split_idx {
            return idx;
        }
        self.ensure_string_runtime();
        self.ensure_collections_runtime();
        let sr = self.string_runtime.clone().unwrap();
        let cr = self.collections_runtime.clone().unwrap();
        let idx = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(
            idx,
            StringRuntime::gen_split(sr.str_substring_idx, cr.vec_new_idx, cr.vec_push_idx),
        );
        self.split_idx = Some(idx);
        idx
    }
    /// Is this top-level form a runnable statement (as opposed to a definition
    /// like `fn`/`type`/`use`/`effect`)? Bare literals and non-definition lists
    /// count as statements.
    fn is_toplevel_statement(e: &Expr) -> bool {
        match &e.kind {
            ExprKind::List(items) => match items.first().map(|x| &x.kind) {
                Some(ExprKind::Symbol(s)) => {
                    // `test` registers a test; like other definitions it is not
                    // run by `loon run`, so it never becomes a `main` statement.
                    !matches!(s.as_str(), "fn" | "type" | "use" | "effect" | "test")
                }
                _ => true,
            },
            _ => true,
        }
    }

    /// If the program defines no `main` but has top-level statements, build a
    /// synthetic `[fn main [] <statements…>]` from them (preserving order).
    fn synthesize_main(exprs: &[Expr]) -> Option<Expr> {
        let has_main = exprs.iter().any(|e| {
            matches!(&e.kind, ExprKind::List(items)
                if items.len() >= 2
                && matches!(&items[0].kind, ExprKind::Symbol(s) if s == "fn")
                && matches!(&items[1].kind, ExprKind::Symbol(n) if n == "main"))
        });
        if has_main {
            return None;
        }
        let stmts: Vec<Expr> = exprs
            .iter()
            .filter(|e| Self::is_toplevel_statement(e))
            .cloned()
            .collect();
        // A file with no `main` and no top-level statements — a library, or a
        // file containing only `[test …]` blocks — is a no-op under `loon run`
        // (which produces no output). Synthesize an empty `main` so the wasm
        // backend still has an entry point and matches that behaviour, rather
        // than failing with "no main export".
        let span = stmts.first().map(|s| s.span).unwrap_or(crate::syntax::Span::ZERO);
        let mut main_items = vec![
            Expr::new(ExprKind::Symbol("fn".into()), span),
            Expr::new(ExprKind::Symbol("main".into()), span),
            Expr::new(ExprKind::List(Vec::new()), span),
        ];
        main_items.extend(stmts);
        Some(Expr::new(ExprKind::List(main_items), span))
    }

    fn compile_program(&mut self, exprs: &[Expr]) -> Result<(), String> {
        // A program with no explicit `[fn main]` but with top-level statements
        // (e.g. a bench script that is a sequence of `[let …]`/`[println …]`)
        // runs those statements in order — matching the interpreter, which
        // evaluates top-level forms. Synthesize `[fn main [] <statements…>]` so
        // the wasm backend has an entry point. Definition forms (fn/type/use/
        // effect) stay where they are.
        let augmented: Vec<Expr>;
        let exprs: &[Expr] = match Self::synthesize_main(exprs) {
            Some(main) => {
                augmented = exprs.iter().cloned().chain(std::iter::once(main)).collect();
                &augmented
            }
            None => exprs,
        };
        // Pass 0: collect [effect ...] declarations
        for expr in exprs {
            if let ExprKind::List(items) = &expr.kind {
                if items.len() >= 2 {
                    if let ExprKind::Symbol(s) = &items[0].kind {
                        if s == "effect" {
                            self.collect_effect_def(&items[1..]);
                        }
                    }
                }
            }
        }
        for expr in exprs {
            if let ExprKind::List(items) = &expr.kind {
                if !items.is_empty() {
                    if let ExprKind::Symbol(s) = &items[0].kind {
                        if s == "use" {
                            self.compile_use(&items[1..])?;
                        }
                    }
                }
            }
        }
        for expr in exprs {
            if let ExprKind::List(items) = &expr.kind {
                if items.len() >= 2 {
                    if let ExprKind::Symbol(s) = &items[0].kind {
                        if s == "type" {
                            self.collect_adt_def(&items[1..])?;
                        }
                    }
                }
            }
        }
        #[allow(clippy::possible_missing_else)]
        for expr in exprs {
            if let ExprKind::List(items) = &expr.kind {
                if items.len() >= 3 {
                    if let ExprKind::Symbol(s) = &items[0].kind {
                        if s == "fn" {
                            if let ExprKind::Symbol(name) = &items[1].kind {
                                let args = &items[1..];
                                if Self::is_multi_arity(args) {
                                    // Register one entry per clause, keyed "name#arity".
                                    for clause in &args[1..] {
                                        if let ExprKind::Tuple(parts) = &clause.kind {
                                            if let Some(ExprKind::List(params)) =
                                                parts.first().map(|e| &e.kind)
                                            {
                                                let arity = Self::param_names(params).len();
                                                let key = format!("{name}#{arity}");
                                                if self.fn_map.contains_key(&key) {
                                                    continue;
                                                }
                                                let idx = self.next_fn_idx;
                                                self.fn_map.insert(
                                                    key,
                                                    FnDef {
                                                        func_idx: idx,
                                                        arity,
                                                        is_closure: false,
                                                    },
                                                );
                                                self.next_fn_idx += 1;
                                            }
                                        }
                                    }
                                    continue;
                                }
                                if self.fn_map.contains_key(name) {
                                    continue;
                                }
                                if let ExprKind::List(params) = &items[2].kind {
                                    let arity = Self::param_names(params).len();
                                    let idx = self.next_fn_idx;
                                    self.fn_map.insert(
                                        name.clone(),
                                        FnDef {
                                            func_idx: idx,
                                            arity,
                                            is_closure: false,
                                        },
                                    );
                                    self.next_fn_idx += 1;
                                }
                            }
                        }
                    }
                }
            }
        }
        self.analyze_string_fns(exprs);
        for expr in exprs {
            if let ExprKind::List(items) = &expr.kind {
                if items.len() >= 3 {
                    if let ExprKind::Symbol(s) = &items[0].kind {
                        if s == "fn" {
                            if let ExprKind::Symbol(_) = &items[1].kind {
                                self.compile_defn(&items[1..])?;
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
    /// Compute, to a fixpoint, which functions statically return a string, so
    /// `println` can route their call results through the byte-printing path.
    fn analyze_string_fns(&mut self, exprs: &[Expr]) {
        // Collect (fn key, return expression) for every clause.
        let mut returns: Vec<(String, &Expr)> = Vec::new();
        for expr in exprs {
            if let ExprKind::List(items) = &expr.kind {
                if items.len() >= 3 && matches!(&items[0].kind, ExprKind::Symbol(s) if s == "fn") {
                    if let ExprKind::Symbol(name) = &items[1].kind {
                        let args = &items[1..];
                        if Self::is_multi_arity(args) {
                            for clause in &args[1..] {
                                if let ExprKind::Tuple(parts) = &clause.kind {
                                    if let (Some(ExprKind::List(params)), Some(last)) =
                                        (parts.first().map(|e| &e.kind), parts.last())
                                    {
                                        if parts.len() >= 2 {
                                            let key =
                                                format!("{name}#{}", Self::param_names(params).len());
                                            returns.push((key, last));
                                        }
                                    }
                                }
                            }
                        } else if let Some(last) = items.last() {
                            returns.push((name.clone(), last));
                        }
                    }
                }
            }
        }
        loop {
            let mut changed = false;
            for (key, ret) in &returns {
                if !self.string_fns.contains(key)
                    && Self::expr_returns_string(ret, &self.string_fns)
                {
                    self.string_fns.insert(key.clone());
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
    }
    /// Whether an expression statically evaluates to a string, consulting the
    /// set of known string-returning functions for calls. Conservative.
    fn expr_returns_string(expr: &Expr, fns: &std::collections::HashSet<String>) -> bool {
        match &expr.kind {
            ExprKind::Str(_) => true,
            ExprKind::List(items) => match items.first().map(|e| &e.kind) {
                Some(ExprKind::Symbol(s)) => match s.as_str() {
                    "str" | "str-concat" | "substring" => true,
                    "do" => items.last().is_some_and(|e| Self::expr_returns_string(e, fns)),
                    "if" => {
                        items.len() >= 4
                            && Self::expr_returns_string(&items[2], fns)
                            && Self::expr_returns_string(&items[3], fns)
                    }
                    // A call: string iff the callee (by name or arity) is known
                    // to return a string.
                    name => {
                        let argc = items.len() - 1;
                        fns.contains(name) || fns.contains(&format!("{name}#{argc}"))
                    }
                },
                _ => false,
            },
            _ => false,
        }
    }
    fn tree_shake(&mut self) {
        // Map provisional func index → position in `self.functions`. Only
        // compiled functions (not imports) have a body to traverse.
        let mut id_to_pos: HashMap<u32, usize> = HashMap::new();
        for (pos, &id) in self.fn_indices.iter().enumerate() {
            id_to_pos.insert(id, pos);
        }
        // `main` is the only entry point, so reachability is seeded from it.
        // With no main — a file of bare definitions or `test` blocks — nothing
        // is reachable and every compiled function is pruned, yielding a valid
        // (empty) module. Bailing out instead would leave functions behind with
        // stale provisional indices that `finish` mis-references, producing
        // invalid wasm.
        let mut reachable = std::collections::HashSet::new();
        let mut queue = std::collections::VecDeque::new();
        if let Some(def) = self.fn_map.get("main") {
            reachable.insert(def.func_idx);
            queue.push_back(def.func_idx);
        }
        while let Some(idx) = queue.pop_front() {
            let pos = match id_to_pos.get(&idx) {
                Some(&p) => p,
                None => continue, // WASI / effect import — a leaf
            };
            let mut has_indirect = false;
            for instr in &self.functions[pos].instructions {
                match instr {
                    WasmInstruction::Call(target) => {
                        if reachable.insert(*target) {
                            queue.push_back(*target);
                        }
                    }
                    WasmInstruction::CallIndirect(_) => {
                        has_indirect = true;
                    }
                    _ => {}
                }
            }
            if has_indirect {
                for &entry in &self.table_entries {
                    if reachable.insert(entry) {
                        queue.push_back(entry);
                    }
                }
            }
        }
        // Build remap: provisional index → final index. Imports come first
        // (used WASI imports, then all effect imports — the host always
        // provides those), then the reachable functions in their push order.
        let used_wasi: Vec<u32> = (0..WASI_IMPORT_COUNT)
            .filter(|i| reachable.contains(i))
            .collect();
        let mut remap = HashMap::new();
        let mut new_idx = 0u32;
        for &old in &used_wasi {
            remap.insert(old, new_idx);
            new_idx += 1;
        }
        // Effect imports are emitted unconditionally by `finish` (in
        // `effect_import_defs` creation order, which matches ascending id).
        let mut effect_old_ids: Vec<u32> = self.effect_imports.values().copied().collect();
        effect_old_ids.sort_unstable();
        for &old in &effect_old_ids {
            remap.insert(old, new_idx);
            new_idx += 1;
        }
        let new_import_count = new_idx;
        let mut kept_fn_indices = Vec::new();
        for (pos, &id) in self.fn_indices.iter().enumerate() {
            if reachable.contains(&id) {
                remap.insert(id, new_idx);
                kept_fn_indices.push(pos);
                new_idx += 1;
            }
        }
        // Filter + reorder functions (and their recorded indices) so that
        // `self.functions[j]` ends up at final index `new_import_count + j`.
        let mut old_fns: Vec<Option<FunctionBody>> = std::mem::take(&mut self.functions)
            .into_iter()
            .map(Some)
            .collect();
        let old_ids = std::mem::take(&mut self.fn_indices);
        self.functions = kept_fn_indices
            .iter()
            .map(|&i| old_fns[i].take().unwrap())
            .collect();
        self.fn_indices = kept_fn_indices.iter().map(|&i| remap[&old_ids[i]]).collect();
        // Rewrite Call targets
        for func in &mut self.functions {
            for instr in &mut func.instructions {
                if let WasmInstruction::Call(ref mut target) = instr {
                    if let Some(&new) = remap.get(target) {
                        *target = new;
                    }
                }
            }
        }
        // Remap table entries
        self.table_entries = self
            .table_entries
            .iter()
            .filter_map(|&old| remap.get(&old).copied())
            .collect();
        self.table_map.clear();
        for (ti, &func_idx) in self.table_entries.iter().enumerate() {
            self.table_map.insert(func_idx, ti as u32);
        }
        // Update fn_map
        for def in self.fn_map.values_mut() {
            if let Some(&new) = remap.get(&def.func_idx) {
                def.func_idx = new;
            }
        }
        // Update effect_imports
        for idx in self.effect_imports.values_mut() {
            if let Some(&new) = remap.get(idx) {
                *idx = new;
            }
        }
        self.used_wasi_imports = Some(used_wasi);
        self.import_count = new_import_count;
        self.next_fn_idx = new_idx;
    }
    fn compile_use(&mut self, args: &[Expr]) -> Result<(), String> {
        if args.is_empty() {
            return Ok(());
        }
        let module_path = match args[0].as_dotted_path() {
            Some(s) => s,
            None => {
                if let ExprKind::Str(s) = &args[0].kind {
                    s.clone()
                } else {
                    return Ok(());
                }
            }
        };
        let base_dir = match &self.base_dir {
            Some(d) => d.clone(),
            None => return Ok(()),
        };
        let file_path = crate::module::ModuleCache::resolve_path(&module_path, &base_dir);
        let canonical = file_path
            .canonicalize()
            .unwrap_or_else(|_| file_path.clone());
        if self.compiled_modules.contains(&canonical) {
            return Ok(());
        }
        self.compiled_modules.insert(canonical);
        let source = std::fs::read_to_string(&file_path).map_err(|e| {
            format!(
                "codegen: cannot read module '{}' at {}: {e}",
                module_path,
                file_path.display()
            )
        })?;
        let module_exprs = crate::parser::parse(&source).map_err(|e| {
            format!(
                "codegen: parse error in module '{}': {}",
                module_path, e.message
            )
        })?;
        let old_base = self.base_dir.clone();
        self.base_dir = file_path.parent().map(|p| p.to_path_buf());
        self.compile_program(&module_exprs)?;
        self.base_dir = old_base;
        Ok(())
    }
    /// Collect [effect Name [op [Type...] Ret] ...] into effect_registry
    fn collect_effect_def(&mut self, args: &[Expr]) {
        if args.is_empty() {
            return;
        }
        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return,
        };
        let mut operations = Vec::new();
        for op_expr in &args[1..] {
            if let ExprKind::List(op_items) = &op_expr.kind {
                if op_items.is_empty() {
                    continue;
                }
                let op_name = match &op_items[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => continue,
                };
                let mut params = Vec::new();
                let mut return_type = None;
                if op_items.len() >= 2 {
                    if let ExprKind::List(param_types) = &op_items[1].kind {
                        for pt in param_types {
                            if let ExprKind::Symbol(ty_name) = &pt.kind {
                                params.push((ty_name.clone(), Some(ty_name.clone())));
                            }
                        }
                    }
                }
                if op_items.len() >= 3 {
                    if let ExprKind::Symbol(ret) = &op_items[2].kind {
                        return_type = Some(ret.clone());
                    }
                }
                operations.push(crate::effects::EffectOp {
                    name: op_name,
                    params,
                    return_type,
                });
            }
        }
        self.effect_registry
            .register(crate::effects::EffectDecl { name, operations });
    }
    /// Get or create an import index for an effect operation.
    fn get_or_create_effect_import(&mut self, effect: &str, op: &str) -> u32 {
        let key = format!("{effect}.{op}");
        if let Some(&idx) = self.effect_imports.get(&key) {
            return idx;
        }
        // Determine arity from registry
        let arity = self
            .effect_registry
            .get_op(effect, op)
            .map(|o| o.params.len())
            .unwrap_or(0);
        let idx = self.next_fn_idx;
        self.next_fn_idx += 1;
        let namespace = format!("loon:effects/{}", effect.to_lowercase());
        self.effect_import_defs
            .push((namespace, op.to_string(), arity));
        self.effect_imports.insert(key, idx);
        idx
    }
    fn collect_adt_def(&mut self, args: &[Expr]) -> Result<(), String> {
        if args.is_empty() {
            return Ok(());
        }
        let type_name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Ok(()),
        };
        let mut constructors = Vec::new();
        let mut tag: u32 = 0;
        for arg in &args[1..] {
            match &arg.kind {
                ExprKind::List(items) if !items.is_empty() => {
                    if let ExprKind::Symbol(cn) = &items[0].kind {
                        if cn.starts_with(char::is_uppercase) {
                            let arity = items.len() - 1;
                            self.adt_constructors.insert(cn.clone(), (tag, arity));
                            constructors.push((cn.clone(), tag, arity));
                            tag += 1;
                        }
                    }
                }
                ExprKind::Symbol(name) if name.starts_with(char::is_uppercase) => {
                    self.adt_constructors.insert(name.clone(), (tag, 0));
                    constructors.push((name.clone(), tag, 0));
                    tag += 1;
                }
                _ => {}
            }
        }
        self.adt_types.push(AdtInfo {
            type_name,
            constructors,
        });
        Ok(())
    }
    /// A `[fn name …]` form is multi-arity when its first post-name element is
    /// a clause `([params] body…)` — parsed as a tuple `(…)`.
    fn is_multi_arity(args: &[Expr]) -> bool {
        matches!(args.get(1).map(|e| &e.kind), Some(ExprKind::Tuple(_)))
    }
    /// Whether `expr` contains a `recur` that targets the enclosing function
    /// (i.e. not nested inside its own `loop`, and not inside a nested `fn`).
    fn contains_bare_recur(expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::List(items) => {
                if let Some(ExprKind::Symbol(s)) = items.first().map(|e| &e.kind) {
                    match s.as_str() {
                        "recur" => return true,
                        // These introduce their own recur scope / binding form.
                        "loop" | "fn" => return false,
                        _ => {}
                    }
                }
                items.iter().any(Self::contains_bare_recur)
            }
            ExprKind::Tuple(items) | ExprKind::Vec(items) | ExprKind::Set(items) => {
                items.iter().any(Self::contains_bare_recur)
            }
            _ => false,
        }
    }
    fn param_names(params: &[Expr]) -> Vec<String> {
        params
            .iter()
            .filter_map(|p| {
                if let ExprKind::Symbol(s) = &p.kind {
                    Some(s.clone())
                } else {
                    None
                }
            })
            .collect()
    }
    fn compile_defn(&mut self, args: &[Expr]) -> Result<(), String> {
        if args.len() < 2 {
            return Err("defn requires name, params, body".into());
        }
        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err("defn name must be a symbol".into()),
        };
        if Self::is_multi_arity(args) {
            // Each clause is compiled as its own function keyed by arity
            // ("name#N"); call sites resolve by argument count.
            for clause in &args[1..] {
                if let ExprKind::Tuple(parts) = &clause.kind {
                    if let Some(ExprKind::List(params)) = parts.first().map(|e| &e.kind) {
                        let names = Self::param_names(params);
                        let key = format!("{name}#{}", names.len());
                        self.compile_fn_body(&key, false, &names, &parts[1..])?;
                    }
                }
            }
            return Ok(());
        }
        let params = match &args[1].kind {
            ExprKind::List(items) => Self::param_names(items),
            _ => return Err("defn params must be a list".into()),
        };
        let mut body_start = 2;
        if body_start < args.len() {
            match &args[body_start].kind {
                // `[fn f [..] / Ret …]` return-type annotation.
                ExprKind::Symbol(s) if s == "/" => body_start += 2,
                // `[fn f [..] #{E1 E2} …]` effect-row annotation: skip it (the
                // backend tracks effects via imports, not the declared row).
                ExprKind::Set(_) | ExprKind::Map(_) => body_start += 1,
                _ => {}
            }
        }
        let is_main = name == "main";
        self.compile_fn_body(&name, is_main, &params, &args[body_start..])
    }
    fn compile_fn_body(
        &mut self,
        key: &str,
        is_main: bool,
        params: &[String],
        body: &[Expr],
    ) -> Result<(), String> {
        let mut ctx = FnCtx {
            locals: HashMap::new(),
            local_count: params.len() as u32,
            instructions: Vec::new(),
            compiler: self,
            loop_starts: Vec::new(),
            loop_vars: Vec::new(),
        };
        for (i, p) in params.iter().enumerate() {
            ctx.locals.insert(p.clone(), i as u32);
        }
        // Self-tail-recursion: a `recur` in the body (not inside a nested
        // `loop`) rebinds the params and jumps to the top. Wrap the body in a
        // wasm loop whose loop variables are the parameter locals.
        let self_recur = body.iter().any(Self::contains_bare_recur);
        if self_recur {
            ctx.instructions
                .push(WasmInstruction::Loop(BlockType::Result(ValType::I64)));
            ctx.loop_starts.push(ctx.instructions.len());
            ctx.loop_vars.push((0..params.len() as u32).collect());
        }
        // Compile each body expression; every one leaves an i64 on the stack,
        // so drop all but the last (a statement sequence keeps only its final
        // value). The last value is the function's result (or dropped for main).
        for (i, expr) in body.iter().enumerate() {
            ctx.compile_expr(expr)?;
            if i + 1 < body.len() {
                ctx.instructions.push(WasmInstruction::Drop);
            }
        }
        if self_recur {
            ctx.instructions.push(WasmInstruction::End);
            ctx.loop_starts.pop();
            ctx.loop_vars.pop();
        }
        let extra_locals = if ctx.local_count > params.len() as u32 {
            vec![ValType::I64; (ctx.local_count - params.len() as u32) as usize]
        } else {
            vec![]
        };
        if is_main && !ctx.instructions.is_empty() {
            ctx.instructions.push(WasmInstruction::Drop);
        }
        let instrs = ctx.instructions.clone();
        drop(ctx);
        let func_idx = self.fn_map.get(key).map(|d| d.func_idx).unwrap_or(0);
        self.push_function(
            func_idx,
            FunctionBody {
                params: vec![ValType::I64; params.len()],
                results: if is_main { vec![] } else { vec![ValType::I64] },
                locals: extra_locals,
                instructions: instrs,
            },
        );
        Ok(())
    }
    fn intern_string(&mut self, s: &str) -> (u32, u32) {
        for (existing, offset) in &self.strings {
            if existing == s {
                return (*offset, s.len() as u32);
            }
        }
        let offset = self.string_offset;
        let len = s.len() as u32;
        self.strings.push((s.to_string(), offset));
        self.string_offset += len + 1;
        (offset, len)
    }
    fn finish(self) -> Vec<u8> {
        let mut module = Module::new();
        let mut types = TypeSection::new();
        types
            .ty()
            .function(vec![ValType::I32; 4], vec![ValType::I32]); // 0: fd_write
        types
            .ty()
            .function(vec![ValType::I32, ValType::I32], vec![]); // 1: println helper
        types
            .ty()
            .function(vec![ValType::I32; 4], vec![ValType::I32]); // 2: fd_read
        types
            .ty()
            .function(vec![ValType::I32, ValType::I32], vec![ValType::I32]); // 3: args_get
        types
            .ty()
            .function(vec![ValType::I32, ValType::I32], vec![ValType::I32]); // 4: args_sizes_get
        types
            .ty()
            .function(vec![ValType::I32, ValType::I32], vec![ValType::I32]); // 5: environ_get
        types
            .ty()
            .function(vec![ValType::I32, ValType::I32], vec![ValType::I32]); // 6: environ_sizes_get
                                                                             // Effect import types (all i64 params → i64 result)
        let mut effect_type_indices = Vec::new();
        for (_ns, _name, arity) in &self.effect_import_defs {
            let ti = types.len();
            types
                .ty()
                .function(vec![ValType::I64; *arity], vec![ValType::I64]);
            effect_type_indices.push(ti);
        }
        let mut fn_type_indices = Vec::new();
        for func in &self.functions {
            let ti = types.len();
            types
                .ty()
                .function(func.params.clone(), func.results.clone());
            fn_type_indices.push(ti);
        }
        let effect_type_count = self.effect_import_defs.len() as u32;
        let mut ie: Vec<(usize, u32)> = self
            .indirect_type_cache
            .iter()
            .map(|(&a, &t)| (a, t))
            .collect();
        ie.sort_by_key(|&(_, idx)| idx);
        let indirect_type_remap: HashMap<u32, u32> = ie
            .iter()
            .enumerate()
            .map(|(i, &(_, cached_idx))| {
                (
                    cached_idx,
                    PRE_ALLOC_TYPES + effect_type_count + self.functions.len() as u32 + i as u32,
                )
            })
            .collect();
        for (arity, _) in &ie {
            types
                .ty()
                .function(vec![ValType::I64; *arity], vec![ValType::I64]);
        }
        module.section(&types);
        let wasi_defs: [(u32, &str, u32); 6] = [
            (0, "fd_write", 0),
            (1, "fd_read", 2),
            (2, "args_get", 3),
            (3, "args_sizes_get", 4),
            (4, "environ_get", 5),
            (5, "environ_sizes_get", 6),
        ];
        let mut imports = ImportSection::new();
        match &self.used_wasi_imports {
            Some(used) => {
                for &idx in used {
                    let (_, name, type_idx) = wasi_defs[idx as usize];
                    imports.import(
                        "wasi_snapshot_preview1",
                        name,
                        EntityType::Function(type_idx),
                    );
                }
            }
            None => {
                for &(_, name, type_idx) in &wasi_defs {
                    imports.import(
                        "wasi_snapshot_preview1",
                        name,
                        EntityType::Function(type_idx),
                    );
                }
            }
        }
        // Effect imports
        for (i, (namespace, func_name, _arity)) in self.effect_import_defs.iter().enumerate() {
            imports.import(
                namespace,
                func_name,
                EntityType::Function(effect_type_indices[i]),
            );
        }
        module.section(&imports);
        let mut functions = FunctionSection::new();
        for idx in &fn_type_indices {
            functions.function(*idx);
        }
        module.section(&functions);
        if !self.table_entries.is_empty() {
            let mut t = TableSection::new();
            t.table(TableType {
                element_type: RefType::FUNCREF,
                minimum: self.table_entries.len() as u64,
                maximum: Some(self.table_entries.len() as u64),
                table64: false,
                shared: false,
            });
            module.section(&t);
        }
        let mut mem = MemorySection::new();
        mem.memory(MemoryType {
            // 256 pages (16 MiB). The bump allocator never frees and vectors
            // are copy-on-write (O(n²) for repeated push), so one 64 KiB page
            // is exhausted by even modest loops; this gives realistic programs
            // headroom without a memory.grow path.
            minimum: 256,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&mem);
        if self.force_heap || !self.table_entries.is_empty() || !self.adt_constructors.is_empty() {
            let mut g = GlobalSection::new();
            g.global(
                GlobalType {
                    val_type: ValType::I32,
                    mutable: true,
                    shared: false,
                },
                &ConstExpr::i32_const(4096),
            );
            module.section(&g);
        }
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        if let Some(main_fn) = self.fn_map.get("main") {
            exports.export("_start", ExportKind::Func, main_fn.func_idx);
        }
        module.section(&exports);
        if !self.table_entries.is_empty() {
            let mut e = ElementSection::new();
            e.active(
                Some(0),
                &ConstExpr::i32_const(0),
                Elements::Functions(self.table_entries.clone().into()),
            );
            module.section(&e);
        }
        let mut code = CodeSection::new();
        for func in &self.functions {
            let mut f = Function::new(func.locals.iter().map(|t| (1, *t)).collect::<Vec<_>>());
            for instr in &func.instructions {
                if let WasmInstruction::CallIndirect(ty) = instr {
                    let actual_ty = indirect_type_remap.get(ty).copied().unwrap_or(*ty);
                    f.instruction(&Instruction::CallIndirect {
                        type_index: actual_ty,
                        table_index: 0,
                    });
                } else {
                    emit_instruction(&mut f, instr);
                }
            }
            f.instruction(&Instruction::End);
            code.function(&f);
        }
        module.section(&code);
        if !self.strings.is_empty() {
            let mut d = DataSection::new();
            for (s, offset) in &self.strings {
                d.active(
                    0,
                    &ConstExpr::i32_const(*offset as i32),
                    s.as_bytes().iter().copied(),
                );
            }
            module.section(&d);
        }
        module.finish()
    }
}

struct FnCtx<'a> {
    locals: HashMap<String, u32>,
    local_count: u32,
    instructions: Vec<WasmInstruction>,
    compiler: &'a mut Compiler,
    /// For each enclosing `loop`: the instruction index just after its `Loop`
    /// op (to compute `recur`'s branch depth) and its loop-variable locals.
    loop_starts: Vec<usize>,
    loop_vars: Vec<Vec<u32>>,
}

impl<'a> FnCtx<'a> {
    fn alloc_local(&mut self) -> u32 {
        let i = self.local_count;
        self.local_count += 1;
        i
    }

    /// Emit code that prints the i64 currently on the stack as a decimal number
    /// (with trailing newline if `newline`) to stdout via WASI fd_write, leaving
    /// UNIT (i64 0) on the stack. A runtime itoa: digits are written backwards
    /// into a low scratch region (below the string table at 1024), then one
    /// fd_write emits them. Handles zero and negatives.
    fn emit_print_i64(&mut self, newline: bool) {
        use WasmInstruction as W;
        // Scratch: newline (if any) at BUF, digits written just below it.
        const BUF: i64 = 542;
        let n = self.alloc_local();
        let p = self.alloc_local();
        let neg = self.alloc_local();
        // n = value; neg = (n < 0); if neg, n = -n.
        self.instructions.push(W::LocalSet(n));
        self.instructions.push(W::LocalGet(n));
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::If(BlockType::Empty));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::LocalSet(neg));
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalGet(n));
        self.instructions.push(W::I64Sub);
        self.instructions.push(W::LocalSet(n));
        self.instructions.push(W::Else);
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalSet(neg));
        self.instructions.push(W::End);
        if newline {
            self.instructions.push(W::I32Const(BUF as i32));
            self.instructions.push(W::I32Const(10)); // '\n'
            self.instructions.push(W::I32Store8(0, 0));
        }
        // p = BUF; do { p-=1; mem[p] = '0' + n%10; n /= 10 } while n != 0
        self.instructions.push(W::I64Const(BUF));
        self.instructions.push(W::LocalSet(p));
        self.instructions.push(W::Loop(BlockType::Empty));
        self.instructions.push(W::LocalGet(p));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Sub);
        self.instructions.push(W::LocalSet(p));
        self.instructions.push(W::LocalGet(p));
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::LocalGet(n));
        self.instructions.push(W::I64Const(10));
        self.instructions.push(W::I64RemU);
        self.instructions.push(W::I64Const(48)); // '0'
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I32Store8(0, 0));
        self.instructions.push(W::LocalGet(n));
        self.instructions.push(W::I64Const(10));
        self.instructions.push(W::I64DivU);
        self.instructions.push(W::LocalSet(n));
        self.instructions.push(W::LocalGet(n));
        self.instructions.push(W::I64Eqz);
        self.instructions.push(W::I32Eqz); // n != 0
        self.instructions.push(W::BrIf(0)); // loop while n != 0
        self.instructions.push(W::End);
        // if neg { p-=1; mem[p] = '-' }
        self.instructions.push(W::LocalGet(neg));
        self.instructions.push(W::I64Eqz);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::If(BlockType::Empty));
        self.instructions.push(W::LocalGet(p));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Sub);
        self.instructions.push(W::LocalSet(p));
        self.instructions.push(W::LocalGet(p));
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I32Const(45)); // '-'
        self.instructions.push(W::I32Store8(0, 0));
        self.instructions.push(W::End);
        // iovec at mem[0] = ptr (=p), mem[4] = len (= end - p)
        let end = if newline { BUF as i32 + 1 } else { BUF as i32 };
        self.instructions.push(W::I32Const(0));
        self.instructions.push(W::LocalGet(p));
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I32Store(2, 0));
        self.instructions.push(W::I32Const(4));
        self.instructions.push(W::I64Const(end as i64));
        self.instructions.push(W::LocalGet(p));
        self.instructions.push(W::I64Sub);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I32Store(2, 0));
        // fd_write(stdout=1, iovec=0, count=1, nwritten=8); drop; result UNIT
        self.instructions.push(W::I32Const(1));
        self.instructions.push(W::I32Const(0));
        self.instructions.push(W::I32Const(1));
        self.instructions.push(W::I32Const(8));
        self.instructions.push(W::Call(0));
        self.instructions.push(W::Drop);
        self.instructions.push(W::I64Const(0));
    }
    /// Consume a packed string `(ptr<<32)|len` on the stack and write its bytes
    /// to stdout via WASI fd_write, optionally followed by a newline, leaving
    /// UNIT (i64 0). Used when `println`'s argument is statically a string.
    fn emit_print_str(&mut self, newline: bool) {
        use WasmInstruction as W;
        let s = self.alloc_local();
        self.instructions.push(W::LocalSet(s));
        // iovec at mem[0] = ptr (= s >> 32), mem[4] = len (= s & 0xFFFFFFFF)
        self.instructions.push(W::I32Const(0));
        self.instructions.push(W::LocalGet(s));
        self.instructions.push(W::I64Const(32));
        self.instructions.push(W::I64ShrU);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I32Store(2, 0));
        self.instructions.push(W::I32Const(4));
        self.instructions.push(W::LocalGet(s));
        self.instructions.push(W::I64Const(0xFFFF_FFFF));
        self.instructions.push(W::I64And);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I32Store(2, 0));
        self.instructions.push(W::I32Const(1)); // stdout
        self.instructions.push(W::I32Const(0)); // iovec
        self.instructions.push(W::I32Const(1)); // count
        self.instructions.push(W::I32Const(8)); // nwritten
        self.instructions.push(W::Call(0));
        self.instructions.push(W::Drop);
        if newline {
            const BUF: i32 = 542;
            self.instructions.push(W::I32Const(BUF));
            self.instructions.push(W::I32Const(10)); // '\n'
            self.instructions.push(W::I32Store8(0, 0));
            self.instructions.push(W::I32Const(0));
            self.instructions.push(W::I32Const(BUF));
            self.instructions.push(W::I32Store(2, 0));
            self.instructions.push(W::I32Const(4));
            self.instructions.push(W::I32Const(1));
            self.instructions.push(W::I32Store(2, 0));
            self.instructions.push(W::I32Const(1));
            self.instructions.push(W::I32Const(0));
            self.instructions.push(W::I32Const(1));
            self.instructions.push(W::I32Const(8));
            self.instructions.push(W::Call(0));
            self.instructions.push(W::Drop);
        }
        self.instructions.push(W::I64Const(0));
    }
    /// Whether an expression statically produces a string value. The value
    /// model is untagged (every value is a raw i64), so `println` can only
    /// pick the string-printing path when it can prove the argument is a
    /// string at compile time.
    fn expr_is_string(&self, expr: &Expr) -> bool {
        Compiler::expr_returns_string(expr, &self.compiler.string_fns)
    }
    fn compile_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match &expr.kind {
            ExprKind::Int(n) => {
                self.instructions.push(WasmInstruction::I64Const(*n));
                Ok(())
            }
            ExprKind::Float(_) => {
                // The value model is untagged i64; arithmetic emits i64 ops, so
                // an f64 here yields a module the validator rejects. Fail
                // cleanly until there's a typed/tagged value model. (Runs on the
                // VM via `loon run`.)
                Err("codegen: floating-point values are not supported by the wasm \
                     backend yet; run with `loon run`"
                    .into())
            }
            ExprKind::Keyword(k) => {
                let id = self.compiler.intern_keyword(k);
                self.instructions.push(WasmInstruction::I64Const(id));
                Ok(())
            }
            ExprKind::Bool(b) => {
                self.instructions
                    .push(WasmInstruction::I64Const(if *b { 1 } else { 0 }));
                Ok(())
            }
            ExprKind::Str(s) => {
                let (offset, len) = self.compiler.intern_string(s);
                let packed = ((offset as i64) << 32) | (len as i64);
                self.instructions.push(WasmInstruction::I64Const(packed));
                Ok(())
            }
            ExprKind::Symbol(name) => {
                if let Some(&idx) = self.locals.get(name) {
                    self.instructions.push(WasmInstruction::LocalGet(idx));
                    Ok(())
                } else if let Some((tag, 0)) =
                    self.compiler.adt_constructors.get(name.as_str()).cloned()
                {
                    self.compile_adt_constructor(name, tag, 0, &[])
                } else {
                    Err(format!("codegen: unbound symbol '{name}'"))
                }
            }
            ExprKind::DotAccess(_, _) => {
                Err("codegen: dot access not supported as expression".into())
            }
            ExprKind::List(items) if items.is_empty() => {
                self.instructions.push(WasmInstruction::I64Const(0));
                Ok(())
            }
            ExprKind::List(items) => {
                if let ExprKind::Symbol(s) = &items[0].kind {
                    if s == "fn" {
                        return self.compile_closure(&items[1..]);
                    }
                }
                self.compile_call(items)
            }
            ExprKind::Vec(items) => {
                // #[a b c] desugars to vec-new + a vec-push per element.
                self.compiler.ensure_collections_runtime();
                let rt = self.compiler.collections_runtime.clone().unwrap();
                self.instructions
                    .push(WasmInstruction::Call(rt.vec_new_idx));
                for item in items {
                    self.compile_expr(item)?;
                    self.instructions
                        .push(WasmInstruction::Call(rt.vec_push_idx));
                }
                Ok(())
            }
            ExprKind::Map(pairs) => {
                // {} / {:k v …} desugars to map-new (an empty pair-vector) plus
                // a map_assoc per entry, preserving insertion order.
                self.compiler.ensure_maps_runtime();
                let cr = self.compiler.collections_runtime.clone().unwrap();
                let mr = self.compiler.maps_runtime.clone().unwrap();
                self.instructions
                    .push(WasmInstruction::Call(cr.vec_new_idx));
                for (k, v) in pairs {
                    self.compile_expr(k)?;
                    self.compile_expr(v)?;
                    self.instructions
                        .push(WasmInstruction::Call(mr.map_assoc_idx));
                }
                Ok(())
            }
            _ => Err(format!("codegen: unsupported expression: {:?}", expr.kind)),
        }
    }
    fn compile_call(&mut self, items: &[Expr]) -> Result<(), String> {
        if items.is_empty() {
            return Ok(());
        }
        if let ExprKind::Symbol(s) = &items[0].kind {
            match s.as_str() {
                "+" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64Add);
                    return Ok(());
                }
                "-" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64Sub);
                    return Ok(());
                }
                "*" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64Mul);
                    return Ok(());
                }
                ">" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64GtS);
                    // Comparisons produce an i32; values are i64 throughout
                    // (incl. `if` conditions and booleans-as-values), so widen.
                    self.instructions.push(WasmInstruction::I64ExtendI32U);
                    return Ok(());
                }
                "<" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64LtS);
                    self.instructions.push(WasmInstruction::I64ExtendI32U);
                    return Ok(());
                }
                "=" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64Eq);
                    self.instructions.push(WasmInstruction::I64ExtendI32U);
                    return Ok(());
                }
                "!=" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64Ne);
                    self.instructions.push(WasmInstruction::I64ExtendI32U);
                    return Ok(());
                }
                "<=" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64LeS);
                    self.instructions.push(WasmInstruction::I64ExtendI32U);
                    return Ok(());
                }
                ">=" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64GeS);
                    self.instructions.push(WasmInstruction::I64ExtendI32U);
                    return Ok(());
                }
                "/" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64DivS);
                    return Ok(());
                }
                "%" | "mod" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64RemS);
                    return Ok(());
                }
                "inc" => {
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::I64Const(1));
                    self.instructions.push(WasmInstruction::I64Add);
                    return Ok(());
                }
                "dec" => {
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::I64Const(1));
                    self.instructions.push(WasmInstruction::I64Sub);
                    return Ok(());
                }
                "abs" => {
                    // x < 0 ? 0 - x : x
                    let x = self.alloc_local();
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::LocalSet(x));
                    self.instructions.push(WasmInstruction::LocalGet(x));
                    self.instructions.push(WasmInstruction::I64Const(0));
                    self.instructions.push(WasmInstruction::I64LtS);
                    self.instructions
                        .push(WasmInstruction::If(BlockType::Result(ValType::I64)));
                    self.instructions.push(WasmInstruction::I64Const(0));
                    self.instructions.push(WasmInstruction::LocalGet(x));
                    self.instructions.push(WasmInstruction::I64Sub);
                    self.instructions.push(WasmInstruction::Else);
                    self.instructions.push(WasmInstruction::LocalGet(x));
                    self.instructions.push(WasmInstruction::End);
                    return Ok(());
                }
                "min" | "max" => {
                    let a = self.alloc_local();
                    let b = self.alloc_local();
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::LocalSet(a));
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::LocalSet(b));
                    self.instructions.push(WasmInstruction::LocalGet(a));
                    self.instructions.push(WasmInstruction::LocalGet(b));
                    self.instructions.push(WasmInstruction::I64LtS); // a < b
                    self.instructions
                        .push(WasmInstruction::If(BlockType::Result(ValType::I64)));
                    // a < b: min -> a, max -> b
                    let (then_l, else_l) = if s == "min" { (a, b) } else { (b, a) };
                    self.instructions.push(WasmInstruction::LocalGet(then_l));
                    self.instructions.push(WasmInstruction::Else);
                    self.instructions.push(WasmInstruction::LocalGet(else_l));
                    self.instructions.push(WasmInstruction::End);
                    return Ok(());
                }
                // not: 1 if the argument is falsy (0), else 0.
                "not" => {
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::I64Eqz);
                    self.instructions.push(WasmInstruction::I64ExtendI32U);
                    return Ok(());
                }
                // and: a truthy ? b : a   (matches the EIR VM; values are i64).
                "and" => {
                    let la = self.alloc_local();
                    let lb = self.alloc_local();
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::LocalSet(la));
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::LocalSet(lb));
                    self.instructions.push(WasmInstruction::LocalGet(la));
                    self.instructions.push(WasmInstruction::I64Eqz);
                    self.instructions
                        .push(WasmInstruction::If(BlockType::Result(ValType::I64)));
                    self.instructions.push(WasmInstruction::LocalGet(la));
                    self.instructions.push(WasmInstruction::Else);
                    self.instructions.push(WasmInstruction::LocalGet(lb));
                    self.instructions.push(WasmInstruction::End);
                    return Ok(());
                }
                // or: a truthy ? a : b.
                "or" => {
                    let la = self.alloc_local();
                    let lb = self.alloc_local();
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::LocalSet(la));
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::LocalSet(lb));
                    self.instructions.push(WasmInstruction::LocalGet(la));
                    self.instructions.push(WasmInstruction::I64Eqz);
                    self.instructions
                        .push(WasmInstruction::If(BlockType::Result(ValType::I64)));
                    self.instructions.push(WasmInstruction::LocalGet(lb));
                    self.instructions.push(WasmInstruction::Else);
                    self.instructions.push(WasmInstruction::LocalGet(la));
                    self.instructions.push(WasmInstruction::End);
                    return Ok(());
                }
                "str-len" => {
                    self.compiler.ensure_string_runtime();
                    let rt = self.compiler.string_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.instructions
                        .push(WasmInstruction::Call(rt.str_len_idx));
                    return Ok(());
                }
                "str-concat" | "str" => {
                    // Variadic: fold str_concat left-to-right over all args.
                    // [str] -> "", [str a] -> a, [str a b c] -> concat(concat(a,b),c).
                    // Each argument is coerced through `to_str`, so non-string
                    // values (e.g. integers) are formatted rather than read as
                    // bogus string pointers.
                    let args = &items[1..];
                    if args.is_empty() {
                        let (offset, len) = self.compiler.intern_string("");
                        self.instructions
                            .push(WasmInstruction::I64Const(((offset as i64) << 32) | len as i64));
                        return Ok(());
                    }
                    self.compiler.ensure_string_runtime();
                    let rt = self.compiler.string_runtime.clone().unwrap();
                    self.compile_expr(&args[0])?;
                    self.instructions
                        .push(WasmInstruction::Call(rt.to_str_idx));
                    for arg in &args[1..] {
                        self.compile_expr(arg)?;
                        self.instructions
                            .push(WasmInstruction::Call(rt.to_str_idx));
                        self.instructions
                            .push(WasmInstruction::Call(rt.str_concat_idx));
                    }
                    return Ok(());
                }
                "substring" => {
                    // [substring s start end] -> new packed string.
                    self.compiler.ensure_string_runtime();
                    let rt = self.compiler.string_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.compile_expr(&items[3])?;
                    self.instructions
                        .push(WasmInstruction::Call(rt.str_substring_idx));
                    return Ok(());
                }
                "char-at" => {
                    // Byte at index `i` of the packed string, as an int.
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::I64Const(32));
                    self.instructions.push(WasmInstruction::I64ShrU); // ptr (i64)
                    self.instructions.push(WasmInstruction::I32WrapI64);
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I32WrapI64);
                    self.instructions.push(WasmInstruction::I32Add); // ptr + i
                    self.instructions.push(WasmInstruction::I32Load8U(0, 0));
                    self.instructions.push(WasmInstruction::I64ExtendI32U);
                    return Ok(());
                }
                "str-eq" => {
                    self.compiler.ensure_string_runtime();
                    let rt = self.compiler.string_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::Call(rt.str_eq_idx));
                    return Ok(());
                }
                "vec-new" => {
                    self.compiler.ensure_collections_runtime();
                    let rt = self.compiler.collections_runtime.clone().unwrap();
                    self.instructions
                        .push(WasmInstruction::Call(rt.vec_new_idx));
                    return Ok(());
                }
                "vec-push" | "conj" => {
                    self.compiler.ensure_collections_runtime();
                    let rt = self.compiler.collections_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions
                        .push(WasmInstruction::Call(rt.vec_push_idx));
                    return Ok(());
                }
                "assoc" => {
                    self.compiler.ensure_maps_runtime();
                    let mr = self.compiler.maps_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.compile_expr(&items[3])?;
                    self.instructions
                        .push(WasmInstruction::Call(mr.map_assoc_idx));
                    return Ok(());
                }
                "get" => {
                    self.compiler.ensure_maps_runtime();
                    let mr = self.compiler.maps_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions
                        .push(WasmInstruction::Call(mr.map_get_idx));
                    return Ok(());
                }
                "entries" => {
                    // A map *is* an insertion-ordered vector of [k v] pairs, so
                    // `entries` is the identity on its argument.
                    self.compile_expr(&items[1])?;
                    return Ok(());
                }
                "split" => {
                    let split_idx = self.compiler.ensure_split_runtime();
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::Call(split_idx));
                    return Ok(());
                }
                "len" | "count" | "vec-len" => {
                    self.compile_expr(&items[1])?;
                    self.emit_seq_len();
                    return Ok(());
                }
                "vec-get" => {
                    self.compiler.ensure_collections_runtime();
                    let rt = self.compiler.collections_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions
                        .push(WasmInstruction::Call(rt.vec_get_idx));
                    return Ok(());
                }
                "first" => {
                    // first = vec-get v 0
                    self.compiler.ensure_collections_runtime();
                    let rt = self.compiler.collections_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::I64Const(0));
                    self.instructions
                        .push(WasmInstruction::Call(rt.vec_get_idx));
                    return Ok(());
                }
                "empty?" => {
                    // len(v) == 0, handling both strings and vectors/maps.
                    self.compile_expr(&items[1])?;
                    self.emit_seq_len();
                    self.instructions.push(WasmInstruction::I64Eqz);
                    self.instructions.push(WasmInstruction::I64ExtendI32U);
                    return Ok(());
                }
                "if" => {
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::I64Eqz);
                    self.instructions
                        .push(WasmInstruction::If(BlockType::Result(ValType::I64)));
                    if items.len() > 3 {
                        self.compile_expr(&items[3])?;
                    } else {
                        self.instructions.push(WasmInstruction::I64Const(0));
                    }
                    self.instructions.push(WasmInstruction::Else);
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::End);
                    return Ok(());
                }
                "let" => {
                    let (ni, vi) = if matches!(&items[1].kind, ExprKind::Symbol(s) if s == "mut") {
                        (3, 3)
                    } else {
                        (1, 2)
                    };
                    let name = match &items[ni].kind {
                        ExprKind::Symbol(s) => s.clone(),
                        _ => return Err("let binding must be a symbol".into()),
                    };
                    self.compile_expr(&items[vi])?;
                    let local = self.alloc_local();
                    self.locals.insert(name, local);
                    self.instructions.push(WasmInstruction::LocalSet(local));
                    self.instructions.push(WasmInstruction::LocalGet(local));
                    return Ok(());
                }
                "do" => {
                    for (i, item) in items[1..].iter().enumerate() {
                        self.compile_expr(item)?;
                        if i < items.len() - 2 {
                            self.instructions.push(WasmInstruction::Drop);
                        }
                    }
                    return Ok(());
                }
                "when" => {
                    // [when c body…] -> [if c [do body…] 0]
                    let cond = items[1].clone();
                    let mut body = vec![Expr::new(
                        ExprKind::Symbol("do".into()),
                        items[0].span,
                    )];
                    body.extend(items[2..].iter().cloned());
                    let do_expr = Expr::new(ExprKind::List(body), items[0].span);
                    let zero = Expr::new(ExprKind::Int(0), items[0].span);
                    let if_expr = Expr::new(
                        ExprKind::List(vec![
                            Expr::new(ExprKind::Symbol("if".into()), items[0].span),
                            cond,
                            do_expr,
                            zero,
                        ]),
                        items[0].span,
                    );
                    return self.compile_expr(&if_expr);
                }
                "unless" => {
                    // [unless c body…] -> [if c 0 [do body…]]
                    let cond = items[1].clone();
                    let mut body = vec![Expr::new(
                        ExprKind::Symbol("do".into()),
                        items[0].span,
                    )];
                    body.extend(items[2..].iter().cloned());
                    let do_expr = Expr::new(ExprKind::List(body), items[0].span);
                    let zero = Expr::new(ExprKind::Int(0), items[0].span);
                    let if_expr = Expr::new(
                        ExprKind::List(vec![
                            Expr::new(ExprKind::Symbol("if".into()), items[0].span),
                            cond,
                            zero,
                            do_expr,
                        ]),
                        items[0].span,
                    );
                    return self.compile_expr(&if_expr);
                }
                "cond" => {
                    // [cond c1 v1 c2 v2 … :else vd] -> right-nested ifs.
                    let sp = items[0].span;
                    let clauses = &items[1..];
                    // Build from the back.
                    let mut acc = Expr::new(ExprKind::Int(0), sp);
                    let mut i = clauses.len();
                    while i >= 2 {
                        let cond = &clauses[i - 2];
                        let val = &clauses[i - 1];
                        // `:else` (or `true`) is an unconditional default.
                        let is_default = matches!(&cond.kind, ExprKind::Keyword(k) if k == "else")
                            || matches!(&cond.kind, ExprKind::Bool(true));
                        acc = if is_default {
                            val.clone()
                        } else {
                            Expr::new(
                                ExprKind::List(vec![
                                    Expr::new(ExprKind::Symbol("if".into()), sp),
                                    cond.clone(),
                                    val.clone(),
                                    acc,
                                ]),
                                sp,
                            )
                        };
                        i -= 2;
                    }
                    return self.compile_expr(&acc);
                }
                "pipe" => {
                    // Thread-last: [pipe x s1 s2 …] feeds the running value in
                    // as the final argument of each step. Desugar to nested
                    // call AST and compile that (matches the EIR lowerer).
                    let args = &items[1..];
                    if args.is_empty() {
                        self.instructions.push(WasmInstruction::I64Const(0));
                        return Ok(());
                    }
                    let mut current = args[0].clone();
                    for step in &args[1..] {
                        let sp = step.span;
                        let call = match &step.kind {
                            ExprKind::List(parts) if !parts.is_empty() => {
                                let mut v = parts.clone();
                                v.push(current);
                                v
                            }
                            // A bare step (symbol or other callable): [step current]
                            _ => vec![step.clone(), current],
                        };
                        current = Expr::new(ExprKind::List(call), sp);
                    }
                    self.compile_expr(&current)?;
                    return Ok(());
                }
                "println" | "print" => {
                    let nl = s == "println";
                    if let Some(arg) = items.get(1) {
                        if let ExprKind::Str(s) = &arg.kind {
                            let msg = if nl { format!("{s}\n") } else { s.clone() };
                            let (offset, len) = self.compiler.intern_string(&msg);
                            self.instructions.push(WasmInstruction::I32Const(0));
                            self.instructions
                                .push(WasmInstruction::I32Const(offset as i32));
                            self.instructions.push(WasmInstruction::I32Store(2, 0));
                            self.instructions.push(WasmInstruction::I32Const(4));
                            self.instructions
                                .push(WasmInstruction::I32Const(len as i32));
                            self.instructions.push(WasmInstruction::I32Store(2, 0));
                            self.instructions.push(WasmInstruction::I32Const(1));
                            self.instructions.push(WasmInstruction::I32Const(0));
                            self.instructions.push(WasmInstruction::I32Const(1));
                            self.instructions.push(WasmInstruction::I32Const(8));
                            self.instructions.push(WasmInstruction::Call(0));
                            self.instructions.push(WasmInstruction::Drop);
                            self.instructions.push(WasmInstruction::I64Const(0));
                            return Ok(());
                        } else if self.expr_is_string(arg) {
                            // A computed string (e.g. [str a b], interpolation).
                            self.compile_expr(arg)?;
                            self.emit_print_str(nl);
                            return Ok(());
                        } else {
                            // A computed (non-literal) argument: evaluate it to
                            // an i64 and print it as a decimal at runtime.
                            self.compile_expr(arg)?;
                            self.emit_print_i64(nl);
                            return Ok(());
                        }
                    }
                    self.instructions.push(WasmInstruction::I64Const(0));
                    return Ok(());
                }
                "match" => {
                    if items.len() < 2 {
                        return Err("match requires a value".into());
                    }
                    self.compile_expr(&items[1])?;
                    let sc = self.alloc_local();
                    self.instructions.push(WasmInstruction::LocalSet(sc));
                    self.compile_match_arms(sc, &items[2..])?;
                    return Ok(());
                }
                "loop" => {
                    // [loop [v0 i0 v1 i1 …] body…] — loop variables become
                    // locals seeded with their inits; the body runs in a
                    // wasm `loop` producing an i64; `recur` updates the locals
                    // and branches back.
                    let bindings = match items.get(1).map(|e| &e.kind) {
                        Some(ExprKind::List(b)) => b.clone(),
                        _ => return Err("loop requires a [v init …] bindings list".into()),
                    };
                    let mut lvars = Vec::new();
                    let mut j = 0;
                    while j + 1 < bindings.len() {
                        let name = match &bindings[j].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err("loop variable must be a symbol".into()),
                        };
                        self.compile_expr(&bindings[j + 1])?;
                        let local = self.alloc_local();
                        self.instructions.push(WasmInstruction::LocalSet(local));
                        self.locals.insert(name, local);
                        lvars.push(local);
                        j += 2;
                    }
                    self.instructions
                        .push(WasmInstruction::Loop(BlockType::Result(ValType::I64)));
                    self.loop_starts.push(self.instructions.len());
                    self.loop_vars.push(lvars);
                    let body = &items[2..];
                    if body.is_empty() {
                        self.instructions.push(WasmInstruction::I64Const(0));
                    }
                    for (k, e) in body.iter().enumerate() {
                        self.compile_expr(e)?;
                        if k + 1 < body.len() {
                            self.instructions.push(WasmInstruction::Drop);
                        }
                    }
                    self.instructions.push(WasmInstruction::End);
                    self.loop_starts.pop();
                    self.loop_vars.pop();
                    return Ok(());
                }
                "recur" => {
                    let lvars = self
                        .loop_vars
                        .last()
                        .cloned()
                        .ok_or("recur outside of a loop")?;
                    let loop_start = *self.loop_starts.last().unwrap();
                    // Evaluate args into temporaries first (so a recur arg may
                    // safely read the current loop variables), then assign.
                    let mut temps = Vec::new();
                    for a in &items[1..] {
                        self.compile_expr(a)?;
                        let t = self.alloc_local();
                        self.instructions.push(WasmInstruction::LocalSet(t));
                        temps.push(t);
                    }
                    for (lv, t) in lvars.iter().zip(temps.iter()) {
                        self.instructions.push(WasmInstruction::LocalGet(*t));
                        self.instructions.push(WasmInstruction::LocalSet(*lv));
                    }
                    // Branch depth = control frames opened since the loop body.
                    let mut depth: i32 = 0;
                    for instr in &self.instructions[loop_start..] {
                        match instr {
                            WasmInstruction::If(_)
                            | WasmInstruction::Block(_)
                            | WasmInstruction::Loop(_) => depth += 1,
                            WasmInstruction::End => depth -= 1,
                            _ => {}
                        }
                    }
                    self.instructions.push(WasmInstruction::Br(depth.max(0) as u32));
                    return Ok(());
                }
                "map" | "filter" | "each" => {
                    // Thread-last: [op f coll] — apply f to each element.
                    if items.len() >= 3 {
                        return self.compile_vec_hof(s, &items[1], &items[2]);
                    }
                    return Err(format!("codegen: {s} requires a function and a collection"));
                }
                "fold" => {
                    // [fold init f coll] — left fold; f takes (acc, elem).
                    if items.len() >= 4 {
                        return self.compile_fold(&items[1], &items[2], &items[3]);
                    }
                    return Err("codegen: fold requires init, function, collection".into());
                }
                "update" => {
                    // [update m k f] — assoc m k (f (get m k)).
                    if items.len() >= 4 {
                        return self.compile_update(&items[1], &items[2], &items[3]);
                    }
                    return Err("codegen: update requires a map, key, and function".into());
                }
                "range" => {
                    // [range a b] — vector of a, a+1, …, b-1.
                    if items.len() >= 3 {
                        return self.compile_range(&items[1], &items[2]);
                    }
                    return Err("codegen: range requires start and end".into());
                }
                "take" => {
                    // [take n coll] — a vector of the first min(n, len) elements.
                    if items.len() >= 3 {
                        return self.compile_take(&items[1], &items[2]);
                    }
                    return Err("codegen: take requires a count and a collection".into());
                }
                "sort-by" => {
                    // [sort-by f coll] or [sort-by f :desc coll].
                    if items.len() >= 3 {
                        let keyfn = &items[1];
                        let coll = items.last().unwrap();
                        let desc = items.len() >= 4
                            && matches!(&items[2].kind, ExprKind::Keyword(k) if k == "desc");
                        return self.compile_sort_by(keyfn, coll, desc);
                    }
                    return Err("codegen: sort-by requires a function and a collection".into());
                }
                "type" | "use" | "effect" => {
                    self.instructions.push(WasmInstruction::I64Const(0));
                    return Ok(());
                }
                "handle" | "resume" | "try" => {
                    // Delimited continuations need to capture and resume a stack
                    // segment, which standalone wasm can't express without a
                    // whole-program CPS/trampoline transform. These run on the
                    // EIR VM (`loon run`) for now. Effect *operations*
                    // (`E.op …`) still compile to host imports.
                    return Err(format!(
                        "codegen: '{s}' (delimited continuations) is not supported by the \
                         wasm backend yet; run it on the VM with `loon run`"
                    ));
                }
                name => {
                    if let Some((tag, arity)) = self.compiler.adt_constructors.get(name).cloned() {
                        return self.compile_adt_constructor(name, tag, arity, &items[1..]);
                    }
                    let argc = items.len() - 1;
                    let fn_def = self.compiler.fn_map.get(name).cloned().or_else(|| {
                        // Multi-arity: clauses are keyed "name#arity".
                        self.compiler.fn_map.get(&format!("{name}#{argc}")).cloned()
                    });
                    if let Some(fn_def) = fn_def {
                        if fn_def.is_closure {
                            return self.compile_closure_call_named(name, &items[1..]);
                        }
                        for arg in &items[1..] {
                            self.compile_expr(arg)?;
                        }
                        self.instructions
                            .push(WasmInstruction::Call(fn_def.func_idx));
                        return Ok(());
                    }
                    if self.locals.contains_key(name) {
                        return self.compile_closure_call_local(name, &items[1..]);
                    }
                    return Err(format!("codegen: unknown function '{name}'"));
                }
            }
        }
        // Check for DotAccess head (Effect.op pattern)
        if let ExprKind::DotAccess(obj, op) = &items[0].kind {
            if let ExprKind::Symbol(effect) = &obj.kind {
                // `IO.println` writes a line to stdout exactly like the
                // `println` builtin — lower it to the same inline WASI path
                // rather than an (unlinked) effect import, so IO-printing
                // programs run on wasm. (`IO.print` is left alone: the VM emits
                // nothing for it, so lowering it would diverge.)
                if effect == "IO" && op == "println" {
                    let mut rewritten = Vec::with_capacity(items.len());
                    rewritten.push(Expr::new(ExprKind::Symbol("println".into()), items[0].span));
                    rewritten.extend(items[1..].iter().cloned());
                    return self.compile_call(&rewritten);
                }
                if effect.starts_with(char::is_uppercase) {
                    let import_idx = self.compiler.get_or_create_effect_import(effect, op);
                    for arg in &items[1..] {
                        self.compile_expr(arg)?;
                    }
                    self.instructions.push(WasmInstruction::Call(import_idx));
                    return Ok(());
                }
            }
        }
        Err("codegen: unsupported call form".into())
    }
    fn compile_match_arms(&mut self, scrutinee: u32, arms: &[Expr]) -> Result<(), String> {
        let parsed = self.parse_arms(arms);
        if self.try_compile_br_table(scrutinee, &parsed)? {
            return Ok(());
        }
        self.compile_match_arms_ifelse(scrutinee, arms)
    }
    fn parse_arms<'b>(&self, arms: &'b [Expr]) -> Vec<(&'b Expr, &'b Expr)> {
        let mut r = Vec::new();
        let mut i = 0;
        while i + 1 < arms.len() {
            r.push((&arms[i], &arms[i + 1]));
            i += 2;
        }
        r
    }
    fn try_compile_br_table(
        &mut self,
        scrutinee: u32,
        parsed: &[(&Expr, &Expr)],
    ) -> Result<bool, String> {
        if parsed.is_empty() {
            return Ok(false);
        }
        let mut int_arms: Vec<(i64, &Expr)> = Vec::new();
        let mut default_body: Option<&Expr> = None;
        let mut default_var: Option<String> = None;
        for (pat, body) in parsed {
            match &pat.kind {
                ExprKind::Int(n) => int_arms.push((*n, body)),
                ExprKind::Symbol(s) if s == "_" => {
                    default_body = Some(body);
                }
                ExprKind::Symbol(s) if !s.starts_with(char::is_uppercase) => {
                    default_var = Some(s.clone()); // bind the scrutinee to this name
                    default_body = Some(body);
                }
                _ => return Ok(false),
            }
        }
        if int_arms.is_empty() {
            return Ok(false);
        }
        int_arms.sort_by_key(|(n, _)| *n);
        let min_val = int_arms[0].0;
        let max_val = int_arms[int_arms.len() - 1].0;
        if min_val != 0 || (max_val - min_val) as usize != int_arms.len() - 1 {
            return Ok(false);
        }
        for (idx, (n, _)) in int_arms.iter().enumerate() {
            if *n != idx as i64 {
                return Ok(false);
            }
        }
        let nc = int_arms.len();
        self.instructions
            .push(WasmInstruction::Block(BlockType::Result(ValType::I64)));
        for _ in 0..=nc {
            self.instructions
                .push(WasmInstruction::Block(BlockType::Empty));
        }
        self.instructions.push(WasmInstruction::LocalGet(scrutinee));
        self.instructions.push(WasmInstruction::I32WrapI64);
        self.instructions.push(WasmInstruction::BrTable(
            (0..nc as u32).collect(),
            nc as u32,
        ));
        for (ci, (_, body)) in int_arms.iter().enumerate() {
            self.instructions.push(WasmInstruction::End);
            self.compile_expr(body)?;
            self.instructions
                .push(WasmInstruction::Br((nc - ci) as u32));
        }
        self.instructions.push(WasmInstruction::End);
        if let Some(body) = default_body {
            // Bind the catch-all variable (if any) to the scrutinee.
            if let Some(name) = default_var {
                let local = self.alloc_local();
                self.locals.insert(name, local);
                self.instructions.push(WasmInstruction::LocalGet(scrutinee));
                self.instructions.push(WasmInstruction::LocalSet(local));
            }
            self.compile_expr(body)?;
        } else {
            self.instructions.push(WasmInstruction::I64Const(0));
        }
        self.instructions.push(WasmInstruction::End);
        Ok(true)
    }
    fn compile_match_arms_ifelse(&mut self, scrutinee: u32, arms: &[Expr]) -> Result<(), String> {
        // Right-nested if/else chain, each `if` producing an i64:
        //   t1; if {b1} else { t2; if {b2} else { … default } } …
        // Every conditional arm opens one `if (Result I64)` and its `else`; a
        // wildcard / variable arm is the innermost else and ends the chain. All
        // opened `if`s are closed with `End` at the end (this is what makes
        // multi-arm and ADT matches produce valid wasm).
        let mut open = 0u32;
        let mut have_default = false;
        let mut i = 0;
        while i + 1 < arms.len() {
            let pattern = &arms[i];
            let body = &arms[i + 1];

            // A `_` or a plain variable (not a nullary constructor) is a
            // catch-all default: bind it (if named), compile its body, stop.
            let is_default = match &pattern.kind {
                ExprKind::Symbol(s) => {
                    s == "_"
                        || !matches!(
                            self.compiler.adt_constructors.get(s.as_str()),
                            Some((_, 0))
                        )
                }
                _ => false,
            };
            if is_default {
                if let ExprKind::Symbol(name) = &pattern.kind {
                    if name != "_" {
                        let local = self.alloc_local();
                        self.locals.insert(name.clone(), local);
                        self.instructions.push(WasmInstruction::LocalGet(scrutinee));
                        self.instructions.push(WasmInstruction::LocalSet(local));
                    }
                }
                self.compile_expr(body)?;
                have_default = true;
                break;
            }

            // A constructor pattern with fields: open the `if`, then bind fields
            // inside it before compiling the body.
            if let ExprKind::List(pat_items) = &pattern.kind {
                let cn = match pat_items.first().map(|e| &e.kind) {
                    Some(ExprKind::Symbol(s)) => s.clone(),
                    _ => return Err("match: malformed constructor pattern".into()),
                };
                let tag = self
                    .compiler
                    .adt_constructors
                    .get(cn.as_str())
                    .map(|(t, _)| *t)
                    .ok_or_else(|| format!("match: unknown constructor '{cn}'"))?;
                self.instructions.push(WasmInstruction::LocalGet(scrutinee));
                self.instructions.push(WasmInstruction::I32WrapI64);
                self.instructions.push(WasmInstruction::I64Load(3, 0));
                self.instructions.push(WasmInstruction::I64Const(tag as i64));
                self.instructions.push(WasmInstruction::I64Eq);
                self.instructions
                    .push(WasmInstruction::If(BlockType::Result(ValType::I64)));
                for (fi, fp) in pat_items[1..].iter().enumerate() {
                    if let ExprKind::Symbol(fn_) = &fp.kind {
                        if fn_ != "_" {
                            let local = self.alloc_local();
                            self.locals.insert(fn_.clone(), local);
                            self.instructions.push(WasmInstruction::LocalGet(scrutinee));
                            self.instructions.push(WasmInstruction::I32WrapI64);
                            self.instructions
                                .push(WasmInstruction::I64Load(3, (8 + fi * 8) as u32));
                            self.instructions.push(WasmInstruction::LocalSet(local));
                        }
                    }
                }
                self.compile_expr(body)?;
                self.instructions.push(WasmInstruction::Else);
                open += 1;
                i += 2;
                continue;
            }

            // A literal int or a nullary constructor: emit the test, then the
            // shared open-if / body / else below.
            match &pattern.kind {
                ExprKind::Int(n) => {
                    self.instructions.push(WasmInstruction::LocalGet(scrutinee));
                    self.instructions.push(WasmInstruction::I64Const(*n));
                    self.instructions.push(WasmInstruction::I64Eq);
                }
                ExprKind::Symbol(name) => {
                    let tag = self
                        .compiler
                        .adt_constructors
                        .get(name.as_str())
                        .map(|(t, _)| *t)
                        .unwrap_or(0);
                    self.instructions.push(WasmInstruction::LocalGet(scrutinee));
                    self.instructions.push(WasmInstruction::I32WrapI64);
                    self.instructions.push(WasmInstruction::I64Load(3, 0));
                    self.instructions.push(WasmInstruction::I64Const(tag as i64));
                    self.instructions.push(WasmInstruction::I64Eq);
                }
                _ => return Err("match: unsupported pattern".into()),
            }
            self.instructions
                .push(WasmInstruction::If(BlockType::Result(ValType::I64)));
            self.compile_expr(body)?;
            self.instructions.push(WasmInstruction::Else);
            open += 1;
            i += 2;
        }
        // Innermost else holds the default (a catch-all body was already
        // compiled above; otherwise fall back to unit).
        if !have_default {
            self.instructions.push(WasmInstruction::I64Const(0));
        }
        for _ in 0..open {
            self.instructions.push(WasmInstruction::End);
        }
        Ok(())
    }
    fn compile_closure(&mut self, args: &[Expr]) -> Result<(), String> {
        if args.is_empty() {
            return Err("closure requires params".into());
        }
        // Each parameter is either a plain symbol or a positional destructuring
        // pattern `[a b …]` (used for `[k v]` map entries), where `_` ignores a
        // slot. A destructuring param still occupies exactly one argument slot.
        let raw_params = match &args[0].kind {
            ExprKind::List(items) => items,
            _ => return Err("closure params must be a list".into()),
        };
        let mut params: Vec<ClosureParam> = Vec::new();
        for p in raw_params {
            match &p.kind {
                ExprKind::Symbol(s) => params.push(ClosureParam::Simple(s.clone())),
                ExprKind::List(subs) => {
                    let names = subs
                        .iter()
                        .map(|e| match &e.kind {
                            ExprKind::Symbol(s) if s != "_" => Some(s.clone()),
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    params.push(ClosureParam::Destructure(names));
                }
                _ => {
                    return Err("closure param must be a symbol or destructuring list".into());
                }
            }
        }
        let body = &args[1..];
        // All names bound by params (including destructured sub-names) are
        // excluded from the free-variable set.
        let mut bound: Vec<String> = Vec::new();
        for p in &params {
            match p {
                ClosureParam::Simple(s) => bound.push(s.clone()),
                ClosureParam::Destructure(names) => {
                    bound.extend(names.iter().flatten().cloned());
                }
            }
        }
        let free = capture::free_vars(&bound, body);
        let mut captures: Vec<(String, u32)> = Vec::new();
        for name in &free {
            if let Some(&idx) = self.locals.get(name) {
                captures.push((name.clone(), idx));
            }
        }
        let lname = format!("__closure_{}", self.compiler.lambda_counter);
        self.compiler.lambda_counter += 1;
        let tp = 1 + params.len();
        let idx = self.compiler.next_fn_idx;
        self.compiler.fn_map.insert(
            lname,
            FnDef {
                func_idx: idx,
                arity: tp,
                is_closure: true,
            },
        );
        self.compiler.next_fn_idx += 1;
        let ti = self.compiler.ensure_in_table(idx);
        let mut cctx = FnCtx {
            locals: HashMap::new(),
            local_count: tp as u32,
            instructions: Vec::new(),
            compiler: self.compiler,
            loop_starts: Vec::new(),
            loop_vars: Vec::new(),
        };
        cctx.locals.insert("__env_ptr".to_string(), 0);
        for (i, p) in params.iter().enumerate() {
            if let ClosureParam::Simple(s) = p {
                cctx.locals.insert(s.clone(), (i + 1) as u32);
            }
        }
        for (ci, (cn, _)) in captures.iter().enumerate() {
            let l = cctx.alloc_local();
            cctx.locals.insert(cn.clone(), l);
            cctx.instructions.push(WasmInstruction::LocalGet(0));
            cctx.instructions.push(WasmInstruction::I32WrapI64);
            cctx.instructions
                .push(WasmInstruction::I64Load(3, (ci * 8) as u32));
            cctx.instructions.push(WasmInstruction::LocalSet(l));
        }
        // Bind destructured sub-names by loading positional elements from the
        // pair/tuple in the param slot (vectors store data ptr at offset 16).
        for (i, p) in params.iter().enumerate() {
            if let ClosureParam::Destructure(names) = p {
                let slot = (i + 1) as u32;
                for (j, name) in names.iter().enumerate() {
                    let Some(name) = name else { continue };
                    let l = cctx.alloc_local();
                    cctx.instructions.push(WasmInstruction::LocalGet(slot));
                    cctx.instructions.push(WasmInstruction::I32WrapI64);
                    cctx.instructions.push(WasmInstruction::I64Load(3, 16));
                    cctx.instructions.push(WasmInstruction::I64Const((j * 8) as i64));
                    cctx.instructions.push(WasmInstruction::I64Add);
                    cctx.instructions.push(WasmInstruction::I32WrapI64);
                    cctx.instructions.push(WasmInstruction::I64Load(3, 0));
                    cctx.instructions.push(WasmInstruction::LocalSet(l));
                    cctx.locals.insert(name.clone(), l);
                }
            }
        }
        for expr in body {
            cctx.compile_expr(expr)?;
        }
        let cinstrs = cctx.instructions.clone();
        let clc = cctx.local_count;
        drop(cctx);
        self.compiler.push_function(
            idx,
            FunctionBody {
                params: vec![ValType::I64; tp],
                results: vec![ValType::I64],
                locals: if clc > tp as u32 {
                    vec![ValType::I64; (clc - tp as u32) as usize]
                } else {
                    vec![]
                },
                instructions: cinstrs,
            },
        );
        if captures.is_empty() {
            self.instructions
                .push(WasmInstruction::I64Const((ti as i64) << 32));
        } else {
            self.emit_alloc((captures.len() * 8) as u32);
            let el = self.alloc_local();
            self.instructions.push(WasmInstruction::LocalSet(el));
            for (ci, (_, sl)) in captures.iter().enumerate() {
                self.instructions.push(WasmInstruction::LocalGet(el));
                self.instructions.push(WasmInstruction::I32WrapI64);
                self.instructions.push(WasmInstruction::LocalGet(*sl));
                self.instructions
                    .push(WasmInstruction::I64Store(3, (ci * 8) as u32));
            }
            self.instructions
                .push(WasmInstruction::I64Const((ti as i64) << 32));
            self.instructions.push(WasmInstruction::LocalGet(el));
            self.instructions.push(WasmInstruction::I64Or);
        }
        Ok(())
    }
    /// Resolve a HOF function argument to something callable per element: a
    /// named top-level function (called directly) or a closure value held in a
    /// local (called via the table). Lambda literals are compiled to a closure
    /// value first.
    fn prepare_fn_arg(&mut self, f: &Expr) -> Result<FnRepr, String> {
        match &f.kind {
            ExprKind::List(items)
                if matches!(items.first().map(|e| &e.kind), Some(ExprKind::Symbol(s)) if s == "fn") =>
            {
                self.compile_closure(&items[1..])?;
                let l = self.alloc_local();
                self.instructions.push(WasmInstruction::LocalSet(l));
                Ok(FnRepr::Closure(l))
            }
            ExprKind::Symbol(name) => {
                if let Some(def) = self.compiler.fn_map.get(name).cloned() {
                    if def.is_closure {
                        if let Some(&l) = self.locals.get(name) {
                            return Ok(FnRepr::Closure(l));
                        }
                    }
                    Ok(FnRepr::Named(def.func_idx))
                } else if let Some(&l) = self.locals.get(name) {
                    Ok(FnRepr::Closure(l))
                } else {
                    Err(format!("codegen: HOF function '{name}' not found"))
                }
            }
            _ => Err("codegen: HOF requires a function argument".into()),
        }
    }
    /// Emit a single-argument application of `f` to the value in local `eloc`,
    /// leaving the result on the stack.
    fn emit_apply1(&mut self, f: &FnRepr, eloc: u32) {
        use WasmInstruction as W;
        match f {
            FnRepr::Named(idx) => {
                self.instructions.push(W::LocalGet(eloc));
                self.instructions.push(W::Call(*idx));
            }
            FnRepr::Closure(cl) => {
                // env = cl & 0xffffffff; ti = cl >> 32
                self.instructions.push(W::LocalGet(*cl));
                self.instructions.push(W::I64Const(0xFFFF_FFFF));
                self.instructions.push(W::I64And);
                self.instructions.push(W::LocalGet(eloc));
                self.instructions.push(W::LocalGet(*cl));
                self.instructions.push(W::I64Const(32));
                self.instructions.push(W::I64ShrU);
                self.instructions.push(W::I32WrapI64);
                let ty = self.get_or_create_indirect_type(2);
                self.instructions.push(W::CallIndirect(ty));
            }
        }
    }
    /// Load `len` (offset 0) and `data_ptr` (offset 16) of the vector held in
    /// `vloc` into freshly allocated locals, returning (len_local, data_local).
    fn emit_vec_header(&mut self, vloc: u32) -> (u32, u32) {
        use WasmInstruction as W;
        let nloc = self.alloc_local();
        self.instructions.push(W::LocalGet(vloc));
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::LocalSet(nloc));
        let dloc = self.alloc_local();
        self.instructions.push(W::LocalGet(vloc));
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I64Load(3, 16));
        self.instructions.push(W::LocalSet(dloc));
        (nloc, dloc)
    }
    fn compile_vec_hof(&mut self, kind: &str, f: &Expr, coll: &Expr) -> Result<(), String> {
        use WasmInstruction as W;
        self.compiler.ensure_collections_runtime();
        let rt = self.compiler.collections_runtime.clone().unwrap();
        self.compile_expr(coll)?;
        let vloc = self.alloc_local();
        self.instructions.push(W::LocalSet(vloc));
        let fr = self.prepare_fn_arg(f)?;
        let (nloc, dloc) = self.emit_vec_header(vloc);
        let rloc = self.alloc_local();
        if kind == "map" || kind == "filter" {
            self.instructions.push(W::Call(rt.vec_new_idx));
            self.instructions.push(W::LocalSet(rloc));
        }
        let iloc = self.alloc_local();
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalSet(iloc));
        let eloc = self.alloc_local();
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        // if i >= n break
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::LocalGet(nloc));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        // elem = data[i]
        self.instructions.push(W::LocalGet(dloc));
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::I64Const(8));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::LocalSet(eloc));
        match kind {
            "each" => {
                self.emit_apply1(&fr, eloc);
                self.instructions.push(W::Drop);
            }
            "map" => {
                self.instructions.push(W::LocalGet(rloc));
                self.emit_apply1(&fr, eloc);
                self.instructions.push(W::Call(rt.vec_push_idx));
                self.instructions.push(W::LocalSet(rloc));
            }
            "filter" => {
                self.emit_apply1(&fr, eloc);
                self.instructions.push(W::I64Eqz); // 1 if falsy
                self.instructions.push(W::If(BlockType::Empty));
                // then: falsy → skip
                self.instructions.push(W::Else);
                self.instructions.push(W::LocalGet(rloc));
                self.instructions.push(W::LocalGet(eloc));
                self.instructions.push(W::Call(rt.vec_push_idx));
                self.instructions.push(W::LocalSet(rloc));
                self.instructions.push(W::End);
            }
            _ => unreachable!(),
        }
        // i++
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(iloc));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End); // loop
        self.instructions.push(W::End); // block
        match kind {
            "map" | "filter" => self.instructions.push(W::LocalGet(rloc)),
            // `each` is run for effect; yield the source collection so it can
            // sit mid-pipe.
            _ => self.instructions.push(W::LocalGet(vloc)),
        }
        Ok(())
    }
    /// `[update m k f]` — a new map with `k` bound to `f` applied to its
    /// current value (0 if absent, per `map_get`).
    fn compile_update(&mut self, m: &Expr, k: &Expr, f: &Expr) -> Result<(), String> {
        use WasmInstruction as W;
        self.compiler.ensure_maps_runtime();
        let mr = self.compiler.maps_runtime.clone().unwrap();
        self.compile_expr(m)?;
        let mloc = self.alloc_local();
        self.instructions.push(W::LocalSet(mloc));
        self.compile_expr(k)?;
        let kloc = self.alloc_local();
        self.instructions.push(W::LocalSet(kloc));
        let fr = self.prepare_fn_arg(f)?;
        // cur = map_get(m, k)
        self.instructions.push(W::LocalGet(mloc));
        self.instructions.push(W::LocalGet(kloc));
        self.instructions.push(W::Call(mr.map_get_idx));
        let eloc = self.alloc_local();
        self.instructions.push(W::LocalSet(eloc));
        // newval = f(cur)
        self.emit_apply1(&fr, eloc);
        let vloc = self.alloc_local();
        self.instructions.push(W::LocalSet(vloc));
        // map_assoc(m, k, newval)
        self.instructions.push(W::LocalGet(mloc));
        self.instructions.push(W::LocalGet(kloc));
        self.instructions.push(W::LocalGet(vloc));
        self.instructions.push(W::Call(mr.map_assoc_idx));
        Ok(())
    }
    /// `[take n coll]` — the first `min(n, len)` elements of `coll` as a new
    /// vector.
    fn compile_take(&mut self, n: &Expr, coll: &Expr) -> Result<(), String> {
        use WasmInstruction as W;
        self.compiler.ensure_collections_runtime();
        let rt = self.compiler.collections_runtime.clone().unwrap();
        self.compile_expr(coll)?;
        let vloc = self.alloc_local();
        self.instructions.push(W::LocalSet(vloc));
        self.compile_expr(n)?;
        let nloc = self.alloc_local();
        self.instructions.push(W::LocalSet(nloc));
        let (lenloc, dloc) = self.emit_vec_header(vloc);
        let rloc = self.alloc_local();
        self.instructions.push(W::Call(rt.vec_new_idx));
        self.instructions.push(W::LocalSet(rloc));
        let iloc = self.alloc_local();
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalSet(iloc));
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        // if i >= len break
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::LocalGet(lenloc));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        // if i >= n break
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::LocalGet(nloc));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        // result = vec_push(result, data[i])
        self.instructions.push(W::LocalGet(rloc));
        self.instructions.push(W::LocalGet(dloc));
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::I64Const(8));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::Call(rt.vec_push_idx));
        self.instructions.push(W::LocalSet(rloc));
        // i++
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(iloc));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        self.instructions.push(W::LocalGet(rloc));
        Ok(())
    }
    /// `[sort-by f coll]` / `[sort-by f :desc coll]` — a stable sort of `coll`
    /// by the integer key `f` returns for each element. Matches the EIR VM:
    /// the key function is evaluated once per element, the sort is stable, and
    /// comparison is by integer key (`:desc` reverses). Implemented as an
    /// insertion sort over a scratch buffer of interleaved (key, value) i64
    /// pairs — fine for the small collections this targets.
    fn compile_sort_by(&mut self, keyfn: &Expr, coll: &Expr, desc: bool) -> Result<(), String> {
        use WasmInstruction as W;
        self.compiler.ensure_collections_runtime();
        let rt = self.compiler.collections_runtime.clone().unwrap();
        self.compile_expr(coll)?;
        let vloc = self.alloc_local();
        self.instructions.push(W::LocalSet(vloc));
        let fr = self.prepare_fn_arg(keyfn)?;
        let (nloc, dloc) = self.emit_vec_header(vloc);
        // base = heap_ptr; heap_ptr += n * 16 (scratch for n (key,val) pairs)
        let base = self.alloc_local();
        self.instructions.push(W::GlobalGet(0));
        self.instructions.push(W::I64ExtendI32U);
        self.instructions.push(W::LocalSet(base));
        self.instructions.push(W::GlobalGet(0));
        self.instructions.push(W::LocalGet(nloc));
        self.instructions.push(W::I64Const(16));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I32Add);
        self.instructions.push(W::GlobalSet(0));
        let i = self.alloc_local();
        let e = self.alloc_local();
        // address helper: push (base + idx*stride + off) as i32
        // (emitted inline below since closures can't borrow self here)
        // ── fill: scratch[i] = (f(data[i]), data[i]) ──
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalSet(i));
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        self.instructions.push(W::LocalGet(i));
        self.instructions.push(W::LocalGet(nloc));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        // e = data[i]
        self.instructions.push(W::LocalGet(dloc));
        self.instructions.push(W::LocalGet(i));
        self.instructions.push(W::I64Const(8));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::LocalSet(e));
        // scratch[i].key = f(e)
        self.emit_pair_addr(base, i);
        self.emit_apply1(&fr, e);
        self.instructions.push(W::I64Store(3, 0));
        // scratch[i].val = e
        self.emit_pair_addr(base, i);
        self.instructions.push(W::LocalGet(e));
        self.instructions.push(W::I64Store(3, 8));
        self.instructions.push(W::LocalGet(i));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(i));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        // ── insertion sort (stable) ──
        let j = self.alloc_local();
        let key_i = self.alloc_local();
        let val_i = self.alloc_local();
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::LocalSet(i));
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        self.instructions.push(W::LocalGet(i));
        self.instructions.push(W::LocalGet(nloc));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        // key_i = scratch[i].key; val_i = scratch[i].val
        self.emit_pair_addr(base, i);
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::LocalSet(key_i));
        self.emit_pair_addr(base, i);
        self.instructions.push(W::I64Load(3, 8));
        self.instructions.push(W::LocalSet(val_i));
        // j = i - 1
        self.instructions.push(W::LocalGet(i));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Sub);
        self.instructions.push(W::LocalSet(j));
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        // if j < 0 break
        self.instructions.push(W::LocalGet(j));
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::BrIf(1));
        // if not (scratch[j].key {>|<} key_i) break  — strict, so equal stays (stable)
        self.emit_pair_addr(base, j);
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::LocalGet(key_i));
        self.instructions
            .push(if desc { W::I64LtS } else { W::I64GtS });
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        // scratch[j+1] = scratch[j]  (shift the larger/smaller element right)
        self.emit_pair_addr_succ(base, j); // dst = scratch[j+1]
        self.emit_pair_addr(base, j); // src = scratch[j]
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::I64Store(3, 0));
        self.emit_pair_addr_succ(base, j);
        self.emit_pair_addr(base, j);
        self.instructions.push(W::I64Load(3, 8));
        self.instructions.push(W::I64Store(3, 8));
        // j--
        self.instructions.push(W::LocalGet(j));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Sub);
        self.instructions.push(W::LocalSet(j));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        // scratch[j+1] = (key_i, val_i)
        self.emit_pair_addr_succ(base, j);
        self.instructions.push(W::LocalGet(key_i));
        self.instructions.push(W::I64Store(3, 0));
        self.emit_pair_addr_succ(base, j);
        self.instructions.push(W::LocalGet(val_i));
        self.instructions.push(W::I64Store(3, 8));
        self.instructions.push(W::LocalGet(i));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(i));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        // ── build result vector from sorted values ──
        let r = self.alloc_local();
        self.instructions.push(W::Call(rt.vec_new_idx));
        self.instructions.push(W::LocalSet(r));
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalSet(i));
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        self.instructions.push(W::LocalGet(i));
        self.instructions.push(W::LocalGet(nloc));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        self.instructions.push(W::LocalGet(r));
        self.emit_pair_addr(base, i);
        self.instructions.push(W::I64Load(3, 8));
        self.instructions.push(W::Call(rt.vec_push_idx));
        self.instructions.push(W::LocalSet(r));
        self.instructions.push(W::LocalGet(i));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(i));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        self.instructions.push(W::LocalGet(r));
        Ok(())
    }
    /// Consume a sequence value on the stack and leave its length (i64). A
    /// packed string (value ≥ 2³², pointer in the high bits) carries its length
    /// in the low 32 bits; a vector/map is a heap pointer whose length lives at
    /// header offset 0.
    fn emit_seq_len(&mut self) {
        use WasmInstruction as W;
        let v = self.alloc_local();
        self.instructions.push(W::LocalSet(v));
        self.instructions.push(W::LocalGet(v));
        self.instructions.push(W::I64Const(0x1_0000_0000));
        self.instructions.push(W::I64GeS);
        self.instructions.push(W::If(BlockType::Result(ValType::I64)));
        self.instructions.push(W::LocalGet(v));
        self.instructions.push(W::I64Const(0xFFFF_FFFF));
        self.instructions.push(W::I64And);
        self.instructions.push(W::Else);
        self.instructions.push(W::LocalGet(v));
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::End);
    }
    /// Push `(base + idx*16)` as an i32 address (the start of pair `idx`).
    fn emit_pair_addr(&mut self, base: u32, idx: u32) {
        use WasmInstruction as W;
        self.instructions.push(W::LocalGet(base));
        self.instructions.push(W::LocalGet(idx));
        self.instructions.push(W::I64Const(16));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I32WrapI64);
    }
    /// Push `(base + (idx+1)*16)` as an i32 address (pair `idx+1`).
    fn emit_pair_addr_succ(&mut self, base: u32, idx: u32) {
        use WasmInstruction as W;
        self.instructions.push(W::LocalGet(base));
        self.instructions.push(W::LocalGet(idx));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I64Const(16));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I32WrapI64);
    }
    fn compile_fold(&mut self, init: &Expr, f: &Expr, coll: &Expr) -> Result<(), String> {
        use WasmInstruction as W;
        self.compiler.ensure_collections_runtime();
        self.compile_expr(coll)?;
        let vloc = self.alloc_local();
        self.instructions.push(W::LocalSet(vloc));
        let fr = self.prepare_fn_arg(f)?;
        let accloc = self.alloc_local();
        self.compile_expr(init)?;
        self.instructions.push(W::LocalSet(accloc));
        let (nloc, dloc) = self.emit_vec_header(vloc);
        let iloc = self.alloc_local();
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalSet(iloc));
        let eloc = self.alloc_local();
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::LocalGet(nloc));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        // elem = data[i]
        self.instructions.push(W::LocalGet(dloc));
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::I64Const(8));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::LocalSet(eloc));
        // acc = f(acc, elem)
        self.emit_apply2(&fr, accloc, eloc);
        self.instructions.push(W::LocalSet(accloc));
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(iloc));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        self.instructions.push(W::LocalGet(accloc));
        Ok(())
    }
    /// Two-argument application of `f` to (`a`, `b`) locals, result on stack.
    fn emit_apply2(&mut self, f: &FnRepr, a: u32, b: u32) {
        use WasmInstruction as W;
        match f {
            FnRepr::Named(idx) => {
                self.instructions.push(W::LocalGet(a));
                self.instructions.push(W::LocalGet(b));
                self.instructions.push(W::Call(*idx));
            }
            FnRepr::Closure(cl) => {
                self.instructions.push(W::LocalGet(*cl));
                self.instructions.push(W::I64Const(0xFFFF_FFFF));
                self.instructions.push(W::I64And);
                self.instructions.push(W::LocalGet(a));
                self.instructions.push(W::LocalGet(b));
                self.instructions.push(W::LocalGet(*cl));
                self.instructions.push(W::I64Const(32));
                self.instructions.push(W::I64ShrU);
                self.instructions.push(W::I32WrapI64);
                let ty = self.get_or_create_indirect_type(3);
                self.instructions.push(W::CallIndirect(ty));
            }
        }
    }
    fn compile_range(&mut self, a: &Expr, b: &Expr) -> Result<(), String> {
        use WasmInstruction as W;
        self.compiler.ensure_collections_runtime();
        let rt = self.compiler.collections_runtime.clone().unwrap();
        let iloc = self.alloc_local();
        self.compile_expr(a)?;
        self.instructions.push(W::LocalSet(iloc));
        let bloc = self.alloc_local();
        self.compile_expr(b)?;
        self.instructions.push(W::LocalSet(bloc));
        let rloc = self.alloc_local();
        self.instructions.push(W::Call(rt.vec_new_idx));
        self.instructions.push(W::LocalSet(rloc));
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::LocalGet(bloc));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        // r = vec_push(r, i)
        self.instructions.push(W::LocalGet(rloc));
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::Call(rt.vec_push_idx));
        self.instructions.push(W::LocalSet(rloc));
        // i++
        self.instructions.push(W::LocalGet(iloc));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(iloc));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        self.instructions.push(W::LocalGet(rloc));
        Ok(())
    }
    fn compile_closure_call_local(&mut self, name: &str, call_args: &[Expr]) -> Result<(), String> {
        let cl = *self
            .locals
            .get(name)
            .ok_or_else(|| format!("codegen: unbound closure '{name}'"))?;
        let el = self.alloc_local();
        let tl = self.alloc_local();
        self.instructions.push(WasmInstruction::LocalGet(cl));
        self.instructions
            .push(WasmInstruction::I64Const(0xFFFFFFFF));
        self.instructions.push(WasmInstruction::I64And);
        self.instructions.push(WasmInstruction::LocalSet(el));
        self.instructions.push(WasmInstruction::LocalGet(cl));
        self.instructions.push(WasmInstruction::I64Const(32));
        self.instructions.push(WasmInstruction::I64ShrU);
        self.instructions.push(WasmInstruction::LocalSet(tl));
        self.instructions.push(WasmInstruction::LocalGet(el));
        for arg in call_args {
            self.compile_expr(arg)?;
        }
        let ta = 1 + call_args.len();
        let ty = self.get_or_create_indirect_type(ta);
        self.instructions.push(WasmInstruction::LocalGet(tl));
        self.instructions.push(WasmInstruction::I32WrapI64);
        self.instructions.push(WasmInstruction::CallIndirect(ty));
        Ok(())
    }
    fn compile_closure_call_named(
        &mut self,
        _name: &str,
        _call_args: &[Expr],
    ) -> Result<(), String> {
        Err("codegen: named closure calls not yet supported".into())
    }
    fn compile_adt_constructor(
        &mut self,
        _name: &str,
        tag: u32,
        arity: usize,
        args: &[Expr],
    ) -> Result<(), String> {
        if args.len() != arity {
            return Err(format!(
                "codegen: constructor expects {} args, got {}",
                arity,
                args.len()
            ));
        }
        self.emit_alloc((8 + arity * 8) as u32);
        let pl = self.alloc_local();
        self.instructions.push(WasmInstruction::LocalTee(pl));
        self.instructions.push(WasmInstruction::I32WrapI64);
        self.instructions
            .push(WasmInstruction::I64Const(tag as i64));
        self.instructions.push(WasmInstruction::I64Store(3, 0));
        for (fi, arg) in args.iter().enumerate() {
            self.instructions.push(WasmInstruction::LocalGet(pl));
            self.instructions.push(WasmInstruction::I32WrapI64);
            self.compile_expr(arg)?;
            self.instructions
                .push(WasmInstruction::I64Store(3, (8 + fi * 8) as u32));
        }
        self.instructions.push(WasmInstruction::LocalGet(pl));
        Ok(())
    }
    fn emit_alloc(&mut self, size: u32) {
        self.instructions.push(WasmInstruction::GlobalGet(0));
        self.instructions.push(WasmInstruction::I64ExtendI32U);
        self.instructions.push(WasmInstruction::GlobalGet(0));
        self.instructions
            .push(WasmInstruction::I32Const(size as i32));
        self.instructions.push(WasmInstruction::I32Add);
        self.instructions.push(WasmInstruction::GlobalSet(0));
    }
    fn get_or_create_indirect_type(&mut self, ta: usize) -> u32 {
        if let Some(&ti) = self.compiler.indirect_type_cache.get(&ta) {
            return ti;
        }
        let ti = self.compiler.type_count;
        self.compiler.type_count += 1;
        self.compiler.indirect_type_cache.insert(ta, ti);
        ti
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    fn ok(src: &str) {
        assert_eq!(&compile(&parse(src).unwrap()).unwrap()[0..4], b"\0asm");
    }

    /// Compile and assert the bytes are a *valid* wasm module (not just magic).
    fn valid(src: &str) {
        let bytes = compile(&parse(src).unwrap()).unwrap();
        wasmparser::Validator::new()
            .validate_all(&bytes)
            .unwrap_or_else(|e| panic!("invalid wasm for {src:?}: {e}"));
    }

    #[test]
    fn compile_if_comparison_is_valid() {
        // `[if [< ..] ..]`: comparisons are widened to i64 so the `if`'s i64.eqz
        // on the condition type-checks (this used to produce invalid wasm).
        valid("[fn main [] [if [< 1 2] [println \"a\"] [println \"b\"]]]");
        valid(
            "[fn fib [n] [if [< n 2] n [+ [fib [- n 1]] [fib [- n 2]]]]] \
             [fn main [] [println [fib 10]]]",
        );
        valid("[fn f [n] [if [= n 0] 1 [* n [f [- n 1]]]]] [fn main [] [println [f 5]]]");
    }

    #[test]
    fn compile_println_int_is_valid() {
        // Printing a computed (non-literal) value emits a runtime itoa + fd_write.
        valid("[fn main [] [println [+ 1 2]]]");
        valid("[fn main [] [println [- 0 42]]]");
    }

    #[test]
    fn compile_multi_statement_is_valid() {
        // A function body / main with several statements: all but the last are
        // dropped (the sequence keeps only its final value).
        valid("[fn main [] [let x 5] [let y 6] [println [* x y]]]");
        valid("[fn main [] [println 7] [println 8] [println 9]]");
        valid("[fn g [a] [let b [* a 2]] [+ b 1]] [fn main [] [println [g 10]]]");
    }

    #[test]
    fn compile_loop_recur_is_valid() {
        valid("[fn main [] [println [loop [i 0 a 0] [if [>= i 5] a [recur [+ i 1] [+ a i]]]]]]");
        valid("[fn sumto [n] [loop [i 1 a 0] [if [> i n] a [recur [+ i 1] [+ a i]]]]] \
               [fn main [] [println [sumto 100]]]");
    }

    #[test]
    fn compile_match_is_valid() {
        valid("[type O [Some Int] [None]] [fn main [] [println [match [Some 5] [Some n] n [None] 0]]]");
        valid("[type C [R] [G] [B]] [fn f [c] [match c [R] 1 [G] 2 [B] 3]] [fn main [] [println [f [B]]]]");
        valid("[type L [Cons Int L] [Nil]] [fn s [xs] [match xs [Cons h t] [+ h [s t]] [Nil] 0]] \
               [fn main [] [println [s [Cons 10 [Cons 20 [Nil]]]]]]");
    }

    #[test]
    fn compile_operators_are_valid() {
        valid("[fn main [] [println [/ 17 5]]]");
        valid("[fn main [] [println [% 17 5]]]");
        valid("[fn main [] [println [if [and [<= 3 3] [>= 4 4]] 1 0]]]");
        valid("[fn main [] [println [if [or [> 1 2] [!= 3 4]] 1 0]]]");
        valid("[fn main [] [println [if [not [= 1 2]] 1 0]]]");
    }
    #[test]
    fn compile_hello_world() {
        ok(r#"[fn main [] [println "hello, world!"]]"#);
    }
    #[test]
    fn compile_println_string_is_valid() {
        // Printing a computed string takes the byte-printing path (not itoa).
        valid(r#"[fn main [] [println [str "a" "b"]]]"#);
        valid(r#"[fn main [] [let s [str "x" "y"]] [println s]]"#);
        valid(r#"[fn main [] [println [str "n=" [str "4" "2"]]]]"#);
    }
    #[test]
    fn compile_closure_and_collections_are_valid() {
        // The function-index decoupling must keep these runnable (imports +
        // _start intact). Regression guard for the push-order bug.
        valid(r#"[fn ap [f x] [f x]] [fn main [] [println [ap [fn [y] [* y 2]] 21]]]"#);
        valid(r#"[fn main [] [println [vec-get [vec-push [vec-push [vec-new] 10] 20] 1]]]"#);
    }
    #[test]
    fn compile_capturing_closure_is_valid() {
        // A closure that captures a free variable: the env-pointer was left on
        // the stack by a stray LocalTee, leaving 2 values where 1 was expected.
        valid(r#"[fn adder [n] [fn [x] [+ x n]]] [fn main [] [let f [adder 10]] [println [f 5]]]"#);
    }
    #[test]
    fn compile_self_recur_is_valid() {
        // `recur` inside a fn body loops back to the top (no stack growth).
        valid(r#"[fn c [n] [if [= n 0] 42 [recur [- n 1]]]] [fn main [] [println [c 1000000]]]"#);
        valid(
            r#"[fn fact [n acc] [if [<= n 1] acc [recur [- n 1] [* acc n]]]]
               [fn main [] [println [fact 10 1]]]"#,
        );
    }
    #[test]
    fn compile_conj_len_are_valid() {
        valid(r#"[fn main [] [println [len [conj [conj #[] 5] 9]]]]"#);
    }
    #[test]
    fn compile_char_at_is_valid() {
        valid(r#"[fn main [] [println [char-at "abc" 0]] [println [char-at "abc" 2]]]"#);
    }
    #[test]
    fn compile_substring_is_valid() {
        // substring returns a string, so println routes through the byte path.
        valid(r#"[fn main [] [println [substring "hello world" 0 5]]]"#);
        valid(r#"[fn main [] [println [substring "hello world" 6 11]]]"#);
    }
    #[test]
    fn compile_when_unless_cond_are_valid() {
        valid(r#"[fn main [] [when [> 5 3] [println 1]] [unless [> 3 5] [println 2]]]"#);
        valid(r#"[fn main [] [println [cond [> 1 2] 10 [< 1 2] 20 :else 30]]]"#);
    }
    #[test]
    fn compile_keywords_are_valid() {
        // Keywords intern to unique i64 ids; `=` compares them.
        valid(r#"[fn main [] [println [if [= :a :a] 1 0]] [println [if [= :a :b] 1 0]]]"#);
        valid(
            r#"[fn k [n] [if [> n 0] :pos :neg]]
               [fn main [] [println [if [= [k 5] :pos] 1 0]]]"#,
        );
    }
    #[test]
    fn compile_numeric_and_seq_builtins_are_valid() {
        valid(r#"[fn main [] [print 5] [print 6] [println 7]]"#);
        valid(r#"[fn main [] [println [mod 17 5]] [println [inc [dec 10]]]]"#);
        valid(r#"[fn main [] [println [abs [- 0 5]]] [println [max 3 [min 9 7]]]]"#);
        valid(r#"[fn main [] [println [if [empty? #[]] 1 0]] [println [first #[7 8 9]]]]"#);
    }
    #[test]
    fn compile_vec_hofs_are_valid() {
        // range + map/filter/each/fold over vectors, with both lambda-literal
        // and named-function arguments, applied per element via the table.
        valid(r#"[fn main [] [each [fn [x] [println x]] [range 0 4]]]"#);
        valid(r#"[fn sq [x] [* x x]] [fn main [] [each [fn [x] [println x]] [map sq [range 1 5]]]]"#);
        valid(r#"[fn main [] [each [fn [x] [println x]] [filter [fn [x] [> x 2]] [range 0 6]]]]"#);
        valid(r#"[fn main [] [println [fold 0 [fn [acc x] [+ acc x]] [range 1 11]]]]"#);
        valid(
            r#"[fn main [] [pipe [range 0 4] [map [fn [x] [* x 10]]] [each [fn [x] [println x]]]]]"#,
        );
    }
    #[test]
    fn compile_vector_literal_is_valid() {
        // #[a b c] desugars to vec-new + vec-push chain; #[] is the empty vec.
        valid(r#"[fn main [] [println [vec-get #[10 20 30] 2]]]"#);
        valid(r#"[fn main [] [println [vec-get [vec-push #[] 99] 0]]]"#);
    }
    #[test]
    fn compile_multi_arity_is_valid() {
        // Each clause becomes its own function; calls resolve by arg count.
        valid(
            r#"[fn f ([x] [* x 10]) ([x y] [+ x y])]
               [fn main [] [println [f 5]] [println [f 3 4]]]"#,
        );
    }
    #[test]
    fn compile_string_returning_fn_println_is_valid() {
        // println of a call whose function statically returns a string takes
        // the byte-printing path (fixpoint analysis), including through chains.
        valid(
            r#"[fn greet [n] [str "hi, " n]]
               [fn loud [s] [str s "!"]]
               [fn main [] [println [loud [greet "cam"]]]]"#,
        );
        valid(
            r#"[fn g ([n] [str "h " n]) ([a b] [str a b])]
               [fn main [] [println [g "x"]] [println [g "a" "b"]]]"#,
        );
    }
    #[test]
    fn compile_pipe_is_valid() {
        // Thread-last desugaring into nested calls.
        valid(r#"[fn main [] [println [pipe 10 [+ 5] [* 2]]]]"#);
        valid(r#"[fn d [x] [* x 2]] [fn main [] [println [pipe 20 d [+ 1]]]]"#);
    }
    #[test]
    fn compile_str_eq_is_valid() {
        // gen_str_eq's length-mismatch guard declared an i64 result on a
        // one-armed if whose then-arm returns — the empty else produced 0.
        valid(r#"[fn main [] [println [str-eq "ab" "ab"]]]"#);
        valid(r#"[fn main [] [println [str-eq "ab" "xyz"]]]"#);
    }
    #[test]
    fn compile_arithmetic() {
        ok(r#"[fn main [] [+ 1 2]]"#);
    }
    #[test]
    fn compile_fib() {
        ok(
            r#"[fn fib [n] [match n 0 0  1 1  n [+ [fib [- n 1]] [fib [- n 2]]]]] [fn main [] [fib 10]]"#,
        );
    }
    #[test]
    fn compile_lambda_lift() {
        ok(
            r#"[fn apply-offset [offset] [map [fn [x] [+ x offset]] offset]] [fn main [] [apply-offset 10]]"#,
        );
    }
    #[test]
    fn compile_closure_no_capture() {
        ok(r#"[fn main [] [let f [fn [x] [+ x 1]]] [f 41]]"#);
    }
    #[test]
    fn compile_closure_with_capture() {
        ok(r#"[fn main [] [let y 10] [let f [fn [x] [+ x y]]] [f 32]]"#);
    }
    #[test]
    fn compile_higher_order() {
        ok(r#"[fn apply [f x] [f x]] [fn main [] [apply [fn [x] [* x 2]] 21]]"#);
    }
    #[test]
    fn compile_adt_constructor_and_match() {
        ok(
            r#"[type Maybe T [Just T] Nothing] [fn main [] [let val [Just 42]] [match val [Just x] x Nothing 0]]"#,
        );
    }
    #[test]
    fn compile_adt_nullary_match() {
        ok(r#"[type Maybe T [Just T] Nothing] [fn main [] [match Nothing [Just x] x Nothing 0]]"#);
    }
    #[test]
    fn compile_string_concat() {
        ok(r#"[fn main [] [let a "hello"] [let b " world"] [str-concat a b]]"#);
    }
    #[test]
    fn compile_string_len() {
        ok(r#"[fn main [] [str-len "test"]]"#);
    }
    #[test]
    fn compile_string_eq() {
        ok(r#"[fn main [] [str-eq "abc" "abc"]]"#);
    }
    #[test]
    fn compile_string_str_alias() {
        ok(r#"[fn main [] [str "foo" "bar"]]"#);
    }
    #[test]
    fn compile_split_is_valid() {
        valid(r#"[fn main [] [println [len [split "a b c" " "]]]]"#);
        valid(r#"[fn main [] [println [str "first=" [first [split "x,y,z" ","]]]]]"#);
    }
    #[test]
    fn compile_destructuring_params_are_valid() {
        valid(
            r#"[fn main [] [each [fn [[word n]] [println [str word ": " n]]]
                 [entries [assoc [assoc {} "a" 5] "b" 7]]]]"#,
        );
        valid(r#"[fn main [] [println [len [map [fn [[_ n]] n] [entries {}]]]]]"#);
    }
    #[test]
    fn compile_sort_by_is_valid() {
        valid(r#"[fn main [] [println [len [sort-by [fn [x] x] #[3 1 2]]]]]"#);
        valid(r#"[fn main [] [println [len [sort-by [fn [[_ n]] n] :desc [entries {}]]]]]"#);
    }
    #[test]
    fn compile_string_len_and_empty_are_valid() {
        valid(r#"[fn main [] [println [len "hello"]]]"#);
        valid(r#"[fn main [] [println [if [empty? "hi"] 1 0]]]"#);
    }
    #[test]
    fn compile_take_is_valid() {
        valid(r#"[fn main [] [println [len [take 3 [range 0 10]]]]]"#);
        // taking more than exists yields the whole collection (no overflow).
        valid(r#"[fn main [] [println [len [take 99 [range 0 4]]]]]"#);
    }
    #[test]
    fn compile_update_is_valid() {
        valid(
            r#"[fn main [] [let m [update {} "a" [fn [n] [+ [or n 0] 1]]]] [println [get m "a"]]]"#,
        );
    }
    #[test]
    fn compile_map_operations_are_valid() {
        // Empty + non-empty literals, assoc, get, len, entries — string keys
        // (structural equality) and integer keys (raw equality).
        valid(r#"[fn main [] [println [len {}]]]"#);
        valid(r#"[fn main [] [let m {"x" 10 "y" 20}] [println [get m "x"]]]"#);
        valid(r#"[fn main [] [let m [assoc [assoc {} "a" 1] "b" 2]] [println [len m]]]"#);
        valid(r#"[fn main [] [let m [assoc {} 1 100]] [println [get m 1]]]"#);
        valid(r#"[fn main [] [println [len [entries {"a" 1 "b" 2}]]]]"#);
    }
    #[test]
    fn str_coerces_integers_to_decimal() {
        // Mixed string literals and integer values (incl. negatives/zero) must
        // produce a valid module; the int args route through to_str/int_to_str.
        valid(r#"[fn main [] [println [str "n=" 42]]]"#);
        valid(r#"[fn main [] [println [str "neg=" -7 " zero=" 0]]]"#);
        // A string-valued variable must still pass through unchanged.
        valid(r#"[fn greet [name] [str "hi, " name]] [fn main [] [println [greet "x"]]]"#);
    }
    #[test]
    fn synthesizes_main_from_toplevel_statements() {
        // No explicit `main`: the top-level statements become the entry point.
        valid(r#"[let x 5] [println [* x 2]]"#);
        // A `test` block is a definition, not a statement: with nothing else to
        // run, no `main` is synthesized (stays a valid, entry-less module).
        valid(r#"[fn f [n] [str "x" n]] [test t [] [assert-eq [f 1] "x1"]]"#);
    }
    #[test]
    fn compile_match_br_table() {
        ok(r#"[fn dispatch [x] [match x 0 100  1 200  2 300  _ 0]] [fn main [] [dispatch 1]]"#);
    }
    #[test]
    fn compile_match_br_table_no_default() {
        ok(r#"[fn dispatch [x] [match x 0 10  1 20  2 30]] [fn main [] [dispatch 2]]"#);
    }
    #[test]
    fn compile_match_noncontiguous_falls_back() {
        ok(r#"[fn dispatch [x] [match x 0 10  5 50  _ 0]] [fn main [] [dispatch 5]]"#);
    }
    #[test]
    fn compile_vec_operations() {
        ok(r#"[fn main [] [let v [vec-new]] [let v2 [vec-push v 42]] [vec-get v2 0]]"#);
    }
    #[test]
    fn compile_multi_file_error_on_missing() {
        let exprs = parse(r#"[use nonexistent.module] [fn main [] 42]"#).unwrap();
        let r = compile_with_imports(&exprs, std::path::Path::new("/tmp/loon_test_nonexistent"));
        assert!(r.is_err());
        assert!(r.unwrap_err().contains("cannot read module"));
    }
    #[test]
    fn compile_multi_file_skips_without_base() {
        ok(r#"[use some.module] [fn main [] 42]"#);
    }
    #[test]
    fn compile_multi_file_with_real_file() {
        let tmp = std::env::temp_dir().join("loon_test_multifile");
        let _ = std::fs::create_dir_all(&tmp);
        std::fs::write(tmp.join("math.loon"), "[fn double [x] [* x 2]]").unwrap();
        let exprs = parse(r#"[use math] [fn main [] [double 21]]"#).unwrap();
        assert_eq!(&compile_with_imports(&exprs, &tmp).unwrap()[0..4], b"\0asm");
        let _ = std::fs::remove_file(tmp.join("math.loon"));
        let _ = std::fs::remove_dir(&tmp);
    }
    #[test]
    fn compile_string_packed_representation() {
        ok(r#"[fn main [] "hi"]"#);
    }
    #[test]
    fn tree_shake_removes_unused_function() {
        let with_unused =
            compile(&parse(r#"[fn unused [x] [+ x 1]] [fn main [] 42]"#).unwrap()).unwrap();
        let without = compile(&parse(r#"[fn main [] 42]"#).unwrap()).unwrap();
        assert!(
            with_unused.len() == without.len(),
            "unused function should be stripped: {} vs {}",
            with_unused.len(),
            without.len()
        );
    }
    #[test]
    fn tree_shake_arithmetic_no_wasi() {
        let wasm = compile(&parse(r#"[fn main [] [+ 1 2]]"#).unwrap()).unwrap();
        assert!(
            !String::from_utf8_lossy(&wasm).contains("fd_write"),
            "pure arithmetic should not import fd_write"
        );
    }
    #[test]
    fn tree_shake_closure_still_works() {
        let with_unused = compile(
            &parse(r#"[fn unused [] 99] [fn main [] [let f [fn [x] [+ x 1]]] [f 41]]"#).unwrap(),
        )
        .unwrap();
        let without =
            compile(&parse(r#"[fn main [] [let f [fn [x] [+ x 1]]] [f 41]]"#).unwrap()).unwrap();
        assert_eq!(
            with_unused.len(),
            without.len(),
            "unused fn should be stripped even with closures"
        );
    }
    #[test]
    fn compile_effect_import() {
        // User-defined effect should compile to a WASM import call
        let wasm = compile(
            &parse(
                r#"
            [effect Fs [read-file [String] String]]
            [fn main [] [Fs.read-file "test.txt"]]
        "#,
            )
            .unwrap(),
        )
        .unwrap();
        assert_eq!(&wasm[0..4], b"\0asm", "should produce valid WASM");
        let wasm_str = String::from_utf8_lossy(&wasm);
        assert!(
            wasm_str.contains("loon:effects/fs"),
            "should contain effect import namespace"
        );
        assert!(
            wasm_str.contains("read-file"),
            "should contain effect op name"
        );
    }
    #[test]
    fn compile_effect_with_helper_fn_is_valid() {
        // Regression: effect imports and ordinary functions draw indices from
        // the same counter but interleave, so the old position-based remap
        // corrupted both. Both the effect call and the helper must survive.
        valid(
            r#"[effect Log [info [String] Unit]]
               [fn helper [x] [* x x]]
               [fn main [] [do [Log.info "hi"] [println [helper 7]]]]"#,
        );
    }
    #[test]
    fn compile_effect_declaration_only() {
        // Effect declaration without usage should compile fine
        ok(r#"[effect Fs [read-file [String] String]] [fn main [] 42]"#);
    }
    #[test]
    fn compile_effect_multi_ops() {
        let wasm = compile(
            &parse(
                r#"
            [effect Fs
                [read-file [String] String]
                [write-file [String String] Unit]]
            [fn main [] [do [Fs.write-file "out" "data"] [Fs.read-file "in"]]]
        "#,
            )
            .unwrap(),
        )
        .unwrap();
        assert_eq!(&wasm[0..4], b"\0asm");
        let wasm_str = String::from_utf8_lossy(&wasm);
        assert!(
            wasm_str.contains("read-file"),
            "should have read-file import"
        );
        assert!(
            wasm_str.contains("write-file"),
            "should have write-file import"
        );
    }
    #[test]
    fn compile_effect_tree_shake_preserves_imports() {
        // Effect imports should survive tree-shaking
        let wasm = compile(
            &parse(
                r#"
            [effect Fs [read-file [String] String]]
            [fn main [] [Fs.read-file "test"]]
        "#,
            )
            .unwrap(),
        )
        .unwrap();
        let wasm_str = String::from_utf8_lossy(&wasm);
        assert!(
            wasm_str.contains("loon:effects/fs"),
            "effect import should survive tree-shaking"
        );
    }
}
