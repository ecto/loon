mod capture;
#[allow(clippy::vec_init_then_push)]
pub mod collections;
#[allow(clippy::vec_init_then_push)]
pub mod maps;
#[allow(clippy::vec_init_then_push)]
pub mod strings;

use crate::ast::{Expr, ExprKind, NodeId};
use crate::types::Type;
use collections::CollectionsRuntime;
use maps::MapRuntime;
use std::collections::HashMap;
use strings::StringRuntime;
use wasm_encoder::*;

/// Run the type checker over an (already macro-expanded) program and return a
/// map from each node to its *resolved* type. Codegen consults this to make
/// type-directed decisions (e.g. how to print a value) rather than re-deriving
/// the information from syntax. Type errors are ignored here — the checker is a
/// separate front door (`loon check`); unresolved nodes simply fall back to the
/// untyped path.
fn infer_node_types(exprs: &[Expr], base_dir: Option<&std::path::Path>) -> HashMap<NodeId, Type> {
    let mut checker = match base_dir {
        Some(dir) => crate::check::Checker::with_base_dir(dir),
        None => crate::check::Checker::new(),
    };
    let _ = checker.check_program(exprs);
    checker
        .type_of
        .iter()
        .map(|(id, ty)| (*id, checker.resolve(ty)))
        .collect()
}

/// Compile a Loon program to WASM bytes.
pub fn compile(exprs: &[Expr]) -> Result<Vec<u8>, String> {
    // Macro expansion phase
    let mut expander = crate::macros::MacroExpander::new();
    let expanded = expander.expand_program(exprs)?;

    let mut compiler = Compiler::new();
    compiler.node_types = infer_node_types(&expanded, None);
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
    compiler.node_types = infer_node_types(&expanded, Some(base_dir));
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
/// Dynamic effect-handler stack, in low memory (below the string table at
/// 1024 and the itoa scratch at ~520). `HANDLER_COUNT` holds the number of
/// installed (op_id, closure) frames; each frame is 16 bytes from `HANDLER_BASE`.
const HANDLER_COUNT_ADDR: i32 = 248;
const HANDLER_BASE: i32 = 256;

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
    map_runtime: Option<MapRuntime>,
    /// Lazily-generated `str_split` (needs both string + vector runtimes).
    str_split_idx: Option<u32>,
    base_dir: Option<std::path::PathBuf>,
    compiled_modules: std::collections::HashSet<std::path::PathBuf>,
    force_heap: bool,
    /// Force a (possibly empty) function table to exist. Effect operations emit
    /// a `call_indirect` for the handler-closure path even when a program has no
    /// closures of its own, and `call_indirect` requires a table.
    force_table: bool,
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
    /// For each function key, the names of its string-typed parameters.
    string_params: HashMap<String, Vec<String>>,
    /// Distinct keyword literals interned to unique i64 ids (for `=` and use as
    /// enum-like tags). Ids start high to avoid colliding with small ints.
    keywords: HashMap<String, i64>,
    /// Distinct effect operations ("Effect.op") interned to small ids, used as
    /// the key in the dynamic handler stack.
    effect_op_ids: HashMap<String, i64>,
    /// Resolved type of each AST node (from the checker). Lets codegen make
    /// type-directed choices instead of guessing from syntax. Synthesized nodes
    /// (desugared `pipe`/`when`/… ) are absent and fall back to the untyped path.
    node_types: HashMap<NodeId, Type>,
    /// Function keys (`name`/`name#arity`) whose result is float. The checker
    /// generalizes polymorphic bodies, so concrete float-ness of a function's
    /// result and params isn't on the body nodes — this structural fixpoint
    /// recovers it so float ops compile inside function bodies.
    float_fns: std::collections::HashSet<String>,
    /// For each function key, the names of its float-typed parameters.
    float_params: HashMap<String, Vec<String>>,
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
    F64Div,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
    F64Eq,
    F64Ne,
    F64ReinterpretI64,
    I64ReinterpretF64,
    F64ConvertI64S,
    I64TruncF64S,
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
    I32And,
    I32Add,
    I32Sub,
    I32Mul,
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
        WasmInstruction::F64Div => {
            f.instruction(&Instruction::F64Div);
        }
        WasmInstruction::F64Lt => {
            f.instruction(&Instruction::F64Lt);
        }
        WasmInstruction::F64Gt => {
            f.instruction(&Instruction::F64Gt);
        }
        WasmInstruction::F64Le => {
            f.instruction(&Instruction::F64Le);
        }
        WasmInstruction::F64Ge => {
            f.instruction(&Instruction::F64Ge);
        }
        WasmInstruction::F64Eq => {
            f.instruction(&Instruction::F64Eq);
        }
        WasmInstruction::F64Ne => {
            f.instruction(&Instruction::F64Ne);
        }
        WasmInstruction::F64ReinterpretI64 => {
            f.instruction(&Instruction::F64ReinterpretI64);
        }
        WasmInstruction::I64ReinterpretF64 => {
            f.instruction(&Instruction::I64ReinterpretF64);
        }
        WasmInstruction::F64ConvertI64S => {
            f.instruction(&Instruction::F64ConvertI64S);
        }
        WasmInstruction::I64TruncF64S => {
            f.instruction(&Instruction::I64TruncF64S);
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
        WasmInstruction::I32Sub => {
            f.instruction(&Instruction::I32Sub);
        }
        WasmInstruction::I32Mul => {
            f.instruction(&Instruction::I32Mul);
        }
        WasmInstruction::I32And => {
            f.instruction(&Instruction::I32And);
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
            map_runtime: None,
            str_split_idx: None,
            base_dir: None,
            compiled_modules: std::collections::HashSet::new(),
            force_heap: false,
            force_table: false,
            used_wasi_imports: None,
            effect_imports: HashMap::new(),
            effect_import_defs: Vec::new(),
            effect_registry: crate::effects::EffectRegistry::new(),
            string_fns: std::collections::HashSet::new(),
            string_params: HashMap::new(),
            keywords: HashMap::new(),
            effect_op_ids: HashMap::new(),
            node_types: HashMap::new(),
            float_fns: std::collections::HashSet::new(),
            float_params: HashMap::new(),
        }
    }
    /// The resolved type of an AST node, if the checker inferred one.
    fn node_type(&self, expr: &Expr) -> Option<&Type> {
        self.node_types.get(&expr.id)
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
    /// Intern an effect operation key ("Effect.op") to a small positive id used
    /// as the lookup key in the dynamic handler stack.
    fn intern_effect_op(&mut self, key: &str) -> i64 {
        if let Some(&id) = self.effect_op_ids.get(key) {
            return id;
        }
        let id = self.effect_op_ids.len() as i64 + 1;
        self.effect_op_ids.insert(key.to_string(), id);
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
        let lc = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(lc, StringRuntime::gen_lowercase());
        self.string_runtime = Some(StringRuntime {
            str_concat_idx: c,
            str_len_idx: l,
            str_eq_idx: e,
            str_substring_idx: sub,
            int_to_str_idx: its,
            lowercase_idx: lc,
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
    fn ensure_map_runtime(&mut self) {
        if self.map_runtime.is_some() {
            return;
        }
        self.force_heap = true;
        // Maps reuse str_eq (string keys) and the vector runtime (for `keys`).
        self.ensure_string_runtime();
        let str_eq = self.string_runtime.clone().unwrap().str_eq_idx;
        self.ensure_collections_runtime();
        let cr = self.collections_runtime.clone().unwrap();
        let new_idx = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(new_idx, MapRuntime::gen_map_new());
        let set_idx = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(set_idx, MapRuntime::gen_map_set(str_eq));
        let get_idx = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(get_idx, MapRuntime::gen_map_get(str_eq));
        let has_idx = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(has_idx, MapRuntime::gen_map_has(str_eq));
        let keys_idx = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(
            keys_idx,
            MapRuntime::gen_map_keys(cr.vec_new_idx, cr.vec_push_idx),
        );
        let entries_idx = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(
            entries_idx,
            MapRuntime::gen_map_entries(cr.vec_new_idx, cr.vec_push_idx),
        );
        let merge_idx = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(merge_idx, MapRuntime::gen_map_merge(set_idx));
        self.map_runtime = Some(MapRuntime {
            map_new_idx: new_idx,
            map_set_idx: set_idx,
            map_get_idx: get_idx,
            map_has_idx: has_idx,
            map_keys_idx: keys_idx,
            map_entries_idx: entries_idx,
            map_merge_idx: merge_idx,
        });
    }
    fn ensure_split_runtime(&mut self) -> u32 {
        if let Some(idx) = self.str_split_idx {
            return idx;
        }
        self.force_heap = true;
        self.ensure_string_runtime();
        let substr = self.string_runtime.clone().unwrap().str_substring_idx;
        self.ensure_collections_runtime();
        let cr = self.collections_runtime.clone().unwrap();
        let idx = self.next_fn_idx;
        self.next_fn_idx += 1;
        self.push_function(
            idx,
            StringRuntime::gen_str_split(cr.vec_new_idx, cr.vec_push_idx, substr),
        );
        self.str_split_idx = Some(idx);
        idx
    }
    fn compile_program(&mut self, exprs: &[Expr]) -> Result<(), String> {
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
        self.analyze_types(exprs);
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
    /// Structural float check: whether `e` evaluates to a float, given the set
    /// of in-scope float-named bindings and the known float-returning functions.
    /// Used both by the whole-program fixpoint and (at runtime) by codegen.
    fn is_float_static(
        e: &Expr,
        floats: &std::collections::HashSet<String>,
        float_fns: &std::collections::HashSet<String>,
    ) -> bool {
        match &e.kind {
            ExprKind::Float(_) => true,
            ExprKind::Symbol(s) => floats.contains(s),
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(h) = &items[0].kind {
                    let argc = items.len() - 1;
                    match h.as_str() {
                        "+" | "-" | "*" | "/" | "min" | "max" if argc >= 2 => {
                            Self::is_float_static(&items[1], floats, float_fns)
                                || Self::is_float_static(&items[2], floats, float_fns)
                        }
                        "abs" | "inc" | "dec" if argc >= 1 => {
                            Self::is_float_static(&items[1], floats, float_fns)
                        }
                        "if" if argc >= 3 => {
                            Self::is_float_static(&items[2], floats, float_fns)
                                || Self::is_float_static(&items[3], floats, float_fns)
                        }
                        "do" => items
                            .last()
                            .is_some_and(|x| Self::is_float_static(x, floats, float_fns)),
                        "match" if argc >= 3 => {
                            // Arms are (pat, body) pairs from items[2..]. A
                            // well-typed match has all arms the same type, so
                            // any float arm body means the whole match is float
                            // (this also sees through pattern-bound vars that
                            // are themselves untracked, e.g. ADT float fields).
                            let mut i = 3;
                            while i < items.len() {
                                if Self::is_float_static(&items[i], floats, float_fns) {
                                    return true;
                                }
                                i += 2;
                            }
                            false
                        }
                        _ => {
                            float_fns.contains(h.as_str())
                                || float_fns.contains(&format!("{h}#{argc}"))
                        }
                    }
                } else {
                    false
                }
            }
            _ => false,
        }
    }
    /// Whole-program fixpoint recovering which functions return floats and which
    /// of their parameters are float. The checker generalizes polymorphic
    /// bodies, so this structural pass fills the gap for monomorphic float use.
    fn analyze_types(&mut self, exprs: &[Expr]) {
        // fn key -> param names; and (key, last body expr) per clause.
        let mut fn_params: HashMap<String, Vec<String>> = HashMap::new();
        let mut fn_bodies: Vec<(String, &Expr)> = Vec::new();
        // (enclosing fn key, callee name, arg exprs)
        let mut calls: Vec<(Option<String>, String, Vec<&Expr>)> = Vec::new();
        fn collect_calls<'e>(
            e: &'e Expr,
            enc: Option<&str>,
            out: &mut Vec<(Option<String>, String, Vec<&'e Expr>)>,
        ) {
            match &e.kind {
                ExprKind::List(items) if !items.is_empty() => {
                    if let ExprKind::Symbol(h) = &items[0].kind {
                        if h == "fn" {
                            return; // don't descend into nested closures
                        }
                        out.push((
                            enc.map(String::from),
                            h.clone(),
                            items[1..].iter().collect(),
                        ));
                    }
                    for it in items {
                        collect_calls(it, enc, out);
                    }
                }
                ExprKind::Vec(items) | ExprKind::Set(items) | ExprKind::Tuple(items) => {
                    for it in items {
                        collect_calls(it, enc, out);
                    }
                }
                _ => {}
            }
        }
        for expr in exprs {
            if let ExprKind::List(items) = &expr.kind {
                if items.len() >= 3 && matches!(&items[0].kind, ExprKind::Symbol(s) if s == "fn") {
                    if let ExprKind::Symbol(name) = &items[1].kind {
                        let args = &items[1..];
                        if Self::is_multi_arity(args) {
                            for clause in &args[1..] {
                                if let ExprKind::Tuple(parts) = &clause.kind {
                                    if let Some(ExprKind::List(params)) =
                                        parts.first().map(|e| &e.kind)
                                    {
                                        let pnames = Self::param_names(params);
                                        let key = format!("{name}#{}", pnames.len());
                                        fn_params.insert(key.clone(), pnames);
                                        if let Some(last) = parts.last() {
                                            fn_bodies.push((key.clone(), last));
                                        }
                                        for p in &parts[1..] {
                                            collect_calls(p, Some(&key), &mut calls);
                                        }
                                    }
                                }
                            }
                        } else if let ExprKind::List(params) = &items[2].kind {
                            let pnames = Self::param_names(params);
                            fn_params.insert(name.clone(), pnames);
                            if let Some(last) = items.last() {
                                fn_bodies.push((name.clone(), last));
                            }
                            for p in &items[3..] {
                                collect_calls(p, Some(name), &mut calls);
                            }
                        }
                    }
                } else {
                    collect_calls(expr, None, &mut calls);
                }
            }
        }
        // Run the same return/param fixpoint for floats and for strings, using
        // the matching structural predicate.
        let (float_fns, float_params) =
            Self::propagate_type(&fn_bodies, &calls, &fn_params, Self::is_float_static);
        self.float_fns = float_fns;
        self.float_params = float_params;
        let (string_fns, string_params) =
            Self::propagate_type(&fn_bodies, &calls, &fn_params, Self::is_str_static);
        self.string_fns = string_fns;
        self.string_params = string_params;
    }
    /// Generic whole-program fixpoint: given a structural predicate `pred` that
    /// decides whether an expression is of the type in question (consulting the
    /// in-scope names + the set of functions returning that type), compute which
    /// functions return that type and which of their params carry it.
    #[allow(clippy::type_complexity)]
    fn propagate_type(
        fn_bodies: &[(String, &Expr)],
        calls: &[(Option<String>, String, Vec<&Expr>)],
        fn_params: &HashMap<String, Vec<String>>,
        pred: fn(&Expr, &std::collections::HashSet<String>, &std::collections::HashSet<String>) -> bool,
    ) -> (
        std::collections::HashSet<String>,
        HashMap<String, Vec<String>>,
    ) {
        let mut ret_fns: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut params: HashMap<String, std::collections::HashSet<String>> = HashMap::new();
        loop {
            let mut changed = false;
            for (key, last) in fn_bodies {
                let names = params.get(key).cloned().unwrap_or_default();
                if !ret_fns.contains(key) && pred(last, &names, &ret_fns) {
                    ret_fns.insert(key.clone());
                    changed = true;
                }
            }
            for (enc, callee, args) in calls {
                let callee_key = if fn_params.contains_key(callee) {
                    callee.clone()
                } else {
                    let k = format!("{callee}#{}", args.len());
                    if fn_params.contains_key(&k) {
                        k
                    } else {
                        continue;
                    }
                };
                let enc_names = enc
                    .as_ref()
                    .and_then(|k| params.get(k))
                    .cloned()
                    .unwrap_or_default();
                let pnames = fn_params[&callee_key].clone();
                for (i, arg) in args.iter().enumerate() {
                    if i >= pnames.len() {
                        break;
                    }
                    if pred(arg, &enc_names, &ret_fns) {
                        let set = params.entry(callee_key.clone()).or_default();
                        if set.insert(pnames[i].clone()) {
                            changed = true;
                        }
                    }
                }
            }
            if !changed {
                break;
            }
        }
        let params = params
            .into_iter()
            .map(|(k, v)| (k, v.into_iter().collect()))
            .collect();
        (ret_fns, params)
    }
    /// Structural string check (mirrors `is_float_static`): whether `e`
    /// evaluates to a string, given in-scope string-named bindings and the set
    /// of known string-returning functions.
    fn is_str_static(
        e: &Expr,
        strs: &std::collections::HashSet<String>,
        string_fns: &std::collections::HashSet<String>,
    ) -> bool {
        match &e.kind {
            ExprKind::Str(_) => true,
            ExprKind::Symbol(s) => strs.contains(s),
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(h) = &items[0].kind {
                    let argc = items.len() - 1;
                    match h.as_str() {
                        "str" | "str-concat" | "substring" | "lowercase" => true,
                        "do" => items
                            .last()
                            .is_some_and(|e| Self::is_str_static(e, strs, string_fns)),
                        "if" if argc >= 3 => {
                            Self::is_str_static(&items[2], strs, string_fns)
                                || Self::is_str_static(&items[3], strs, string_fns)
                        }
                        "match" if argc >= 3 => {
                            let mut i = 3;
                            while i < items.len() {
                                if Self::is_str_static(&items[i], strs, string_fns) {
                                    return true;
                                }
                                i += 2;
                            }
                            false
                        }
                        _ => {
                            string_fns.contains(h.as_str())
                                || string_fns.contains(&format!("{h}#{argc}"))
                        }
                    }
                } else {
                    false
                }
            }
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
        let mut reachable = std::collections::HashSet::new();
        let mut queue = std::collections::VecDeque::new();
        // Root the reachability at `main`. With no `main` (e.g. a top-level
        // script or a library module), keep *all* functions — we can't prune,
        // but this pass also performs the essential index relocation, so it
        // must still run to keep Call targets / `_start` consistent.
        match self.fn_map.get("main") {
            Some(def) => {
                reachable.insert(def.func_idx);
                queue.push_back(def.func_idx);
            }
            None => {
                for &id in &self.fn_indices {
                    if reachable.insert(id) {
                        queue.push_back(id);
                    }
                }
            }
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
                        let key = format!("{name}#{}", params.len());
                        self.compile_fn_body(&key, false, params, &parts[1..])?;
                    }
                }
            }
            return Ok(());
        }
        let params: Vec<Expr> = match &args[1].kind {
            ExprKind::List(items) => items.clone(),
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
        param_exprs: &[Expr],
        body: &[Expr],
    ) -> Result<(), String> {
        let arity = param_exprs.len();
        // Params known to be float/string (from the fixpoint) seed the locals.
        let float_locals: std::collections::HashSet<String> = self
            .float_params
            .get(key)
            .map(|v| v.iter().cloned().collect())
            .unwrap_or_default();
        let string_locals: std::collections::HashSet<String> = self
            .string_params
            .get(key)
            .map(|v| v.iter().cloned().collect())
            .unwrap_or_default();
        let mut ctx = FnCtx {
            locals: HashMap::new(),
            local_count: arity as u32,
            instructions: Vec::new(),
            compiler: self,
            loop_starts: Vec::new(),
            loop_vars: Vec::new(),
            float_locals,
            string_locals,
        };
        // Bind params positionally; a list/tuple-pattern param destructures the
        // (pair) argument via vec-get into named locals (`_` is a wildcard).
        for (i, p) in param_exprs.iter().enumerate() {
            match &p.kind {
                ExprKind::Symbol(s) => {
                    ctx.locals.insert(s.clone(), i as u32);
                }
                ExprKind::List(sub) | ExprKind::Tuple(sub) => {
                    ctx.compiler.ensure_collections_runtime();
                    let vget = ctx.compiler.collections_runtime.clone().unwrap().vec_get_idx;
                    for (j, e) in sub.iter().enumerate() {
                        if let ExprKind::Symbol(s) = &e.kind {
                            if s == "_" {
                                continue;
                            }
                            let l = ctx.alloc_local();
                            ctx.instructions.push(WasmInstruction::LocalGet(i as u32));
                            ctx.instructions.push(WasmInstruction::I64Const(j as i64));
                            ctx.instructions.push(WasmInstruction::Call(vget));
                            ctx.instructions.push(WasmInstruction::LocalSet(l));
                            ctx.locals.insert(s.clone(), l);
                        }
                    }
                }
                _ => {}
            }
        }
        // Self-tail-recursion: a `recur` in the body (not inside a nested
        // `loop`) rebinds the params and jumps to the top. Wrap the body in a
        // wasm loop whose loop variables are the parameter locals.
        let self_recur = body.iter().any(Self::contains_bare_recur);
        if self_recur {
            ctx.instructions
                .push(WasmInstruction::Loop(BlockType::Result(ValType::I64)));
            ctx.loop_starts.push(ctx.instructions.len());
            ctx.loop_vars.push((0..arity as u32).collect());
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
        let extra_locals = if ctx.local_count > arity as u32 {
            vec![ValType::I64; (ctx.local_count - arity as u32) as usize]
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
                params: vec![ValType::I64; arity],
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
        if !self.table_entries.is_empty() || self.force_table {
            let mut t = TableSection::new();
            let n = self.table_entries.len() as u64;
            t.table(TableType {
                element_type: RefType::FUNCREF,
                minimum: n,
                maximum: Some(n),
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
    /// Names of in-scope bindings (params + lets) known to hold float values,
    /// so arithmetic on them picks the f64 path.
    float_locals: std::collections::HashSet<String>,
    /// Names of in-scope bindings known to hold string values, so `=`/map keys/
    /// `println` pick the string path.
    string_locals: std::collections::HashSet<String>,
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
    /// Emit a WASI fd_write of `len` bytes at memory address `addr` to stdout,
    /// using the iovec scratch at mem[0..8]. Leaves nothing on the stack.
    fn emit_fd_write_buf(&mut self, addr: i32, len: i32) {
        use WasmInstruction as W;
        self.instructions.push(W::I32Const(0));
        self.instructions.push(W::I32Const(addr));
        self.instructions.push(W::I32Store(2, 0));
        self.instructions.push(W::I32Const(4));
        self.instructions.push(W::I32Const(len));
        self.instructions.push(W::I32Store(2, 0));
        self.instructions.push(W::I32Const(1)); // stdout
        self.instructions.push(W::I32Const(0)); // iovec
        self.instructions.push(W::I32Const(1)); // count
        self.instructions.push(W::I32Const(8)); // nwritten
        self.instructions.push(W::Call(0));
        self.instructions.push(W::Drop);
    }
    /// Consume an i64 holding f64 bits and print it as `[-]int.dddddd` (six
    /// fixed fractional digits), optionally with a newline. Leaves UNIT.
    fn emit_print_f64(&mut self, newline: bool) {
        use WasmInstruction as W;
        const FB: i32 = 600; // scratch: '.', six digits, then newline byte
        let bits = self.alloc_local();
        let ip = self.alloc_local();
        let fr = self.alloc_local();
        let dg = self.alloc_local();
        self.instructions.push(W::LocalSet(bits));
        // sign: if fv < 0 { print '-'; fv = -fv }
        self.instructions.push(W::LocalGet(bits));
        self.instructions.push(W::F64ReinterpretI64);
        self.instructions.push(W::F64Const(0.0));
        self.instructions.push(W::F64Lt);
        self.instructions.push(W::If(BlockType::Empty));
        self.instructions.push(W::I32Const(FB));
        self.instructions.push(W::I32Const(45)); // '-'
        self.instructions.push(W::I32Store8(0, 0));
        self.emit_fd_write_buf(FB, 1);
        self.instructions.push(W::F64Const(0.0));
        self.instructions.push(W::LocalGet(bits));
        self.instructions.push(W::F64ReinterpretI64);
        self.instructions.push(W::F64Sub);
        self.instructions.push(W::I64ReinterpretF64);
        self.instructions.push(W::LocalSet(bits));
        self.instructions.push(W::End);
        // integer part = trunc(fv); print it with no newline (drop its UNIT).
        self.instructions.push(W::LocalGet(bits));
        self.instructions.push(W::F64ReinterpretI64);
        self.instructions.push(W::I64TruncF64S);
        self.instructions.push(W::LocalSet(ip));
        self.instructions.push(W::LocalGet(ip));
        self.emit_print_i64(false);
        self.instructions.push(W::Drop);
        // fractional part = fv - (ip as f64)
        self.instructions.push(W::LocalGet(bits));
        self.instructions.push(W::F64ReinterpretI64);
        self.instructions.push(W::LocalGet(ip));
        self.instructions.push(W::F64ConvertI64S);
        self.instructions.push(W::F64Sub);
        self.instructions.push(W::I64ReinterpretF64);
        self.instructions.push(W::LocalSet(fr));
        // buffer[0] = '.'
        self.instructions.push(W::I32Const(FB));
        self.instructions.push(W::I32Const(46));
        self.instructions.push(W::I32Store8(0, 0));
        // six fractional digits
        for k in 0..6i32 {
            // t = frac * 10 ; keep its bits in `fr`
            self.instructions.push(W::LocalGet(fr));
            self.instructions.push(W::F64ReinterpretI64);
            self.instructions.push(W::F64Const(10.0));
            self.instructions.push(W::F64Mul);
            self.instructions.push(W::I64ReinterpretF64);
            self.instructions.push(W::LocalSet(fr));
            // digit = trunc(t)
            self.instructions.push(W::LocalGet(fr));
            self.instructions.push(W::F64ReinterpretI64);
            self.instructions.push(W::I64TruncF64S);
            self.instructions.push(W::LocalSet(dg));
            // buffer[1+k] = '0' + digit
            self.instructions.push(W::I32Const(FB + 1 + k));
            self.instructions.push(W::I64Const(48));
            self.instructions.push(W::LocalGet(dg));
            self.instructions.push(W::I64Add);
            self.instructions.push(W::I32WrapI64);
            self.instructions.push(W::I32Store8(0, 0));
            // frac = t - digit
            self.instructions.push(W::LocalGet(fr));
            self.instructions.push(W::F64ReinterpretI64);
            self.instructions.push(W::LocalGet(dg));
            self.instructions.push(W::F64ConvertI64S);
            self.instructions.push(W::F64Sub);
            self.instructions.push(W::I64ReinterpretF64);
            self.instructions.push(W::LocalSet(fr));
        }
        self.emit_fd_write_buf(FB, 7); // '.' + 6 digits
        if newline {
            self.instructions.push(W::I32Const(FB + 8));
            self.instructions.push(W::I32Const(10));
            self.instructions.push(W::I32Store8(0, 0));
            self.emit_fd_write_buf(FB + 8, 1);
        }
        self.instructions.push(W::I64Const(0));
    }
    /// Whether an expression statically produces a string value. The value
    /// model is untagged (every value is a raw i64), so `println` can only
    /// pick the string-printing path when it can prove the argument is a
    /// string at compile time.
    fn expr_is_string(&self, expr: &Expr) -> bool {
        match self.compiler.node_type(expr) {
            Some(Type::Str) => return true,
            // A concrete non-string type is authoritative.
            Some(t) if !matches!(t, Type::Var(_)) => return false,
            _ => {}
        }
        // Generalized/synthesized nodes: structural fallback.
        Compiler::is_str_static(expr, &self.string_locals, &self.compiler.string_fns)
    }
    /// Compile `expr` so it leaves a packed *string* on the stack: strings are
    /// used as-is; other values are rendered via `int_to_str` (type-directed
    /// Display — covers ints/bools/keywords; floats fall here too and render
    /// their integer value, a known rough edge).
    fn compile_as_string(&mut self, expr: &Expr) -> Result<(), String> {
        use WasmInstruction as W;
        if self.expr_is_string(expr) {
            return self.compile_expr(expr);
        }
        self.compiler.ensure_string_runtime();
        let its = self.compiler.string_runtime.clone().unwrap().int_to_str_idx;
        // Statically-known non-string (int/bool/keyword): render via int_to_str.
        if let Some(ty) = self.compiler.node_type(expr) {
            if !matches!(ty, Type::Var(_)) {
                self.compile_expr(expr)?;
                self.instructions.push(W::Call(its));
                return Ok(());
            }
        }
        // Unknown static type (e.g. a destructured binding): self-describing —
        // if the value looks like a string pointer at runtime use it directly,
        // else stringify as an integer.
        let x = self.alloc_local();
        self.compile_expr(expr)?;
        self.instructions.push(W::LocalSet(x));
        self.instructions.push(W::LocalGet(x));
        self.instructions.push(W::I64Const(32));
        self.instructions.push(W::I64ShrU);
        self.instructions.push(W::I64Const(1024));
        self.instructions.push(W::I64GeS);
        self.instructions.push(W::If(BlockType::Result(ValType::I64)));
        self.instructions.push(W::LocalGet(x));
        self.instructions.push(W::Else);
        self.instructions.push(W::LocalGet(x));
        self.instructions.push(W::Call(its));
        self.instructions.push(W::End);
        Ok(())
    }
    /// Whether an expression is statically of float type. Trusts the checker's
    /// concrete type when present; otherwise falls back to a structural check
    /// (the checker generalizes polymorphic function bodies, so body nodes
    /// often carry a type var rather than `Float`).
    fn expr_is_float(&self, expr: &Expr) -> bool {
        match self.compiler.node_type(expr) {
            Some(Type::Float) => return true,
            // A concrete non-float type is authoritative.
            Some(t) if !matches!(t, Type::Var(_)) => return false,
            _ => {}
        }
        Compiler::is_float_static(expr, &self.float_locals, &self.compiler.float_fns)
    }
    /// Compile a binary arithmetic op, choosing the i64 or f64 instruction by
    /// the operands' static type. Float values are carried as f64 bits in the
    /// i64 slot, so the float path reinterprets in and out.
    fn compile_arith(
        &mut self,
        a: &Expr,
        b: &Expr,
        iop: WasmInstruction,
        fop: WasmInstruction,
    ) -> Result<(), String> {
        if self.expr_is_float(a) || self.expr_is_float(b) {
            self.compile_expr(a)?;
            self.instructions.push(WasmInstruction::F64ReinterpretI64);
            self.compile_expr(b)?;
            self.instructions.push(WasmInstruction::F64ReinterpretI64);
            self.instructions.push(fop);
            self.instructions.push(WasmInstruction::I64ReinterpretF64);
        } else {
            self.compile_expr(a)?;
            self.compile_expr(b)?;
            self.instructions.push(iop);
        }
        Ok(())
    }
    /// Compile a comparison, choosing i64 or f64 by operand type. Both paths
    /// yield an i32 which is widened to the i64 boolean value model.
    fn compile_cmp(
        &mut self,
        a: &Expr,
        b: &Expr,
        iop: WasmInstruction,
        fop: WasmInstruction,
    ) -> Result<(), String> {
        if self.expr_is_float(a) || self.expr_is_float(b) {
            self.compile_expr(a)?;
            self.instructions.push(WasmInstruction::F64ReinterpretI64);
            self.compile_expr(b)?;
            self.instructions.push(WasmInstruction::F64ReinterpretI64);
            self.instructions.push(fop);
        } else {
            self.compile_expr(a)?;
            self.compile_expr(b)?;
            self.instructions.push(iop);
        }
        self.instructions.push(WasmInstruction::I64ExtendI32U);
        Ok(())
    }
    fn compile_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match &expr.kind {
            ExprKind::Int(n) => {
                self.instructions.push(WasmInstruction::I64Const(*n));
                Ok(())
            }
            ExprKind::Float(n) => {
                // Floats are carried as their f64 bit-pattern in the i64 value
                // slot; arithmetic/comparison reinterpret to f64 at the op.
                self.instructions
                    .push(WasmInstruction::I64Const(n.to_bits() as i64));
                Ok(())
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
                        // `[fn [params] …]` is an anonymous closure. A nested
                        // *named* form `[fn name [params] …]` (a local function
                        // definition inside a body) compiles to a closure value
                        // bound to `name` in scope.
                        let named = items.len() >= 3
                            && matches!(&items[1].kind, ExprKind::Symbol(_))
                            && matches!(&items[2].kind, ExprKind::List(_));
                        if named {
                            let name = match &items[1].kind {
                                ExprKind::Symbol(n) => n.clone(),
                                _ => unreachable!(),
                            };
                            self.compile_closure(&items[2..])?;
                            let l = self.alloc_local();
                            self.instructions.push(WasmInstruction::LocalSet(l));
                            self.locals.insert(name, l);
                            self.instructions.push(WasmInstruction::LocalGet(l));
                            return Ok(());
                        }
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
            ExprKind::Tuple(items) => {
                // (a b …) — represented like a vector so destructuring and
                // pair access (vec-get) work uniformly.
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
                // {:k v …} desugars to map-new + a map-set per pair.
                self.compiler.ensure_map_runtime();
                let rt = self.compiler.map_runtime.clone().unwrap();
                self.instructions.push(WasmInstruction::Call(rt.map_new_idx));
                for (k, v) in pairs {
                    let is_str = self.expr_is_string(k) as i64;
                    self.compile_expr(k)?;
                    self.compile_expr(v)?;
                    self.instructions.push(WasmInstruction::I64Const(is_str));
                    self.instructions.push(WasmInstruction::Call(rt.map_set_idx));
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
                    return self.compile_arith(
                        &items[1],
                        &items[2],
                        WasmInstruction::I64Add,
                        WasmInstruction::F64Add,
                    );
                }
                "-" => {
                    return self.compile_arith(
                        &items[1],
                        &items[2],
                        WasmInstruction::I64Sub,
                        WasmInstruction::F64Sub,
                    );
                }
                "*" => {
                    return self.compile_arith(
                        &items[1],
                        &items[2],
                        WasmInstruction::I64Mul,
                        WasmInstruction::F64Mul,
                    );
                }
                "/" => {
                    return self.compile_arith(
                        &items[1],
                        &items[2],
                        WasmInstruction::I64DivS,
                        WasmInstruction::F64Div,
                    );
                }
                ">" => {
                    return self.compile_cmp(
                        &items[1],
                        &items[2],
                        WasmInstruction::I64GtS,
                        WasmInstruction::F64Gt,
                    );
                }
                "<" => {
                    return self.compile_cmp(
                        &items[1],
                        &items[2],
                        WasmInstruction::I64LtS,
                        WasmInstruction::F64Lt,
                    );
                }
                "=" => {
                    // String equality is structural (str_eq), not pointer
                    // identity — type-directed when either side is a string.
                    if self.expr_is_string(&items[1]) || self.expr_is_string(&items[2]) {
                        self.compiler.ensure_string_runtime();
                        let rt = self.compiler.string_runtime.clone().unwrap();
                        self.compile_expr(&items[1])?;
                        self.compile_expr(&items[2])?;
                        self.instructions.push(WasmInstruction::Call(rt.str_eq_idx));
                        return Ok(());
                    }
                    return self.compile_cmp(
                        &items[1],
                        &items[2],
                        WasmInstruction::I64Eq,
                        WasmInstruction::F64Eq,
                    );
                }
                "!=" => {
                    if self.expr_is_string(&items[1]) || self.expr_is_string(&items[2]) {
                        self.compiler.ensure_string_runtime();
                        let rt = self.compiler.string_runtime.clone().unwrap();
                        self.compile_expr(&items[1])?;
                        self.compile_expr(&items[2])?;
                        self.instructions.push(WasmInstruction::Call(rt.str_eq_idx));
                        // negate: (str_eq == 0)
                        self.instructions.push(WasmInstruction::I64Eqz);
                        self.instructions.push(WasmInstruction::I64ExtendI32U);
                        return Ok(());
                    }
                    return self.compile_cmp(
                        &items[1],
                        &items[2],
                        WasmInstruction::I64Ne,
                        WasmInstruction::F64Ne,
                    );
                }
                "<=" => {
                    return self.compile_cmp(
                        &items[1],
                        &items[2],
                        WasmInstruction::I64LeS,
                        WasmInstruction::F64Le,
                    );
                }
                ">=" => {
                    return self.compile_cmp(
                        &items[1],
                        &items[2],
                        WasmInstruction::I64GeS,
                        WasmInstruction::F64Ge,
                    );
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
                    // Variadic: stringify each arg (type-directed Display) and
                    // fold str_concat. [str] -> "", [str a] -> a.
                    let args = &items[1..];
                    if args.is_empty() {
                        let (offset, len) = self.compiler.intern_string("");
                        self.instructions
                            .push(WasmInstruction::I64Const(((offset as i64) << 32) | len as i64));
                        return Ok(());
                    }
                    self.compile_as_string(&args[0])?;
                    if args.len() > 1 {
                        self.compiler.ensure_string_runtime();
                        let rt = self.compiler.string_runtime.clone().unwrap();
                        for arg in &args[1..] {
                            self.compile_as_string(arg)?;
                            self.instructions
                                .push(WasmInstruction::Call(rt.str_concat_idx));
                        }
                    }
                    return Ok(());
                }
                "lowercase" => {
                    self.compiler.ensure_string_runtime();
                    let rt = self.compiler.string_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.instructions
                        .push(WasmInstruction::Call(rt.lowercase_idx));
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
                "split" => {
                    // [split s sep] -> vector of substrings (sep is one char).
                    let idx = self.compiler.ensure_split_runtime();
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::Call(idx));
                    return Ok(());
                }
                "cons" => {
                    // [cons x v] -> new vector with x prepended.
                    self.compiler.ensure_collections_runtime();
                    let rt = self.compiler.collections_runtime.clone().unwrap();
                    let xl = self.alloc_local();
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::LocalSet(xl));
                    self.compile_expr(&items[2])?;
                    let vl = self.alloc_local();
                    self.instructions.push(WasmInstruction::LocalSet(vl));
                    // r = vec_push(vec_new(), x)
                    self.instructions
                        .push(WasmInstruction::Call(rt.vec_new_idx));
                    self.instructions.push(WasmInstruction::LocalGet(xl));
                    self.instructions
                        .push(WasmInstruction::Call(rt.vec_push_idx));
                    let rl = self.alloc_local();
                    self.instructions.push(WasmInstruction::LocalSet(rl));
                    // append each element of v
                    let (n, d) = self.emit_vec_header(vl);
                    let iv = self.alloc_local();
                    let el = self.alloc_local();
                    use WasmInstruction as W;
                    self.instructions.push(W::I64Const(0));
                    self.instructions.push(W::LocalSet(iv));
                    self.instructions.push(W::Block(BlockType::Empty));
                    self.instructions.push(W::Loop(BlockType::Empty));
                    self.instructions.push(W::LocalGet(iv));
                    self.instructions.push(W::LocalGet(n));
                    self.instructions.push(W::I64LtS);
                    self.instructions.push(W::I32Eqz);
                    self.instructions.push(W::BrIf(1));
                    // el = d[iv]
                    self.instructions.push(W::LocalGet(d));
                    self.instructions.push(W::LocalGet(iv));
                    self.instructions.push(W::I64Const(8));
                    self.instructions.push(W::I64Mul);
                    self.instructions.push(W::I64Add);
                    self.instructions.push(W::I32WrapI64);
                    self.instructions.push(W::I64Load(3, 0));
                    self.instructions.push(W::LocalSet(el));
                    // r = vec_push(r, el)
                    self.instructions.push(W::LocalGet(rl));
                    self.instructions.push(W::LocalGet(el));
                    self.instructions.push(W::Call(rt.vec_push_idx));
                    self.instructions.push(W::LocalSet(rl));
                    self.instructions.push(W::LocalGet(iv));
                    self.instructions.push(W::I64Const(1));
                    self.instructions.push(W::I64Add);
                    self.instructions.push(W::LocalSet(iv));
                    self.instructions.push(W::Br(0));
                    self.instructions.push(W::End);
                    self.instructions.push(W::End);
                    self.instructions.push(W::LocalGet(rl));
                    return Ok(());
                }
                "len" | "count" | "vec-len" => {
                    // Vector length lives at header offset 0.
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::I32WrapI64);
                    self.instructions.push(WasmInstruction::I64Load(3, 0));
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
                "assoc" => {
                    // [assoc m k v] — copy-on-write insert; string keys use
                    // structural equality (is_str flag).
                    self.compiler.ensure_map_runtime();
                    let rt = self.compiler.map_runtime.clone().unwrap();
                    let is_str = self.expr_is_string(&items[2]) as i64;
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.compile_expr(&items[3])?;
                    self.instructions.push(WasmInstruction::I64Const(is_str));
                    self.instructions.push(WasmInstruction::Call(rt.map_set_idx));
                    return Ok(());
                }
                "update" => {
                    // [update m k f] = assoc m k (f (get m k)).
                    self.compiler.ensure_map_runtime();
                    let rt = self.compiler.map_runtime.clone().unwrap();
                    let fr = self.prepare_fn_arg(&items[3])?;
                    let ml = self.alloc_local();
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::LocalSet(ml));
                    let kl = self.alloc_local();
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::LocalSet(kl));
                    // cur = map_get(m, k, 0)
                    let curl = self.alloc_local();
                    self.instructions.push(WasmInstruction::LocalGet(ml));
                    self.instructions.push(WasmInstruction::LocalGet(kl));
                    self.instructions.push(WasmInstruction::I64Const(0));
                    self.instructions.push(WasmInstruction::Call(rt.map_get_idx));
                    self.instructions.push(WasmInstruction::LocalSet(curl));
                    // newv = f(cur)
                    let newv = self.alloc_local();
                    self.emit_apply1(&fr, curl);
                    self.instructions.push(WasmInstruction::LocalSet(newv));
                    // map_set(m, k, newv, 0)
                    self.instructions.push(WasmInstruction::LocalGet(ml));
                    self.instructions.push(WasmInstruction::LocalGet(kl));
                    self.instructions.push(WasmInstruction::LocalGet(newv));
                    self.instructions.push(WasmInstruction::I64Const(0));
                    self.instructions.push(WasmInstruction::Call(rt.map_set_idx));
                    return Ok(());
                }
                "get" => {
                    // [get m k] — map lookup (UNIT if absent).
                    self.compiler.ensure_map_runtime();
                    let rt = self.compiler.map_runtime.clone().unwrap();
                    let is_str = self.expr_is_string(&items[2]) as i64;
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64Const(is_str));
                    self.instructions.push(WasmInstruction::Call(rt.map_get_idx));
                    return Ok(());
                }
                "contains?" | "has-key?" => {
                    self.compiler.ensure_map_runtime();
                    let rt = self.compiler.map_runtime.clone().unwrap();
                    let is_str = self.expr_is_string(&items[2]) as i64;
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64Const(is_str));
                    self.instructions.push(WasmInstruction::Call(rt.map_has_idx));
                    return Ok(());
                }
                "keys" => {
                    self.compiler.ensure_map_runtime();
                    let rt = self.compiler.map_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.instructions.push(WasmInstruction::Call(rt.map_keys_idx));
                    return Ok(());
                }
                "entries" => {
                    self.compiler.ensure_map_runtime();
                    let rt = self.compiler.map_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.instructions
                        .push(WasmInstruction::Call(rt.map_entries_idx));
                    return Ok(());
                }
                "merge" => {
                    self.compiler.ensure_map_runtime();
                    let rt = self.compiler.map_runtime.clone().unwrap();
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions
                        .push(WasmInstruction::Call(rt.map_merge_idx));
                    return Ok(());
                }
                "take" => {
                    // [take n v] -> new vector of the first min(n, len) elems.
                    self.compiler.ensure_collections_runtime();
                    let rt = self.compiler.collections_runtime.clone().unwrap();
                    use WasmInstruction as W;
                    let nl = self.alloc_local();
                    self.compile_expr(&items[1])?;
                    self.instructions.push(W::LocalSet(nl));
                    self.compile_expr(&items[2])?;
                    let vl = self.alloc_local();
                    self.instructions.push(W::LocalSet(vl));
                    let (len, d) = self.emit_vec_header(vl);
                    // limit = min(n, len)
                    let lim = self.alloc_local();
                    self.instructions.push(W::LocalGet(nl));
                    self.instructions.push(W::LocalGet(len));
                    self.instructions.push(W::I64LtS);
                    self.instructions
                        .push(W::If(BlockType::Result(ValType::I64)));
                    self.instructions.push(W::LocalGet(nl));
                    self.instructions.push(W::Else);
                    self.instructions.push(W::LocalGet(len));
                    self.instructions.push(W::End);
                    self.instructions.push(W::LocalSet(lim));
                    self.instructions.push(W::Call(rt.vec_new_idx));
                    let rl = self.alloc_local();
                    self.instructions.push(W::LocalSet(rl));
                    let iv = self.alloc_local();
                    self.instructions.push(W::I64Const(0));
                    self.instructions.push(W::LocalSet(iv));
                    self.instructions.push(W::Block(BlockType::Empty));
                    self.instructions.push(W::Loop(BlockType::Empty));
                    self.instructions.push(W::LocalGet(iv));
                    self.instructions.push(W::LocalGet(lim));
                    self.instructions.push(W::I64LtS);
                    self.instructions.push(W::I32Eqz);
                    self.instructions.push(W::BrIf(1));
                    self.instructions.push(W::LocalGet(rl));
                    self.instructions.push(W::LocalGet(d));
                    self.instructions.push(W::LocalGet(iv));
                    self.instructions.push(W::I64Const(8));
                    self.instructions.push(W::I64Mul);
                    self.instructions.push(W::I64Add);
                    self.instructions.push(W::I32WrapI64);
                    self.instructions.push(W::I64Load(3, 0));
                    self.instructions.push(W::Call(rt.vec_push_idx));
                    self.instructions.push(W::LocalSet(rl));
                    self.instructions.push(W::LocalGet(iv));
                    self.instructions.push(W::I64Const(1));
                    self.instructions.push(W::I64Add);
                    self.instructions.push(W::LocalSet(iv));
                    self.instructions.push(W::Br(0));
                    self.instructions.push(W::End);
                    self.instructions.push(W::End);
                    self.instructions.push(W::LocalGet(rl));
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
                    // Self-describing: a string value packs its length in the
                    // low 32 bits (high bits = ptr >= 1024); a vector value is a
                    // small header address (high bits 0). So check string length
                    // for the former and the vector's len field for the latter.
                    use WasmInstruction as W;
                    let x = self.alloc_local();
                    self.compile_expr(&items[1])?;
                    self.instructions.push(W::LocalSet(x));
                    self.instructions.push(W::LocalGet(x));
                    self.instructions.push(W::I64Const(32));
                    self.instructions.push(W::I64ShrU);
                    self.instructions.push(W::I64Const(1024));
                    self.instructions.push(W::I64GeS); // i32: looks like a string
                    self.instructions
                        .push(W::If(BlockType::Result(ValType::I64)));
                    // string: (x & 0xffffffff) == 0
                    self.instructions.push(W::LocalGet(x));
                    self.instructions.push(W::I64Const(0xFFFF_FFFF));
                    self.instructions.push(W::I64And);
                    self.instructions.push(W::I64Eqz);
                    self.instructions.push(W::I64ExtendI32U);
                    self.instructions.push(W::Else);
                    // vector/map: mem[addr].len == 0
                    self.instructions.push(W::LocalGet(x));
                    self.instructions.push(W::I32WrapI64);
                    self.instructions.push(W::I64Load(3, 0));
                    self.instructions.push(W::I64Eqz);
                    self.instructions.push(W::I64ExtendI32U);
                    self.instructions.push(W::End);
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
                    // Remember float/string bindings so later uses dispatch.
                    if self.expr_is_float(&items[vi]) {
                        self.float_locals.insert(name.clone());
                    }
                    if self.expr_is_string(&items[vi]) {
                        self.string_locals.insert(name.clone());
                    }
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
                        } else if self.expr_is_float(arg) {
                            self.compile_expr(arg)?;
                            self.emit_print_f64(nl);
                            return Ok(());
                        } else if matches!(
                            self.compiler.node_type(arg),
                            Some(t) if !matches!(t, Type::Var(_))
                        ) {
                            // Statically a concrete non-string type: print as int.
                            self.compile_expr(arg)?;
                            self.emit_print_i64(nl);
                            return Ok(());
                        } else {
                            // Unknown static type (e.g. a `handle` result): print
                            // self-describingly — string bytes if it looks like a
                            // string pointer at runtime, else a decimal int.
                            use WasmInstruction as W;
                            let x = self.alloc_local();
                            self.compile_expr(arg)?;
                            self.instructions.push(W::LocalSet(x));
                            self.instructions.push(W::LocalGet(x));
                            self.instructions.push(W::I64Const(32));
                            self.instructions.push(W::I64ShrU);
                            self.instructions.push(W::I64Const(1024));
                            self.instructions.push(W::I64GeS);
                            self.instructions.push(W::If(BlockType::Result(ValType::I64)));
                            self.instructions.push(W::LocalGet(x));
                            self.emit_print_str(nl);
                            self.instructions.push(W::Else);
                            self.instructions.push(W::LocalGet(x));
                            self.emit_print_i64(nl);
                            self.instructions.push(W::End);
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
                "group-by" => {
                    // [group-by f coll] -> map from f(elem) to a vector of the
                    // elements with that key.
                    if items.len() >= 3 {
                        return self.compile_group_by(&items[1], &items[2]);
                    }
                    return Err("codegen: group-by requires function, collection".into());
                }
                "sort-by" => {
                    // [sort-by f order coll] — sort coll by integer key f(elem).
                    if items.len() >= 4 {
                        let order_desc =
                            matches!(&items[2].kind, ExprKind::Keyword(k) if k == "desc");
                        return self.compile_sort_by(&items[1], order_desc, &items[3]);
                    }
                    return Err("codegen: sort-by requires function, order, collection".into());
                }
                "range" => {
                    // [range a b] — vector of a, a+1, …, b-1.
                    if items.len() >= 3 {
                        return self.compile_range(&items[1], &items[2]);
                    }
                    return Err("codegen: range requires start and end".into());
                }
                "type" | "use" | "effect" => {
                    self.instructions.push(WasmInstruction::I64Const(0));
                    return Ok(());
                }
                "resume" => {
                    // Tail-resumptive: `[resume v]` makes the effect operation
                    // return v, so it compiles to just v (the handler closure's
                    // return value becomes the op result). Non-tail / escaping
                    // resume (the State pattern) is not supported here.
                    if let Some(v) = items.get(1) {
                        return self.compile_expr(v);
                    }
                    self.instructions.push(WasmInstruction::I64Const(0));
                    return Ok(());
                }
                "handle" => {
                    return self.compile_handle(&items[1..]);
                }
                "try" => {
                    // Abort-style `try` needs non-local unwind across calls,
                    // which standalone wasm can't express without exceptions or
                    // a CPS transform. Run on the VM.
                    return Err("codegen: 'try' (delimited continuations) is not \
                                supported by the wasm backend yet; run it on the VM \
                                with `loon run`"
                        .into());
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
                if effect.starts_with(char::is_uppercase) {
                    return self.emit_effect_op(effect, op, &items[1..]);
                }
            }
        }
        Err("codegen: unsupported call form".into())
    }
    /// Compile an effect operation `Effect.op args`. If a matching handler is
    /// installed on the dynamic handler stack at runtime, call it (tail-
    /// resumptive); otherwise fall back to the host import.
    fn emit_effect_op(&mut self, effect: &str, op: &str, args: &[Expr]) -> Result<(), String> {
        use WasmInstruction as W;
        let key = format!("{effect}.{op}");
        let op_id = self.compiler.intern_effect_op(&key);
        let import_idx = self.compiler.get_or_create_effect_import(effect, op);
        self.compiler.force_heap = true;
        self.compiler.force_table = true; // the handler-closure path uses call_indirect
        // Evaluate args into locals (reused by either call path).
        let mut arglocs = Vec::new();
        for a in args {
            self.compile_expr(a)?;
            let l = self.alloc_local();
            self.instructions.push(W::LocalSet(l));
            arglocs.push(l);
        }
        // Scan the handler stack top-down for op_id. Track `found` separately:
        // a capture-less closure value can legitimately be 0 (table index 0).
        let cl = self.alloc_local();
        let found = self.alloc_local();
        let f = self.alloc_local();
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalSet(cl));
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalSet(found));
        // f = count - 1
        self.instructions.push(W::I32Const(HANDLER_COUNT_ADDR));
        self.instructions.push(W::I32Load(2, 0));
        self.instructions.push(W::I64ExtendI32U);
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Sub);
        self.instructions.push(W::LocalSet(f));
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        self.instructions.push(W::LocalGet(f));
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::BrIf(1)); // f < 0: not found
        // addr = HANDLER_BASE + f*16  (kept as i64; wrapped at memory ops)
        let addr = self.alloc_local();
        self.instructions.push(W::LocalGet(f));
        self.instructions.push(W::I64Const(16));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I64Const(HANDLER_BASE as i64));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(addr));
        // entry op_id == op_id ?
        self.instructions.push(W::LocalGet(addr));
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::I64Const(op_id));
        self.instructions.push(W::I64Eq);
        self.instructions.push(W::If(BlockType::Empty));
        self.instructions.push(W::LocalGet(addr));
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I64Load(3, 8));
        self.instructions.push(W::LocalSet(cl));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::LocalSet(found));
        self.instructions.push(W::Br(2)); // found: exit scan
        self.instructions.push(W::End);
        self.instructions.push(W::LocalGet(f));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Sub);
        self.instructions.push(W::LocalSet(f));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        // if !found: import(args) else closure(env, args) via the table.
        self.instructions.push(W::LocalGet(found));
        self.instructions.push(W::I64Eqz);
        self.instructions.push(W::If(BlockType::Result(ValType::I64)));
        for &l in &arglocs {
            self.instructions.push(W::LocalGet(l));
        }
        self.instructions.push(W::Call(import_idx));
        self.instructions.push(W::Else);
        // env = cl & 0xffffffff
        self.instructions.push(W::LocalGet(cl));
        self.instructions.push(W::I64Const(0xFFFF_FFFF));
        self.instructions.push(W::I64And);
        for &l in &arglocs {
            self.instructions.push(W::LocalGet(l));
        }
        self.instructions.push(W::LocalGet(cl));
        self.instructions.push(W::I64Const(32));
        self.instructions.push(W::I64ShrU);
        self.instructions.push(W::I32WrapI64);
        let ty = self.get_or_create_indirect_type(1 + arglocs.len());
        self.instructions.push(W::CallIndirect(ty));
        self.instructions.push(W::End);
        Ok(())
    }
    /// Compile `[handle body clause…]` for tail-resumptive handlers. Each
    /// `[E.op params] hbody` clause installs a handler closure on the dynamic
    /// stack for the duration of `body`; a `[return x]` clause post-processes
    /// the body's value.
    fn compile_handle(&mut self, args: &[Expr]) -> Result<(), String> {
        use WasmInstruction as W;
        if args.is_empty() {
            self.instructions.push(W::I64Const(0));
            return Ok(());
        }
        let body = &args[0];
        let clauses = &args[1..];
        self.compiler.force_heap = true;
        let mut return_clause: Option<(String, &Expr)> = None;
        let mut pushed = 0i32;
        let mut i = 0;
        while i + 1 < clauses.len() {
            let pat = &clauses[i];
            let hbody = &clauses[i + 1];
            i += 2;
            let parts = match &pat.kind {
                ExprKind::List(p) if !p.is_empty() => p,
                _ => continue,
            };
            // [return x] clause
            if let ExprKind::Symbol(s) = &parts[0].kind {
                if s == "return" {
                    if let Some(ExprKind::Symbol(x)) = parts.get(1).map(|e| &e.kind) {
                        return_clause = Some((x.clone(), hbody));
                    }
                    continue;
                }
            }
            // [E.op params…] clause
            if let ExprKind::DotAccess(obj, op) = &parts[0].kind {
                if let ExprKind::Symbol(effect) = &obj.kind {
                    let key = format!("{effect}.{op}");
                    let op_id = self.compiler.intern_effect_op(&key);
                    // handler closure: [fn [params…] hbody]
                    let sp = pat.span;
                    let params_list =
                        Expr::new(ExprKind::List(parts[1..].to_vec()), sp);
                    self.compile_closure(&[params_list, hbody.clone()])?;
                    let cval = self.alloc_local();
                    self.instructions.push(W::LocalSet(cval));
                    // cnt = count (kept as i64); addr = HANDLER_BASE + cnt*16
                    let cnt = self.alloc_local();
                    self.instructions.push(W::I32Const(HANDLER_COUNT_ADDR));
                    self.instructions.push(W::I32Load(2, 0));
                    self.instructions.push(W::I64ExtendI32U);
                    self.instructions.push(W::LocalSet(cnt));
                    let addr = self.alloc_local();
                    self.instructions.push(W::LocalGet(cnt));
                    self.instructions.push(W::I64Const(16));
                    self.instructions.push(W::I64Mul);
                    self.instructions.push(W::I64Const(HANDLER_BASE as i64));
                    self.instructions.push(W::I64Add);
                    self.instructions.push(W::LocalSet(addr));
                    // mem[addr] = op_id ; mem[addr+8] = cval
                    self.instructions.push(W::LocalGet(addr));
                    self.instructions.push(W::I32WrapI64);
                    self.instructions.push(W::I64Const(op_id));
                    self.instructions.push(W::I64Store(3, 0));
                    self.instructions.push(W::LocalGet(addr));
                    self.instructions.push(W::I32WrapI64);
                    self.instructions.push(W::LocalGet(cval));
                    self.instructions.push(W::I64Store(3, 8));
                    // count += 1
                    self.instructions.push(W::I32Const(HANDLER_COUNT_ADDR));
                    self.instructions.push(W::LocalGet(cnt));
                    self.instructions.push(W::I32WrapI64);
                    self.instructions.push(W::I32Const(1));
                    self.instructions.push(W::I32Add);
                    self.instructions.push(W::I32Store(2, 0));
                    pushed += 1;
                }
            }
        }
        // Compile the body, then pop the installed frames.
        self.compile_expr(body)?;
        let bodyval = self.alloc_local();
        self.instructions.push(W::LocalSet(bodyval));
        if pushed > 0 {
            self.instructions.push(W::I32Const(HANDLER_COUNT_ADDR));
            self.instructions.push(W::I32Const(HANDLER_COUNT_ADDR));
            self.instructions.push(W::I32Load(2, 0));
            self.instructions.push(W::I32Const(pushed));
            self.instructions.push(W::I32Sub);
            self.instructions.push(W::I32Store(2, 0));
        }
        // Apply the [return x] clause, if any.
        if let Some((x, hbody)) = return_clause {
            self.locals.insert(x, bodyval);
            self.compile_expr(hbody)?;
        } else {
            self.instructions.push(W::LocalGet(bodyval));
        }
        Ok(())
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
        // Params may be plain symbols or destructuring patterns ([a b] for a
        // pair). `param_exprs` keeps the raw forms; `bound_names` is the flat
        // list of names they bind (used to exclude them from captures).
        let param_exprs: Vec<Expr> = match &args[0].kind {
            ExprKind::List(items) => items.clone(),
            _ => return Err("closure params must be a list".into()),
        };
        let mut bound_names: Vec<String> = Vec::new();
        for p in &param_exprs {
            match &p.kind {
                ExprKind::Symbol(s) => bound_names.push(s.clone()),
                ExprKind::List(sub) | ExprKind::Tuple(sub) => {
                    for e in sub {
                        if let ExprKind::Symbol(s) = &e.kind {
                            if s != "_" {
                                bound_names.push(s.clone());
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        let params = bound_names.clone();
        let body = &args[1..];
        let free = capture::free_vars(&params, body);
        let mut captures: Vec<(String, u32)> = Vec::new();
        for name in &free {
            if let Some(&idx) = self.locals.get(name) {
                captures.push((name.clone(), idx));
            }
        }
        let lname = format!("__closure_{}", self.compiler.lambda_counter);
        self.compiler.lambda_counter += 1;
        let tp = 1 + param_exprs.len();
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
            float_locals: std::collections::HashSet::new(),
            string_locals: std::collections::HashSet::new(),
        };
        cctx.locals.insert("__env_ptr".to_string(), 0);
        for (i, p) in param_exprs.iter().enumerate() {
            let pos = (i + 1) as u32; // positional local (after __env_ptr)
            match &p.kind {
                ExprKind::Symbol(s) => {
                    cctx.locals.insert(s.clone(), pos);
                }
                ExprKind::List(sub) | ExprKind::Tuple(sub) => {
                    // Destructure a pair/tuple arg: name_j = vec-get(arg, j).
                    cctx.compiler.ensure_collections_runtime();
                    let vget = cctx.compiler.collections_runtime.clone().unwrap().vec_get_idx;
                    for (j, e) in sub.iter().enumerate() {
                        if let ExprKind::Symbol(s) = &e.kind {
                            if s == "_" {
                                continue;
                            }
                            let l = cctx.alloc_local();
                            cctx.instructions.push(WasmInstruction::LocalGet(pos));
                            cctx.instructions.push(WasmInstruction::I64Const(j as i64));
                            cctx.instructions.push(WasmInstruction::Call(vget));
                            cctx.instructions.push(WasmInstruction::LocalSet(l));
                            cctx.locals.insert(s.clone(), l);
                        }
                    }
                }
                _ => {}
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
                    // A builtin (or otherwise non-fn symbol) used as a HOF value:
                    // wrap it in a unary lambda `[fn [g] [name g]]` and compile
                    // that closure, so e.g. `[map lowercase coll]` works.
                    let sp = f.span;
                    let g = "__hof_arg".to_string();
                    let params = Expr::new(
                        ExprKind::List(vec![Expr::new(ExprKind::Symbol(g.clone()), sp)]),
                        sp,
                    );
                    let body = Expr::new(
                        ExprKind::List(vec![
                            Expr::new(ExprKind::Symbol(name.clone()), sp),
                            Expr::new(ExprKind::Symbol(g), sp),
                        ]),
                        sp,
                    );
                    self.compile_closure(&[params, body])?;
                    let l = self.alloc_local();
                    self.instructions.push(WasmInstruction::LocalSet(l));
                    Ok(FnRepr::Closure(l))
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
    /// `[sort-by f order coll]` — stable-ish insertion sort of `coll` by the key
    /// `f(elem)` (an integer key), ascending unless `order` is `:desc`. Keys are
    /// precomputed once into a scratch array, then the element array is sorted.
    fn compile_sort_by(
        &mut self,
        f: &Expr,
        order_desc: bool,
        coll: &Expr,
    ) -> Result<(), String> {
        use WasmInstruction as W;
        self.compiler.ensure_collections_runtime();
        let rt = self.compiler.collections_runtime.clone().unwrap();
        let fr = self.prepare_fn_arg(f)?;
        self.compile_expr(coll)?;
        let vl = self.alloc_local();
        self.instructions.push(W::LocalSet(vl));
        let (n, data) = self.emit_vec_header(vl);
        // elems = alloc n*8 ; keys = alloc n*8  (heap offsets as i64)
        let elems = self.alloc_local();
        let keys = self.alloc_local();
        for slot in [elems, keys] {
            self.instructions.push(W::GlobalGet(0));
            self.instructions.push(W::I64ExtendI32U);
            self.instructions.push(W::LocalSet(slot));
            self.instructions.push(W::GlobalGet(0));
            self.instructions.push(W::LocalGet(n));
            self.instructions.push(W::I64Const(8));
            self.instructions.push(W::I64Mul);
            self.instructions.push(W::I32WrapI64);
            self.instructions.push(W::I32Add);
            self.instructions.push(W::GlobalSet(0));
        }
        let iv = self.alloc_local();
        let eloc = self.alloc_local();
        // copy elems[i] = data[i]; keys[i] = f(elems[i])
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalSet(iv));
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::LocalGet(n));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        // el = data[i]
        self.instructions.push(W::LocalGet(data));
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::I64Const(8));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::LocalSet(eloc));
        // elems[i] = el
        self.instructions.push(W::LocalGet(elems));
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::I64Const(8));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::LocalGet(eloc));
        self.instructions.push(W::I64Store(3, 0));
        // keys[i] = f(el)
        self.instructions.push(W::LocalGet(keys));
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::I64Const(8));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I32WrapI64);
        self.emit_apply1(&fr, eloc);
        self.instructions.push(W::I64Store(3, 0));
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(iv));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        // insertion sort: for i in 1..n { ek=elems[i]; kk=keys[i]; j=i-1;
        //   while j>=0 && cmp(keys[j], kk) { shift; j-- } place }
        let jv = self.alloc_local();
        let ek = self.alloc_local();
        let kk = self.alloc_local();
        let addr = self.alloc_local(); // scratch addr (i64)
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::LocalSet(iv));
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::LocalGet(n));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        // ek = elems[i]; kk = keys[i]
        let load_at = |s: &mut Vec<WasmInstruction>, base: u32, idx: u32| {
            s.push(W::LocalGet(base));
            s.push(W::LocalGet(idx));
            s.push(W::I64Const(8));
            s.push(W::I64Mul);
            s.push(W::I64Add);
            s.push(W::I32WrapI64);
            s.push(W::I64Load(3, 0));
        };
        load_at(&mut self.instructions, elems, iv);
        self.instructions.push(W::LocalSet(ek));
        load_at(&mut self.instructions, keys, iv);
        self.instructions.push(W::LocalSet(kk));
        // j = i - 1
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Sub);
        self.instructions.push(W::LocalSet(jv));
        // inner while
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        // cond: j >= 0
        self.instructions.push(W::LocalGet(jv));
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::I64GeS); // i32
        // && cmp(keys[j], kk)
        load_at(&mut self.instructions, keys, jv);
        self.instructions.push(W::LocalSet(addr)); // reuse addr to hold keys[j]
        self.instructions.push(W::LocalGet(addr));
        self.instructions.push(W::LocalGet(kk));
        if order_desc {
            self.instructions.push(W::I64LtS); // keys[j] < kk -> move (larger first)
        } else {
            self.instructions.push(W::I64GtS); // keys[j] > kk
        }
        self.instructions.push(W::I32And);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1)); // exit inner when cond false
        // elems[j+1] = elems[j]; keys[j+1] = keys[j]
        for base in [elems, keys] {
            // dest addr = base + (j+1)*8
            self.instructions.push(W::LocalGet(base));
            self.instructions.push(W::LocalGet(jv));
            self.instructions.push(W::I64Const(1));
            self.instructions.push(W::I64Add);
            self.instructions.push(W::I64Const(8));
            self.instructions.push(W::I64Mul);
            self.instructions.push(W::I64Add);
            self.instructions.push(W::I32WrapI64);
            // value = base[j]
            load_at(&mut self.instructions, base, jv);
            self.instructions.push(W::I64Store(3, 0));
        }
        // j--
        self.instructions.push(W::LocalGet(jv));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Sub);
        self.instructions.push(W::LocalSet(jv));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        // place: elems[j+1] = ek ; keys[j+1] = kk
        for (base, val) in [(elems, ek), (keys, kk)] {
            self.instructions.push(W::LocalGet(base));
            self.instructions.push(W::LocalGet(jv));
            self.instructions.push(W::I64Const(1));
            self.instructions.push(W::I64Add);
            self.instructions.push(W::I64Const(8));
            self.instructions.push(W::I64Mul);
            self.instructions.push(W::I64Add);
            self.instructions.push(W::I32WrapI64);
            self.instructions.push(W::LocalGet(val));
            self.instructions.push(W::I64Store(3, 0));
        }
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(iv));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        // build result vector from sorted elems
        self.instructions.push(W::Call(rt.vec_new_idx));
        let rl = self.alloc_local();
        self.instructions.push(W::LocalSet(rl));
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalSet(iv));
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::LocalGet(n));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        self.instructions.push(W::LocalGet(rl));
        load_at(&mut self.instructions, elems, iv);
        self.instructions.push(W::Call(rt.vec_push_idx));
        self.instructions.push(W::LocalSet(rl));
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(iv));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        self.instructions.push(W::LocalGet(rl));
        Ok(())
    }
    fn compile_group_by(&mut self, f: &Expr, coll: &Expr) -> Result<(), String> {
        use WasmInstruction as W;
        self.compiler.ensure_map_runtime();
        let mr = self.compiler.map_runtime.clone().unwrap();
        let cr = self.compiler.collections_runtime.clone().unwrap();
        let fr = self.prepare_fn_arg(f)?;
        self.compile_expr(coll)?;
        let vl = self.alloc_local();
        self.instructions.push(W::LocalSet(vl));
        let (n, data) = self.emit_vec_header(vl);
        let ml = self.alloc_local();
        self.instructions.push(W::Call(mr.map_new_idx));
        self.instructions.push(W::LocalSet(ml));
        let iv = self.alloc_local();
        let eloc = self.alloc_local();
        let kloc = self.alloc_local();
        let vecloc = self.alloc_local();
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::LocalSet(iv));
        self.instructions.push(W::Block(BlockType::Empty));
        self.instructions.push(W::Loop(BlockType::Empty));
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::LocalGet(n));
        self.instructions.push(W::I64LtS);
        self.instructions.push(W::I32Eqz);
        self.instructions.push(W::BrIf(1));
        // elem = data[i]
        self.instructions.push(W::LocalGet(data));
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::I64Const(8));
        self.instructions.push(W::I64Mul);
        self.instructions.push(W::I64Add);
        self.instructions.push(W::I32WrapI64);
        self.instructions.push(W::I64Load(3, 0));
        self.instructions.push(W::LocalSet(eloc));
        // k = f(elem)
        self.emit_apply1(&fr, eloc);
        self.instructions.push(W::LocalSet(kloc));
        // cur = map_get(m, k, 0); vec = (cur == 0 ? vec_new() : cur)
        self.instructions.push(W::LocalGet(ml));
        self.instructions.push(W::LocalGet(kloc));
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::Call(mr.map_get_idx));
        self.instructions.push(W::LocalSet(vecloc));
        self.instructions.push(W::LocalGet(vecloc));
        self.instructions.push(W::I64Eqz);
        self.instructions.push(W::If(BlockType::Result(ValType::I64)));
        self.instructions.push(W::Call(cr.vec_new_idx));
        self.instructions.push(W::Else);
        self.instructions.push(W::LocalGet(vecloc));
        self.instructions.push(W::End);
        // vec2 = vec_push(vec, elem)
        self.instructions.push(W::LocalGet(eloc));
        self.instructions.push(W::Call(cr.vec_push_idx));
        self.instructions.push(W::LocalSet(vecloc));
        // m = map_set(m, k, vec2, 0)
        self.instructions.push(W::LocalGet(ml));
        self.instructions.push(W::LocalGet(kloc));
        self.instructions.push(W::LocalGet(vecloc));
        self.instructions.push(W::I64Const(0));
        self.instructions.push(W::Call(mr.map_set_idx));
        self.instructions.push(W::LocalSet(ml));
        self.instructions.push(W::LocalGet(iv));
        self.instructions.push(W::I64Const(1));
        self.instructions.push(W::I64Add);
        self.instructions.push(W::LocalSet(iv));
        self.instructions.push(W::Br(0));
        self.instructions.push(W::End);
        self.instructions.push(W::End);
        self.instructions.push(W::LocalGet(ml));
        Ok(())
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
    fn compile_nested_named_fn_is_valid() {
        valid(
            r#"[fn outer [n] [fn dbl [x] [* x 2]] [+ [dbl n] [dbl 1]]]
               [fn main [] [println [outer 10]]]"#,
        );
    }
    #[test]
    fn compile_lowercase_and_builtin_hof_are_valid() {
        valid(r#"[fn main [] [println [lowercase "Hello WORLD"]]]"#);
        valid(r#"[fn main [] [each [fn [w] [println w]] [map lowercase [split "A B" " "]]]]"#);
        valid(r#"[fn main [] [each [fn [x] [println x]] [map inc #[1 2 3]]]]"#);
    }
    #[test]
    fn compile_sort_by_is_valid() {
        valid(r#"[fn id [x] x] [fn main [] [each [fn [x] [println x]] [sort-by id :asc #[3 1 2]]]]"#);
        valid(
            r#"[fn main [] [each [fn [[k v]] [println v]]
                 [sort-by [fn [[_ n]] n] :desc [entries {:a 1 :b 2}]]]]"#,
        );
    }
    #[test]
    fn compile_entries_and_destructuring_are_valid() {
        valid(r#"[fn main [] [println [len [entries {:a 1 :b 2 :c 3}]]]]"#);
        valid(r#"[fn main [] [each [fn [[k v]] [println v]] [entries {:a 10 :b 20}]]]"#);
        valid(r#"[fn main [] [each [fn [[_ v]] [println v]] [entries {:x 7}]]]"#);
    }
    #[test]
    fn compile_merge_is_valid() {
        valid(
            r#"[fn main [] [let m [merge {:a 1 :b 2} {:b 9 :c 3}]]
               [println [get m :a]] [println [get m :b]] [println [get m :c]]]"#,
        );
    }
    #[test]
    fn compile_update_is_valid() {
        valid(r#"[fn main [] [println [get [update {:a 5} :a [fn [n] [+ n 10]]] :a]]]"#);
        valid(
            r#"[fn bump [m k] [update m k [fn [n] [+ [or n 0] 1]]]]
               [fn main [] [println [get [fold {} bump [split "a b a" " "]] "a"]]]"#,
        );
    }
    #[test]
    fn compile_take_is_valid() {
        valid(r#"[fn main [] [println [len [take 3 #[10 20 30 40 50]]]]]"#);
        valid(r#"[fn main [] [println [len [take 9 #[1 2]]]]]"#);
    }
    #[test]
    fn compile_str_stringifies_ints() {
        // Type-directed Display: non-string args to `str` render via int_to_str.
        valid(r#"[fn main [] [println [str "k" 42]]]"#);
        valid(r#"[fn main [] [println [str "n=" 7 "!"]]]"#);
        valid(r#"[fn main [] [println [str 123]]]"#);
    }
    #[test]
    fn compile_split_and_cons_are_valid() {
        valid(r#"[fn main [] [println [len [split "a b c d" " "]]]]"#);
        valid(r#"[fn main [] [each [fn [w] [println w]] [split "the quick fox" " "]]]"#);
        valid(r#"[fn main [] [println [len [cons 1 [cons 2 #[3 4]]]]]]"#);
    }
    #[test]
    fn compile_string_param_equality_is_valid() {
        // A directly-called fn with string params gets structural `=`.
        valid(r#"[fn same [a b] [if [= a b] 1 0]] [fn main [] [println [same "xy" [str "x" "y"]]]]"#);
    }
    #[test]
    fn compile_maps_are_valid() {
        // literals, assoc/get/contains?/keys, keyword + computed-string keys,
        // and a fold building a frequency map (the word-count pattern).
        valid(r#"[fn main [] [let m {:x 10 :y 20}] [println [get m :y]]]"#);
        valid(r#"[fn main [] [let m [assoc {} "hi" 7]] [println [get m [str "h" "i"]]]]"#);
        valid(r#"[fn main [] [let m {:a 1}] [println [if [contains? m :a] 1 0]]]"#);
        valid(r#"[fn main [] [println [len [keys {:a 1 :b 2}]]]]"#);
        valid(
            r#"[fn add1 [m k] [assoc m k [+ [get m k] 1]]]
               [fn main [] [let m [fold {} add1 #[5 5 7]]] [println [get m 5]]]"#,
        );
        // String keys through a generic accumulator passed to fold — relies on
        // the self-describing key comparison, not a static key-type flag.
        valid(
            r#"[fn add1 [m k] [assoc m k [+ [get m k] 1]]]
               [fn main [] [let m [fold {} add1 [split "a b a" " "]]] [println [get m "a"]]]"#,
        );
    }
    #[test]
    fn compile_string_equality_is_structural() {
        // `=` on strings must compare content (str_eq), not pointer identity.
        valid(r#"[fn main [] [println [if [= [str "a" "b"] "ab"] 1 0]]]"#);
        valid(r#"[fn main [] [println [if [!= [str "a" "b"] "ac"] 1 0]]]"#);
    }
    #[test]
    fn compile_floats_are_valid() {
        // Float literals, arithmetic, comparison, and println (incl. through a
        // function body whose param the checker generalizes).
        valid(r#"[fn main [] [println 3.14] [println [+ 1.5 2.0]]]"#);
        valid(r#"[fn main [] [println [if [< 1.5 2.5] 1 0]]]"#);
        valid(r#"[fn area [r] [* [* 3.14 r] r]] [fn main [] [println [area 2.0]]]"#);
        valid(r#"[fn add [a b] [+ a b]] [fn main [] [println [if [= [add 1.5 2.0] 3.5] 1 0]]]"#);
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
    fn compile_tail_resumptive_handlers_are_valid() {
        // A handler that `resume`s in tail position is installed on the dynamic
        // handler stack and intercepts the op (even through a function call).
        valid(
            r#"[effect Log [ask [] Int]]
               [fn use-log [] [+ [Log.ask] 1]]
               [fn main [] [println [handle [use-log] [Log.ask] [resume 41]]]]"#,
        );
        // `[return x]` clause and multiple ops.
        valid(
            r#"[effect E [a [] Int] [b [] Int]]
               [fn main [] [println [handle [+ [E.a] [E.b]]
                 [return x] [* x 2] [E.a] [resume 10] [E.b] [resume 20]]]]"#,
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
