mod capture;
pub mod collections;
pub mod strings;

use crate::ast::{Expr, ExprKind};
use collections::CollectionsRuntime;
use std::collections::HashMap;
use strings::StringRuntime;
use wasm_encoder::*;

/// Compile a Loon program to WASM bytes.
pub fn compile(exprs: &[Expr]) -> Result<Vec<u8>, String> {
    let mut compiler = Compiler::new();
    compiler.compile_program(exprs)?;
    compiler.tree_shake();
    Ok(compiler.finish())
}

/// Compile a Loon program with multi-file support.
pub fn compile_with_imports(exprs: &[Expr], base_dir: &std::path::Path) -> Result<Vec<u8>, String> {
    let mut compiler = Compiler::new();
    compiler.base_dir = Some(base_dir.to_path_buf());
    compiler.compile_program(exprs)?;
    compiler.tree_shake();
    Ok(compiler.finish())
}

#[derive(Clone)]
struct FnDef { func_idx: u32, #[allow(dead_code)] arity: usize, is_closure: bool }

#[derive(Clone, Debug)]
struct AdtInfo { #[allow(dead_code)] type_name: String, #[allow(dead_code)] constructors: Vec<(String, u32, usize)> }

const WASI_IMPORT_COUNT: u32 = 6;
const PRE_ALLOC_TYPES: u32 = 7;

struct Compiler {
    functions: Vec<FunctionBody>,
    fn_map: HashMap<String, FnDef>,
    strings: Vec<(String, u32)>,
    string_offset: u32,
    next_fn_idx: u32,
    #[allow(dead_code)] import_count: u32,
    lambda_counter: u32,
    adt_constructors: HashMap<String, (u32, usize)>,
    #[allow(dead_code)] adt_types: Vec<AdtInfo>,
    table_entries: Vec<u32>,
    table_map: HashMap<u32, u32>,
    indirect_type_cache: HashMap<usize, u32>,
    type_count: u32,
    string_runtime: Option<StringRuntime>,
    collections_runtime: Option<CollectionsRuntime>,
    base_dir: Option<std::path::PathBuf>,
    compiled_modules: std::collections::HashSet<std::path::PathBuf>,
    force_heap: bool,
    used_wasi_imports: Option<Vec<u32>>,
}

struct FunctionBody { params: Vec<ValType>, results: Vec<ValType>, locals: Vec<ValType>, instructions: Vec<WasmInstruction> }

#[derive(Clone, Debug)]
#[allow(dead_code)]
enum WasmInstruction {
    I64Const(i64), F64Const(f64), I64Add, I64Sub, I64Mul, I64GtS, I64LtS, I64Eqz, I64Eq,
    F64Add, F64Sub, F64Mul,
    LocalGet(u32), LocalSet(u32), LocalTee(u32), Call(u32), If(BlockType), Else, End, Drop, Return,
    I32Const(i32), I32Store(u32, u32), I32Store16(u32, u32),
    CallIndirect(u32), I64Store(u32, u32), I64Load(u32, u32), I32Load(u32, u32),
    GlobalGet(u32), GlobalSet(u32),
    I32WrapI64, I64ExtendI32U, I64ShrU, I64And, I64Or, I64Shl, I32Add,
    I32Load8U(u32, u32), I32Store8(u32, u32), I32Eq, I32Eqz,
    Block(BlockType), Loop(BlockType), Br(u32), BrIf(u32), BrTable(Vec<u32>, u32),
}

fn emit_instruction(f: &mut Function, instr: &WasmInstruction) {
    match instr {
        WasmInstruction::I64Const(n) => { f.instruction(&Instruction::I64Const(*n)); }
        WasmInstruction::F64Const(n) => { f.instruction(&Instruction::F64Const(*n)); }
        WasmInstruction::I64Add => { f.instruction(&Instruction::I64Add); }
        WasmInstruction::I64Sub => { f.instruction(&Instruction::I64Sub); }
        WasmInstruction::I64Mul => { f.instruction(&Instruction::I64Mul); }
        WasmInstruction::I64GtS => { f.instruction(&Instruction::I64GtS); }
        WasmInstruction::I64LtS => { f.instruction(&Instruction::I64LtS); }
        WasmInstruction::I64Eqz => { f.instruction(&Instruction::I64Eqz); }
        WasmInstruction::I64Eq => { f.instruction(&Instruction::I64Eq); }
        WasmInstruction::F64Add => { f.instruction(&Instruction::F64Add); }
        WasmInstruction::F64Sub => { f.instruction(&Instruction::F64Sub); }
        WasmInstruction::F64Mul => { f.instruction(&Instruction::F64Mul); }
        WasmInstruction::LocalGet(i) => { f.instruction(&Instruction::LocalGet(*i)); }
        WasmInstruction::LocalSet(i) => { f.instruction(&Instruction::LocalSet(*i)); }
        WasmInstruction::LocalTee(i) => { f.instruction(&Instruction::LocalTee(*i)); }
        WasmInstruction::Call(i) => { f.instruction(&Instruction::Call(*i)); }
        WasmInstruction::CallIndirect(ty) => { f.instruction(&Instruction::CallIndirect { type_index: *ty, table_index: 0 }); }
        WasmInstruction::If(bt) => { f.instruction(&Instruction::If(*bt)); }
        WasmInstruction::Else => { f.instruction(&Instruction::Else); }
        WasmInstruction::End => { f.instruction(&Instruction::End); }
        WasmInstruction::Drop => { f.instruction(&Instruction::Drop); }
        WasmInstruction::Return => { f.instruction(&Instruction::Return); }
        WasmInstruction::I32Const(n) => { f.instruction(&Instruction::I32Const(*n)); }
        WasmInstruction::I32Store(a, o) => { f.instruction(&Instruction::I32Store(MemArg { offset: *o as u64, align: *a, memory_index: 0 })); }
        WasmInstruction::I32Store16(a, o) => { f.instruction(&Instruction::I32Store16(MemArg { offset: *o as u64, align: *a, memory_index: 0 })); }
        WasmInstruction::I64Store(a, o) => { f.instruction(&Instruction::I64Store(MemArg { offset: *o as u64, align: *a, memory_index: 0 })); }
        WasmInstruction::I64Load(a, o) => { f.instruction(&Instruction::I64Load(MemArg { offset: *o as u64, align: *a, memory_index: 0 })); }
        WasmInstruction::I32Load(a, o) => { f.instruction(&Instruction::I32Load(MemArg { offset: *o as u64, align: *a, memory_index: 0 })); }
        WasmInstruction::I32Load8U(a, o) => { f.instruction(&Instruction::I32Load8U(MemArg { offset: *o as u64, align: *a, memory_index: 0 })); }
        WasmInstruction::I32Store8(a, o) => { f.instruction(&Instruction::I32Store8(MemArg { offset: *o as u64, align: *a, memory_index: 0 })); }
        WasmInstruction::GlobalGet(i) => { f.instruction(&Instruction::GlobalGet(*i)); }
        WasmInstruction::GlobalSet(i) => { f.instruction(&Instruction::GlobalSet(*i)); }
        WasmInstruction::I32WrapI64 => { f.instruction(&Instruction::I32WrapI64); }
        WasmInstruction::I64ExtendI32U => { f.instruction(&Instruction::I64ExtendI32U); }
        WasmInstruction::I64ShrU => { f.instruction(&Instruction::I64ShrU); }
        WasmInstruction::I64And => { f.instruction(&Instruction::I64And); }
        WasmInstruction::I64Or => { f.instruction(&Instruction::I64Or); }
        WasmInstruction::I64Shl => { f.instruction(&Instruction::I64Shl); }
        WasmInstruction::I32Add => { f.instruction(&Instruction::I32Add); }
        WasmInstruction::I32Eq => { f.instruction(&Instruction::I32Eq); }
        WasmInstruction::I32Eqz => { f.instruction(&Instruction::I32Eqz); }
        WasmInstruction::Block(bt) => { f.instruction(&Instruction::Block(*bt)); }
        WasmInstruction::Loop(bt) => { f.instruction(&Instruction::Loop(*bt)); }
        WasmInstruction::Br(l) => { f.instruction(&Instruction::Br(*l)); }
        WasmInstruction::BrIf(l) => { f.instruction(&Instruction::BrIf(*l)); }
        WasmInstruction::BrTable(labels, default) => { f.instruction(&Instruction::BrTable(std::borrow::Cow::Borrowed(labels), *default)); }
    }
}

impl Compiler {
    fn new() -> Self {
        Self {
            functions: Vec::new(), fn_map: HashMap::new(), strings: Vec::new(),
            string_offset: 1024, next_fn_idx: WASI_IMPORT_COUNT, import_count: WASI_IMPORT_COUNT,
            lambda_counter: 0, adt_constructors: HashMap::new(), adt_types: Vec::new(),
            table_entries: Vec::new(), table_map: HashMap::new(), indirect_type_cache: HashMap::new(),
            type_count: PRE_ALLOC_TYPES, string_runtime: None, collections_runtime: None,
            base_dir: None, compiled_modules: std::collections::HashSet::new(), force_heap: false, used_wasi_imports: None,
        }
    }
    fn ensure_in_table(&mut self, func_idx: u32) -> u32 {
        if let Some(&ti) = self.table_map.get(&func_idx) { return ti; }
        let ti = self.table_entries.len() as u32;
        self.table_entries.push(func_idx); self.table_map.insert(func_idx, ti); ti
    }
    fn ensure_string_runtime(&mut self) {
        if self.string_runtime.is_some() { return; }
        self.force_heap = true;
        let c = self.next_fn_idx; self.next_fn_idx += 1; self.functions.push(StringRuntime::gen_str_concat());
        let l = self.next_fn_idx; self.next_fn_idx += 1; self.functions.push(StringRuntime::gen_str_len());
        let e = self.next_fn_idx; self.next_fn_idx += 1; self.functions.push(StringRuntime::gen_str_eq());
        self.string_runtime = Some(StringRuntime { str_concat_idx: c, str_len_idx: l, str_eq_idx: e });
    }
    fn ensure_collections_runtime(&mut self) {
        if self.collections_runtime.is_some() { return; }
        self.force_heap = true;
        let n = self.next_fn_idx; self.next_fn_idx += 1; self.functions.push(CollectionsRuntime::gen_vec_new());
        let p = self.next_fn_idx; self.next_fn_idx += 1; self.functions.push(CollectionsRuntime::gen_vec_push());
        let g = self.next_fn_idx; self.next_fn_idx += 1; self.functions.push(CollectionsRuntime::gen_vec_get());
        self.collections_runtime = Some(CollectionsRuntime { vec_new_idx: n, vec_push_idx: p, vec_get_idx: g });
    }
    fn compile_program(&mut self, exprs: &[Expr]) -> Result<(), String> {
        for expr in exprs { if let ExprKind::List(items) = &expr.kind { if !items.is_empty() { if let ExprKind::Symbol(s) = &items[0].kind { if s == "use" { self.compile_use(&items[1..])?; } } } } }
        for expr in exprs { if let ExprKind::List(items) = &expr.kind { if items.len() >= 2 { if let ExprKind::Symbol(s) = &items[0].kind { if s == "type" { self.collect_adt_def(&items[1..])?; } } } } }
        for expr in exprs { if let ExprKind::List(items) = &expr.kind { if items.len() >= 3 { if let ExprKind::Symbol(s) = &items[0].kind { if s == "defn" { if let ExprKind::Symbol(name) = &items[1].kind { if self.fn_map.contains_key(name) { continue; } if let ExprKind::List(params) = &items[2].kind { let arity = params.len(); let idx = self.next_fn_idx; self.fn_map.insert(name.clone(), FnDef { func_idx: idx, arity, is_closure: false }); self.next_fn_idx += 1; } } } } } } }
        for expr in exprs { if let ExprKind::List(items) = &expr.kind { if items.len() >= 3 { if let ExprKind::Symbol(s) = &items[0].kind { if s == "defn" { self.compile_defn(&items[1..])?; } } } } }
        Ok(())
    }
    fn tree_shake(&mut self) {
        let main_idx = match self.fn_map.get("main") {
            Some(def) => def.func_idx,
            None => return,
        };
        let mut reachable = std::collections::HashSet::new();
        let mut queue = std::collections::VecDeque::new();
        reachable.insert(main_idx);
        queue.push_back(main_idx);
        while let Some(idx) = queue.pop_front() {
            if idx < WASI_IMPORT_COUNT { continue; }
            let fn_offset = (idx - WASI_IMPORT_COUNT) as usize;
            if fn_offset >= self.functions.len() { continue; }
            let mut has_indirect = false;
            for instr in &self.functions[fn_offset].instructions {
                match instr {
                    WasmInstruction::Call(target) => {
                        if reachable.insert(*target) { queue.push_back(*target); }
                    }
                    WasmInstruction::CallIndirect(_) => { has_indirect = true; }
                    _ => {}
                }
            }
            if has_indirect {
                for &entry in &self.table_entries {
                    if reachable.insert(entry) { queue.push_back(entry); }
                }
            }
        }
        // Build remap: old func index → new func index
        let used_wasi: Vec<u32> = (0..WASI_IMPORT_COUNT).filter(|i| reachable.contains(i)).collect();
        let mut remap = HashMap::new();
        let mut new_idx = 0u32;
        for &old in &used_wasi { remap.insert(old, new_idx); new_idx += 1; }
        let new_import_count = new_idx;
        let mut kept_fn_indices = Vec::new();
        for i in 0..self.functions.len() {
            let old = WASI_IMPORT_COUNT + i as u32;
            if reachable.contains(&old) { remap.insert(old, new_idx); kept_fn_indices.push(i); new_idx += 1; }
        }
        // Filter functions
        let mut old_fns: Vec<Option<FunctionBody>> = std::mem::take(&mut self.functions).into_iter().map(Some).collect();
        self.functions = kept_fn_indices.iter().map(|&i| old_fns[i].take().unwrap()).collect();
        // Rewrite Call targets
        for func in &mut self.functions {
            for instr in &mut func.instructions {
                if let WasmInstruction::Call(ref mut target) = instr {
                    if let Some(&new) = remap.get(target) { *target = new; }
                }
            }
        }
        // Remap table entries
        self.table_entries = self.table_entries.iter().filter_map(|&old| remap.get(&old).copied()).collect();
        self.table_map.clear();
        for (ti, &func_idx) in self.table_entries.iter().enumerate() { self.table_map.insert(func_idx, ti as u32); }
        // Update fn_map
        for def in self.fn_map.values_mut() {
            if let Some(&new) = remap.get(&def.func_idx) { def.func_idx = new; }
        }
        self.used_wasi_imports = Some(used_wasi);
        self.import_count = new_import_count;
        self.next_fn_idx = new_idx;
    }
    fn compile_use(&mut self, args: &[Expr]) -> Result<(), String> {
        if args.is_empty() { return Ok(()); }
        let module_path = match &args[0].kind { ExprKind::Symbol(s) | ExprKind::Str(s) => s.clone(), _ => return Ok(()) };
        let base_dir = match &self.base_dir { Some(d) => d.clone(), None => return Ok(()) };
        let file_path = crate::module::ModuleCache::resolve_path(&module_path, &base_dir);
        let canonical = file_path.canonicalize().unwrap_or_else(|_| file_path.clone());
        if self.compiled_modules.contains(&canonical) { return Ok(()); }
        self.compiled_modules.insert(canonical);
        let source = std::fs::read_to_string(&file_path).map_err(|e| format!("codegen: cannot read module '{}' at {}: {e}", module_path, file_path.display()))?;
        let module_exprs = crate::parser::parse(&source).map_err(|e| format!("codegen: parse error in module '{}': {}", module_path, e.message))?;
        let old_base = self.base_dir.clone();
        self.base_dir = file_path.parent().map(|p| p.to_path_buf());
        self.compile_program(&module_exprs)?;
        self.base_dir = old_base;
        Ok(())
    }
    fn collect_adt_def(&mut self, args: &[Expr]) -> Result<(), String> {
        if args.is_empty() { return Ok(()); }
        let type_name = match &args[0].kind { ExprKind::Symbol(s) => s.clone(), _ => return Ok(()) };
        let mut constructors = Vec::new(); let mut tag: u32 = 0;
        for arg in &args[1..] { match &arg.kind {
            ExprKind::List(items) if !items.is_empty() => { if let ExprKind::Symbol(cn) = &items[0].kind { if cn.starts_with(char::is_uppercase) { let arity = items.len() - 1; self.adt_constructors.insert(cn.clone(), (tag, arity)); constructors.push((cn.clone(), tag, arity)); tag += 1; } } }
            ExprKind::Symbol(name) if name.starts_with(char::is_uppercase) => { self.adt_constructors.insert(name.clone(), (tag, 0)); constructors.push((name.clone(), tag, 0)); tag += 1; }
            _ => {}
        }}
        self.adt_types.push(AdtInfo { type_name, constructors }); Ok(())
    }
    fn compile_defn(&mut self, args: &[Expr]) -> Result<(), String> {
        if args.len() < 2 { return Err("defn requires name, params, body".into()); }
        let name = match &args[0].kind { ExprKind::Symbol(s) => s.clone(), _ => return Err("defn name must be a symbol".into()) };
        let params = match &args[1].kind { ExprKind::List(items) => items.iter().filter_map(|p| if let ExprKind::Symbol(s) = &p.kind { Some(s.clone()) } else { None }).collect::<Vec<_>>(), _ => return Err("defn params must be a list".into()) };
        let mut body_start = 2;
        if body_start < args.len() { if let ExprKind::Symbol(s) = &args[body_start].kind { if s == "/" { body_start += 2; } } }
        let mut ctx = FnCtx { locals: HashMap::new(), local_count: params.len() as u32, instructions: Vec::new(), compiler: self };
        for (i, p) in params.iter().enumerate() { ctx.locals.insert(p.clone(), i as u32); }
        for expr in &args[body_start..] { ctx.compile_expr(expr)?; }
        let is_main = name == "main";
        let extra_locals = if ctx.local_count > params.len() as u32 { vec![ValType::I64; (ctx.local_count - params.len() as u32) as usize] } else { vec![] };
        if is_main && !ctx.instructions.is_empty() { ctx.instructions.push(WasmInstruction::Drop); }
        let instrs = ctx.instructions.clone(); drop(ctx);
        self.functions.push(FunctionBody { params: vec![ValType::I64; params.len()], results: if is_main { vec![] } else { vec![ValType::I64] }, locals: extra_locals, instructions: instrs });
        Ok(())
    }
    fn intern_string(&mut self, s: &str) -> (u32, u32) {
        for (existing, offset) in &self.strings { if existing == s { return (*offset, s.len() as u32); } }
        let offset = self.string_offset; let len = s.len() as u32;
        self.strings.push((s.to_string(), offset)); self.string_offset += len + 1; (offset, len)
    }
    fn finish(self) -> Vec<u8> {
        let mut module = Module::new();
        let mut types = TypeSection::new();
        types.ty().function(vec![ValType::I32; 4], vec![ValType::I32]); // 0: fd_write
        types.ty().function(vec![ValType::I32, ValType::I32], vec![]); // 1: println helper
        types.ty().function(vec![ValType::I32; 4], vec![ValType::I32]); // 2: fd_read
        types.ty().function(vec![ValType::I32, ValType::I32], vec![ValType::I32]); // 3: args_get
        types.ty().function(vec![ValType::I32, ValType::I32], vec![ValType::I32]); // 4: args_sizes_get
        types.ty().function(vec![ValType::I32, ValType::I32], vec![ValType::I32]); // 5: environ_get
        types.ty().function(vec![ValType::I32, ValType::I32], vec![ValType::I32]); // 6: environ_sizes_get
        let mut fn_type_indices = Vec::new();
        for func in &self.functions { let ti = types.len(); types.ty().function(func.params.clone(), func.results.clone()); fn_type_indices.push(ti); }
        let mut ie: Vec<(usize, u32)> = self.indirect_type_cache.iter().map(|(&a, &t)| (a, t)).collect();
        ie.sort_by_key(|&(_, idx)| idx);
        let indirect_type_remap: HashMap<u32, u32> = ie.iter().enumerate().map(|(i, &(_, cached_idx))| {
            (cached_idx, PRE_ALLOC_TYPES + self.functions.len() as u32 + i as u32)
        }).collect();
        for (arity, _) in &ie { types.ty().function(vec![ValType::I64; *arity], vec![ValType::I64]); }
        module.section(&types);
        let wasi_defs: [(u32, &str, u32); 6] = [
            (0, "fd_write", 0), (1, "fd_read", 2), (2, "args_get", 3),
            (3, "args_sizes_get", 4), (4, "environ_get", 5), (5, "environ_sizes_get", 6),
        ];
        let mut imports = ImportSection::new();
        match &self.used_wasi_imports {
            Some(used) => { for &idx in used { let (_, name, type_idx) = wasi_defs[idx as usize]; imports.import("wasi_snapshot_preview1", name, EntityType::Function(type_idx)); } }
            None => { for &(_, name, type_idx) in &wasi_defs { imports.import("wasi_snapshot_preview1", name, EntityType::Function(type_idx)); } }
        }
        module.section(&imports);
        let mut functions = FunctionSection::new();
        for idx in &fn_type_indices { functions.function(*idx); }
        module.section(&functions);
        if !self.table_entries.is_empty() { let mut t = TableSection::new(); t.table(TableType { element_type: RefType::FUNCREF, minimum: self.table_entries.len() as u64, maximum: Some(self.table_entries.len() as u64), table64: false, shared: false }); module.section(&t); }
        let mut mem = MemorySection::new(); mem.memory(MemoryType { minimum: 1, maximum: None, memory64: false, shared: false, page_size_log2: None }); module.section(&mem);
        if self.force_heap || !self.table_entries.is_empty() || !self.adt_constructors.is_empty() { let mut g = GlobalSection::new(); g.global(GlobalType { val_type: ValType::I32, mutable: true, shared: false }, &ConstExpr::i32_const(4096)); module.section(&g); }
        let mut exports = ExportSection::new(); exports.export("memory", ExportKind::Memory, 0);
        if let Some(main_fn) = self.fn_map.get("main") { exports.export("_start", ExportKind::Func, main_fn.func_idx); }
        module.section(&exports);
        if !self.table_entries.is_empty() { let mut e = ElementSection::new(); e.active(Some(0), &ConstExpr::i32_const(0), Elements::Functions(self.table_entries.clone().into())); module.section(&e); }
        let mut code = CodeSection::new();
        for func in &self.functions {
            let mut f = Function::new(func.locals.iter().map(|t| (1, *t)).collect::<Vec<_>>());
            for instr in &func.instructions {
                if let WasmInstruction::CallIndirect(ty) = instr {
                    let actual_ty = indirect_type_remap.get(ty).copied().unwrap_or(*ty);
                    f.instruction(&Instruction::CallIndirect { type_index: actual_ty, table_index: 0 });
                } else { emit_instruction(&mut f, instr); }
            }
            f.instruction(&Instruction::End); code.function(&f);
        }
        module.section(&code);
        if !self.strings.is_empty() { let mut d = DataSection::new(); for (s, offset) in &self.strings { d.active(0, &ConstExpr::i32_const(*offset as i32), s.as_bytes().iter().copied()); } module.section(&d); }
        module.finish()
    }
}

struct FnCtx<'a> { locals: HashMap<String, u32>, local_count: u32, instructions: Vec<WasmInstruction>, compiler: &'a mut Compiler }

impl<'a> FnCtx<'a> {
    fn alloc_local(&mut self) -> u32 { let i = self.local_count; self.local_count += 1; i }
    fn compile_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match &expr.kind {
            ExprKind::Int(n) => { self.instructions.push(WasmInstruction::I64Const(*n)); Ok(()) }
            ExprKind::Float(n) => { self.instructions.push(WasmInstruction::F64Const(*n)); Ok(()) }
            ExprKind::Bool(b) => { self.instructions.push(WasmInstruction::I64Const(if *b { 1 } else { 0 })); Ok(()) }
            ExprKind::Str(s) => { let (offset, len) = self.compiler.intern_string(s); let packed = ((offset as i64) << 32) | (len as i64); self.instructions.push(WasmInstruction::I64Const(packed)); Ok(()) }
            ExprKind::Symbol(name) => {
                if let Some(&idx) = self.locals.get(name) { self.instructions.push(WasmInstruction::LocalGet(idx)); Ok(()) }
                else if let Some((tag, 0)) = self.compiler.adt_constructors.get(name.as_str()).cloned() { self.compile_adt_constructor(name, tag, 0, &[]) }
                else { Err(format!("codegen: unbound symbol '{name}'")) }
            }
            ExprKind::List(items) if items.is_empty() => { self.instructions.push(WasmInstruction::I64Const(0)); Ok(()) }
            ExprKind::List(items) => { if let ExprKind::Symbol(s) = &items[0].kind { if s == "fn" { return self.compile_closure(&items[1..]); } } self.compile_call(items) }
            _ => Err(format!("codegen: unsupported expression: {:?}", expr.kind)),
        }
    }
    fn compile_call(&mut self, items: &[Expr]) -> Result<(), String> {
        if items.is_empty() { return Ok(()); }
        if let ExprKind::Symbol(s) = &items[0].kind {
            match s.as_str() {
                "+" => { self.compile_expr(&items[1])?; self.compile_expr(&items[2])?; self.instructions.push(WasmInstruction::I64Add); return Ok(()); }
                "-" => { self.compile_expr(&items[1])?; self.compile_expr(&items[2])?; self.instructions.push(WasmInstruction::I64Sub); return Ok(()); }
                "*" => { self.compile_expr(&items[1])?; self.compile_expr(&items[2])?; self.instructions.push(WasmInstruction::I64Mul); return Ok(()); }
                ">" => { self.compile_expr(&items[1])?; self.compile_expr(&items[2])?; self.instructions.push(WasmInstruction::I64GtS); return Ok(()); }
                "<" => { self.compile_expr(&items[1])?; self.compile_expr(&items[2])?; self.instructions.push(WasmInstruction::I64LtS); return Ok(()); }
                "=" => { self.compile_expr(&items[1])?; self.compile_expr(&items[2])?; self.instructions.push(WasmInstruction::I64Eq); return Ok(()); }
                "str-len" => { self.compiler.ensure_string_runtime(); let rt = self.compiler.string_runtime.clone().unwrap(); self.compile_expr(&items[1])?; self.instructions.push(WasmInstruction::Call(rt.str_len_idx)); return Ok(()); }
                "str-concat" | "str" => { self.compiler.ensure_string_runtime(); let rt = self.compiler.string_runtime.clone().unwrap(); self.compile_expr(&items[1])?; self.compile_expr(&items[2])?; self.instructions.push(WasmInstruction::Call(rt.str_concat_idx)); return Ok(()); }
                "str-eq" => { self.compiler.ensure_string_runtime(); let rt = self.compiler.string_runtime.clone().unwrap(); self.compile_expr(&items[1])?; self.compile_expr(&items[2])?; self.instructions.push(WasmInstruction::Call(rt.str_eq_idx)); return Ok(()); }
                "vec-new" => { self.compiler.ensure_collections_runtime(); let rt = self.compiler.collections_runtime.clone().unwrap(); self.instructions.push(WasmInstruction::Call(rt.vec_new_idx)); return Ok(()); }
                "vec-push" => { self.compiler.ensure_collections_runtime(); let rt = self.compiler.collections_runtime.clone().unwrap(); self.compile_expr(&items[1])?; self.compile_expr(&items[2])?; self.instructions.push(WasmInstruction::Call(rt.vec_push_idx)); return Ok(()); }
                "vec-get" => { self.compiler.ensure_collections_runtime(); let rt = self.compiler.collections_runtime.clone().unwrap(); self.compile_expr(&items[1])?; self.compile_expr(&items[2])?; self.instructions.push(WasmInstruction::Call(rt.vec_get_idx)); return Ok(()); }
                "if" => { self.compile_expr(&items[1])?; self.instructions.push(WasmInstruction::I64Eqz); self.instructions.push(WasmInstruction::If(BlockType::Result(ValType::I64))); if items.len() > 3 { self.compile_expr(&items[3])?; } else { self.instructions.push(WasmInstruction::I64Const(0)); } self.instructions.push(WasmInstruction::Else); self.compile_expr(&items[2])?; self.instructions.push(WasmInstruction::End); return Ok(()); }
                "let" => { let (ni, vi) = if matches!(&items[1].kind, ExprKind::Symbol(s) if s == "mut") { (3, 3) } else { (1, 2) }; let name = match &items[ni].kind { ExprKind::Symbol(s) => s.clone(), _ => return Err("let binding must be a symbol".into()) }; self.compile_expr(&items[vi])?; let local = self.alloc_local(); self.locals.insert(name, local); self.instructions.push(WasmInstruction::LocalSet(local)); self.instructions.push(WasmInstruction::LocalGet(local)); return Ok(()); }
                "do" => { for (i, item) in items[1..].iter().enumerate() { self.compile_expr(item)?; if i < items.len() - 2 { self.instructions.push(WasmInstruction::Drop); } } return Ok(()); }
                "println" => {
                    if let Some(arg) = items.get(1) {
                        if let ExprKind::Str(s) = &arg.kind {
                            let msg = format!("{s}\n"); let (offset, len) = self.compiler.intern_string(&msg);
                            self.instructions.push(WasmInstruction::I32Const(0)); self.instructions.push(WasmInstruction::I32Const(offset as i32)); self.instructions.push(WasmInstruction::I32Store(2, 0));
                            self.instructions.push(WasmInstruction::I32Const(4)); self.instructions.push(WasmInstruction::I32Const(len as i32)); self.instructions.push(WasmInstruction::I32Store(2, 0));
                            self.instructions.push(WasmInstruction::I32Const(1)); self.instructions.push(WasmInstruction::I32Const(0)); self.instructions.push(WasmInstruction::I32Const(1)); self.instructions.push(WasmInstruction::I32Const(8));
                            self.instructions.push(WasmInstruction::Call(0)); self.instructions.push(WasmInstruction::Drop); self.instructions.push(WasmInstruction::I64Const(0)); return Ok(());
                        } else { self.compile_expr(arg)?; self.instructions.push(WasmInstruction::Drop); self.instructions.push(WasmInstruction::I64Const(0)); return Ok(()); }
                    }
                    self.instructions.push(WasmInstruction::I64Const(0)); return Ok(());
                }
                "match" => { if items.len() < 2 { return Err("match requires a value".into()); } self.compile_expr(&items[1])?; let sc = self.alloc_local(); self.instructions.push(WasmInstruction::LocalSet(sc)); self.compile_match_arms(sc, &items[2..])?; return Ok(()); }
                "map" | "filter" => { if items.len() >= 3 { if let ExprKind::List(li) = &items[1].kind { if !li.is_empty() { if let ExprKind::Symbol(fs) = &li[0].kind { if fs == "fn" { return self.compile_hof_lambda(s, &li[1..], &items[2..]); } } } } } return Err(format!("codegen: {s} requires a lambda literal argument.")); }
                "type" | "use" => { self.instructions.push(WasmInstruction::I64Const(0)); return Ok(()); }
                name => {
                    if let Some((tag, arity)) = self.compiler.adt_constructors.get(name).cloned() { return self.compile_adt_constructor(name, tag, arity, &items[1..]); }
                    if let Some(fn_def) = self.compiler.fn_map.get(name).cloned() { if fn_def.is_closure { return self.compile_closure_call_named(name, &items[1..]); } for arg in &items[1..] { self.compile_expr(arg)?; } self.instructions.push(WasmInstruction::Call(fn_def.func_idx)); return Ok(()); }
                    if self.locals.contains_key(name) { return self.compile_closure_call_local(name, &items[1..]); }
                    return Err(format!("codegen: unknown function '{name}'"));
                }
            }
        }
        Err("codegen: unsupported call form".into())
    }
    fn compile_match_arms(&mut self, scrutinee: u32, arms: &[Expr]) -> Result<(), String> {
        let parsed = self.parse_arms(arms);
        if self.try_compile_br_table(scrutinee, &parsed)? { return Ok(()); }
        self.compile_match_arms_ifelse(scrutinee, arms)
    }
    fn parse_arms<'b>(&self, arms: &'b [Expr]) -> Vec<(&'b Expr, &'b Expr)> {
        let mut r = Vec::new(); let mut i = 0;
        while i + 2 < arms.len() { if let ExprKind::Symbol(s) = &arms[i+1].kind { if s == "=>" { r.push((&arms[i], &arms[i+2])); i += 3; continue; } } i += 1; }
        r
    }
    fn try_compile_br_table(&mut self, scrutinee: u32, parsed: &[(&Expr, &Expr)]) -> Result<bool, String> {
        if parsed.is_empty() { return Ok(false); }
        let mut int_arms: Vec<(i64, &Expr)> = Vec::new(); let mut default_body: Option<&Expr> = None;
        for (pat, body) in parsed { match &pat.kind {
            ExprKind::Int(n) => int_arms.push((*n, body)),
            ExprKind::Symbol(s) if s == "_" => { default_body = Some(body); }
            ExprKind::Symbol(s) if !s.starts_with(char::is_uppercase) => { default_body = Some(body); }
            _ => return Ok(false),
        }}
        if int_arms.is_empty() { return Ok(false); }
        int_arms.sort_by_key(|(n, _)| *n);
        let min_val = int_arms[0].0; let max_val = int_arms[int_arms.len()-1].0;
        if min_val != 0 || (max_val - min_val) as usize != int_arms.len() - 1 { return Ok(false); }
        for (idx, (n, _)) in int_arms.iter().enumerate() { if *n != idx as i64 { return Ok(false); } }
        let nc = int_arms.len();
        self.instructions.push(WasmInstruction::Block(BlockType::Result(ValType::I64)));
        for _ in 0..=nc { self.instructions.push(WasmInstruction::Block(BlockType::Empty)); }
        self.instructions.push(WasmInstruction::LocalGet(scrutinee));
        self.instructions.push(WasmInstruction::I32WrapI64);
        self.instructions.push(WasmInstruction::BrTable((0..nc as u32).collect(), nc as u32));
        for (ci, (_, body)) in int_arms.iter().enumerate() { self.instructions.push(WasmInstruction::End); self.compile_expr(body)?; self.instructions.push(WasmInstruction::Br((nc - ci) as u32)); }
        self.instructions.push(WasmInstruction::End);
        if let Some(body) = default_body { self.compile_expr(body)?; } else { self.instructions.push(WasmInstruction::I64Const(0)); }
        self.instructions.push(WasmInstruction::End);
        Ok(true)
    }
    fn compile_match_arms_ifelse(&mut self, scrutinee: u32, arms: &[Expr]) -> Result<(), String> {
        let mut i = 0; let mut first = true;
        while i < arms.len() {
            let pattern = &arms[i];
            if i + 1 < arms.len() { if let ExprKind::Symbol(s) = &arms[i+1].kind { if s == "=>" && i + 2 < arms.len() { match &pattern.kind {
                ExprKind::Symbol(s) if s == "_" => { if !first { self.instructions.push(WasmInstruction::Else); } self.compile_expr(&arms[i+2])?; if !first { self.instructions.push(WasmInstruction::End); } return Ok(()); }
                ExprKind::Int(n) => { self.instructions.push(WasmInstruction::LocalGet(scrutinee)); self.instructions.push(WasmInstruction::I64Const(*n)); self.instructions.push(WasmInstruction::I64Eq); self.instructions.push(WasmInstruction::If(BlockType::Result(ValType::I64))); self.compile_expr(&arms[i+2])?; first = false; i += 3; continue; }
                ExprKind::Symbol(name) if name != "=>" => {
                    if let Some((tag, 0)) = self.compiler.adt_constructors.get(name.as_str()).cloned() { self.instructions.push(WasmInstruction::LocalGet(scrutinee)); self.instructions.push(WasmInstruction::I32WrapI64); self.instructions.push(WasmInstruction::I64Load(3, 0)); self.instructions.push(WasmInstruction::I64Const(tag as i64)); self.instructions.push(WasmInstruction::I64Eq); self.instructions.push(WasmInstruction::If(BlockType::Result(ValType::I64))); self.compile_expr(&arms[i+2])?; first = false; i += 3; continue; }
                    if !first { self.instructions.push(WasmInstruction::Else); } let local = self.alloc_local(); self.locals.insert(name.clone(), local); self.instructions.push(WasmInstruction::LocalGet(scrutinee)); self.instructions.push(WasmInstruction::LocalSet(local)); self.compile_expr(&arms[i+2])?; if !first { self.instructions.push(WasmInstruction::End); } return Ok(());
                }
                ExprKind::List(pat_items) if !pat_items.is_empty() => { if let ExprKind::Symbol(cn) = &pat_items[0].kind { if let Some((tag, _)) = self.compiler.adt_constructors.get(cn.as_str()).cloned() {
                    self.instructions.push(WasmInstruction::LocalGet(scrutinee)); self.instructions.push(WasmInstruction::I32WrapI64); self.instructions.push(WasmInstruction::I64Load(3, 0)); self.instructions.push(WasmInstruction::I64Const(tag as i64)); self.instructions.push(WasmInstruction::I64Eq); self.instructions.push(WasmInstruction::If(BlockType::Result(ValType::I64)));
                    for (fi, fp) in pat_items[1..].iter().enumerate() { if let ExprKind::Symbol(fn_) = &fp.kind { if fn_ != "_" { let local = self.alloc_local(); self.locals.insert(fn_.clone(), local); self.instructions.push(WasmInstruction::LocalGet(scrutinee)); self.instructions.push(WasmInstruction::I32WrapI64); self.instructions.push(WasmInstruction::I64Load(3, (8 + fi * 8) as u32)); self.instructions.push(WasmInstruction::LocalSet(local)); } } }
                    self.compile_expr(&arms[i+2])?; first = false; i += 3; continue;
                } } }
                _ => {}
            } i += 3; continue; } } }
            i += 1;
        }
        if !first { self.instructions.push(WasmInstruction::Else); self.instructions.push(WasmInstruction::I64Const(0)); self.instructions.push(WasmInstruction::End); } else { self.instructions.push(WasmInstruction::I64Const(0)); }
        Ok(())
    }
    fn compile_hof_lambda(&mut self, hof_name: &str, lambda_args: &[Expr], _hof_rest: &[Expr]) -> Result<(), String> {
        if lambda_args.is_empty() { return Err("lambda requires params".into()); }
        let params = match &lambda_args[0].kind { ExprKind::List(items) => items.iter().filter_map(|p| if let ExprKind::Symbol(s) = &p.kind { Some(s.clone()) } else { None }).collect::<Vec<_>>(), _ => return Err("lambda params must be a list".into()) };
        let body = &lambda_args[1..];
        let free = capture::free_vars(&params, body); let mut captures: Vec<(String, u32)> = Vec::new();
        for name in &free { if let Some(&idx) = self.locals.get(name) { captures.push((name.clone(), idx)); } }
        let lname = format!("__lambda_{}", self.compiler.lambda_counter); self.compiler.lambda_counter += 1;
        let mut all_params: Vec<String> = captures.iter().map(|(n,_)| n.clone()).collect(); all_params.extend(params.clone());
        let idx = self.compiler.next_fn_idx; self.compiler.fn_map.insert(lname, FnDef { func_idx: idx, arity: all_params.len(), is_closure: false }); self.compiler.next_fn_idx += 1;
        let mut lctx = FnCtx { locals: HashMap::new(), local_count: all_params.len() as u32, instructions: Vec::new(), compiler: self.compiler };
        for (i, p) in all_params.iter().enumerate() { lctx.locals.insert(p.clone(), i as u32); }
        for expr in body { lctx.compile_expr(expr)?; }
        let linstrs = lctx.instructions.clone(); let llc = lctx.local_count; drop(lctx);
        self.compiler.functions.push(FunctionBody { params: vec![ValType::I64; all_params.len()], results: vec![ValType::I64], locals: if llc > all_params.len() as u32 { vec![ValType::I64; (llc - all_params.len() as u32) as usize] } else { vec![] }, instructions: linstrs });
        match hof_name { "map" | "filter" => { for (_, li) in &captures { self.instructions.push(WasmInstruction::LocalGet(*li)); } self.instructions.push(WasmInstruction::I64Const(0)); self.instructions.push(WasmInstruction::Call(idx)); } _ => return Err(format!("codegen: HOF '{hof_name}' not supported")) }
        Ok(())
    }
    fn compile_closure(&mut self, args: &[Expr]) -> Result<(), String> {
        if args.is_empty() { return Err("closure requires params".into()); }
        let params = match &args[0].kind { ExprKind::List(items) => items.iter().filter_map(|p| if let ExprKind::Symbol(s) = &p.kind { Some(s.clone()) } else { None }).collect::<Vec<_>>(), _ => return Err("closure params must be a list".into()) };
        let body = &args[1..]; let free = capture::free_vars(&params, body); let mut captures: Vec<(String, u32)> = Vec::new();
        for name in &free { if let Some(&idx) = self.locals.get(name) { captures.push((name.clone(), idx)); } }
        let lname = format!("__closure_{}", self.compiler.lambda_counter); self.compiler.lambda_counter += 1;
        let tp = 1 + params.len(); let idx = self.compiler.next_fn_idx;
        self.compiler.fn_map.insert(lname, FnDef { func_idx: idx, arity: tp, is_closure: true }); self.compiler.next_fn_idx += 1;
        let ti = self.compiler.ensure_in_table(idx);
        let mut cctx = FnCtx { locals: HashMap::new(), local_count: tp as u32, instructions: Vec::new(), compiler: self.compiler };
        cctx.locals.insert("__env_ptr".to_string(), 0); for (i, p) in params.iter().enumerate() { cctx.locals.insert(p.clone(), (i+1) as u32); }
        for (ci, (cn, _)) in captures.iter().enumerate() { let l = cctx.alloc_local(); cctx.locals.insert(cn.clone(), l); cctx.instructions.push(WasmInstruction::LocalGet(0)); cctx.instructions.push(WasmInstruction::I32WrapI64); cctx.instructions.push(WasmInstruction::I64Load(3, (ci*8) as u32)); cctx.instructions.push(WasmInstruction::LocalSet(l)); }
        for expr in body { cctx.compile_expr(expr)?; }
        let cinstrs = cctx.instructions.clone(); let clc = cctx.local_count; drop(cctx);
        self.compiler.functions.push(FunctionBody { params: vec![ValType::I64; tp], results: vec![ValType::I64], locals: if clc > tp as u32 { vec![ValType::I64; (clc - tp as u32) as usize] } else { vec![] }, instructions: cinstrs });
        if captures.is_empty() { self.instructions.push(WasmInstruction::I64Const((ti as i64) << 32)); } else {
            self.emit_alloc((captures.len() * 8) as u32); let el = self.alloc_local(); self.instructions.push(WasmInstruction::LocalTee(el));
            for (ci, (_, sl)) in captures.iter().enumerate() { self.instructions.push(WasmInstruction::LocalGet(el)); self.instructions.push(WasmInstruction::I32WrapI64); self.instructions.push(WasmInstruction::LocalGet(*sl)); self.instructions.push(WasmInstruction::I64Store(3, (ci*8) as u32)); }
            self.instructions.push(WasmInstruction::I64Const((ti as i64) << 32)); self.instructions.push(WasmInstruction::LocalGet(el)); self.instructions.push(WasmInstruction::I64Or);
        }
        Ok(())
    }
    fn compile_closure_call_local(&mut self, name: &str, call_args: &[Expr]) -> Result<(), String> {
        let cl = *self.locals.get(name).ok_or_else(|| format!("codegen: unbound closure '{name}'"))?;
        let el = self.alloc_local(); let tl = self.alloc_local();
        self.instructions.push(WasmInstruction::LocalGet(cl)); self.instructions.push(WasmInstruction::I64Const(0xFFFFFFFF)); self.instructions.push(WasmInstruction::I64And); self.instructions.push(WasmInstruction::LocalSet(el));
        self.instructions.push(WasmInstruction::LocalGet(cl)); self.instructions.push(WasmInstruction::I64Const(32)); self.instructions.push(WasmInstruction::I64ShrU); self.instructions.push(WasmInstruction::LocalSet(tl));
        self.instructions.push(WasmInstruction::LocalGet(el)); for arg in call_args { self.compile_expr(arg)?; }
        let ta = 1 + call_args.len(); let ty = self.get_or_create_indirect_type(ta);
        self.instructions.push(WasmInstruction::LocalGet(tl)); self.instructions.push(WasmInstruction::I32WrapI64); self.instructions.push(WasmInstruction::CallIndirect(ty));
        Ok(())
    }
    fn compile_closure_call_named(&mut self, _name: &str, _call_args: &[Expr]) -> Result<(), String> { Err("codegen: named closure calls not yet supported".into()) }
    fn compile_adt_constructor(&mut self, _name: &str, tag: u32, arity: usize, args: &[Expr]) -> Result<(), String> {
        if args.len() != arity { return Err(format!("codegen: constructor expects {} args, got {}", arity, args.len())); }
        self.emit_alloc((8 + arity * 8) as u32); let pl = self.alloc_local(); self.instructions.push(WasmInstruction::LocalTee(pl));
        self.instructions.push(WasmInstruction::I32WrapI64); self.instructions.push(WasmInstruction::I64Const(tag as i64)); self.instructions.push(WasmInstruction::I64Store(3, 0));
        for (fi, arg) in args.iter().enumerate() { self.instructions.push(WasmInstruction::LocalGet(pl)); self.instructions.push(WasmInstruction::I32WrapI64); self.compile_expr(arg)?; self.instructions.push(WasmInstruction::I64Store(3, (8 + fi * 8) as u32)); }
        self.instructions.push(WasmInstruction::LocalGet(pl)); Ok(())
    }
    fn emit_alloc(&mut self, size: u32) {
        self.instructions.push(WasmInstruction::GlobalGet(0)); self.instructions.push(WasmInstruction::I64ExtendI32U);
        self.instructions.push(WasmInstruction::GlobalGet(0)); self.instructions.push(WasmInstruction::I32Const(size as i32)); self.instructions.push(WasmInstruction::I32Add); self.instructions.push(WasmInstruction::GlobalSet(0));
    }
    fn get_or_create_indirect_type(&mut self, ta: usize) -> u32 {
        if let Some(&ti) = self.compiler.indirect_type_cache.get(&ta) { return ti; }
        let ti = self.compiler.type_count; self.compiler.type_count += 1; self.compiler.indirect_type_cache.insert(ta, ti); ti
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    fn ok(src: &str) { assert_eq!(&compile(&parse(src).unwrap()).unwrap()[0..4], b"\0asm"); }
    #[test] fn compile_hello_world() { ok(r#"[defn main [] [println "hello, world!"]]"#); }
    #[test] fn compile_arithmetic() { ok(r#"[defn main [] [+ 1 2]]"#); }
    #[test] fn compile_fib() { ok(r#"[defn fib [n] [match n 0 => 0  1 => 1  n => [+ [fib [- n 1]] [fib [- n 2]]]]] [defn main [] [fib 10]]"#); }
    #[test] fn compile_lambda_lift() { ok(r#"[defn apply-offset [offset] [map [fn [x] [+ x offset]] offset]] [defn main [] [apply-offset 10]]"#); }
    #[test] fn compile_closure_no_capture() { ok(r#"[defn main [] [let f [fn [x] [+ x 1]]] [f 41]]"#); }
    #[test] fn compile_closure_with_capture() { ok(r#"[defn main [] [let y 10] [let f [fn [x] [+ x y]]] [f 32]]"#); }
    #[test] fn compile_higher_order() { ok(r#"[defn apply [f x] [f x]] [defn main [] [apply [fn [x] [* x 2]] 21]]"#); }
    #[test] fn compile_adt_constructor_and_match() { ok(r#"[type Maybe T [Just T] Nothing] [defn main [] [let val [Just 42]] [match val [Just x] => x Nothing => 0]]"#); }
    #[test] fn compile_adt_nullary_match() { ok(r#"[type Maybe T [Just T] Nothing] [defn main [] [match Nothing [Just x] => x Nothing => 0]]"#); }
    #[test] fn compile_string_concat() { ok(r#"[defn main [] [let a "hello"] [let b " world"] [str-concat a b]]"#); }
    #[test] fn compile_string_len() { ok(r#"[defn main [] [str-len "test"]]"#); }
    #[test] fn compile_string_eq() { ok(r#"[defn main [] [str-eq "abc" "abc"]]"#); }
    #[test] fn compile_string_str_alias() { ok(r#"[defn main [] [str "foo" "bar"]]"#); }
    #[test] fn compile_match_br_table() { ok(r#"[defn dispatch [x] [match x 0 => 100  1 => 200  2 => 300  _ => 0]] [defn main [] [dispatch 1]]"#); }
    #[test] fn compile_match_br_table_no_default() { ok(r#"[defn dispatch [x] [match x 0 => 10  1 => 20  2 => 30]] [defn main [] [dispatch 2]]"#); }
    #[test] fn compile_match_noncontiguous_falls_back() { ok(r#"[defn dispatch [x] [match x 0 => 10  5 => 50  _ => 0]] [defn main [] [dispatch 5]]"#); }
    #[test] fn compile_vec_operations() { ok(r#"[defn main [] [let v [vec-new]] [let v2 [vec-push v 42]] [vec-get v2 0]]"#); }
    #[test] fn compile_multi_file_error_on_missing() {
        let exprs = parse(r#"[use nonexistent.module] [defn main [] 42]"#).unwrap();
        let r = compile_with_imports(&exprs, std::path::Path::new("/tmp/loon_test_nonexistent"));
        assert!(r.is_err()); assert!(r.unwrap_err().contains("cannot read module"));
    }
    #[test] fn compile_multi_file_skips_without_base() { ok(r#"[use some.module] [defn main [] 42]"#); }
    #[test] fn compile_multi_file_with_real_file() {
        let tmp = std::env::temp_dir().join("loon_test_multifile"); let _ = std::fs::create_dir_all(&tmp);
        std::fs::write(tmp.join("math.loon"), "[defn double [x] [* x 2]]").unwrap();
        let exprs = parse(r#"[use math] [defn main [] [double 21]]"#).unwrap();
        assert_eq!(&compile_with_imports(&exprs, &tmp).unwrap()[0..4], b"\0asm");
        let _ = std::fs::remove_file(tmp.join("math.loon")); let _ = std::fs::remove_dir(&tmp);
    }
    #[test] fn compile_string_packed_representation() { ok(r#"[defn main [] "hi"]"#); }
    #[test] fn tree_shake_removes_unused_function() {
        let with_unused = compile(&parse(r#"[defn unused [x] [+ x 1]] [defn main [] 42]"#).unwrap()).unwrap();
        let without = compile(&parse(r#"[defn main [] 42]"#).unwrap()).unwrap();
        assert!(with_unused.len() == without.len(), "unused function should be stripped: {} vs {}", with_unused.len(), without.len());
    }
    #[test] fn tree_shake_arithmetic_no_wasi() {
        let wasm = compile(&parse(r#"[defn main [] [+ 1 2]]"#).unwrap()).unwrap();
        assert!(!String::from_utf8_lossy(&wasm).contains("fd_write"), "pure arithmetic should not import fd_write");
    }
    #[test] fn tree_shake_closure_still_works() {
        let with_unused = compile(&parse(r#"[defn unused [] 99] [defn main [] [let f [fn [x] [+ x 1]]] [f 41]]"#).unwrap()).unwrap();
        let without = compile(&parse(r#"[defn main [] [let f [fn [x] [+ x 1]]] [f 41]]"#).unwrap()).unwrap();
        assert_eq!(with_unused.len(), without.len(), "unused fn should be stripped even with closures");
    }
}
