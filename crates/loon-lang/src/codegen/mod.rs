use crate::ast::{Expr, ExprKind};
use std::collections::HashMap;
use wasm_encoder::*;

/// Compile a Loon program to WASM bytes.
/// Supports: integer arithmetic, function defs/calls, locals, if/else,
/// string literals (via data segment), println (via WASI fd_write).
pub fn compile(exprs: &[Expr]) -> Result<Vec<u8>, String> {
    let mut compiler = Compiler::new();
    compiler.compile_program(exprs)?;
    Ok(compiler.finish())
}

#[derive(Clone)]
struct FnDef {
    /// Index in the WASM function section
    func_idx: u32,
    /// Number of parameters
    arity: usize,
}

struct Compiler {
    /// All function bodies, indexed by function section index
    functions: Vec<FunctionBody>,
    /// Maps loon function names to their wasm indices
    fn_map: HashMap<String, FnDef>,
    /// String data segments: content → (offset, len)
    strings: Vec<(String, u32)>,
    string_offset: u32,
    /// Next function index (starting after imports)
    next_fn_idx: u32,
    /// Number of imported functions
    import_count: u32,
}

struct FunctionBody {
    params: Vec<ValType>,
    results: Vec<ValType>,
    locals: Vec<ValType>,
    instructions: Vec<WasmInstruction>,
}

/// Simplified instruction set we emit
#[derive(Clone, Debug)]
enum WasmInstruction {
    I64Const(i64),
    F64Const(f64),
    I64Add,
    I64Sub,
    I64Mul,
    I64GtS,
    I64LtS,
    I64Eqz,
    I64Eq,
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
    // For println: store string, call fd_write
    I32Const(i32),
    I32Store(u32, u32),   // align, offset
    I32Store16(u32, u32), // for newline
}

impl Compiler {
    fn new() -> Self {
        Self {
            functions: Vec::new(),
            fn_map: HashMap::new(),
            strings: Vec::new(),
            string_offset: 1024, // Start string data at offset 1024
            next_fn_idx: 1,      // 0 is fd_write import
            import_count: 1,
        }
    }

    fn compile_program(&mut self, exprs: &[Expr]) -> Result<(), String> {
        // First pass: collect function definitions
        for expr in exprs {
            if let ExprKind::List(items) = &expr.kind {
                if items.len() >= 3 {
                    if let ExprKind::Symbol(s) = &items[0].kind {
                        if s == "defn" {
                            if let ExprKind::Symbol(name) = &items[1].kind {
                                if let ExprKind::List(params) = &items[2].kind {
                                    let arity = params.len();
                                    let idx = self.next_fn_idx;
                                    self.fn_map.insert(
                                        name.clone(),
                                        FnDef {
                                            func_idx: idx,
                                            arity,
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

        // Second pass: compile function bodies
        for expr in exprs {
            if let ExprKind::List(items) = &expr.kind {
                if items.len() >= 3 {
                    if let ExprKind::Symbol(s) = &items[0].kind {
                        if s == "defn" {
                            self.compile_defn(&items[1..])?;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn compile_defn(&mut self, args: &[Expr]) -> Result<(), String> {
        if args.len() < 2 {
            return Err("defn requires name, params, body".to_string());
        }

        let name = match &args[0].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err("defn name must be a symbol".to_string()),
        };

        let params = match &args[1].kind {
            ExprKind::List(items) => items
                .iter()
                .filter_map(|p| {
                    if let ExprKind::Symbol(s) = &p.kind {
                        Some(s.clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>(),
            _ => return Err("defn params must be a list".to_string()),
        };

        // Skip effect annotation
        let mut body_start = 2;
        if body_start < args.len() {
            if let ExprKind::Symbol(s) = &args[body_start].kind {
                if s == "/" {
                    body_start += 2;
                }
            }
        }

        let mut ctx = FnCtx {
            locals: HashMap::new(),
            local_count: params.len() as u32,
            instructions: Vec::new(),
            compiler: self,
        };

        // Register params as locals
        for (i, p) in params.iter().enumerate() {
            ctx.locals.insert(p.clone(), i as u32);
        }

        // Compile body
        for expr in &args[body_start..] {
            ctx.compile_expr(expr)?;
        }

        let is_main = name == "main";
        let param_types = vec![ValType::I64; params.len()];
        let result_types = if is_main { vec![] } else { vec![ValType::I64] };

        // Extra locals beyond params
        let extra_locals = if ctx.local_count > params.len() as u32 {
            vec![ValType::I64; (ctx.local_count - params.len() as u32) as usize]
        } else {
            vec![]
        };

        // For main, drop the last value if any body was compiled
        if is_main && !ctx.instructions.is_empty() {
            // Check if last instruction produces a value we need to drop
            ctx.instructions.push(WasmInstruction::Drop);
        }

        let body = FunctionBody {
            params: param_types,
            results: result_types,
            locals: extra_locals,
            instructions: ctx.instructions,
        };
        self.functions.push(body);

        Ok(())
    }

    fn intern_string(&mut self, s: &str) -> (u32, u32) {
        // Check if already interned
        for (existing, offset) in &self.strings {
            if existing == s {
                return (*offset, s.len() as u32);
            }
        }
        let offset = self.string_offset;
        let len = s.len() as u32;
        self.strings.push((s.to_string(), offset));
        self.string_offset += len + 1; // +1 for potential null terminator
        (offset, len)
    }

    fn finish(self) -> Vec<u8> {
        let mut module = Module::new();

        // Type section
        let mut types = TypeSection::new();
        // Type 0: fd_write(fd: i32, iovs: i32, iovs_len: i32, nwritten: i32) -> i32
        types.ty().function(
            vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            vec![ValType::I32],
        );
        // Type 1: println(ptr: i32, len: i32) -> ()
        types
            .ty()
            .function(vec![ValType::I32, ValType::I32], vec![]);

        // Generate types for each function
        let mut fn_type_indices = Vec::new();
        for (i, func) in self.functions.iter().enumerate() {
            let type_idx = types.len();
            types
                .ty()
                .function(func.params.clone(), func.results.clone());
            fn_type_indices.push(type_idx);
            let _ = i;
        }

        module.section(&types);

        // Import section: fd_write from wasi_snapshot_preview1
        let mut imports = ImportSection::new();
        imports.import(
            "wasi_snapshot_preview1",
            "fd_write",
            EntityType::Function(0), // type index 0
        );
        module.section(&imports);

        // Function section
        let mut functions = FunctionSection::new();
        for idx in &fn_type_indices {
            functions.function(*idx);
        }
        module.section(&functions);

        // Memory section
        let mut memories = MemorySection::new();
        memories.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memories);

        // Export section
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        // Export _start (main)
        if let Some(main_fn) = self.fn_map.get("main") {
            exports.export("_start", ExportKind::Func, main_fn.func_idx);
        }
        module.section(&exports);

        // Code section
        let mut code = CodeSection::new();
        for func in &self.functions {
            let mut f = Function::new(
                func.locals
                    .iter()
                    .map(|t| (1, *t))
                    .collect::<Vec<_>>(),
            );
            for instr in &func.instructions {
                match instr {
                    WasmInstruction::I64Const(n) => f.instruction(&Instruction::I64Const(*n)),
                    WasmInstruction::F64Const(n) => f.instruction(&Instruction::F64Const(*n)),
                    WasmInstruction::I64Add => f.instruction(&Instruction::I64Add),
                    WasmInstruction::I64Sub => f.instruction(&Instruction::I64Sub),
                    WasmInstruction::I64Mul => f.instruction(&Instruction::I64Mul),
                    WasmInstruction::I64GtS => f.instruction(&Instruction::I64GtS),
                    WasmInstruction::I64LtS => f.instruction(&Instruction::I64LtS),
                    WasmInstruction::I64Eqz => f.instruction(&Instruction::I64Eqz),
                    WasmInstruction::I64Eq => f.instruction(&Instruction::I64Eq),
                    WasmInstruction::F64Add => f.instruction(&Instruction::F64Add),
                    WasmInstruction::F64Sub => f.instruction(&Instruction::F64Sub),
                    WasmInstruction::F64Mul => f.instruction(&Instruction::F64Mul),
                    WasmInstruction::LocalGet(i) => {
                        f.instruction(&Instruction::LocalGet(*i))
                    }
                    WasmInstruction::LocalSet(i) => {
                        f.instruction(&Instruction::LocalSet(*i))
                    }
                    WasmInstruction::LocalTee(i) => {
                        f.instruction(&Instruction::LocalTee(*i))
                    }
                    WasmInstruction::Call(i) => f.instruction(&Instruction::Call(*i)),
                    WasmInstruction::If(bt) => f.instruction(&Instruction::If(*bt)),
                    WasmInstruction::Else => f.instruction(&Instruction::Else),
                    WasmInstruction::End => f.instruction(&Instruction::End),
                    WasmInstruction::Drop => f.instruction(&Instruction::Drop),
                    WasmInstruction::Return => f.instruction(&Instruction::Return),
                    WasmInstruction::I32Const(n) => {
                        f.instruction(&Instruction::I32Const(*n))
                    }
                    WasmInstruction::I32Store(align, offset) => {
                        f.instruction(&Instruction::I32Store(MemArg {
                            offset: *offset as u64,
                            align: *align,
                            memory_index: 0,
                        }))
                    }
                    WasmInstruction::I32Store16(align, offset) => {
                        f.instruction(&Instruction::I32Store16(MemArg {
                            offset: *offset as u64,
                            align: *align,
                            memory_index: 0,
                        }))
                    }
                };
            }
            f.instruction(&Instruction::End);
            code.function(&f);
        }
        module.section(&code);

        // Data section: string literals
        if !self.strings.is_empty() {
            let mut data = DataSection::new();
            for (s, offset) in &self.strings {
                data.active(
                    0,
                    &ConstExpr::i32_const(*offset as i32),
                    s.as_bytes().iter().copied(),
                );
            }
            module.section(&data);
        }

        module.finish()
    }
}

struct FnCtx<'a> {
    locals: HashMap<String, u32>,
    local_count: u32,
    instructions: Vec<WasmInstruction>,
    compiler: &'a mut Compiler,
}

impl<'a> FnCtx<'a> {
    fn alloc_local(&mut self) -> u32 {
        let idx = self.local_count;
        self.local_count += 1;
        idx
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match &expr.kind {
            ExprKind::Int(n) => {
                self.instructions.push(WasmInstruction::I64Const(*n));
                Ok(())
            }
            ExprKind::Float(n) => {
                self.instructions.push(WasmInstruction::F64Const(*n));
                Ok(())
            }
            ExprKind::Bool(b) => {
                self.instructions
                    .push(WasmInstruction::I64Const(if *b { 1 } else { 0 }));
                Ok(())
            }
            ExprKind::Str(s) => {
                // Strings compile to (ptr, len) pair — for now just push the ptr as i64
                let (offset, _len) = self.compiler.intern_string(s);
                self.instructions
                    .push(WasmInstruction::I64Const(offset as i64));
                Ok(())
            }
            ExprKind::Symbol(name) => {
                if let Some(&idx) = self.locals.get(name) {
                    self.instructions.push(WasmInstruction::LocalGet(idx));
                    Ok(())
                } else {
                    Err(format!("codegen: unbound symbol '{name}'"))
                }
            }
            ExprKind::List(items) if items.is_empty() => {
                self.instructions.push(WasmInstruction::I64Const(0));
                Ok(())
            }
            ExprKind::List(items) => self.compile_call(items),
            _ => Err(format!("codegen: unsupported expression: {:?}", expr.kind)),
        }
    }

    fn compile_call(&mut self, items: &[Expr]) -> Result<(), String> {
        if items.is_empty() {
            return Ok(());
        }
        let head = &items[0];
        if let ExprKind::Symbol(s) = &head.kind {
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
                    return Ok(());
                }
                "<" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64LtS);
                    return Ok(());
                }
                "=" => {
                    self.compile_expr(&items[1])?;
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::I64Eq);
                    return Ok(());
                }
                "if" => {
                    self.compile_expr(&items[1])?; // condition
                    // Convert i64 to i32 for if
                    self.instructions.push(WasmInstruction::I64Eqz);
                    self.instructions.push(WasmInstruction::I64Eqz);
                    // Wrap to i32... actually WASM if needs i32
                    // Let's use a simpler approach: compare with 0
                    // Actually, i64.eqz returns i32, so:
                    // We need: if (cond != 0) ...
                    // i64.eqz gives 1 if cond==0, so we need the inverse
                    // Let's redo: compile cond, then i64.eqz (gives i32),
                    // then if block for the false case...
                    // Actually simpler: compile cond, use i32.wrap_i64, then if
                    // For now, since we push i64 bools as 0/1, let's just
                    // remove the eqz and use a different approach
                    self.instructions.pop(); // remove second eqz
                    self.instructions.pop(); // remove first eqz
                    // Instead: the condition is an i64, we need i32
                    // i64.eqz gives i32(1) if zero, i32(0) if nonzero
                    // We want: if nonzero -> then branch
                    // So: i64.eqz -> if 0 (false case) else (true case) end
                    // OR: use i64.eqz, then if -> else branch, else -> then branch
                    // Simplest: i64 const 0, i64.ne -> i32
                    self.instructions.push(WasmInstruction::I64Const(0));
                    // i64.ne not in our enum, use eqz + eqz pattern differently
                    // Actually let's just test truthiness with i64.eqz (returns i32)
                    // if i64.eqz == 0 then truthy, so we need to negate
                    self.instructions.pop(); // remove the i64.const 0
                    self.instructions.push(WasmInstruction::I64Eqz);
                    // Now we have i32: 1 if zero, 0 if nonzero
                    // Use If for the "zero" (false) case, Else for "nonzero" (true) case
                    self.instructions
                        .push(WasmInstruction::If(BlockType::Result(ValType::I64)));
                    // False branch (condition was falsy)
                    if items.len() > 3 {
                        self.compile_expr(&items[3])?;
                    } else {
                        self.instructions.push(WasmInstruction::I64Const(0));
                    }
                    self.instructions.push(WasmInstruction::Else);
                    // True branch (condition was truthy)
                    self.compile_expr(&items[2])?;
                    self.instructions.push(WasmInstruction::End);
                    return Ok(());
                }
                "let" => {
                    // [let name val] or [let mut name val]
                    let (name_idx, val_idx) =
                        if matches!(&items[1].kind, ExprKind::Symbol(s) if s == "mut") {
                            (3, 3) // mut not really handled in codegen
                        } else {
                            (1, 2)
                        };
                    let name = match &items[name_idx].kind {
                        ExprKind::Symbol(s) => s.clone(),
                        _ => return Err("let binding must be a symbol".to_string()),
                    };
                    self.compile_expr(&items[val_idx])?;
                    let local = self.alloc_local();
                    self.locals.insert(name, local);
                    self.instructions.push(WasmInstruction::LocalSet(local));
                    // Let returns the value — push it back
                    self.instructions.push(WasmInstruction::LocalGet(local));
                    return Ok(());
                }
                "do" => {
                    for (i, item) in items[1..].iter().enumerate() {
                        self.compile_expr(item)?;
                        // Drop intermediate values, keep last
                        if i < items.len() - 2 {
                            self.instructions.push(WasmInstruction::Drop);
                        }
                    }
                    return Ok(());
                }
                "println" => {
                    // For string literals: use fd_write via WASI
                    if let Some(arg) = items.get(1) {
                        if let ExprKind::Str(s) = &arg.kind {
                            let msg = format!("{s}\n");
                            let (offset, len) = self.compiler.intern_string(&msg);
                            // Set up iov at memory offset 0:
                            // iov[0].buf = offset (i32 at mem[0])
                            // iov[0].buf_len = len (i32 at mem[4])
                            self.instructions.push(WasmInstruction::I32Const(0));
                            self.instructions
                                .push(WasmInstruction::I32Const(offset as i32));
                            self.instructions.push(WasmInstruction::I32Store(2, 0));
                            self.instructions.push(WasmInstruction::I32Const(4));
                            self.instructions
                                .push(WasmInstruction::I32Const(len as i32));
                            self.instructions.push(WasmInstruction::I32Store(2, 0));
                            // Call fd_write(fd=1, iovs=0, iovs_len=1, nwritten=8)
                            self.instructions.push(WasmInstruction::I32Const(1)); // stdout
                            self.instructions.push(WasmInstruction::I32Const(0)); // iovs ptr
                            self.instructions.push(WasmInstruction::I32Const(1)); // iovs count
                            self.instructions.push(WasmInstruction::I32Const(8)); // nwritten ptr
                            self.instructions.push(WasmInstruction::Call(0)); // fd_write
                            self.instructions.push(WasmInstruction::Drop); // drop return val
                            // Push unit (i64 0)
                            self.instructions.push(WasmInstruction::I64Const(0));
                            return Ok(());
                        } else {
                            // For non-string args, just compile and drop (can't print numbers yet)
                            self.compile_expr(arg)?;
                            self.instructions.push(WasmInstruction::Drop);
                            self.instructions.push(WasmInstruction::I64Const(0));
                            return Ok(());
                        }
                    }
                    self.instructions.push(WasmInstruction::I64Const(0));
                    return Ok(());
                }
                "match" => {
                    // Basic match on integers: compile as chain of if/else
                    if items.len() < 2 {
                        return Err("match requires a value".to_string());
                    }
                    self.compile_expr(&items[1])?;
                    let scrutinee = self.alloc_local();
                    self.instructions
                        .push(WasmInstruction::LocalSet(scrutinee));

                    let arms = &items[2..];
                    self.compile_match_arms(scrutinee, arms)?;
                    return Ok(());
                }
                name => {
                    // User-defined function call
                    if let Some(fn_def) = self.compiler.fn_map.get(name).cloned() {
                        for arg in &items[1..] {
                            self.compile_expr(arg)?;
                        }
                        self.instructions
                            .push(WasmInstruction::Call(fn_def.func_idx));
                        return Ok(());
                    }
                    return Err(format!("codegen: unknown function '{name}'"));
                }
            }
        }
        Err("codegen: unsupported call form".to_string())
    }

    fn compile_match_arms(
        &mut self,
        scrutinee: u32,
        arms: &[Expr],
    ) -> Result<(), String> {
        let mut i = 0;
        let mut first = true;
        while i < arms.len() {
            let pattern = &arms[i];
            // Look for => separator
            if i + 1 < arms.len() {
                if let ExprKind::Symbol(s) = &arms[i + 1].kind {
                    if s == "=>" && i + 2 < arms.len() {
                        match &pattern.kind {
                            ExprKind::Symbol(s) if s == "_" => {
                                // Wildcard — always matches, just compile body
                                if !first {
                                    self.instructions.push(WasmInstruction::Else);
                                }
                                self.compile_expr(&arms[i + 2])?;
                                // Close all open if blocks
                                if !first {
                                    self.instructions.push(WasmInstruction::End);
                                }
                                return Ok(());
                            }
                            ExprKind::Int(n) => {
                                // Integer pattern
                                self.instructions
                                    .push(WasmInstruction::LocalGet(scrutinee));
                                self.instructions
                                    .push(WasmInstruction::I64Const(*n));
                                self.instructions.push(WasmInstruction::I64Eq);
                                self.instructions.push(WasmInstruction::If(
                                    BlockType::Result(ValType::I64),
                                ));
                                self.compile_expr(&arms[i + 2])?;
                                first = false;
                                i += 3;
                                continue;
                            }
                            ExprKind::Symbol(name) if name != "=>" => {
                                // Variable binding — matches anything
                                if !first {
                                    self.instructions.push(WasmInstruction::Else);
                                }
                                // Bind the variable
                                let local = self.alloc_local();
                                self.locals.insert(name.clone(), local);
                                self.instructions
                                    .push(WasmInstruction::LocalGet(scrutinee));
                                self.instructions
                                    .push(WasmInstruction::LocalSet(local));
                                self.compile_expr(&arms[i + 2])?;
                                if !first {
                                    self.instructions.push(WasmInstruction::End);
                                }
                                return Ok(());
                            }
                            _ => {}
                        }
                        i += 3;
                        continue;
                    }
                }
            }
            i += 1;
        }

        // Default: push 0
        if !first {
            self.instructions.push(WasmInstruction::Else);
            self.instructions.push(WasmInstruction::I64Const(0));
            self.instructions.push(WasmInstruction::End);
        } else {
            self.instructions.push(WasmInstruction::I64Const(0));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn compile_hello_world() {
        let src = r#"[defn main [] [println "hello, world!"]]"#;
        let exprs = parse(src).unwrap();
        let wasm = compile(&exprs).unwrap();
        // Verify it starts with the WASM magic number
        assert_eq!(&wasm[0..4], b"\0asm");
    }

    #[test]
    fn compile_arithmetic() {
        let src = r#"[defn main [] [+ 1 2]]"#;
        let exprs = parse(src).unwrap();
        let wasm = compile(&exprs).unwrap();
        assert_eq!(&wasm[0..4], b"\0asm");
    }

    #[test]
    fn compile_fib() {
        let src = r#"
            [defn fib [n]
              [match n
                0 => 0
                1 => 1
                n => [+ [fib [- n 1]] [fib [- n 2]]]]]
            [defn main [] [fib 10]]
        "#;
        let exprs = parse(src).unwrap();
        let wasm = compile(&exprs).unwrap();
        assert_eq!(&wasm[0..4], b"\0asm");
    }
}
