mod repl;

use clap::{Parser, Subcommand};
use owo_colors::OwoColorize;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "loon", about = "The Loon programming language", version)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Create a new Loon project
    New { name: String },
    /// Build a Loon file to WASM
    Build {
        file: PathBuf,
        /// Optimize the output
        #[arg(long)]
        release: bool,
    },
    /// Run a Loon file (interpreter)
    Run {
        file: PathBuf,
        /// Run via WASM compilation + wasmtime instead of interpreter
        #[arg(long)]
        wasm: bool,
    },
    /// Start the REPL
    Repl,
    /// Run tests in a file
    Test { file: PathBuf },
    /// Type-check without building
    Check { file: PathBuf },
    /// Explain an error code
    Explain { code: String },
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Command::Run { ref file, wasm } => {
            if wasm {
                run_file_wasm(file);
            } else {
                run_file(file);
            }
        }
        Command::Check { ref file } => check_file(file),
        Command::Build { ref file, release } => build_file(file, release),
        Command::Repl => repl::run_repl(),
        Command::New { ref name } => new_project(name),
        Command::Test { ref file } => test_file(file),
        Command::Explain { ref code } => explain_error(code),
    }
}

fn run_file(path: &PathBuf) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{} reading {}: {e}", "error".red().bold(), path.display());
            std::process::exit(1);
        }
    };

    let filename = path.to_string_lossy().to_string();
    let base_dir = path.parent().unwrap_or(std::path::Path::new("."));
    match loon_lang::parser::parse(&source) {
        Ok(exprs) => match loon_lang::interp::eval_program_with_base_dir(&exprs, Some(base_dir)) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("{}: {e}", "error".red().bold());
                std::process::exit(1);
            }
        },
        Err(e) => {
            loon_lang::errors::report_error(&filename, &source, &e.message, e.span);
            std::process::exit(1);
        }
    }
}

fn run_file_wasm(path: &PathBuf) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{} reading {}: {e}", "error".red().bold(), path.display());
            std::process::exit(1);
        }
    };

    let filename = path.to_string_lossy().to_string();
    let exprs = match loon_lang::parser::parse(&source) {
        Ok(exprs) => exprs,
        Err(e) => {
            loon_lang::errors::report_error(&filename, &source, &e.message, e.span);
            std::process::exit(1);
        }
    };

    let wasm_bytes = match loon_lang::codegen::compile(&exprs) {
        Ok(w) => w,
        Err(e) => {
            eprintln!("{}: {e}", "codegen error".red().bold());
            std::process::exit(1);
        }
    };

    let engine = wasmtime::Engine::default();
    let module = match wasmtime::Module::new(&engine, &wasm_bytes) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("{}: {e}", "wasmtime error".red().bold());
            std::process::exit(1);
        }
    };

    let mut linker = wasmtime::Linker::new(&engine);
    wasmtime_wasi::preview1::add_to_linker_sync(&mut linker, |s: &mut WasiCtx| s).unwrap_or_else(
        |e| {
            eprintln!("{}: failed to link WASI: {e}", "wasmtime error".red().bold());
            std::process::exit(1);
        },
    );

    let wasi = wasmtime_wasi::WasiCtxBuilder::new()
        .inherit_stdio()
        .inherit_env()
        .build_p1();

    let mut store = wasmtime::Store::new(&engine, wasi);

    let instance = match linker.instantiate(&mut store, &module) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("{}: {e}", "wasmtime error".red().bold());
            std::process::exit(1);
        }
    };

    // Try _start (WASI command), then fall back to main
    if let Ok(start) = instance.get_typed_func::<(), ()>(&mut store, "_start") {
        if let Err(e) = start.call(&mut store, ()) {
            eprintln!("{}: {e}", "runtime error".red().bold());
            std::process::exit(1);
        }
    } else if let Ok(main_fn) = instance.get_typed_func::<(), ()>(&mut store, "main") {
        if let Err(e) = main_fn.call(&mut store, ()) {
            eprintln!("{}: {e}", "runtime error".red().bold());
            std::process::exit(1);
        }
    } else {
        eprintln!(
            "{}: no _start or main export found",
            "error".red().bold()
        );
        std::process::exit(1);
    }
}

type WasiCtx = wasmtime_wasi::preview1::WasiP1Ctx;

fn check_file(path: &PathBuf) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{} reading {}: {e}", "error".red().bold(), path.display());
            std::process::exit(1);
        }
    };

    let filename = path.to_string_lossy().to_string();
    match loon_lang::parser::parse(&source) {
        Ok(exprs) => {
            let base_dir = path.parent().unwrap_or(std::path::Path::new("."));
            let mut checker = loon_lang::check::Checker::with_base_dir(base_dir);
            let errors = checker.check_program(&exprs);
            if errors.is_empty() {
                println!("{}", "OK — no type errors".green().bold());
            } else {
                for err in &errors {
                    loon_lang::errors::report_type_error(&filename, &source, err);
                }
                std::process::exit(1);
            }
        }
        Err(e) => {
            loon_lang::errors::report_error(&filename, &source, &e.message, e.span);
            std::process::exit(1);
        }
    }
}

fn build_file(path: &PathBuf, release: bool) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{} reading {}: {e}", "error".red().bold(), path.display());
            std::process::exit(1);
        }
    };

    let filename = path.to_string_lossy().to_string();
    match loon_lang::parser::parse(&source) {
        Ok(exprs) => match loon_lang::codegen::compile(&exprs) {
            Ok(wasm) => {
                let original_size = wasm.len();
                let output = if release {
                    optimize_wasm(&wasm)
                } else {
                    wasm
                };

                let out_dir = path.parent().unwrap_or(std::path::Path::new(".")).join("target");
                let _ = std::fs::create_dir_all(&out_dir);
                let stem = path.file_stem().unwrap_or_default().to_string_lossy();
                let out_path = out_dir.join(format!("{stem}.wasm"));
                std::fs::write(&out_path, &output).unwrap_or_else(|e| {
                    eprintln!("{} writing {}: {e}", "error".red().bold(), out_path.display());
                    std::process::exit(1);
                });

                if release {
                    let saved = original_size.saturating_sub(output.len());
                    let pct = if original_size > 0 {
                        (saved as f64 / original_size as f64) * 100.0
                    } else {
                        0.0
                    };
                    println!(
                        "  {} {} ({} bytes, optimized from {} bytes, -{:.1}%)",
                        "Compiled".green().bold(),
                        out_path.display(),
                        output.len(),
                        original_size,
                        pct,
                    );
                } else {
                    println!(
                        "  {} {} ({} bytes)",
                        "Compiled".green().bold(),
                        out_path.display(),
                        output.len()
                    );
                }
            }
            Err(e) => {
                eprintln!("{}: {e}", "codegen error".red().bold());
                std::process::exit(1);
            }
        },
        Err(e) => {
            loon_lang::errors::report_error(&filename, &source, &e.message, e.span);
            std::process::exit(1);
        }
    }
}

/// Basic WASM optimization: strip the custom name section.
/// A more thorough pass would do dead-code elimination by walking exports,
/// but parsing the full WASM function/code sections is non-trivial without
/// a dedicated library. Stripping name sections is simple and effective.
fn optimize_wasm(wasm: &[u8]) -> Vec<u8> {
    // WASM binary format: 8-byte header (magic + version), then sections.
    // Each section: 1 byte id, LEB128 size, payload.
    // Custom sections have id 0. The name section is a custom section
    // whose payload starts with the name "name".
    if wasm.len() < 8 {
        return wasm.to_vec();
    }

    let mut out = Vec::with_capacity(wasm.len());
    out.extend_from_slice(&wasm[..8]); // magic + version

    let mut pos = 8;
    while pos < wasm.len() {
        if pos >= wasm.len() {
            break;
        }
        let section_id = wasm[pos];
        pos += 1;

        // Read LEB128 size
        let (size, bytes_read) = read_leb128(&wasm[pos..]);
        let size_start = pos;
        pos += bytes_read;

        let section_end = pos + size as usize;
        if section_end > wasm.len() {
            // Malformed, just copy the rest
            out.push(section_id);
            out.extend_from_slice(&wasm[size_start..]);
            break;
        }

        if section_id == 0 {
            // Custom section - check if it's the "name" section
            let payload = &wasm[pos..section_end];
            if is_name_section(payload) {
                // Skip this section entirely
                pos = section_end;
                continue;
            }
        }

        // Copy this section as-is
        out.push(section_id);
        out.extend_from_slice(&wasm[size_start..section_end]);
        pos = section_end;
    }

    out
}

fn read_leb128(bytes: &[u8]) -> (u32, usize) {
    let mut result: u32 = 0;
    let mut shift = 0;
    for (i, &byte) in bytes.iter().enumerate() {
        result |= ((byte & 0x7f) as u32) << shift;
        if byte & 0x80 == 0 {
            return (result, i + 1);
        }
        shift += 7;
        if shift >= 35 {
            return (result, i + 1);
        }
    }
    (result, bytes.len())
}

fn is_name_section(payload: &[u8]) -> bool {
    // The custom section payload starts with a name (LEB128 length + UTF-8 bytes).
    if payload.is_empty() {
        return false;
    }
    let (name_len, bytes_read) = read_leb128(payload);
    let name_start = bytes_read;
    let name_end = name_start + name_len as usize;
    if name_end > payload.len() {
        return false;
    }
    &payload[name_start..name_end] == b"name"
}

fn new_project(name: &str) {
    let dir = std::path::Path::new(name);
    let src_dir = dir.join("src");
    std::fs::create_dir_all(&src_dir).unwrap_or_else(|e| {
        eprintln!("{} creating directory: {e}", "error".red().bold());
        std::process::exit(1);
    });

    let toml_content = format!(
        r#"[package]
name = "{name}"
version = "0.1.0"
"#
    );
    std::fs::write(dir.join("loon.toml"), toml_content).unwrap();

    let main_content = r#"[defn main []
  [println "hello, world!"]]
"#;
    std::fs::write(src_dir.join("main.loon"), main_content).unwrap();

    println!("  {} {name}/", "Created".green().bold());
    println!("  {} {name}/src/main.loon", "Created".green().bold());
    println!("  {} {name}/loon.toml", "Created".green().bold());
    println!("  {} cd {name} && loon run src/main.loon", "->".dimmed());
}

fn test_file(path: &PathBuf) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{} reading {}: {e}", "error".red().bold(), path.display());
            std::process::exit(1);
        }
    };

    let filename = path.to_string_lossy().to_string();
    match loon_lang::parser::parse(&source) {
        Ok(exprs) => {
            // First eval all definitions
            let mut env = loon_lang::interp::Env::new();
            loon_lang::interp::register_builtins_pub(&mut env);
            loon_lang::interp::sync_global_env_pub(&env);

            for expr in &exprs {
                if let Err(e) = loon_lang::interp::eval(expr, &mut env) {
                    eprintln!("{}: {e}", "error".red().bold());
                    std::process::exit(1);
                }
            }

            // Find and run test functions (names starting with "test-")
            let mut passed = 0;
            let mut failed = 0;
            let test_fns: Vec<String> = {
                let mut names = Vec::new();
                for expr in &exprs {
                    if let loon_lang::ast::ExprKind::List(items) = &expr.kind {
                        if items.len() >= 3 {
                            if let loon_lang::ast::ExprKind::Symbol(s) = &items[0].kind {
                                if s == "test" {
                                    if let loon_lang::ast::ExprKind::Symbol(s2) = &items[1].kind {
                                        if s2 == "defn" {
                                            if let loon_lang::ast::ExprKind::Symbol(name) =
                                                &items[2].kind
                                            {
                                                names.push(name.clone());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                names
            };

            if test_fns.is_empty() {
                println!("No tests found in {filename}");
                return;
            }

            println!(
                "  Running {} tests from {}...",
                test_fns.len().bold(),
                filename.dimmed()
            );

            for name in &test_fns {
                let start = std::time::Instant::now();
                // Call the test function with no args
                if let Some(loon_lang::interp::Value::Fn(lf)) = env.get(name) {
                    let mut test_env = env.clone();
                    match loon_lang::interp::eval(
                        &loon_lang::ast::Expr::new(
                            loon_lang::ast::ExprKind::List(vec![loon_lang::ast::Expr::new(
                                loon_lang::ast::ExprKind::Symbol(name.clone()),
                                loon_lang::syntax::Span::new(0, 0),
                            )]),
                            loon_lang::syntax::Span::new(0, 0),
                        ),
                        &mut test_env,
                    ) {
                        Ok(_) => {
                            let elapsed = start.elapsed();
                            println!(
                                "  {} {name} ({:.1}ms)",
                                "pass".green(),
                                elapsed.as_secs_f64() * 1000.0
                            );
                            passed += 1;
                        }
                        Err(e) => {
                            let elapsed = start.elapsed();
                            println!(
                                "  {} {name} ({:.1}ms)\n    {}",
                                "FAIL".red().bold(),
                                elapsed.as_secs_f64() * 1000.0,
                                e
                            );
                            failed += 1;
                        }
                    }
                    let _ = lf;
                }
            }

            println!();
            if failed == 0 {
                println!(
                    "  {} {} passed",
                    "✓".green().bold(),
                    passed
                );
            } else {
                println!(
                    "  {} passed, {} failed",
                    passed.to_string().green(),
                    failed.to_string().red().bold()
                );
            }
            if failed > 0 {
                std::process::exit(1);
            }
        }
        Err(e) => {
            loon_lang::errors::report_error(&filename, &source, &e.message, e.span);
            std::process::exit(1);
        }
    }
}

fn explain_error(code: &str) {
    let explanation = match code {
        "E0001" => {
            "E0001: Use after move\n\n\
             A value was used after it was moved to another binding.\n\
             In Loon, when a non-Copy value is assigned or passed to a\n\
             function, ownership transfers and the original binding\n\
             becomes invalid.\n\n\
             Example:\n\
               [let xs [vec 1 2 3]]\n\
               [let ys xs]          ;; xs moved to ys\n\
               [println xs]         ;; error: xs was moved\n\n\
             Fix: clone the value if you need both bindings.\n\
               [let ys [clone xs]]"
        }
        "E0002" => {
            "E0002: Mutating an immutable binding\n\n\
             Attempted to mutate a binding that was not declared with `mut`.\n\n\
             Example:\n\
               [let xs [vec 1 2 3]]\n\
               [push! xs 4]         ;; error: xs is not mutable\n\n\
             Fix: declare the binding as mutable.\n\
               [let mut xs [vec 1 2 3]]\n\
               [push! xs 4]         ;; ok"
        }
        "E0003" => {
            "E0003: Double mutable borrow\n\n\
             A value was mutably borrowed while another mutable borrow\n\
             was still active. Loon enforces exclusive mutable access\n\
             to prevent data races.\n\n\
             Example:\n\
               [let mut xs [vec 1 2 3]]\n\
               [push! xs [len xs]]  ;; error: xs borrowed mutably and immutably\n\n\
             Fix: compute the value before mutating.\n\
               [let n [len xs]]\n\
               [push! xs n]"
        }
        "E0010" => {
            "E0010: Type mismatch\n\n\
             The inferred type of an expression does not match the\n\
             expected type from context.\n\n\
             Example:\n\
               [+ 1 \"hello\"]       ;; error: expected Int, got Str\n\n\
             Fix: ensure operand types match."
        }
        "E0020" => {
            "E0020: Unhandled effect\n\n\
             A function performs an effect that is not handled by\n\
             any enclosing `handle` block.\n\n\
             Example:\n\
               [IO.println \"hello\"]  ;; error if no IO handler\n\n\
             Fix: wrap in a handle block or add the effect to the\n\
             function's effect annotation.\n\
               [handle [IO.println \"hello\"]\n\
                 IO => [fn [op args resume]\n\
                         [resume [println [nth args 0]]]]]"
        }
        _ => {
            eprintln!("Unknown error code: {code}");
            eprintln!("Known codes: E0001, E0002, E0003, E0010, E0020");
            std::process::exit(1);
        }
    };
    println!("{explanation}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wasm_magic_number() {
        // A minimal Loon program that compiles to WASM should produce valid WASM bytes
        let source = r#"[defn main [] 42]"#;
        let exprs = loon_lang::parser::parse(source).expect("parse failed");
        let wasm = loon_lang::codegen::compile(&exprs).expect("compile failed");
        assert!(wasm.len() >= 8, "WASM output too small: {} bytes", wasm.len());
        assert_eq!(&wasm[..4], b"\0asm", "WASM magic number missing");
        // Version 1
        assert_eq!(&wasm[4..8], &[1, 0, 0, 0], "unexpected WASM version");
    }

    #[test]
    fn optimize_wasm_preserves_magic() {
        let source = r#"[defn main [] 42]"#;
        let exprs = loon_lang::parser::parse(source).expect("parse failed");
        let wasm = loon_lang::codegen::compile(&exprs).expect("compile failed");
        let optimized = optimize_wasm(&wasm);
        assert!(optimized.len() >= 8);
        assert_eq!(&optimized[..4], b"\0asm");
        assert!(optimized.len() <= wasm.len(), "optimized should not be larger");
    }

    #[test]
    fn optimize_wasm_strips_name_section() {
        // Build a WASM binary with a custom "name" section appended
        let source = r#"[defn main [] 42]"#;
        let exprs = loon_lang::parser::parse(source).expect("parse failed");
        let mut wasm = loon_lang::codegen::compile(&exprs).expect("compile failed");

        // Append a custom name section: id=0, payload = [4, 'n','a','m','e', ...some data...]
        let name_payload = b"\x04name\x00\x01\x00"; // minimal name section content
        let section_size = name_payload.len() as u8;
        wasm.push(0); // custom section id
        wasm.push(section_size); // LEB128 size (fits in one byte)
        wasm.extend_from_slice(name_payload);

        let original_len = wasm.len();
        let optimized = optimize_wasm(&wasm);
        assert!(
            optimized.len() < original_len,
            "expected name section to be stripped: {} vs {}",
            optimized.len(),
            original_len
        );
    }

    #[test]
    fn leb128_decoding() {
        assert_eq!(read_leb128(&[0x00]), (0, 1));
        assert_eq!(read_leb128(&[0x7f]), (127, 1));
        assert_eq!(read_leb128(&[0x80, 0x01]), (128, 2));
        assert_eq!(read_leb128(&[0xe5, 0x8e, 0x26]), (624485, 3));
    }
}
