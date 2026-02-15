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
    Build { file: PathBuf },
    /// Run a Loon file (interpreter)
    Run { file: PathBuf },
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
        Command::Run { ref file } => run_file(file),
        Command::Check { ref file } => check_file(file),
        Command::Build { ref file } => build_file(file),
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
    match loon_lang::parser::parse(&source) {
        Ok(exprs) => match loon_lang::interp::eval_program(&exprs) {
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
            let mut checker = loon_lang::check::Checker::new();
            let errors = checker.check_program(&exprs);
            if errors.is_empty() {
                println!("{}", "OK — no type errors".green().bold());
            } else {
                for err in &errors {
                    eprintln!("{}: {err}", "type error".red().bold());
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

fn build_file(path: &PathBuf) {
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
                let out_dir = path.parent().unwrap_or(std::path::Path::new(".")).join("target");
                let _ = std::fs::create_dir_all(&out_dir);
                let stem = path.file_stem().unwrap_or_default().to_string_lossy();
                let out_path = out_dir.join(format!("{stem}.wasm"));
                std::fs::write(&out_path, &wasm).unwrap_or_else(|e| {
                    eprintln!("{} writing {}: {e}", "error".red().bold(), out_path.display());
                    std::process::exit(1);
                });
                println!(
                    "  {} {} ({} bytes)",
                    "Compiled".green().bold(),
                    out_path.display(),
                    wasm.len()
                );
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
