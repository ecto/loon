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
    /// Start the LSP server
    Lsp,
    /// Format Loon source files
    Fmt {
        /// Files to format (recursively finds .loon files in directories)
        files: Vec<PathBuf>,
        /// Check formatting without modifying files (exit 1 if unformatted)
        #[arg(long)]
        check: bool,
    },
    /// Pre-render a route to static HTML (for SSR/SSG)
    Render {
        file: PathBuf,
        /// The route path to render (e.g. "/guide/basics")
        #[arg(long)]
        route: String,
    },
    /// Add a dependency to pkg.loon
    Add {
        /// Package source (e.g. github.com/cam/json)
        source: String,
        /// Version constraint (e.g. ^1.2)
        #[arg(short, long)]
        version: Option<String>,
        /// Effects to grant (e.g. Net,IO)
        #[arg(short, long)]
        grant: Option<String>,
    },
    /// Remove a dependency from pkg.loon
    Remove {
        /// Package source to remove
        source: String,
    },
    /// Update dependencies
    Update {
        /// Specific package to update (all if omitted)
        source: Option<String>,
    },
    /// Show why a dependency is included
    Why {
        /// Package source to trace
        source: String,
    },
    /// Audit dependencies for security
    Audit {
        /// Show capability grants for all deps
        #[arg(long)]
        capabilities: bool,
    },
    /// Manage the package cache
    Cache {
        #[command(subcommand)]
        action: CacheAction,
    },
    /// Search for packages
    Search {
        /// Search query
        query: String,
    },
    /// Initialize pkg.loon in the current directory
    Init,
}

#[derive(Subcommand)]
enum CacheAction {
    /// Remove unused cached packages
    Clean,
    /// Pre-download all dependencies for offline work
    Warm,
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
        Command::Lsp => start_lsp(),
        Command::Fmt {
            ref files, check, ..
        } => fmt_files(files, check),
        Command::Render {
            ref file,
            ref route,
        } => render_route(file, route),
        Command::Add {
            ref source,
            ref version,
            ref grant,
        } => pkg_add(source, version.as_deref(), grant.as_deref()),
        Command::Remove { ref source } => pkg_remove(source),
        Command::Update { ref source } => pkg_update(source.as_deref()),
        Command::Why { ref source } => pkg_why(source),
        Command::Audit { capabilities } => pkg_audit(capabilities),
        Command::Cache { ref action } => match action {
            CacheAction::Clean => pkg_cache_clean(),
            CacheAction::Warm => pkg_cache_warm(),
        },
        Command::Search { ref query } => pkg_search(query),
        Command::Init => pkg_init(),
    }
}

fn start_lsp() {
    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(loon_lsp::run_stdio());
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
                    loon_lang::errors::report_diagnostic(&filename, &source, err);
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

    let pkg_content = format!(
        r#"{{
  :name "{name}"
  :version "0.1.0"

  :deps {{}}
}}
"#
    );
    std::fs::write(dir.join("pkg.loon"), pkg_content).unwrap();

    let main_content = r#"[fn main []
  [println "hello, world!"]]
"#;
    std::fs::write(src_dir.join("main.loon"), main_content).unwrap();

    println!("  {} {name}/", "Created".green().bold());
    println!("  {} {name}/src/main.loon", "Created".green().bold());
    println!("  {} {name}/pkg.loon", "Created".green().bold());
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
                                    // [test name [params] body...] or [test fn name [params] body...]
                                    if let loon_lang::ast::ExprKind::Symbol(s2) = &items[1].kind {
                                        if s2 == "fn" {
                                            // [test fn name ...] — name is items[2]
                                            if items.len() >= 4 {
                                                if let loon_lang::ast::ExprKind::Symbol(name) =
                                                    &items[2].kind
                                                {
                                                    names.push(name.clone());
                                                }
                                            }
                                        } else {
                                            // [test name [params] body...] — name is items[1]
                                            names.push(s2.clone());
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

fn fmt_files(files: &[PathBuf], check: bool) {
    let paths = if files.is_empty() {
        collect_loon_files(&PathBuf::from("."))
    } else {
        let mut all = Vec::new();
        for f in files {
            if f.is_dir() {
                all.extend(collect_loon_files(f));
            } else {
                all.push(f.clone());
            }
        }
        all
    };

    if paths.is_empty() {
        println!("No .loon files found");
        return;
    }

    let mut changed_count = 0;
    let mut error_count = 0;

    for path in &paths {
        let source = match std::fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("{} reading {}: {e}", "error".red().bold(), path.display());
                error_count += 1;
                continue;
            }
        };

        let exprs = match loon_lang::parser::parse(&source) {
            Ok(exprs) => exprs,
            Err(e) => {
                eprintln!(
                    "{} parsing {}: {}",
                    "error".red().bold(),
                    path.display(),
                    e.message
                );
                error_count += 1;
                continue;
            }
        };

        let formatted = loon_lang::fmt::format_program(&exprs);

        if formatted != source {
            changed_count += 1;
            if check {
                println!("  {} {}", "would format".yellow().bold(), path.display());
            } else {
                if let Err(e) = std::fs::write(path, &formatted) {
                    eprintln!("{} writing {}: {e}", "error".red().bold(), path.display());
                    error_count += 1;
                    continue;
                }
                println!("  {} {}", "formatted".green().bold(), path.display());
            }
        }
    }

    if check {
        if changed_count > 0 {
            println!(
                "\n{} file(s) would be reformatted",
                changed_count.to_string().yellow().bold()
            );
            std::process::exit(1);
        } else if error_count == 0 {
            println!("{}", "All files already formatted".green().bold());
        }
    } else if changed_count == 0 && error_count == 0 {
        println!("{}", "All files already formatted".green().bold());
    } else if changed_count > 0 {
        println!("\n{} file(s) formatted", changed_count);
    }

    if error_count > 0 {
        std::process::exit(1);
    }
}

fn collect_loon_files(dir: &PathBuf) -> Vec<PathBuf> {
    let mut result = Vec::new();
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return result,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            // Skip hidden directories and target/
            let name = path.file_name().unwrap_or_default().to_string_lossy();
            if !name.starts_with('.') && name != "target" {
                result.extend(collect_loon_files(&path));
            }
        } else if path.extension().is_some_and(|e| e == "loon") {
            result.push(path);
        }
    }
    result.sort();
    result
}

/// Try to parse, type-check, and eval a snippet. Returns true on success.
/// Prints diagnostics or the result value.
fn run_code_live(code: &str) -> bool {
    use loon_lang::check::Checker;
    use loon_lang::check::ownership::OwnershipChecker;

    let exprs = match loon_lang::parser::parse(code) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("    {} {}", "parse error:".red(), e.message);
            return false;
        }
    };

    // Type check
    let mut checker = Checker::new();
    let type_errors = checker.check_program(&exprs);
    if !type_errors.is_empty() {
        for diag in &type_errors {
            eprintln!("    {} {}", format!("[{}]", diag.code).red(), diag.what);
        }
        return false;
    }

    // Ownership check (on macro-expanded program)
    let mut own = OwnershipChecker::new();
    let own_errors = own.check_program(&checker.expanded_program);
    if !own_errors.is_empty() {
        for diag in &own_errors {
            eprintln!("    {} {}", format!("[{}]", diag.code).red(), diag.what);
        }
        return false;
    }

    // Eval
    match loon_lang::interp::eval_program(&exprs) {
        Ok(val) => {
            let s = format!("{val}");
            if s != "()" {
                println!("    => {}", s.green());
            }
            true
        }
        Err(e) => {
            eprintln!("    {} {e}", "runtime error:".red());
            false
        }
    }
}

fn explain_error(code: &str) {
    use loon_lang::errors::codes::ErrorCode;
    use loon_lang::errors::tutorials::{get_tutorial, TutorialStep};
    use rustyline::DefaultEditor;

    // Map old codes to new codes for backwards compatibility
    let error_code = match code {
        "E0001" | "E0300" => Some(ErrorCode::E0300),
        "E0002" | "E0301" => Some(ErrorCode::E0301),
        "E0003" | "E0302" => Some(ErrorCode::E0302),
        "E0010" | "E0200" => Some(ErrorCode::E0200),
        "E0020" | "E0400" => Some(ErrorCode::E0400),
        "E0100" => Some(ErrorCode::E0100),
        "E0101" => Some(ErrorCode::E0101),
        "E0102" => Some(ErrorCode::E0102),
        "E0103" => Some(ErrorCode::E0103),
        "E0201" => Some(ErrorCode::E0201),
        "E0202" => Some(ErrorCode::E0202),
        "E0203" => Some(ErrorCode::E0203),
        "E0204" => Some(ErrorCode::E0204),
        "E0205" => Some(ErrorCode::E0205),
        "E0206" => Some(ErrorCode::E0206),
        "E0207" => Some(ErrorCode::E0207),
        "E0401" => Some(ErrorCode::E0401),
        "E0500" => Some(ErrorCode::E0500),
        "E0501" => Some(ErrorCode::E0501),
        "E0502" => Some(ErrorCode::E0502),
        _ => None,
    };

    let Some(ec) = error_code else {
        eprintln!("Unknown error code: {code}");
        eprintln!("Known codes:");
        eprintln!("  E01xx: parse errors    (E0100-E0103)");
        eprintln!("  E02xx: type errors     (E0200-E0207)");
        eprintln!("  E03xx: ownership errors (E0300-E0302)");
        eprintln!("  E04xx: effect errors   (E0400-E0401)");
        eprintln!("  E05xx: module errors   (E0500-E0502)");
        eprintln!("Legacy codes: E0001 -> E0300, E0002 -> E0301, E0003 -> E0302, E0010 -> E0200, E0020 -> E0400");
        std::process::exit(1);
    };

    let Some(tutorial) = get_tutorial(ec) else {
        println!("{}: {}\n", ec, ec.title());
        println!("Category: {}", ec.category());
        println!("No tutorial available for this error code yet.");
        return;
    };

    let total = tutorial.steps.len();
    let mut idx = 0;
    let mut rl = DefaultEditor::new().expect("failed to create editor");

    println!(
        "\n{} {} — {}\n",
        "Tutorial:".bold(),
        ec.to_string().cyan().bold(),
        tutorial.title.bold()
    );
    println!(
        "  {} steps · press {} to advance, {} to go back, {} to quit\n",
        total,
        "Enter".bold(),
        "b".bold(),
        "q".bold(),
    );

    while idx < total {
        let step = &tutorial.steps[idx];
        println!(
            "  {} {}/{total}",
            "Step".dimmed(),
            (idx + 1),
        );
        println!();

        match step {
            TutorialStep::Text(text) => {
                println!("{text}\n");
            }
            TutorialStep::Demo { code, explanation } => {
                println!("  {}", "Example:".cyan().bold());
                for line in code.lines() {
                    println!("    {}", line.dimmed());
                }
                println!();
                println!("  {}", "Running...".dimmed());
                // Strip trailing comments for execution
                let exec_code: String = code
                    .lines()
                    .map(|l| l.split(";;").next().unwrap_or(l).trim_end())
                    .collect::<Vec<_>>()
                    .join("\n");
                run_code_live(&exec_code);
                println!();
                println!("  {explanation}\n");
            }
            TutorialStep::Fix {
                before,
                after,
                explanation,
            } => {
                println!("  {} (should fail):", "Before".red().bold());
                for line in before.lines() {
                    println!("    {}", line.dimmed());
                }
                println!();
                run_code_live(before);
                println!();

                println!("  {} (should succeed):", "After".green().bold());
                for line in after.lines() {
                    println!("    {}", line.dimmed());
                }
                println!();
                run_code_live(after);
                println!();

                println!("  {explanation}\n");
            }
            TutorialStep::Try {
                prompt,
                hint,
                expected_output: _,
            } => {
                println!("  {}", "Your turn!".yellow().bold());
                println!("  {prompt}");
                println!(
                    "  Type {} for a hint, {} to skip.\n",
                    ":hint".bold(),
                    ":skip".bold(),
                );

                loop {
                    match rl.readline("  try> ") {
                        Ok(line) => {
                            let trimmed = line.trim();
                            if trimmed.is_empty() {
                                continue;
                            }
                            let _ = rl.add_history_entry(trimmed);
                            match trimmed {
                                ":hint" | ":h" => {
                                    println!("  {}: {hint}\n", "Hint".yellow());
                                    continue;
                                }
                                ":skip" | ":s" => {
                                    println!("  {}\n", "Skipped.".dimmed());
                                    break;
                                }
                                ":quit" | ":q" => return,
                                _ => {}
                            }
                            if run_code_live(trimmed) {
                                println!("  {}\n", "Correct!".green().bold());
                                break;
                            }
                            println!("  {}\n", "Try again, or type :hint for help.".dimmed());
                        }
                        Err(_) => return,
                    }
                }
            }
        }

        // Navigation prompt (skip for Try steps which handle their own flow)
        if !matches!(step, TutorialStep::Try { .. }) {
            if idx + 1 >= total {
                println!("  {}", "Tutorial complete!".green().bold());
                return;
            }
            match rl.readline(&format!(
                "  [step {}/{}] press Enter to continue, (b)ack, (q)uit: ",
                idx + 1,
                total
            )) {
                Ok(line) => {
                    let t = line.trim().to_lowercase();
                    if t == "q" || t == "quit" {
                        return;
                    }
                    if (t == "b" || t == "back") && idx > 0 {
                        idx -= 1;
                        continue;
                    }
                }
                Err(_) => return,
            }
        }

        idx += 1;
    }

    println!("  {}", "Tutorial complete!".green().bold());
}

fn render_route(path: &PathBuf, route: &str) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{} reading {}: {e}", "error".red().bold(), path.display());
            std::process::exit(1);
        }
    };

    let filename = path.to_string_lossy().to_string();
    let base_dir = path.parent().unwrap_or(std::path::Path::new("."));

    let exprs = match loon_lang::parser::parse(&source) {
        Ok(exprs) => exprs,
        Err(e) => {
            loon_lang::errors::report_error(&filename, &source, &e.message, e.span);
            std::process::exit(1);
        }
    };

    // Set up the HTML bridge with the target route
    let bridge = loon_lang::interp::html_bridge::HtmlBridge::new(route.to_string());
    bridge.install();

    // Run the program — this will call main() which mounts the app
    match loon_lang::interp::eval_program_with_base_dir(&exprs, Some(base_dir)) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}: {e}", "error".red().bold());
            std::process::exit(1);
        }
    }

    // Serialize the result as JSON
    let html = bridge.to_html();
    let title = bridge.title();

    let html_json = serde_json::json!({
        "html": html,
        "title": title,
    });
    println!("{}", html_json);
}

// --- Package manager commands ---

fn pkg_init() {
    let cwd = std::env::current_dir().unwrap_or_else(|e| {
        eprintln!("{}: {e}", "error".red().bold());
        std::process::exit(1);
    });
    let pkg_path = cwd.join("pkg.loon");
    if pkg_path.exists() {
        eprintln!("{}: pkg.loon already exists", "error".red().bold());
        std::process::exit(1);
    }

    let dir_name = cwd
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| "my-project".to_string());

    let content = format!(
        r#"{{
  :name "{dir_name}"
  :version "0.1.0"

  :deps {{}}
}}
"#
    );
    std::fs::write(&pkg_path, content).unwrap_or_else(|e| {
        eprintln!("{} writing pkg.loon: {e}", "error".red().bold());
        std::process::exit(1);
    });
    println!("  {} pkg.loon", "Created".green().bold());
}

fn pkg_add(source: &str, version: Option<&str>, grant: Option<&str>) {
    let cwd = std::env::current_dir().unwrap_or_else(|e| {
        eprintln!("{}: {e}", "error".red().bold());
        std::process::exit(1);
    });
    let pkg_path = cwd.join("pkg.loon");
    if !pkg_path.exists() {
        eprintln!(
            "{}: no pkg.loon found (run {} first)",
            "error".red().bold(),
            "loon init".bold()
        );
        std::process::exit(1);
    }

    let mut content = std::fs::read_to_string(&pkg_path).unwrap_or_else(|e| {
        eprintln!("{}: {e}", "error".red().bold());
        std::process::exit(1);
    });

    // Build the dep value
    let dep_value = match (version, grant) {
        (Some(v), None) => format!("\"{}\"", v),
        (version, Some(g)) => {
            let effects: Vec<&str> = g.split(',').map(|s| s.trim()).collect();
            let grant_vec = effects
                .iter()
                .map(|e| format!("\"{}\"", e))
                .collect::<Vec<_>>()
                .join(" ");
            let ver = version.unwrap_or("*");
            format!("{{:version \"{}\" :grant #[{}]}}", ver, grant_vec)
        }
        (None, None) => "\"*\"".to_string(),
    };

    // Insert before the closing `}` of :deps
    // Simple approach: find `:deps {` and its matching `}`
    if let Some(deps_start) = content.find(":deps {") {
        let after_deps = &content[deps_start..];
        if let Some(brace_offset) = find_matching_brace(after_deps) {
            let insert_pos = deps_start + brace_offset;
            let entry = format!("\n    \"{}\" {}\n  ", source, dep_value);
            content.insert_str(insert_pos, &entry);
        }
    } else {
        // No :deps section — add one before the final `}`
        if let Some(last_brace) = content.rfind('}') {
            let entry = format!(
                "\n  :deps {{\n    \"{}\" {}\n  }}",
                source, dep_value
            );
            content.insert_str(last_brace, &entry);
        }
    }

    std::fs::write(&pkg_path, content).unwrap_or_else(|e| {
        eprintln!("{}: {e}", "error".red().bold());
        std::process::exit(1);
    });
    println!("  {} {source}", "Added".green().bold());
}

/// Find the position of the closing `}` matching the first `{` in s.
fn find_matching_brace(s: &str) -> Option<usize> {
    let mut depth = 0;
    let mut in_string = false;
    let mut escape = false;
    for (i, ch) in s.char_indices() {
        if escape {
            escape = false;
            continue;
        }
        if ch == '\\' && in_string {
            escape = true;
            continue;
        }
        if ch == '"' {
            in_string = !in_string;
            continue;
        }
        if in_string {
            continue;
        }
        if ch == '{' {
            depth += 1;
        } else if ch == '}' {
            depth -= 1;
            if depth == 0 {
                return Some(i);
            }
        }
    }
    None
}

fn pkg_remove(source: &str) {
    let cwd = std::env::current_dir().unwrap_or_else(|e| {
        eprintln!("{}: {e}", "error".red().bold());
        std::process::exit(1);
    });
    let pkg_path = cwd.join("pkg.loon");
    if !pkg_path.exists() {
        eprintln!("{}: no pkg.loon found", "error".red().bold());
        std::process::exit(1);
    }

    let content = std::fs::read_to_string(&pkg_path).unwrap_or_else(|e| {
        eprintln!("{}: {e}", "error".red().bold());
        std::process::exit(1);
    });

    // Parse, remove the dep, rewrite
    let base_dir = cwd.clone();
    match loon_lang::pkg::Manifest::parse(&content, &base_dir) {
        Ok(manifest) => {
            if !manifest.deps.contains_key(source) {
                eprintln!("{}: '{}' not found in deps", "error".red().bold(), source);
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("{}: {e}", "error".red().bold());
            std::process::exit(1);
        }
    }

    // Remove the line(s) containing the source from the deps section
    // Simple approach: find the dep entry and remove it
    let needle = format!("\"{}\"", source);
    let lines: Vec<&str> = content.lines().collect();
    let mut result = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        if lines[i].contains(&needle) && lines[i].trim().starts_with('"') {
            // Check if value is on the same line or spans multiple lines
            if lines[i].contains('{') && !lines[i].contains('}') {
                // Multi-line map value — skip until closing }
                i += 1;
                while i < lines.len() && !lines[i].contains('}') {
                    i += 1;
                }
                i += 1; // skip the closing }
            } else {
                i += 1; // skip single line
            }
            continue;
        }
        result.push(lines[i]);
        i += 1;
    }

    std::fs::write(&pkg_path, result.join("\n") + "\n").unwrap_or_else(|e| {
        eprintln!("{}: {e}", "error".red().bold());
        std::process::exit(1);
    });
    println!("  {} {source}", "Removed".green().bold());
}

fn pkg_update(source: Option<&str>) {
    match source {
        Some(s) => println!("  {} updating {s} (Phase 3)", "TODO".yellow().bold()),
        None => println!("  {} updating all deps (Phase 3)", "TODO".yellow().bold()),
    }
}

fn pkg_why(source: &str) {
    let cwd = std::env::current_dir().unwrap_or_else(|e| {
        eprintln!("{}: {e}", "error".red().bold());
        std::process::exit(1);
    });

    match loon_lang::pkg::Manifest::load(&cwd) {
        Ok(Some(manifest)) => {
            if manifest.deps.contains_key(source) {
                println!("  {} -> {source} (direct dependency)", manifest.name);
            } else {
                println!("  {source} is not a dependency");
            }
        }
        Ok(None) => {
            eprintln!("{}: no pkg.loon found", "error".red().bold());
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("{}: {e}", "error".red().bold());
            std::process::exit(1);
        }
    }
}

fn pkg_audit(capabilities: bool) {
    let cwd = std::env::current_dir().unwrap_or_else(|e| {
        eprintln!("{}: {e}", "error".red().bold());
        std::process::exit(1);
    });

    match loon_lang::pkg::Manifest::load(&cwd) {
        Ok(Some(manifest)) => {
            if capabilities {
                println!("  {}", "Dependency Capabilities".bold());
                println!("  {}", "─".repeat(50));
                if manifest.deps.is_empty() {
                    println!("  No dependencies");
                    return;
                }
                for (name, dep) in &manifest.deps {
                    let grants = if dep.grant.is_empty() {
                        "pure (no effects)".dimmed().to_string()
                    } else {
                        dep.grant.join(", ")
                    };
                    println!("  {} -> {}", name, grants);
                }
            } else {
                println!(
                    "  {} vulnerability auditing (Phase 3)",
                    "TODO".yellow().bold()
                );
            }
        }
        Ok(None) => {
            eprintln!("{}: no pkg.loon found", "error".red().bold());
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("{}: {e}", "error".red().bold());
            std::process::exit(1);
        }
    }
}

fn pkg_cache_clean() {
    let cache_dir = loon_lang::pkg::fetch::cache_dir();
    if cache_dir.exists() {
        match std::fs::remove_dir_all(&cache_dir) {
            Ok(_) => println!("  {} cache at {}", "Cleaned".green().bold(), cache_dir.display()),
            Err(e) => eprintln!("{}: {e}", "error".red().bold()),
        }
    } else {
        println!("  Cache directory does not exist");
    }
}

fn pkg_cache_warm() {
    println!(
        "  {} cache warming (Phase 2)",
        "TODO".yellow().bold()
    );
}

fn pkg_search(query: &str) {
    let index = loon_lang::pkg::index::builtin_index();
    let results = index.search(query);
    if results.is_empty() {
        println!("  No packages found for '{query}'");
        println!(
            "  {} the built-in index is empty — packages will be added as the ecosystem grows",
            "note:".dimmed()
        );
    } else {
        for entry in results {
            println!("  {} — {}", entry.source.bold(), entry.description);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wasm_magic_number() {
        // A minimal Loon program that compiles to WASM should produce valid WASM bytes
        let source = r#"[fn main [] 42]"#;
        let exprs = loon_lang::parser::parse(source).expect("parse failed");
        let wasm = loon_lang::codegen::compile(&exprs).expect("compile failed");
        assert!(wasm.len() >= 8, "WASM output too small: {} bytes", wasm.len());
        assert_eq!(&wasm[..4], b"\0asm", "WASM magic number missing");
        // Version 1
        assert_eq!(&wasm[4..8], &[1, 0, 0, 0], "unexpected WASM version");
    }

    #[test]
    fn optimize_wasm_preserves_magic() {
        let source = r#"[fn main [] 42]"#;
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
        let source = r#"[fn main [] 42]"#;
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
