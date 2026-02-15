use loon_lang::interp::{eval, Env, Value};
use loon_lang::parser::parse;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Serialize, Deserialize)]
struct ReplHistory {
    entries: Vec<ReplEntry>,
    cursor: usize,
}

#[derive(Serialize, Deserialize)]
struct ReplEntry {
    input: String,
    output: String,
}

pub fn run_repl() {
    let mut rl = DefaultEditor::new().expect("failed to create editor");
    let mut env = Env::new();
    loon_lang::interp::register_builtins_pub(&mut env);

    let mut history: Vec<(String, Env)> = Vec::new();
    let mut fork_stack: Vec<(Env, Vec<(String, Env)>)> = Vec::new();

    // Load history file
    let history_path = state_dir().join("repl-history");
    let _ = rl.load_history(&history_path);

    let prompt = "loon> ";
    let mut fork_depth = 0;

    println!("Loon v0.1.0 REPL — type an expression or :help");

    loop {
        let p = if fork_depth > 0 {
            format!("loon (fork-{fork_depth})> ")
        } else {
            prompt.to_string()
        };

        match rl.readline(&p) {
            Ok(line) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                let _ = rl.add_history_entry(trimmed);

                // Meta-commands
                match trimmed {
                    ":help" | ":h" => {
                        println!("  :help       — show this help");
                        println!("  :quit       — exit the REPL");
                        println!("  [rewind n]  — undo last n evaluations");
                        println!("  [snapshot]  — save current state");
                        println!("  [fork]      — branch this session");
                        println!("  [exit-fork] — discard fork, return to parent");
                        continue;
                    }
                    ":quit" | ":q" => break,
                    _ => {}
                }

                // Check for built-in REPL commands
                if trimmed == "[fork]" {
                    fork_stack.push((env.clone(), history.clone()));
                    fork_depth += 1;
                    println!("  forked session (depth {fork_depth})");
                    continue;
                }
                if trimmed == "[exit-fork]" {
                    if let Some((prev_env, prev_history)) = fork_stack.pop() {
                        env = prev_env;
                        history = prev_history;
                        fork_depth -= 1;
                        println!("  exited fork");
                    } else {
                        println!("  not in a fork");
                    }
                    continue;
                }

                // Check for [rewind n]
                if trimmed.starts_with("[rewind") {
                    if let Some(n_str) = trimmed
                        .strip_prefix("[rewind ")
                        .and_then(|s| s.strip_suffix(']'))
                    {
                        if let Ok(n) = n_str.trim().parse::<usize>() {
                            let target = history.len().saturating_sub(n);
                            if target > 0 {
                                let (_, snapshot) = &history[target - 1];
                                env = snapshot.clone();
                                history.truncate(target);
                                println!("  rewound {n} steps");
                            } else if !history.is_empty() {
                                // Reset to initial state
                                env = Env::new();
                                loon_lang::interp::register_builtins_pub(&mut env);
                                history.clear();
                                println!("  rewound to start");
                            }
                            continue;
                        }
                    }
                }

                // Check if input is natural language (doesn't start with [ or is not a bare symbol)
                if !trimmed.starts_with('[')
                    && !trimmed.starts_with('#')
                    && !trimmed.starts_with('{')
                    && !trimmed.starts_with(':')
                    && !trimmed.starts_with('"')
                    && trimmed.contains(' ')
                {
                    println!("  AI features not yet implemented");
                    continue;
                }

                // Save state before eval
                history.push((trimmed.to_string(), env.clone()));

                match parse(trimmed) {
                    Ok(exprs) => {
                        let mut last = Value::Unit;
                        for expr in &exprs {
                            match eval(expr, &mut env) {
                                Ok(val) => last = val,
                                Err(e) => {
                                    eprintln!("  {e}");
                                    // Restore env on error
                                    if let Some((_, prev)) = history.last() {
                                        env = prev.clone();
                                    }
                                    last = Value::Unit;
                                    break;
                                }
                            }
                        }
                        if last != Value::Unit {
                            println!("{last}");
                        }
                        // Sync global env for nested calls
                        loon_lang::interp::sync_global_env_pub(&env);
                    }
                    Err(e) => {
                        eprintln!("  {e}");
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => break,
            Err(e) => {
                eprintln!("error: {e}");
                break;
            }
        }
    }

    // Save history
    let _ = std::fs::create_dir_all(state_dir());
    let _ = rl.save_history(&history_path);
    println!("goodbye!");
}

fn state_dir() -> PathBuf {
    dirs::home_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join(".loon")
}
