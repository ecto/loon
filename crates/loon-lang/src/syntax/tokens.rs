use logos::Logos;

fn unescape(s: &str) -> String {
    let mut out = String::new();
    let mut chars = s[1..s.len() - 1].chars().peekable();
    let mut brace_depth: u32 = 0;
    let mut in_string = false; // inside "..." within interpolation

    while let Some(c) = chars.next() {
        if brace_depth > 0 {
            // Inside {…} interpolation block.
            // Convert \" → " (needed for inner string delimiters).
            // Preserve other escapes (\n, \t, \\) verbatim for re-parse.
            if c == '\\' {
                match chars.next() {
                    Some('"') => {
                        out.push('"');
                        in_string = !in_string;
                    }
                    Some(other) => {
                        out.push('\\');
                        out.push(other);
                    }
                    None => out.push('\\'),
                }
            } else {
                out.push(c);
                if !in_string {
                    match c {
                        '{' => brace_depth += 1,
                        '}' => brace_depth -= 1,
                        _ => {}
                    }
                }
            }
            continue;
        }

        // Outside interpolation — normal unescape
        if c == '\\' {
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('\\') => out.push('\\'),
                Some('"') => out.push('"'),
                Some('{') => out.push_str("{{"),
                Some('}') => out.push_str("}}"),
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
                None => out.push('\\'),
            }
        } else if c == '{' {
            out.push('{');
            brace_depth = 1;
        } else {
            out.push(c);
        }
    }
    out
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n]+")]
pub enum Token {
    // Line comments. Captured rather than skipped so the formatter can put
    // them back; the parser routes them into a side table so the grammar
    // never sees them.
    #[regex(r";[^\n]*", |lex| lex.slice().to_string())]
    Comment(String),

    // Delimiters
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    // Compound delimiters
    #[token("#[")]
    HashBracket,
    #[token("#{")]
    HashBrace,

    // Operators
    #[token("?")]
    Question,
    #[token("/")]
    Slash,
    #[token("->")]
    Arrow,
    #[token(":")]
    Colon,

    // Macro quasiquoting
    #[token("`")]
    Backtick,
    #[token("~@", priority = 3)]
    TildeSplice,
    #[token("~", priority = 2)]
    Tilde,

    // Literals — higher priority than Symbol
    #[regex(r"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?(f32|f64|[a-zA-Z]+)?", priority = 10, callback = |lex| lex.slice().to_string())]
    Float(String),
    #[regex(r"-?[0-9]+(i32|i64|u32|u64|[a-zA-Z]+)?", priority = 10, callback = |lex| lex.slice().to_string())]
    Int(String),
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r#""([^"\\]|\\.)*""#, |lex| unescape(lex.slice()))]
    Str(String),

    // Keywords (Clojure-style :keyword)
    #[regex(r":[a-zA-Z_][a-zA-Z0-9_\-]*", |lex| lex.slice()[1..].to_string())]
    Keyword(String),

    // Symbols (identifiers, operators like +, -, etc.)
    // Allows / in the middle for namespaced names like dom/create-element
    #[regex(r"[a-zA-Z_+\-*!<>=&%][a-zA-Z0-9_\-?!.*%/+<>=]*", priority = 1, callback = |lex| lex.slice().to_string())]
    Symbol(String),
}
