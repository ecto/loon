use logos::Logos;

// Interpolation sentinels: unambiguous markers delimiting an interpolated
// expression in the unescaped string, so the parser (desugar_fmt) can find them
// without colliding with any literal text (these control chars can't appear in
// source). `\(expr)` becomes START expr END.
pub(crate) const INTERP_START: char = '\u{1}';
pub(crate) const INTERP_END: char = '\u{2}';

/// Unescape a string literal token.
///
/// Interpolation is Swift/Roc-style `\(expr)`: bare `{` and `}` are ordinary
/// literal characters (so map/JSON/embedded-Loon text needs no escaping). `\{`
/// and `\}` are also accepted as literal braces for back-compat. Standard
/// escapes `\n \t \\ \"` apply. An `\(…)` span is emitted between sentinels with
/// its expression text preserved verbatim (so it can be re-parsed); inner string
/// quotes are written `\"` and balance parens are tracked so a `)` inside a
/// nested string doesn't close the interpolation early.
fn unescape(s: &str) -> String {
    let mut out = String::new();
    let mut chars = s[1..s.len() - 1].chars();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('\\') => out.push('\\'),
                Some('"') => out.push('"'),
                Some('{') => out.push('{'), // literal brace (back-compat)
                Some('}') => out.push('}'), // literal brace (back-compat)
                Some('(') => {
                    // Interpolation: collect the expression verbatim up to the
                    // matching ')'.
                    out.push(INTERP_START);
                    let mut depth: u32 = 1;
                    let mut in_string = false;
                    while let Some(ec) = chars.next() {
                        if ec == '\\' {
                            // \" toggles the inner string and becomes a bare
                            // quote for re-parsing; any other escape is kept.
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
                            if !in_string {
                                if ec == '(' {
                                    depth += 1;
                                } else if ec == ')' {
                                    depth -= 1;
                                    if depth == 0 {
                                        break;
                                    }
                                }
                            }
                            out.push(ec);
                        }
                    }
                    out.push(INTERP_END);
                }
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
                None => out.push('\\'),
            }
        } else {
            // Bare `{` / `}` and everything else are literal characters.
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
