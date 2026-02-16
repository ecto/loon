use logos::Logos;

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
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
                None => out.push('\\'),
            }
        } else {
            out.push(c);
        }
    }
    out
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n]+")]
#[logos(skip r";[^\n]*")]
pub enum Token {
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
    #[token("=>")]
    FatArrow,
    #[token("|>")]
    Pipe,
    #[token("?")]
    Question,
    #[token("/")]
    Slash,
    #[token("->")]
    Arrow,

    // Macro quasiquoting
    #[token("`")]
    Backtick,
    #[token("~@", priority = 3)]
    TildeSplice,
    #[token("~", priority = 2)]
    Tilde,

    // Literals — higher priority than Symbol
    #[regex(r"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?(f32|f64)?", priority = 10, callback = |lex| lex.slice().to_string())]
    Float(String),
    #[regex(r"-?[0-9]+(i32|i64|u32|u64)?", priority = 10, callback = |lex| lex.slice().to_string())]
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
    #[regex(r"[a-zA-Z_+\-*!<>=&%][a-zA-Z0-9_\-?!.*%/+]*", priority = 1, callback = |lex| lex.slice().to_string())]
    Symbol(String),
}
