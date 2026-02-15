use logos::Logos;

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

    // Literals — higher priority than Symbol
    #[regex(r"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?(f32|f64)?", priority = 10, callback = |lex| lex.slice().to_string())]
    Float(String),
    #[regex(r"-?[0-9]+(i32|i64|u32|u64)?", priority = 10, callback = |lex| lex.slice().to_string())]
    Int(String),
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string()
    })]
    Str(String),

    // Keywords (Clojure-style :keyword)
    #[regex(r":[a-zA-Z_][a-zA-Z0-9_\-]*", |lex| lex.slice()[1..].to_string())]
    Keyword(String),

    // Symbols (identifiers, operators like +, -, etc.)
    #[regex(r"[a-zA-Z_+\-*!<>=&%][a-zA-Z0-9_\-?!.*%]*", priority = 1, callback = |lex| lex.slice().to_string())]
    Symbol(String),
}
