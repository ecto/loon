use crate::ast::{Expr, ExprKind};
use crate::syntax::{Span, Token};
use logos::Logos;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "parse error at {}..{}: {}",
            self.span.start, self.span.end, self.message
        )
    }
}

impl std::error::Error for ParseError {}

struct Parser<'a> {
    tokens: Vec<(Token, Span)>,
    pos: usize,
    source: &'a str,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Result<Self, ParseError> {
        let mut tokens = Vec::new();
        let mut lexer = Token::lexer(source);
        while let Some(tok) = lexer.next() {
            let span = lexer.span();
            let span = Span::new(span.start, span.end);
            match tok {
                Ok(t) => tokens.push((t, span)),
                Err(()) => {
                    return Err(ParseError {
                        message: format!(
                            "unexpected character: {:?}",
                            &source[span.start..span.end]
                        ),
                        span,
                    });
                }
            }
        }
        Ok(Self {
            tokens,
            pos: 0,
            source,
        })
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|(t, _)| t)
    }

    fn peek_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|(_, s)| *s)
            .unwrap_or(Span::new(self.source.len(), self.source.len()))
    }

    fn advance(&mut self) -> Option<(Token, Span)> {
        let item = self.tokens.get(self.pos).cloned();
        if item.is_some() {
            self.pos += 1;
        }
        item
    }

    fn expect(&mut self, expected: &Token) -> Result<Span, ParseError> {
        match self.advance() {
            Some((ref tok, span)) if tok == expected => Ok(span),
            Some((tok, span)) => Err(ParseError {
                message: format!("expected {expected:?}, got {tok:?}"),
                span,
            }),
            None => Err(ParseError {
                message: format!("expected {expected:?}, got EOF"),
                span: Span::new(self.source.len(), self.source.len()),
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let Some((tok, span)) = self.advance() else {
            return Err(ParseError {
                message: "unexpected EOF".to_string(),
                span: Span::new(self.source.len(), self.source.len()),
            });
        };

        match tok {
            Token::Int(s) => {
                let n: i64 = s
                    .trim_end_matches(|c: char| c.is_alphabetic())
                    .parse()
                    .map_err(|e| ParseError {
                        message: format!("invalid integer: {e}"),
                        span,
                    })?;
                Ok(Expr::new(ExprKind::Int(n), span))
            }
            Token::Float(s) => {
                let n: f64 = s
                    .trim_end_matches(|c: char| c.is_alphabetic())
                    .parse()
                    .map_err(|e| ParseError {
                        message: format!("invalid float: {e}"),
                        span,
                    })?;
                Ok(Expr::new(ExprKind::Float(n), span))
            }
            Token::True => Ok(Expr::new(ExprKind::Bool(true), span)),
            Token::False => Ok(Expr::new(ExprKind::Bool(false), span)),
            Token::Str(s) => Ok(Expr::new(ExprKind::Str(s), span)),
            Token::Keyword(k) => Ok(Expr::new(ExprKind::Keyword(k), span)),
            Token::Symbol(s) => Ok(Expr::new(ExprKind::Symbol(s), span)),
            Token::Pipe => Ok(Expr::new(ExprKind::Symbol("|>".to_string()), span)),
            Token::Slash => Ok(Expr::new(ExprKind::Symbol("/".to_string()), span)),
            Token::FatArrow => Ok(Expr::new(ExprKind::Symbol("=>".to_string()), span)),
            Token::Question => Ok(Expr::new(ExprKind::Symbol("?".to_string()), span)),

            // S-expression: [head args...]
            Token::LBracket => {
                let mut items = Vec::new();
                while self.peek() != Some(&Token::RBracket) {
                    if self.peek().is_none() {
                        return Err(ParseError {
                            message: "unclosed [".to_string(),
                            span,
                        });
                    }
                    items.push(self.parse_expr()?);
                }
                let end = self.expect(&Token::RBracket)?;
                Ok(Expr::new(ExprKind::List(items), span.merge(end)))
            }

            // Persistent vector: #[a b c]
            Token::HashBracket => {
                let mut items = Vec::new();
                while self.peek() != Some(&Token::RBracket) {
                    if self.peek().is_none() {
                        return Err(ParseError {
                            message: "unclosed #[".to_string(),
                            span,
                        });
                    }
                    items.push(self.parse_expr()?);
                }
                let end = self.expect(&Token::RBracket)?;
                Ok(Expr::new(ExprKind::Vec(items), span.merge(end)))
            }

            // Set: #{a b c}
            Token::HashBrace => {
                let mut items = Vec::new();
                while self.peek() != Some(&Token::RBrace) {
                    if self.peek().is_none() {
                        return Err(ParseError {
                            message: "unclosed #{".to_string(),
                            span,
                        });
                    }
                    items.push(self.parse_expr()?);
                }
                let end = self.expect(&Token::RBrace)?;
                Ok(Expr::new(ExprKind::Set(items), span.merge(end)))
            }

            // Map: {:key val ...}
            Token::LBrace => {
                let mut pairs = Vec::new();
                while self.peek() != Some(&Token::RBrace) {
                    if self.peek().is_none() {
                        return Err(ParseError {
                            message: "unclosed {".to_string(),
                            span,
                        });
                    }
                    let key = self.parse_expr()?;
                    let val = self.parse_expr()?;
                    pairs.push((key, val));
                }
                let end = self.expect(&Token::RBrace)?;
                Ok(Expr::new(ExprKind::Map(pairs), span.merge(end)))
            }

            // Tuple: (a, b) — parse comma-separated exprs
            Token::LParen => {
                let mut items = Vec::new();
                while self.peek() != Some(&Token::RParen) {
                    if self.peek().is_none() {
                        return Err(ParseError {
                            message: "unclosed (".to_string(),
                            span,
                        });
                    }
                    items.push(self.parse_expr()?);
                }
                let end = self.expect(&Token::RParen)?;
                Ok(Expr::new(ExprKind::Tuple(items), span.merge(end)))
            }

            _ => Err(ParseError {
                message: format!("unexpected token: {tok:?}"),
                span,
            }),
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut exprs = Vec::new();
        while self.peek().is_some() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

/// Parse a source string into a list of top-level expressions.
pub fn parse(source: &str) -> Result<Vec<Expr>, ParseError> {
    let mut parser = Parser::new(source)?;
    parser.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_hello_world() {
        let src = r#"[defn main [] [println "hello, world!"]]"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
        assert_eq!(exprs[0].to_string(), r#"[defn main [] [println "hello, world!"]]"#);
    }

    #[test]
    fn parse_fib() {
        let src = r#"
[defn fib [n]
  [match n
    0 => 0
    1 => 1
    n => [+ [fib [- n 1]] [fib [- n 2]]]]]
"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
    }

    #[test]
    fn parse_vector_set_map() {
        let src = r#"
#[1 2 3]
#{:a :b :c}
{:name "loon" :version "0.1"}
"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 3);
        assert!(matches!(exprs[0].kind, ExprKind::Vec(_)));
        assert!(matches!(exprs[1].kind, ExprKind::Set(_)));
        assert!(matches!(exprs[2].kind, ExprKind::Map(_)));
    }

    #[test]
    fn parse_pipe() {
        let src = r#"[|> #[1 2 3] [map inc] [collect]]"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
        if let ExprKind::List(items) = &exprs[0].kind {
            assert!(matches!(items[0].kind, ExprKind::Symbol(ref s) if s == "|>"));
        } else {
            panic!("expected list");
        }
    }

    #[test]
    fn parse_multi_arity() {
        let src = r#"
[defn greet
  ([name]
    [str "hello, " name])
  ([greeting name]
    [str greeting ", " name])]
"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
    }

    #[test]
    fn parse_match_with_guards() {
        let src = r#"
[match n
  0 => "zero"
  n [when [> n 0]] => "positive"
  _ => "negative"]
"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
    }

    #[test]
    fn parse_effect_annotation() {
        let src = r#"[defn load-config [path] / {IO Fail} [IO.read-file path]]"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
    }

    #[test]
    fn parse_type_def() {
        let src = r#"
[type Option T
  [Some T]
  None]
"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
    }
}
