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
        let expr = self.parse_expr_inner()?;
        // Postfix ? operator: desugar expr? into
        // [match expr [Ok __v] => __v [Err __e] => [Fail.fail __e]]
        if self.peek() == Some(&Token::Question) {
            let (_, q_span) = self.advance().unwrap();
            let full_span = expr.span.merge(q_span);
            return Ok(desugar_question(expr, full_span));
        }
        Ok(expr)
    }

    fn parse_expr_inner(&mut self) -> Result<Expr, ParseError> {
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
            Token::Arrow => Ok(Expr::new(ExprKind::Symbol("->".to_string()), span)),

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
                let full_span = span.merge(end);
                // Desugar [fmt "...{expr}..."] → [str parts...]
                if items.len() == 2 {
                    if let ExprKind::Symbol(ref s) = items[0].kind {
                        if s == "fmt" {
                            if let ExprKind::Str(ref template) = items[1].kind {
                                return desugar_fmt(template, full_span, self.source);
                            }
                        }
                    }
                }
                Ok(Expr::new(ExprKind::List(items), full_span))
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

            // Quasiquote: `expr
            Token::Backtick => {
                let inner = self.parse_expr()?;
                let full_span = span.merge(inner.span);
                Ok(Expr::new(ExprKind::Quote(Box::new(inner)), full_span))
            }

            // Unquote-splice: ~@expr (must come before Tilde)
            Token::TildeSplice => {
                let inner = self.parse_expr()?;
                let full_span = span.merge(inner.span);
                Ok(Expr::new(ExprKind::UnquoteSplice(Box::new(inner)), full_span))
            }

            // Unquote: ~expr
            Token::Tilde => {
                let inner = self.parse_expr()?;
                let full_span = span.merge(inner.span);
                Ok(Expr::new(ExprKind::Unquote(Box::new(inner)), full_span))
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

/// Desugar `[fmt "hello {name}, {[+ 1 2]}"]` into `[str "hello " name ", " [+ 1 2]]`
fn desugar_fmt(template: &str, span: Span, _source: &str) -> Result<Expr, ParseError> {
    let mut parts: Vec<Expr> = Vec::new();
    let mut literal = String::new();
    let mut chars = template.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '{' {
            if chars.peek() == Some(&'{') {
                // Escaped {{ → literal {
                chars.next();
                literal.push('{');
            } else {
                // Flush literal
                if !literal.is_empty() {
                    parts.push(Expr::new(ExprKind::Str(literal.clone()), span));
                    literal.clear();
                }
                // Collect expression text until matching }
                let mut expr_text = String::new();
                let mut depth = 1;
                for ec in chars.by_ref() {
                    if ec == '{' {
                        depth += 1;
                        expr_text.push(ec);
                    } else if ec == '}' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        expr_text.push(ec);
                    } else {
                        expr_text.push(ec);
                    }
                }
                if depth != 0 {
                    return Err(ParseError {
                        message: "unclosed { in fmt string".to_string(),
                        span,
                    });
                }
                // Parse the expression text
                let inner_exprs = parse(&expr_text).map_err(|e| ParseError {
                    message: format!("error in fmt interpolation: {}", e.message),
                    span,
                })?;
                if inner_exprs.len() == 1 {
                    parts.push(inner_exprs.into_iter().next().unwrap());
                } else {
                    return Err(ParseError {
                        message: "fmt interpolation must contain exactly one expression".to_string(),
                        span,
                    });
                }
            }
        } else if c == '}' {
            if chars.peek() == Some(&'}') {
                // Escaped }} → literal }
                chars.next();
                literal.push('}');
            } else {
                return Err(ParseError {
                    message: "unmatched } in fmt string".to_string(),
                    span,
                });
            }
        } else {
            literal.push(c);
        }
    }

    // Flush remaining literal
    if !literal.is_empty() {
        parts.push(Expr::new(ExprKind::Str(literal), span));
    }

    // If no interpolation, just return a plain string
    if parts.len() == 1 {
        if let ExprKind::Str(_) = &parts[0].kind {
            return Ok(parts.into_iter().next().unwrap());
        }
    }

    // Build [str part0 part1 ...]
    let mut items = vec![Expr::new(ExprKind::Symbol("str".to_string()), span)];
    items.extend(parts);
    Ok(Expr::new(ExprKind::List(items), span))
}

/// Desugar `expr?` into `[match expr [Ok __v] => __v [Err __e] => [Fail.fail __e]]`
fn desugar_question(expr: Expr, span: Span) -> Expr {
    let ok_var = Expr::new(ExprKind::Symbol("__v".to_string()), span);
    let err_var = Expr::new(ExprKind::Symbol("__e".to_string()), span);
    let ok_pattern = Expr::new(
        ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("Ok".to_string()), span),
            Expr::new(ExprKind::Symbol("__v".to_string()), span),
        ]),
        span,
    );
    let err_pattern = Expr::new(
        ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("Err".to_string()), span),
            Expr::new(ExprKind::Symbol("__e".to_string()), span),
        ]),
        span,
    );
    let arrow = Expr::new(ExprKind::Symbol("=>".to_string()), span);
    let fail_call = Expr::new(
        ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("Fail.fail".to_string()), span),
            err_var,
        ]),
        span,
    );
    Expr::new(
        ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("match".to_string()), span),
            expr,
            ok_pattern,
            arrow.clone(),
            ok_var,
            err_pattern,
            arrow,
            fail_call,
        ]),
        span,
    )
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
    fn parse_question_desugar() {
        let src = "[Ok 42]?";
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
        // Should be a match expression
        if let ExprKind::List(items) = &exprs[0].kind {
            assert!(matches!(&items[0].kind, ExprKind::Symbol(s) if s == "match"));
        } else {
            panic!("expected list (match)");
        }
    }

    #[test]
    fn parse_fmt_simple() {
        let src = r#"[fmt "hello {name}"]"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
        // Should desugar to [str "hello " name]
        assert_eq!(exprs[0].to_string(), r#"[str "hello " name]"#);
    }

    #[test]
    fn parse_fmt_expr() {
        let src = r#"[fmt "2 + 2 = {[+ 2 2]}"]"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
        assert_eq!(exprs[0].to_string(), r#"[str "2 + 2 = " [+ 2 2]]"#);
    }

    #[test]
    fn parse_fmt_no_interp() {
        let src = r#"[fmt "no interpolation"]"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
        // Should be a plain string, not a str call
        assert_eq!(exprs[0].to_string(), r#""no interpolation""#);
    }

    #[test]
    fn parse_fmt_escaped_braces() {
        let src = r#"[fmt "escaped {{braces}}"]"#;
        let exprs = parse(src).unwrap();
        assert_eq!(exprs.len(), 1);
        assert_eq!(exprs[0].to_string(), r#""escaped {braces}""#);
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
