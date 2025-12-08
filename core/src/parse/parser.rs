use crate::ast::*;
use crate::parse::error::{ParseError, ParseResult};
use crate::parse::span::Span;
use crate::parse::token::{Token, TokenType, Tokenizer};
use crate::source::code_source::CodeSource;
use std::cell::RefCell;
use std::rc::Rc;
use std::str::FromStr;

pub struct SourceParser<T: Tokenizer> {
    tokenizer: Box<T>,
    curr: Token,
    next: Token,
}

impl<T: Tokenizer> SourceParser<T> {
    pub fn new(mut tokenizer: Box<T>) -> Self {
        Self {
            curr: tokenizer.next_token(),
            next: tokenizer.peek_token(),
            tokenizer,
        }
    }

    #[inline]
    fn origin(&self) -> &Rc<dyn CodeSource> {
        self.tokenizer.origin()
    }

    fn bump(&mut self) -> Token {
        let prev = std::mem::replace(&mut self.curr, self.tokenizer.next_token());
        self.next = self.tokenizer.peek_token();
        prev
    }

    fn parse_module(&mut self) -> Module {
        let start_pos = self.curr.span.position();

        let mut stmts = vec![];
        while !self.curr.is_eof() {
            match self.parse_stmt() {
                Ok(stmt) => {
                    stmts.push(stmt);
                }
                Err(e) => {
                    println!("{e}");
                    // TODO: Error recovery
                    break;
                }
            }
        }
        Module {
            origin: self.tokenizer.origin().clone(),
            stmts,
            span: Span::from_range(&start_pos, &self.curr.span.position()),
        }
    }

    fn parse_stmt(&mut self) -> ParseResult<Rc<RefCell<Stmt>>> {
        let expr = self.parse_expr()?;
        expect!(self, TokenType::Semicolon)?;
        Ok(Rc::new(RefCell::new(Stmt::Expr(expr))))
    }

    fn parse_expr(&mut self) -> ParseResult<Rc<RefCell<Expr>>> {
        let expr = match &self.curr.token_type {
            TokenType::Ident => Expr::FuncCall(self.parse_func_call()?),
            TokenType::Literal(_) => Expr::Lit(self.parse_literal()?),
            // TODO: Use a list of valid expression tokens for expected tokens
            _ => return Err(ParseError::UnexpectedToken(self.curr.clone(), vec![])),
        };
        Ok(Rc::new(RefCell::new(expr)))
    }

    fn parse_func_call(&mut self) -> ParseResult<FuncCall> {
        let id = self.parse_ident()?;
        let args = self.parse_func_args()?;
        let span = self.span_from(&id.span);
        Ok(FuncCall { id, args, span })
    }

    fn parse_func_args(&mut self) -> ParseResult<Vec<Rc<RefCell<Expr>>>> {
        expect!(self, TokenType::ParenOpen)?;
        let args = self.parse_sequence(Self::parse_expr, TokenType::Comma)?;
        expect!(self, TokenType::ParenClose)?;
        Ok(args)
    }

    fn parse_sequence(
        &mut self,
        mut parse_item: impl FnMut(&mut Self) -> ParseResult<Rc<RefCell<Expr>>>,
        sep: TokenType,
    ) -> ParseResult<Vec<Rc<RefCell<Expr>>>> {
        let mut args = vec![];
        loop {
            match parse_item(self) {
                Ok(item) => {
                    args.push(item);

                    if !consume!(self, sep) {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
        Ok(args)
    }

    fn parse_ident(&mut self) -> ParseResult<Ident> {
        let tok = expect!(self, TokenType::Ident)?;
        let name = self.origin().get_substr_from_span(&tok.span).to_owned();
        let span = tok.span;
        Ok(Ident { name, span })
    }

    fn parse_literal(&mut self) -> ParseResult<Literal> {
        let tok = expect!(self, TokenType::Literal(_))?;
        let raw_str = self.origin().get_substr_from_span(&tok.span);
        let val = i32::from_str(raw_str).expect("Invalid integer literal");
        let span = tok.span;
        Ok(Literal::Integer { val, span })
    }

    #[inline]
    fn span_from(&self, start: &Span) -> Span {
        Span::new(
            start.idx,
            self.curr.span.idx - start.idx,
            start.line,
            start.col,
        )
    }
}

impl<T: Tokenizer> ASTBuilder for SourceParser<T> {
    fn build_module(mut self) -> Module {
        self.parse_module()
    }
}

#[cfg(test)]
mod test {
    use crate::parse::error::ParseError;
    use crate::parse::parser::SourceParser;
    use crate::parse::token::{Literal, MockTokenizer, TokenType};

    #[test]
    fn expect_field_value() {
        let tokenizer = MockTokenizer::new(vec![TokenType::Fn]);
        let mut parser = SourceParser::new(Box::new(tokenizer));

        // Match exact token
        let res = expect!(parser, TokenType::Fn);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().token_type, TokenType::Fn);

        // Next is EOF
        assert_eq!(parser.curr.token_type, TokenType::Eof);
    }

    #[test]
    fn expect_expr_list() {
        let tokenizer = MockTokenizer::new(vec![TokenType::Plus, TokenType::Minus]);
        let mut parser = SourceParser::new(Box::new(tokenizer));

        // Match one of the list (Plus)
        let res = expect!(parser, [TokenType::Minus, TokenType::Plus]);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().token_type, TokenType::Plus);

        // Match one of the list (Minus)
        let res = expect!(parser, [TokenType::Minus, TokenType::Plus]);
        assert!(res.is_ok());
        assert_eq!(res.unwrap().token_type, TokenType::Minus);
    }

    #[test]
    fn expect_pattern() {
        let tokenizer = MockTokenizer::new(vec![
            TokenType::Literal(Literal::Integer),
            TokenType::Literal(Literal::String),
        ]);
        let mut parser = SourceParser::new(Box::new(tokenizer));

        // Match pattern ignoring inner value
        let res = expect!(parser, TokenType::Literal(_));
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap().token_type,
            TokenType::Literal(Literal::Integer)
        );

        let res = expect!(parser, TokenType::Literal(_));
        assert!(res.is_ok());
        assert_eq!(res.unwrap().token_type, TokenType::Literal(Literal::String));
    }

    #[test]
    fn expect_pattern_guarded() {
        let tokenizer = MockTokenizer::new(vec![
            TokenType::Literal(Literal::Bool(true)),
            TokenType::Literal(Literal::Bool(false)),
        ]);
        let mut parser = SourceParser::new(Box::new(tokenizer));

        // Match only if bool is true
        let res = expect!(parser, TokenType::Literal(Literal::Bool(b)) if b);
        assert!(res.is_ok());

        // Next is false, so guard should fail pattern match, resulting in an error
        let res = expect!(parser, TokenType::Literal(Literal::Bool(b)) if b);
        assert!(res.is_err());
        assert!(matches!(
            res.unwrap_err(),
            ParseError::UnexpectedToken(_, _)
        ));
    }

    #[test]
    fn expect_invalid_symbol() {
        let tokenizer = MockTokenizer::new(vec![TokenType::Unknown]);
        let mut parser = SourceParser::new(Box::new(tokenizer));

        let res = expect!(parser, TokenType::Fn);
        assert!(res.is_err());
        assert!(matches!(res.unwrap_err(), ParseError::InvalidSymbol(_, _)));
    }

    #[test]
    fn expect_unexpected_token() {
        let tokenizer = MockTokenizer::new(vec![TokenType::Var]);
        let mut parser = SourceParser::new(Box::new(tokenizer));

        let res = expect!(parser, TokenType::Fn);
        assert!(res.is_err());

        match res.err() {
            Some(ParseError::UnexpectedToken(tok, expected)) => {
                assert_eq!(tok.token_type, TokenType::Var);
                assert_eq!(expected[0], "TokenType::Fn");
            }
            Some(err) => panic!("Unexpected error type: {err}"),
            None => panic!("Unexpected None error"),
        }
    }

    #[test]
    fn expect_unexpected_eof() {
        let tokenizer = MockTokenizer::new(vec![]); // Immediately EOF
        let mut parser = SourceParser::new(Box::new(tokenizer));

        let res = expect!(parser, TokenType::Fn);
        assert!(res.is_err());
        assert!(matches!(res.unwrap_err(), ParseError::UnexpectedEof(_)));
    }

    #[test]
    fn consume_matches() {
        let tokenizer = MockTokenizer::new(vec![TokenType::Semicolon]);
        let mut parser = SourceParser::new(Box::new(tokenizer));

        // Should return true and advance
        assert!(consume!(parser, TokenType::Semicolon));
        assert_eq!(parser.curr.token_type, TokenType::Eof);
    }

    #[test]
    fn consume_mismatches() {
        let tokenizer = MockTokenizer::new(vec![TokenType::Var]);
        let mut parser = SourceParser::new(Box::new(tokenizer));

        // Should return false and NOT advance
        assert!(!consume!(parser, TokenType::Fn));
        assert_eq!(parser.curr.token_type, TokenType::Var);
    }
}
