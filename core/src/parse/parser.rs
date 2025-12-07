use crate::ast::*;
use crate::parse::error::{ParseError, ParseResult};
use crate::parse::token::{Token, TokenType, Tokenizer};
use std::cell::RefCell;
use std::rc::Rc;
use std::str::FromStr;

pub struct SourceParser<TokPin, T: Tokenizer<Pin = TokPin>> {
    tokenizer: Box<T>,
    curr: Token,
    next: Token,
}

impl<TokPin, T: Tokenizer<Pin = TokPin>> SourceParser<TokPin, T> {
    pub fn new(mut tokenizer: Box<T>) -> Self {
        Self {
            curr: tokenizer.next_token(),
            next: tokenizer.peek_token(),
            tokenizer,
        }
    }

    fn bump(&mut self) -> Token {
        let prev = std::mem::replace(&mut self.curr, self.tokenizer.next_token());
        self.next = self.tokenizer.peek_token();
        prev
    }

    fn parse_module(&mut self) -> Module {
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
        }
    }

    fn parse_stmt(&mut self) -> ParseResult<Rc<RefCell<Stmt>>> {
        let expr = self.parse_expr()?;
        expect!(self, TokenType::Semicolon)?;
        Ok(Rc::new(RefCell::new(Stmt::Expr(expr))))
    }

    fn parse_expr(&mut self) -> ParseResult<Rc<RefCell<Expr>>> {
        let expr = match &self.curr.token_type {
            TokenType::Ident => self.parse_func_call()?,
            TokenType::Literal(_) => self.parse_literal()?,
            // TODO: Use a list of valid expression tokens for expected tokens
            _ => return Err(ParseError::UnexpectedToken(self.curr.clone(), vec![])),
        };
        Ok(Rc::new(RefCell::new(expr)))
    }

    fn parse_func_call(&mut self) -> ParseResult<Expr> {
        let id = self.parse_ident()?;
        let args = self.parse_func_args()?;
        Ok(Expr::FuncCall { id, args })
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
        let name = tok.get_raw().to_owned();
        Ok(Ident { name })
    }

    fn parse_literal(&mut self) -> ParseResult<Expr> {
        let tok = expect!(self, TokenType::Literal(_))?;
        let val = i32::from_str(tok.span.raw_str()).expect("Invalid integer literal");
        Ok(Expr::Lit(Literal::Integer(val)))
    }
}

impl<TokPin, T: Tokenizer<Pin = TokPin>> ASTBuilder for SourceParser<TokPin, T> {
    fn build_module(mut self) -> Module {
        self.parse_module()
    }
}
