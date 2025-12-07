use crate::ast::*;
use crate::parse::token::{Token, TokenType, Tokenizer};
use std::cell::RefCell;
use std::rc::Rc;
use std::str::FromStr;

pub struct SourceParser<TokPin, T: Tokenizer<Pin = TokPin>> {
    tokenizer: Box<T>,
    curr: Token,
    next: Token,
}

macro_rules! expect {
    ( $this:ident, $( $ex:ident ).+ ) => {
        match $this.curr.token_type == $( $ex ).+ {
            true => Some($this.bump()),
            _ => None,
        }
    };
    ( $this:ident, $pattern:pat $(if $guard:expr)? $(,)? ) => {
        match $this.curr.token_type {
            $pattern $(if $guard)? => Some($this.bump()),
            _ => None,
        }
    };
}

macro_rules! consume {
    ( $this:ident, $( $ex:ident ).+ ) => {
        match $this.curr.token_type == $( $ex ).+ {
            true => {
                $this.bump();
                true
            },
            _ => false,
        }
    };
    ( $this:ident, [ $( $ex:expr ),+ ] ) => {
        match [ $( $ex ),+ ].contains(&$this.curr.token_type) {
            true => {
                $this.bump();
                true
            },
            _ => false,
        }
    };
    ( $this:ident, $pattern:pat $(if $guard:expr)? $(,)? ) => {
        match $this.curr.token_type {
            $pattern $(if $guard)? => {
                $this.bump();
                true
            },
            _ => false,
        }
    };
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
                Some(stmt) => {
                    stmts.push(stmt);
                },
                None => {
                    println!("PARSE FAILED");
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

    fn parse_stmt(&mut self) -> Option<Rc<RefCell<Stmt>>> {
        let expr = self.parse_expr()?;
        expect!(self, TokenType::Semicolon)?;
        Some(Rc::new(RefCell::new(Stmt::Expr(expr))))
    }

    fn parse_expr(&mut self) -> Option<Rc<RefCell<Expr>>> {
        let expr = match &self.curr.token_type {
            TokenType::Ident => self.parse_func_call()?,
            TokenType::Literal(_) => self.parse_literal()?,
            _ => return None,
        };
        Some(Rc::new(RefCell::new(expr)))
    }

    fn parse_func_call(&mut self) -> Option<Expr> {
        let id = self.parse_ident()?;
        let args = self.parse_func_args()?;
        Some(Expr::FuncCall { id, args })
    }

    fn parse_func_args(&mut self) -> Option<Vec<Rc<RefCell<Expr>>>> {
        match consume!(self, TokenType::ParenOpen) {
            true => {
                let args = self.parse_sequence(Self::parse_expr, TokenType::Comma)?;
                expect!(self, TokenType::ParenClose)?;
                Some(args)
            }
            false => None,
        }
    }

    fn parse_sequence(
        &mut self,
        mut parse_item: impl FnMut(&mut Self) -> Option<Rc<RefCell<Expr>>>,
        sep: TokenType,
    ) -> Option<Vec<Rc<RefCell<Expr>>>> {
        let mut args = vec![];
        loop {
            match parse_item(self) {
                Some(item) => {
                    args.push(item);

                    if !consume!(self, sep) {
                        break;
                    }
                }
                None => break,
            }
        }
        Some(args)
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        let tok = expect!(self, TokenType::Ident)?;
        let name = tok.get_raw().to_owned();
        Some(Ident { name })
    }

    fn parse_literal(&mut self) -> Option<Expr> {
        let tok = expect!(self, TokenType::Literal(_))?;
        let val = i32::from_str(tok.span.raw_str()).expect("Invalid integer literal");
        Some(Expr::Lit(Literal::Integer(val)))
    }
}

impl<TokPin, T: Tokenizer<Pin = TokPin>> ASTBuilder for SourceParser<TokPin, T> {
    fn build_module(mut self) -> Module {
        self.parse_module()
    }
}
