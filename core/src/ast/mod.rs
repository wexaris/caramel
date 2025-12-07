mod builder;

use crate::source::code_source::CodeSource;
pub use builder::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Module {
    pub origin: Rc<dyn CodeSource>,
    pub stmts: Vec<Rc<RefCell<Stmt>>>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Rc<RefCell<Expr>>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    FuncCall {
        id: Ident,
        args: Vec<Rc<RefCell<Expr>>>,
    },
    Lit(Literal),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i32),
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
}
