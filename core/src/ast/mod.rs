mod builder;
mod tree_printer;

use crate::parse::span::Span;
use crate::source::code_source::CodeSource;
pub use builder::*;
use std::cell::RefCell;
use std::rc::Rc;
pub use tree_printer::*;

#[derive(Debug, Clone)]
pub struct Module {
    pub origin: Rc<dyn CodeSource>,
    pub stmts: Vec<Rc<RefCell<Stmt>>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Rc<RefCell<Expr>>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    FuncCall(FuncCall),
    Lit(Literal),
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub id: Ident,
    pub args: Vec<Rc<RefCell<Expr>>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer { val: i32, span: Span },
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}
