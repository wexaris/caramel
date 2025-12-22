mod builder;
mod print;

use crate::parse::span::Span;
use crate::source::code_source::CodeSource;
pub use builder::*;
pub use print::printer::*;
pub use print::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Module {
    pub origin: Rc<dyn CodeSource>,
    pub decls: Vec<Decl>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Func(FuncDecl),
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub id: Ident,
    pub return_ty: Type,
    pub params: Vec<Param>,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub id: Ident,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub ty: TypeType,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeType {
    Custom(String),
    I64,
    I32,
    I16,
    I8,
    U64,
    U32,
    U16,
    U8,
    F64,
    F32,
    String,
    Char,
    Bool,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Rc<RefCell<Expr>>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    FuncCall(FuncCall),
    BinaryOp(BinaryOp),
    VarAccess(Ident),
    Lit(Literal),
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub id: Ident,
    pub args: Vec<Rc<RefCell<Expr>>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub ty: BinaryOpType,
    pub lhs: Rc<RefCell<Expr>>,
    pub rhs: Rc<RefCell<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOpType {
    Sum,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub ty: ValueType,
    pub raw_str: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    Integer,
    Real,
    String,
    Char,
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Expr::FuncCall(expr) => &expr.span,
            Expr::BinaryOp(expr) => &expr.span,
            Expr::VarAccess(expr) => &expr.span,
            Expr::Lit(expr) => &expr.span,
        }
    }
}
