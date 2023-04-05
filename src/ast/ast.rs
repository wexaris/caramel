#![allow(dead_code)]

use std::fmt::{Display, Formatter};
use std::ops::Deref;
use crate::parse::Span;

pub struct Root {
    pub stmt_list: StmtList,
}


/// ///////////////////////////////////////////////////////
/// STATEMENTS

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Stmt {
    Skip(SkipStmt),
    Read(ReadStmt),
    Write(WriteStmt),
    If(IfStmt),
    While(WhileStmt),
    Assign(AssignStmt),
}

impl Stmt {
    pub fn get_span(&self) -> Span {
        match self {
            Stmt::Skip(stmt) => stmt.span,
            Stmt::Read(stmt) => stmt.span,
            Stmt::Write(stmt) => stmt.span,
            Stmt::If(stmt) => stmt.span,
            Stmt::While(stmt) => stmt.span,
            Stmt::Assign(stmt) => stmt.span,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StmtList {
    pub span: Span,
    pub items: Vec<Stmt>,
}

impl Deref for StmtList {
    type Target = Vec<Stmt>;
    fn deref(&self) -> &Self::Target { &self.items }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SkipStmt {
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ReadStmt {
    pub span: Span,
    pub var_list: IdentList,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct WriteStmt {
    pub span: Span,
    pub var_list: IdentList,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IfStmt {
    pub span: Span,
    pub check: Box<Expr>,
    pub branch_true: StmtList,
    pub branch_false: Option<StmtList>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct WhileStmt {
    pub span: Span,
    pub check: Box<Expr>,
    pub branch_true: StmtList,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AssignStmt {
    pub span: Span,
    pub id: Ident,
    pub expr: Box<Expr>,
}


/// ///////////////////////////////////////////////////////
/// EXPRESSIONS

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expr {
    Lit(Literal),
    Var(Ident),
    UnaryOp(UnaryOpExpr),
    BinaryOp(BinaryOpExpr),
}

impl Expr {
    pub fn get_span(&self) -> Span {
        match self {
            Expr::Lit(lit) => lit.span,
            Expr::Var(id) => id.span,
            Expr::UnaryOp(op) => op.span,
            Expr::BinaryOp(op) => op.span,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Op { Neg, Plus, Minus, Mul, Div, Eq, Neq, Less, LessEq, More, MoreEq, And, Or }

impl Op {
    pub fn to_str(&self) -> &'static str {
        match self {
            Op::Neg => "Negation",
            Op::Plus => "Plus",
            Op::Minus => "Minus",
            Op::Mul => "Mul",
            Op::Div => "Div",
            Op::Eq => "Eq",
            Op::Neq => "Neq",
            Op::Less => "Less",
            Op::LessEq => "LessEq",
            Op::More => "More",
            Op::MoreEq => "MoreEq",
            Op::And => "And",
            Op::Or => "Or",
        }
    }
}

impl ToString for Op {
    fn to_string(&self) -> String { self.to_str().to_string() }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct UnaryOpExpr {
    pub span: Span,
    pub op: Op,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BinaryOpExpr {
    pub span: Span,
    pub op: Op,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}


/// ///////////////////////////////////////////////////////
/// IDENTIFIERS

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Ident {
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IdentList {
    pub span: Span,
    pub items: Vec<Ident>,
}

impl Deref for IdentList {
    type Target = Vec<Ident>;
    fn deref(&self) -> &Self::Target { &self.items }
}


/// ///////////////////////////////////////////////////////
/// LITERALS

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Literal {
    pub span: Span,
    pub value: Value,
}

impl Literal {
    pub fn get_type(&self) -> ValueType {
        self.value.get_type()
    }
}


/// ///////////////////////////////////////////////////////
/// TYPES

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ValueType {
    Integer,
    Boolean
}

impl Display for ValueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Integer => write!(f, "integer"),
            ValueType::Boolean => write!(f, "boolean")
        }
    }
}

/// ///////////////////////////////////////////////////////
/// VALUES

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Value {
    Integer(i32),
    Boolean(bool),
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Integer(_) => ValueType::Integer,
            Value::Boolean(_) => ValueType::Boolean,
        }
    }
}
