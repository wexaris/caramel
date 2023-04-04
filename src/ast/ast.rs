#![allow(dead_code)]

use std::fmt::{Display, Formatter};

pub struct Root {
    pub stmt_list: StmtList,
}


/// ///////////////////////////////////////////////////////
/// STATEMENTS

pub enum Stmt {
    Skip,
    Read(ReadStmt),
    Write(WriteStmt),
    If(IfStmt),
    While(WhileStmt),
    Assign(AssignStmt),
}

pub type StmtList = Vec<Stmt>;

pub struct ReadStmt {
    pub var_list: IdentList,
}

pub struct WriteStmt {
    pub var_list: IdentList,
}

pub struct IfStmt {
    pub check: Box<Expr>,
    pub branch_true: StmtList,
    pub branch_false: Option<StmtList>,
}

pub struct WhileStmt {
    pub check: Box<Expr>,
    pub branch_true: StmtList,
}

pub struct AssignStmt {
    pub id: Ident,
    pub expr: Box<Expr>,
}


/// ///////////////////////////////////////////////////////
/// EXPRESSIONS

#[derive(Clone, PartialEq)]
pub enum Expr {
    Lit(Literal),
    Var(Ident),
    UnaryOp(Op, Box<Expr>),
    BinaryOp(Op, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Op { Neg, Plus, Minus, Mul, Div, Eq, Neq, Less, LessEq, More, MoreEq, And, Or }


/// ///////////////////////////////////////////////////////
/// IDENTIFIERS

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Ident {
    pub name: String,
}

pub type IdentList = Vec<Ident>;


/// ///////////////////////////////////////////////////////
/// LITERALS

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Literal {
    Integer(i32),
    Boolean(bool),
}

impl Literal {
    pub fn get_type(&self) -> ValueType {
        match self {
            Literal::Integer(_) => ValueType::Integer,
            Literal::Boolean(_) => ValueType::Boolean,
        }
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
