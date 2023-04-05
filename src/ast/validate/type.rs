#![allow(dead_code)]

use std::collections::HashMap;
use crate::ast::*;
use self::result::*;

pub struct TypeValidator {
    context: TypeStack,
    error_count: u32,
}

macro_rules! print_err {
    ( $validator:expr, $e:expr ) => {
        match $e {
            Ok(x) => x,
            Err(e) => {
                $validator.error_count += 1;
                println!("{}", e);
            }
        }
    }
}

impl TypeValidator {
    pub fn new() -> Self {
        Self {
            context: TypeStack::new(),
            error_count: 0,
        }
    }

    pub fn validate(&mut self, ast: &Root) {
        self.validate_stmt_list(&ast.stmt_list);
    }

    fn validate_stmt_list(&mut self, stmt_list: &StmtList) {
        self.push_scope();
        for stmt in stmt_list {
            self.validate_stmt(stmt);
        }
        self.pop_scope();
    }

    pub fn validate_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Skip => {},
            Stmt::Read(stmt) => self.validate_read(stmt),
            Stmt::Write(stmt) => self.validate_write(stmt),
            Stmt::If(stmt) => self.validate_if(stmt),
            Stmt::While(stmt) => self.validate_while(stmt),
            Stmt::Assign(stmt) => self.validate_assign(stmt),
        }
    }

    pub fn validate_read(&mut self, stmt: &ReadStmt) {
        for id in &stmt.var_list {
            self.save(id, ValueType::Integer);
        }
    }

    pub fn validate_write(&mut self, stmt: &WriteStmt) {
        for id in &stmt.var_list {
            print_err!(self, self.validate_var(id, ValueType::Integer));
        }
    }

    pub fn validate_if(&mut self, stmt: &IfStmt) {
        print_err!(self, self.validate_expr(&stmt.check, ValueType::Boolean));

        self.validate_stmt_list(&stmt.branch_true);

        if let Some(branch_false) = &stmt.branch_false {
            self.validate_stmt_list(branch_false);
        }
    }

    pub fn validate_while(&mut self, stmt: &WhileStmt) {
        print_err!(self, self.validate_expr(&stmt.check, ValueType::Boolean));
        self.validate_stmt_list(&stmt.branch_true);
    }

    pub fn validate_assign(&mut self, stmt: &AssignStmt) {
        print_err!(self, self.validate_expr(&stmt.expr, ValueType::Integer));
        self.save(&stmt.id, ValueType::Integer);
    }

    pub fn validate_expr(&mut self, expr: &Expr, expected: ValueType) -> Result<()> {
        match expr {
            Expr::Lit(lit) => self.validate_literal(lit, expected)?,
            Expr::Var(id) => self.validate_var(id, expected)?,
            Expr::UnaryOp(op, expr) => {
                match op {
                    Op::Neg => {
                        self.validate_expr(&expr, ValueType::Boolean)?;
                        if ValueType::Boolean != expected {
                            return Err(Error::TypeMismatch(expected, ValueType::Boolean))
                        }
                    }
                    _ => return Err(Error::InternalError("invalid operator in Expr::UnaryOp".to_string())),
                }
            }
            Expr::BinaryOp(op, lhs, rhs) => {
                match op {
                    Op::Plus | Op::Minus |
                    Op::Mul | Op::Div => {
                        self.validate_expr(&lhs, ValueType::Integer)?;
                        self.validate_expr(&rhs, ValueType::Integer)?;
                        if ValueType::Integer != expected {
                            return Err(Error::TypeMismatch(expected, ValueType::Integer))
                        }
                    }
                    Op::Eq | Op::Neq |
                    Op::Less | Op::LessEq |
                    Op::More | Op::MoreEq => {
                        self.validate_expr(&lhs, ValueType::Integer)?;
                        self.validate_expr(&rhs, ValueType::Integer)?;
                        if ValueType::Boolean != expected {
                            return Err(Error::TypeMismatch(expected, ValueType::Boolean))
                        }
                    }
                    Op::And | Op::Or => {
                        self.validate_expr(&lhs, ValueType::Boolean)?;
                        self.validate_expr(&rhs, ValueType::Boolean)?;
                        if ValueType::Boolean != expected {
                            return Err(Error::TypeMismatch(expected, ValueType::Boolean))
                        }
                    }
                    _ => return Err(Error::InternalError("invalid operator in Expr::BinaryOp".to_string())),
                }
            }
        }
        Ok(())
    }

    pub fn validate_var(&mut self, id: &Ident, expected: ValueType) -> Result<()> {
        let found = self.find(id)?;
        if found != expected {
            return Err(Error::TypeMismatch(expected, found));
        }
        Ok(())
    }

    pub fn validate_literal(&mut self, lit: &Literal, expected: ValueType) -> Result<()> {
        if lit.get_type() != expected {
            return Err(Error::TypeMismatch(expected, lit.get_type()));
        }
        Ok(())
    }

    fn save(&mut self, id: &Ident, ty: ValueType) {
        self.context.set(&id.name, ty);
    }

    fn find(&mut self, id: &Ident) -> Result<ValueType> {
        self.context.find(&id.name).ok_or(Error::UndeclaredIdent(id.clone()))
    }

    fn push_scope(&mut self) { self.context.push(); }
    fn pop_scope(&mut self)  { self.context.pop();  }

    pub fn has_errors(&self) -> bool {
        self.error_count != 0
    }
}

#[derive(Debug, Clone)]
struct TypeStack {
    inner: Vec<HashMap<String, ValueType>>,
}

impl TypeStack {
    pub fn new() -> Self {
        Self { inner: vec![HashMap::new()] }
    }

    pub fn push(&mut self) {
        self.inner.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.inner.pop();
    }

    pub fn find(&self, id: &str) -> Option<ValueType> {
        for frame in self.inner.iter().rev() {
            if let Some(ty) = frame.get(id) {
                return Some(*ty);
            }
        }
        return None;
    }

    pub fn set(&mut self, id: &str, ty: ValueType) {
        let frame = self.inner.last_mut().expect("TypeStack missing context frame; this is a bug!");
        frame.insert(id.to_owned(), ty);
    }
}

pub mod result {
    use std::fmt::{Display, Formatter};
    use crate::ast::Ident;
    use crate::ast::validate::r#type::ValueType;

    pub type Result<T> = std::result::Result<T, Error>;

    #[derive(Debug, Clone)]
    pub enum Error {
        UndeclaredIdent(Ident),
        TypeMismatch(ValueType, ValueType),
        InternalError(String),
    }

    impl Display for Error {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Error::UndeclaredIdent(id) => write!(f, "undeclared identifier: {}", id.name),
                Error::TypeMismatch(expected, found) => write!(f, "type mismatch; expected {}, found {}", expected, found),
                Error::InternalError(msg) => write!(f, "internal error; {}", msg),
            }
        }
    }
}
