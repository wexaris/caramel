#![allow(dead_code)]

use std::collections::HashMap;
use crate::ast::*;
use self::result::*;

#[derive(Debug)]
pub struct Interpreter {
    context: Stack,
    data: Vec<String>,
    data_idx: usize,
}

macro_rules! expect {
    ($found:expr, $expect:ident, $span:expr, $err:expr) => {
        match $found.get_type() {
            ValueType::$expect => {
                if let Value::$expect(val) = $found { val }
                else { return Err(Error::InvalidOp($span, $err.to_string())); }
            },
            _ => return Err(Error::InvalidOp($span, $err.to_string())),
        }
    };
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            context: Stack::new(),
            data: vec![],
            data_idx: 0,
        }
    }

    pub fn run(&mut self, ast: &Root) -> Result<()> {
        // Read data file
        self.data = match Self::read_data_file() {
            Ok(data) => data,
            Err(e) => {
                let msg = format!("failed to load data file; {}", e);
                return Err(Error::RuntimeError(msg));
            }
        };

        // Execute
        for stmt in ast.stmt_list.iter() {
            self.run_stmt(stmt)?;
        }

        Ok(())
    }

    fn read_data_file() -> std::io::Result<Vec<String>> {
        const DATA_FILE: &str = "data.txt";
        let data = std::fs::read_to_string(DATA_FILE)?;
        let pieces = data.split(',')
            .map(|x| x.trim().to_owned())
            .collect();
        Ok(pieces)
    }

    fn run_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Skip(stmt) => self.run_skip(stmt),
            Stmt::Read(stmt) => self.run_read(stmt),
            Stmt::Write(stmt) => self.run_write(stmt),
            Stmt::If(stmt) => self.run_if(stmt),
            Stmt::While(stmt) => self.run_while(stmt),
            Stmt::Assign(stmt) => self.run_assign(stmt),
        }
    }

    fn run_skip(&mut self, _: &SkipStmt) -> Result<()> {
        Ok(())
    }

    fn run_read(&mut self, stmt: &ReadStmt) -> Result<()> {
        for id in stmt.var_list.iter() {
            let read = self.data.get(self.data_idx);
            match read {
                Some(val_str) => {
                    self.data_idx += 1;

                    use std::str::FromStr;
                    let val = match i32::from_str(val_str) {
                        Ok(val) => val,
                        Err(_) => {
                            let msg = format!("invalid data argument: {}", val_str);
                            return Err(Error::RuntimeError(msg));
                        }
                    };

                    self.save(id, Value::Integer(val));
                },
                None => {
                    return Err(Error::RuntimeError("no more data to read".to_string()));
                }
            }
        }
        Ok(())
    }

    fn run_write(&mut self, stmt: &WriteStmt) -> Result<()> {
        for id in stmt.var_list.iter() {
            let val = expect!(self.eval_ident(id)?, Integer,
                stmt.span, "writing booleans is not supported");

            print!("{} ", val);
        }
        Ok(())
    }

    fn run_if(&mut self, stmt: &IfStmt) -> Result<()> {
        let val = expect!(self.eval_expr(&stmt.check)?, Boolean,
            stmt.span, "conditional argument must be a boolean");

        if val {
            for stmt in stmt.branch_true.iter() {
                self.run_stmt(stmt)?;
            }
        }
        else if let Some(branch_else) = &stmt.branch_false {
            for stmt in branch_else.iter() {
                self.run_stmt(stmt)?;
            }
        }
        Ok(())
    }

    fn run_while(&mut self, stmt: &WhileStmt) -> Result<()> {
        loop {
            let val = expect!(self.eval_expr(&stmt.check)?, Boolean,
                stmt.span, "conditional argument must be a boolean");

            if !val { break; }

            for stmt in stmt.branch_true.iter() {
                self.run_stmt(stmt)?;
            }
        }
        Ok(())
    }

    fn run_assign(&mut self, stmt: &AssignStmt) -> Result<()> {
        let val = self.eval_expr(&stmt.expr)?;
        self.save(&stmt.id, val);
        Ok(())
    }

    fn eval_expr(&self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Lit(lit) => self.eval_literal(lit),
            Expr::Var(id) => self.eval_ident(id),
            Expr::UnaryOp(expr) => self.eval_unary_op(expr),
            Expr::BinaryOp(expr) => self.eval_binary_op(expr),
        }
    }

    fn eval_ident(&self, id: &Ident) -> Result<Value> {
        self.find(id)
    }

    fn eval_literal(&self, lit: &Literal) -> Result<Value> {
        Ok(lit.value.clone())
    }

    fn eval_unary_op(&self, expr: &UnaryOpExpr) -> Result<Value> {
        match expr.op {
            Op::Neg => self.eval_neg(&expr.expr),
            _ => Err(Error::InternalError("invalid operator in UnaryOpExpr".to_string())),
        }
    }

    fn eval_binary_op(&self, expr: &BinaryOpExpr) -> Result<Value> {
        match expr.op {
            Op::Plus => self.eval_sum(&expr.lhs, &expr.rhs),
            Op::Minus => self.eval_sub(&expr.lhs, &expr.rhs),
            Op::Mul => self.eval_mul(&expr.lhs, &expr.rhs),
            Op::Div => self.eval_div(&expr.lhs, &expr.rhs),
            Op::Eq => self.eval_eq(&expr.lhs, &expr.rhs),
            Op::Neq => self.eval_neq(&expr.lhs, &expr.rhs),
            Op::Less => self.eval_less(&expr.lhs, &expr.rhs),
            Op::LessEq => self.eval_less_eq(&expr.lhs, &expr.rhs),
            Op::More => self.eval_more(&expr.lhs, &expr.rhs),
            Op::MoreEq => self.eval_more_eq(&expr.lhs, &expr.rhs),
            Op::And => self.eval_and(&expr.lhs, &expr.rhs),
            Op::Or => self.eval_or(&expr.lhs, &expr.rhs),
            _ => Err(Error::InternalError("invalid operator in BinaryOpExpr".to_string())),
        }
    }

    fn eval_neg(&self, expr: &Expr) -> Result<Value> {
        let val = self.eval_boolean(expr)?;
        Ok(Value::Boolean(!val))
    }

    fn eval_sum(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs = self.eval_integer(lhs)?;
        let rhs = self.eval_integer(rhs)?;
        Ok(Value::Integer(lhs + rhs))
    }

    fn eval_sub(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs = self.eval_integer(lhs)?;
        let rhs = self.eval_integer(rhs)?;
        Ok(Value::Integer(lhs - rhs))
    }

    fn eval_mul(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs = self.eval_integer(lhs)?;
        let rhs = self.eval_integer(rhs)?;
        Ok(Value::Integer(lhs * rhs))
    }

    fn eval_div(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs_val = self.eval_integer(lhs)?;
        let rhs_val = self.eval_integer(rhs)?;

        if rhs_val == 0 {
            let span = rhs.get_span().hi - lhs.get_span().lo;
            return Err(Error::InvalidOp(span, "cannot divide by zero".to_string()))
        }

        Ok(Value::Integer(lhs_val / rhs_val))
    }

    fn eval_eq(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs = self.eval_integer(lhs)?;
        let rhs = self.eval_integer(rhs)?;
        Ok(Value::Boolean(lhs == rhs))
    }

    fn eval_neq(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs = self.eval_integer(lhs)?;
        let rhs = self.eval_integer(rhs)?;
        Ok(Value::Boolean(lhs != rhs))
    }

    fn eval_less(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs = self.eval_integer(lhs)?;
        let rhs = self.eval_integer(rhs)?;
        Ok(Value::Boolean(lhs < rhs))
    }

    fn eval_less_eq(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs = self.eval_integer(lhs)?;
        let rhs = self.eval_integer(rhs)?;
        Ok(Value::Boolean(lhs <= rhs))
    }

    fn eval_more(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs = self.eval_integer(lhs)?;
        let rhs = self.eval_integer(rhs)?;
        Ok(Value::Boolean(lhs > rhs))
    }

    fn eval_more_eq(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs = self.eval_integer(lhs)?;
        let rhs = self.eval_integer(rhs)?;
        Ok(Value::Boolean(lhs >= rhs))
    }

    fn eval_and(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs = self.eval_boolean(lhs)?;
        if lhs {
            let rhs = self.eval_boolean(rhs)?;
            Ok(Value::Boolean(lhs && rhs))
        }
        else {
            Ok(Value::Boolean(lhs))
        }
    }

    fn eval_or(&self, lhs: &Expr, rhs: &Expr) -> Result<Value> {
        let lhs = self.eval_boolean(lhs)?;
        if lhs {
            Ok(Value::Boolean(lhs))
        }
        else {
            let rhs = self.eval_boolean(rhs)?;
            Ok(Value::Boolean(lhs && rhs))
        }
    }

    fn eval_integer(&self, expr: &Expr) -> Result<i32> {
        let val = expect!(self.eval_expr(expr)?, Integer,
            expr.get_span(), "expected integer; found boolean");

        Ok(val)
    }

    fn eval_boolean(&self, expr: &Expr) -> Result<bool> {
        let val = expect!(self.eval_expr(expr)?, Boolean,
            expr.get_span(), "expected boolean; found integer");

        Ok(val)
    }

    fn save(&mut self, id: &Ident, val: Value) {
        self.context.set(&id.name, val);
    }

    fn find(&self, id: &Ident) -> Result<Value> {
        self.context.find(&id.name)
            .ok_or(Error::UndeclaredIdent(id.span, id.clone()))
    }

    fn push_scope(&mut self) { self.context.push(); }
    fn pop_scope(&mut self)  { self.context.pop();  }
}

#[derive(Debug, Clone)]
struct Stack {
    inner: Vec<HashMap<String, Value>>
}

impl Stack {
    pub fn new() -> Self {
        Self { inner: vec![HashMap::new()] }
    }

    pub fn push(&mut self) {
        self.inner.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.inner.pop();
    }

    pub fn find(&self, id: &str) -> Option<Value> {
        for frame in self.inner.iter().rev() {
            if let Some(ty) = frame.get(id) {
                return Some(*ty);
            }
        }
        return None;
    }

    pub fn set(&mut self, id: &str, val: Value) {
        // Update value
        for frame in self.inner.iter_mut().rev() {
            if frame.contains_key(id) {
                frame.insert(id.to_owned(), val);
                return;
            }
        }

        // Save new value in last scope
        let scope = self.inner.last_mut().expect("TypeStack missing context frame; this is a bug!");
        scope.insert(id.to_owned(), val);
    }
}

pub mod result {
    use std::fmt::{Display, Formatter};
    use crate::ast::Ident;
    use crate::parse::Span;

    pub type Result<T> = std::result::Result<T, Error>;

    #[derive(Debug, Clone)]
    pub enum Error {
        UndeclaredIdent(Span, Ident),
        InvalidOp(Span, String),
        RuntimeError(String),
        InternalError(String),
    }

    impl Display for Error {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Error::UndeclaredIdent(span, id) => write!(f, "{}: undeclared identifier: {}", span, id.name),
                Error::InvalidOp(span, msg) => write!(f, "{}: invalid operation; {}", span, msg),
                Error::RuntimeError(msg) => write!(f, "runtime error; {}", msg),
                Error::InternalError(msg) => write!(f, "internal error; {}", msg),
            }
        }
    }
}
