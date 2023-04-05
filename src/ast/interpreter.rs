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
    ($found:expr, $expect:ident, $err:expr) => {
        match $found.get_type() {
            ValueType::$expect => {
                if let Value::$expect(val) = $found { val }
                else { return Err(Error::InvalidOp($err.to_string())); }
            },
            _ => return Err(Error::InvalidOp($err.to_string())),
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
        for stmt in &ast.stmt_list {
            self.run_stmt(stmt)?;
        }

        Ok(())
    }

    fn read_data_file() -> std::io::Result<Vec<String>> {
        const DATA_FILE: &str = "data.txt";
        let data = std::fs::read_to_string(DATA_FILE)?;
        let pieces = data.split(',')
            .map(|x| x.to_owned())
            .collect();
        Ok(pieces)
    }

    fn run_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Skip => Ok(()),
            Stmt::Read(stmt) => self.run_read(stmt),
            Stmt::Write(stmt) => self.run_write(stmt),
            Stmt::If(stmt) => self.run_if(stmt),
            Stmt::While(stmt) => self.run_while(stmt),
            Stmt::Assign(stmt) => self.run_assign(stmt),
        }
    }

    fn run_read(&mut self, stmt: &ReadStmt) -> Result<()> {
        for id in &stmt.var_list {
            let read = self.data.get(self.data_idx);
            match read {
                Some(val_str) => {
                    self.data_idx += 1;

                    use std::str::FromStr;
                    let val = match i32::from_str(val_str) {
                        Ok(val) => val,
                        Err(_) => {
                            return Err(Error::RuntimeError("invalid data argument".to_string()));
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
        for id in &stmt.var_list {
            let val = self.eval_ident(id)?;
            let val = expect!(val, Integer, "writing booleans is not supported");
            print!("{} ", val);
        }
        Ok(())
    }

    fn run_if(&mut self, stmt: &IfStmt) -> Result<()> {
        let val = self.eval_expr(&stmt.check)?;
        let val = expect!(val, Boolean, "conditional argument must be a boolean");

        if val {
            for stmt in &stmt.branch_true {
                self.run_stmt(stmt)?;
            }
        }
        else if let Some(branch_else) = &stmt.branch_false {
            for stmt in branch_else {
                self.run_stmt(stmt)?;
            }
        }
        Ok(())
    }

    fn run_while(&mut self, stmt: &WhileStmt) -> Result<()> {
        loop {
            let val = self.eval_expr(&stmt.check)?;
            let val = expect!(val, Boolean, "conditional argument must be a boolean");

            if !val { break; }

            for stmt in &stmt.branch_true {
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
            Expr::UnaryOp(op, expr) => {
                match op {
                    Op::Neg => self.eval_neg(&expr),
                    _ => Err(Error::InternalError("invalid operator in Expr::UnaryOp".to_string())),
                }
            }
            Expr::BinaryOp(op, lhs, rhs) => {
                match op {
                    Op::Plus => self.eval_sum(&lhs, &rhs),
                    Op::Minus => self.eval_sub(&lhs, &rhs),
                    Op::Mul => self.eval_mul(&lhs, &rhs),
                    Op::Div => self.eval_div(&lhs, &rhs),
                    Op::Eq => self.eval_eq(&lhs, &rhs),
                    Op::Neq => self.eval_neq(&lhs, &rhs),
                    Op::Less => self.eval_less(&lhs, &rhs),
                    Op::LessEq => self.eval_less_eq(&lhs, &rhs),
                    Op::More => self.eval_more(&lhs, &rhs),
                    Op::MoreEq => self.eval_more_eq(&lhs, &rhs),
                    Op::And => self.eval_and(&lhs, &rhs),
                    Op::Or => self.eval_or(&lhs, &rhs),
                    _ => Err(Error::InternalError("invalid operator in Expr::UnaryOp".to_string())),
                }
            }
        }
    }

    fn eval_ident(&self, id: &Ident) -> Result<Value> {
        self.find(id)
    }

    fn eval_literal(&self, lit: &Literal) -> Result<Value> {
        Ok(lit.clone().into())
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
        let lhs = self.eval_integer(lhs)?;
        let rhs = self.eval_integer(rhs)?;

        if rhs == 0 {
            return Err(Error::InvalidOp("cannot divide by zero".to_string()))
        }

        Ok(Value::Integer(lhs / rhs))
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
        let val = self.eval_expr(expr)?;
        let val = expect!(val, Integer, "only integers can be summed");
        Ok(val)
    }

    fn eval_boolean(&self, expr: &Expr) -> Result<bool> {
        let val = self.eval_expr(expr)?;
        let val = expect!(val, Boolean, "only integers can be summed");
        Ok(val)
    }

    fn save(&mut self, id: &Ident, val: Value) {
        self.context.set(&id.name, val);
    }

    fn find(&self, id: &Ident) -> Result<Value> {
        self.context.find(&id.name)
            .ok_or(Error::UndeclaredIdent(id.clone()))
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Value {
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

impl From<Literal> for Value {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Integer(val) => Value::Integer(val),
            Literal::Boolean(val) => Value::Boolean(val),
        }
    }
}

pub mod result {
    use std::fmt::{Display, Formatter};
    use crate::ast::Ident;

    pub type Result<T> = std::result::Result<T, Error>;

    #[derive(Debug, Clone)]
    pub enum Error {
        UndeclaredIdent(Ident),
        InvalidOp(String),
        RuntimeError(String),
        InternalError(String),
    }

    impl Display for Error {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Error::UndeclaredIdent(id) => write!(f, "undeclared identifier: {}", id.name),
                Error::InvalidOp(msg) => write!(f, "invalid operation; {}", msg),
                Error::RuntimeError(msg) => write!(f, "runtime error; {}", msg),
                Error::InternalError(msg) => write!(f, "internal error; {}", msg),
            }
        }
    }
}
