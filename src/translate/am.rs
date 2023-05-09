#![allow(dead_code)]

use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use crate::ast::*;
use self::result::*;

pub struct AMTranslator {
    out: std::fs::File,
    tmp_var_map: HashMap<String, usize>,
    scope: usize,
}

impl AMTranslator {
    pub fn new(out_file: &Path) -> Result<Self> {
        Ok(Self {
            out: std::fs::File::create(out_file)?,
            tmp_var_map: HashMap::default(),
            scope: 0,
        })
    }

    pub fn run(&mut self, ast: &Root) -> Result<()> {
        for stmt in ast.stmt_list.iter() {
            self.run_stmt(stmt)?;
        }
        Ok(())
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
        self.writeln("NOOP")?;
        Ok(())
    }

    fn run_read(&mut self, stmt: &ReadStmt) -> Result<()> {
        for id in stmt.var_list.iter() {
            self.writeln(&format!("READ {}", id.name))?
        }
        Ok(())
    }

    fn run_write(&mut self, stmt: &WriteStmt) -> Result<()> {
        for id in stmt.var_list.iter() {
            self.writeln(&format!("WRITE {}", id.name))?
        }
        Ok(())
    }

    fn run_if(&mut self, stmt: &IfStmt) -> Result<()> {
        self.run_expr(&stmt.check)?;
        self.writeln("BRANCH(")?;

        self.push_scope();
        for stmt in stmt.branch_true.iter() {
            self.run_stmt(&stmt)?;
        }
        self.pop_scope();

        if let Some(branch_false) = &stmt.branch_false {
            self.writeln(", ")?;
            self.push_scope();
            for stmt in branch_false.iter() {
                self.run_stmt(&stmt)?;
            }
            self.pop_scope();
            self.writeln(")")?;
        }
        else {
            self.writeln(",)")?;
        }

        Ok(())
    }

    fn run_while(&mut self, stmt: &WhileStmt) -> Result<()> {
        self.writeln("LOOP(")?;

        self.push_scope();
        self.run_expr(&stmt.check)?;
        self.pop_scope();

        self.writeln(", ")?;

        self.push_scope();
        for stmt in stmt.branch_true.iter() {
            self.run_stmt(&stmt)?;
        }
        self.pop_scope();

        self.writeln(")")?;
        Ok(())
    }

    fn run_assign(&mut self, stmt: &AssignStmt) -> Result<()> {
        self.run_expr(&stmt.expr)?;
        self.writeln(&format!("STORE({})", stmt.id.name))?;
        Ok(())
    }

    fn run_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Lit(lit) => self.run_literal(lit),
            Expr::Var(id) => self.run_ident(id),
            Expr::UnaryOp(expr) => self.run_unary_op(expr),
            Expr::BinaryOp(expr) => self.run_binary_op(expr),
        }
    }

    fn run_ident(&mut self, id: &Ident) -> Result<()> {
        self.writeln(&format!("FETCH({})", id.name))?;
        Ok(())
    }

    fn run_literal(&mut self, lit: &Literal) -> Result<()> {
        let val_str = match lit.value {
            Value::Integer(x) => x.to_string(),
            Value::Boolean(true) => "tt".to_string(),
            Value::Boolean(false) => "ff".to_string(),
        };
        self.writeln(&format!("PUSH({})", val_str))?;
        Ok(())
    }

    fn run_unary_op(&mut self, expr: &UnaryOpExpr) -> Result<()> {
        match expr.op {
            Op::Neg => self.run_neg(&expr.expr),
            _ => Err(Error::InternalError("invalid operator in UnaryOpExpr".to_string())),
        }
    }

    fn run_binary_op(&mut self, expr: &BinaryOpExpr) -> Result<()> {
        match expr.op {
            Op::Plus => self.run_sum(&expr.lhs, &expr.rhs),
            Op::Minus => self.run_sub(&expr.lhs, &expr.rhs),
            Op::Mul => self.run_mul(&expr.lhs, &expr.rhs),
            Op::Div => self.run_div(&expr.lhs, &expr.rhs),
            Op::Eq => self.run_eq(&expr.lhs, &expr.rhs),
            Op::Neq => self.run_neq(&expr.lhs, &expr.rhs),
            Op::Less => self.run_less(&expr.lhs, &expr.rhs),
            Op::LessEq => self.run_less_eq(&expr.lhs, &expr.rhs),
            Op::More => self.run_more(&expr.lhs, &expr.rhs),
            Op::MoreEq => self.run_more_eq(&expr.lhs, &expr.rhs),
            Op::And => self.run_and(&expr.lhs, &expr.rhs),
            Op::Or => self.run_or(&expr.lhs, &expr.rhs),
            _ => Err(Error::InternalError("invalid operator in BinaryOpExpr".to_string())),
        }
    }

    fn run_neg(&mut self, expr: &Expr) -> Result<()> {
        self.run_expr(expr)?;
        self.writeln("NEG")?;
        Ok(())
    }

    fn run_sum(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        self.run_expr(rhs)?;
        let rhs = self.store_tmp("sum_rhs")?;
        self.run_expr(lhs)?;
        self.fetch_tmp(&rhs)?;
        self.writeln("ADD")?;
        Ok(())
    }

    fn run_sub(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        self.run_expr(rhs)?;
        let rhs = self.store_tmp("sub_rhs")?;
        self.run_expr(lhs)?;
        self.fetch_tmp(&rhs)?;
        self.writeln("SUB")?;
        Ok(())
    }

    fn run_mul(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        self.run_expr(rhs)?;
        let rhs = self.store_tmp("mul_rhs")?;
        self.run_expr(lhs)?;
        self.fetch_tmp(&rhs)?;
        self.writeln("MUL")?;
        Ok(())
    }

    fn run_div(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        self.run_expr(rhs)?;
        let rhs = self.store_tmp("div_rhs")?;
        self.run_expr(lhs)?;
        self.fetch_tmp(&rhs)?;
        self.writeln("DIV")?;
        Ok(())
    }

    // lhs == rhs
    fn run_eq(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        self.run_expr(rhs)?;
        let rhs = self.store_tmp("eq_rhs")?;
        self.run_expr(lhs)?;
        self.fetch_tmp(&rhs)?;
        self.writeln("EQ")?;
        Ok(())
    }

    // lhs != rhs
    fn run_neq(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        self.run_expr(rhs)?;
        let rhs = self.store_tmp("eq_rhs")?;
        self.run_expr(lhs)?;
        self.fetch_tmp(&rhs)?;
        self.writeln("EQ")?;
        self.writeln("NEG")?;
        Ok(())
    }

    // !(rhs <= lhs)
    fn run_less(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        self.run_expr(lhs)?;
        let lhs = self.store_tmp("le_lhs")?;
        self.run_expr(rhs)?;
        self.fetch_tmp(&lhs)?;
        self.writeln("LE")?;
        self.writeln("NEG")?;
        Ok(())
    }

    // lhs <= rhs
    fn run_less_eq(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        self.run_expr(rhs)?;
        let rhs = self.store_tmp("le_rhs")?;
        self.run_expr(lhs)?;
        self.fetch_tmp(&rhs)?;
        self.writeln("LE")?;
        Ok(())
    }

    // !(lhs <= rhs)
    fn run_more(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        self.run_expr(rhs)?;
        let rhs = self.store_tmp("gt_rhs")?;
        self.run_expr(lhs)?;
        self.fetch_tmp(&rhs)?;
        self.writeln("LE")?;
        self.writeln("NEG")?;
        Ok(())
    }

    // !(lhs <= rhs) | lhs == rhs
    fn run_more_eq(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        // lhs == rhs
        self.run_expr(rhs)?;
        let rhs = self.store_tmp("geq_rhs")?;
        self.run_expr(lhs)?;
        let lhs = self.store_tmp("geq_lhs")?;
        self.fetch_tmp(&rhs)?;
        self.writeln("EQ")?;

        let eq = self.store_tmp("eq")?;

        // !(lhs <= rhs)
        self.fetch_tmp(&lhs)?;
        self.fetch_tmp(&rhs)?;
        self.writeln("LE")?;
        self.writeln("NEG")?;

        // OR
        self.fetch_tmp(&eq)?;
        self.writeln("OR")?;
        Ok(())
    }

    fn run_and(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        self.run_expr(rhs)?;
        let rhs = self.store_tmp("and_rhs")?;
        self.run_expr(lhs)?;
        self.fetch_tmp(&rhs)?;
        self.writeln("AND")?;
        Ok(())
    }

    fn run_or(&mut self, lhs: &Expr, rhs: &Expr) -> Result<()> {
        self.run_expr(rhs)?;
        let rhs = self.store_tmp("and_rhs")?;
        self.run_expr(lhs)?;
        self.fetch_tmp(&rhs)?;
        self.writeln("OR")?;
        Ok(())
    }

    fn store_tmp(&mut self, key: &str) -> Result<String> {
        let var_name = match self.tmp_var_map.get_mut(key) {
            Some(count) => {
                *count += 1;
                format!("{}{}", key, count)
            }
            None => {
                self.tmp_var_map.insert(key.to_owned(), 1);
                format!("{}{}", key, 1)
            }
        };
        self.writeln(&format!("STORE({})", var_name))?;
        Ok(var_name)
    }

    fn fetch_tmp(&mut self, tmp_name: &str) -> Result<()> {
        self.writeln(&format!("FETCH({})", tmp_name))?;
        Ok(())
    }

    fn write(&mut self, text: &str) -> Result<()> {
        write!(self.out, "{} ", text)?;
        Ok(())
    }

    fn writeln(&mut self, text: &str) -> Result<()> {
        let indent = (0..self.scope).map(|_| "  ").collect::<String>();
        writeln!(self.out, "{}{} ", indent, text)?;
        Ok(())
    }

    fn push_scope(&mut self) { self.scope += 1; }
    fn pop_scope(&mut self)  { self.scope -= 1; }
}

pub mod result {
    use std::fmt::{Display, Formatter};

    pub type Result<T> = std::result::Result<T, Error>;

    #[derive(Debug)]
    pub enum Error {
        IoError(std::io::Error),
        InternalError(String),
    }

    impl Display for Error {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Error::IoError(e) => e.fmt(f),
                Error::InternalError(msg) => write!(f, "internal error; {}", msg),
            }
        }
    }

    impl From<std::io::Error> for Error {
        fn from(e: std::io::Error) -> Self { Self::IoError(e) }
    }
}
