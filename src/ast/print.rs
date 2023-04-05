#![allow(dead_code)]

use crate::ast::*;
use crate::parse::Span;

pub struct ASTPrinter {
    level: u32,
    branches: Vec<bool>,
}

impl ASTPrinter {
    pub fn new() -> Self {
        ASTPrinter { level: 0, branches: vec![] }
    }

    pub fn print(&mut self, ast: &Root) {
        self.print_stmt_list(&ast.stmt_list, "Root");
    }

    fn print_stmt_list(&mut self, stmt_list: &StmtList, name: &str) {
        self.write_and_push(&stmt_list.span, name);
        for (idx, stmt) in stmt_list.iter().enumerate() {
            if idx + 1 == stmt_list.len() as usize {
                self.mark_last();
            }
            self.print_stmt(stmt);
        }
        self.pop();
    }

    fn print_ident_list(&mut self, ident_list: &IdentList) {
        self.push();
        for (idx, id) in ident_list.iter().enumerate() {
            if idx + 1 == ident_list.len() as usize {
                self.mark_last();
            }
            self.print_ident(id);
        }
        self.pop();
    }

    pub fn print_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Skip(stmt) => self.print_skip(stmt),
            Stmt::Read(stmt) => self.print_read(stmt),
            Stmt::Write(stmt) => self.print_write(stmt),
            Stmt::If(stmt) => self.print_if(stmt),
            Stmt::While(stmt) => self.print_while(stmt),
            Stmt::Assign(stmt) => self.print_assign(stmt),
        }
    }

    pub fn print_skip(&mut self, stmt: &SkipStmt) {
        self.write(&stmt.span, "Skip");
    }

    pub fn print_read(&mut self, stmt: &ReadStmt) {
        self.write(&stmt.span, "Read");
        self.print_ident_list(&stmt.var_list);
    }

    pub fn print_write(&mut self, stmt: &WriteStmt) {
        self.write(&stmt.span, "Read");
        self.print_ident_list(&stmt.var_list);
    }

    pub fn print_if(&mut self, stmt: &IfStmt) {
        self.write_and_push(&stmt.span, "If");

        self.print_expr(stmt.check.as_ref());

        self.print_stmt_list(&stmt.branch_true, "If_True");

        self.mark_last();

        if let Some(branch_false) = &stmt.branch_false {
            self.print_stmt_list(branch_false, "If_False");
        }

        self.pop();
    }

    pub fn print_while(&mut self, stmt: &WhileStmt) {
        self.write_and_push(&stmt.span, "While");
        self.print_expr(stmt.check.as_ref());
        self.mark_last();
        self.print_stmt_list(&stmt.branch_true, "While_True");
        self.pop();
    }

    pub fn print_assign(&mut self, stmt: &AssignStmt) {
        self.write_and_push(&stmt.span, "Assign");
        self.print_ident(&stmt.id);
        self.mark_last();
        self.print_expr(stmt.expr.as_ref());
        self.pop();
    }

    pub fn print_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Lit(lit) => self.print_literal(lit),
            Expr::Var(id) => self.print_ident(id),
            Expr::UnaryOp(expr) => self.print_unary_op(expr),
            Expr::BinaryOp(expr) => self.print_binary_op(expr),
        }
    }

    pub fn print_unary_op(&mut self, expr: &UnaryOpExpr) {
        self.write_and_push(&expr.span, expr.op.to_str());
        self.mark_last();
        self.print_expr(&expr.expr);
        self.pop();
    }

    pub fn print_binary_op(&mut self, expr: &BinaryOpExpr) {
        self.write_and_push(&expr.span, expr.op.to_str());
        self.print_expr(&expr.lhs);
        self.mark_last();
        self.print_expr(&expr.rhs);
        self.pop();
    }

    pub fn print_ident(&mut self, id: &Ident) {
        self.write(&id.span, &format!("Ident {}", id.name));
    }

    pub fn print_literal(&mut self, lit: &Literal) {
        match lit.value {
            Value::Integer(val) => self.write(&lit.span, &format!("Integer {}", val)),
            Value::Boolean(val) => self.write(&lit.span, &format!("Boolean {}", val)),
        }
    }

    fn write(&self, span: &Span, txt: &str) {
        println!("{} <{}> [{}]", self.curr_indent(), txt, span);
    }

    fn write_and_push(&mut self, span: &Span, print: &str) {
        self.write(span, print);
        self.push();
    }

    fn curr_indent(&self) -> String {
        if self.level == 0 {
            return String::new();
        }

        let indent = (0..self.level * 2)
            .enumerate()
            .map(|(idx, _)| {
                if idx % 2 == 0 { return ' ' }

                let idx = (idx - 1) / 2;
                if idx == (self.level - 1) as usize { return '└' }
                else if !self.branches[idx] { '|' }
                else { ' ' }
            })
            .collect::<String>();

        indent
    }

    fn push(&mut self) {
        self.level += 1;
        self.branches.push(false);
    }

    fn pop(&mut self) {
        self.level -= 1;
        self.branches.pop();
    }

    fn mark_last(&mut self) {
        if let Some(last) = self.branches.last_mut() {
            *last = true;
        }
    }
}
