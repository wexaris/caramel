#![allow(dead_code)]

use crate::ast::*;

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
        self.write_and_push(name);
        for (idx, stmt) in stmt_list.iter().enumerate() {
            if idx + 1 == stmt_list.len() as usize {
                self.mark_last();
            }
            self.print_stmt(stmt);
        }
        self.pop();
    }

    fn print_ident_list(&mut self, ident_list: &IdentList, name: &str) {
        self.write_and_push(name);
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
            Stmt::Skip => self.print_skip(),
            Stmt::Read(stmt) => self.print_read(stmt),
            Stmt::Write(stmt) => self.print_write(stmt),
            Stmt::If(stmt) => self.print_if(stmt),
            Stmt::While(stmt) => self.print_while(stmt),
            Stmt::Assign(stmt) => self.print_assign(stmt),
        }
    }

    pub fn print_skip(&mut self) {
        self.write("Skip");
    }

    pub fn print_read(&mut self, stmt: &ReadStmt) {
        self.print_ident_list(&stmt.var_list, "Read");
    }

    pub fn print_write(&mut self, stmt: &WriteStmt) {
        self.print_ident_list(&stmt.var_list, "Write");
    }

    pub fn print_if(&mut self, stmt: &IfStmt) {
        self.write_and_push("If");

        self.print_expr(stmt.check.as_ref());

        self.print_stmt_list(&stmt.branch_true, "If_True");

        self.mark_last();

        if let Some(branch_false) = &stmt.branch_false {
            self.print_stmt_list(branch_false, "If_False");
        }

        self.pop();
    }

    pub fn print_while(&mut self, stmt: &WhileStmt) {
        self.write_and_push("While");
        self.print_expr(stmt.check.as_ref());
        self.mark_last();
        self.print_stmt_list(&stmt.branch_true, "While_True");
        self.pop();
    }

    pub fn print_assign(&mut self, stmt: &AssignStmt) {
        self.write_and_push("Assign");
        self.print_ident(&stmt.id);
        self.mark_last();
        self.print_expr(stmt.expr.as_ref());
        self.pop();
    }

    pub fn print_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Lit(lit) => self.print_literal(lit),
            Expr::Var(id) => self.print_ident(id),
            Expr::UnaryOp(op, expr) => {
                self.print_op(op);
                self.push();
                self.mark_last();
                self.print_expr(expr);
                self.pop();
            }
            Expr::BinaryOp(op, lhs, rhs) => {
                self.print_op(op);
                self.push();
                self.print_expr(lhs);
                self.mark_last();
                self.print_expr(rhs);
                self.pop();
            }
        }
    }

    pub fn print_op(&mut self, op: &Op) {
        match op {
            Op::Neg => self.write("Negation"),
            Op::Plus => self.write("Plus"),
            Op::Minus => self.write("Minus"),
            Op::Mul => self.write("Mul"),
            Op::Div => self.write("Div"),
            Op::Eq => self.write("Eq"),
            Op::Neq => self.write("Neq"),
            Op::Less => self.write("Less"),
            Op::LessEq => self.write("LessEq"),
            Op::More => self.write("More"),
            Op::MoreEq => self.write("MoreEq"),
            Op::And => self.write("And"),
            Op::Or => self.write("Or"),
        }
    }

    pub fn print_ident(&mut self, id: &Ident) {
        self.write(&format!("Ident {}", id.name));
    }

    pub fn print_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::Integer(val) => self.write(&format!("Integer {}", val)),
            Literal::Boolean(val) => self.write(&format!("Boolean {}", val)),
        }
    }

    fn write(&self, txt: &str) {
        println!("{} <{}>", self.curr_indent(), txt);
    }

    fn write_and_push(&mut self, print: &str) {
        self.write(print);
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
