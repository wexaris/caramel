use crate::ast::{Expr, Literal, Module, Stmt};
use std::fmt::Write;

pub struct TreePrinter {
    buffer: String,
    indent: usize,
}

impl TreePrinter {
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
            indent: 0,
        }
    }

    #[inline]
    pub fn result(self) -> String {
        self.buffer
    }

    pub fn indent<F>(&mut self, f: F) -> Result<(), std::fmt::Error>
    where
        F: FnOnce(&mut TreePrinter) -> Result<(), std::fmt::Error>,
    {
        // print the subtree
        self.push_indent();
        let subtree = self.fmt_subtree(Self::fmt_tree(f)?)?;
        self.buffer.push_str(&subtree);
        self.pop_indent();
        Ok(())
    }

    fn fmt_tree<F>(f: F) -> Result<Self, std::fmt::Error>
    where
        F: FnOnce(&mut TreePrinter) -> Result<(), std::fmt::Error>,
    {
        let mut tp = TreePrinter::new();
        f(&mut tp)?;
        Ok(tp)
    }

    fn fmt_subtree(&mut self, t: Self) -> Result<String, std::fmt::Error> {
        Ok(t.result()
            .lines()
            .map(|line| format!("{}{}", " ".repeat(self.indent * 2), line))
            .collect::<Vec<_>>()
            .join("\n"))
    }

    #[inline]
    fn push_indent(&mut self) {
        self.indent += 1;
    }

    #[inline]
    fn pop_indent(&mut self) {
        self.indent -= 1;
    }
}

impl Write for TreePrinter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        let ident = " ".repeat(self.indent * 2);
        write!(self.buffer, "{}{}", ident, s)
    }
}

pub trait PrintTree {
    fn print_tree(&self, p: &mut TreePrinter) -> std::fmt::Result;
}

impl PrintTree for Module {
    fn print_tree(&self, p: &mut TreePrinter) -> std::fmt::Result {
        writeln!(p, "Module: {}", self.origin)?;
        p.indent(|p| {
            for stmt in &self.stmts {
                stmt.borrow().print_tree(p)?;
            }
            Ok(())
        })
    }
}

impl PrintTree for Stmt {
    fn print_tree(&self, p: &mut TreePrinter) -> std::fmt::Result {
        match self {
            Stmt::Expr(expr) => expr.borrow().print_tree(p),
        }
    }
}

impl PrintTree for Expr {
    fn print_tree(&self, p: &mut TreePrinter) -> std::fmt::Result {
        match self {
            Expr::FuncCall { id, args } => {
                writeln!(p, "FuncCall: {}", id.name)?;
                p.indent(|p| {
                    for arg in args {
                        arg.borrow().print_tree(p)?;
                    }
                    Ok(())
                })
            }
            Expr::Lit(lit) => lit.print_tree(p),
        }
    }
}

impl PrintTree for Literal {
    fn print_tree(&self, p: &mut TreePrinter) -> std::fmt::Result {
        match self {
            Literal::Integer(int) => writeln!(p, "Integer: {}", int),
        }
    }
}
