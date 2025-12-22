use crate::ast::{ASTPrinter, BinaryOp, Decl, Expr, FuncCall, FuncDecl, Literal, Module, Stmt};
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

pub mod printer;

pub trait PrintTree {
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()>;
}

macro_rules! indent {
    ($p:expr, { $( $f:expr $(;)? )* }) => {
        {
            $p.push_indent();
            $( $f; )*
            $p.pop_indent();
            Ok(())
        }
    };
}

impl PrintTree for Module {
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()> {
        writeln!(p, "Module: {} [{}]", self.origin, self.span)?;
        indent!(p, {
            self.decls.print_tree(p)?;
        })
    }
}

impl PrintTree for Decl {
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()> {
        match self {
            Decl::Func(decl) => decl.print_tree(p),
        }
    }
}

impl PrintTree for FuncDecl {
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()> {
        let args_str = self
            .params
            .iter()
            .map(|p| p.id.name.as_str())
            .collect::<Vec<_>>()
            .join(",");
        writeln!(
            p,
            "FuncDecl: {}({}) -> {:?} [{}]",
            self.id.name, args_str, self.return_ty.ty, self.span
        )?;
        indent!(p, {
            self.body.print_tree(p)?;
        })
    }
}

impl PrintTree for Stmt {
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()> {
        match self {
            Stmt::Expr(expr) => expr.borrow().print_tree(p),
        }
    }
}

impl PrintTree for Expr {
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()> {
        match self {
            Expr::FuncCall(expr) => expr.print_tree(p),
            Expr::BinaryOp(expr) => expr.print_tree(p),
            Expr::Lit(expr) => expr.print_tree(p),
        }
    }
}

impl PrintTree for FuncCall {
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()> {
        writeln!(p, "FuncCall: {} [{}]", self.id.name, self.span)?;
        indent!(p, {
            self.args.print_tree(p)?;
        })
    }
}

impl PrintTree for BinaryOp {
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()> {
        writeln!(p, "BinaryOp: {:?} [{}]", self.ty, self.span)?;
        indent!(p, {
            self.lhs.print_tree(p)?;
            p.mark_last();
            self.rhs.print_tree(p)?;
        })
    }
}

impl PrintTree for Literal {
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()> {
        writeln!(
            p,
            "Literal: {:?} ({}) [{}]",
            self.ty, self.raw_str, self.span
        )
    }
}

/// -----------------------------------------------------------------
/// ---- Helpers

impl<T: PrintTree> PrintTree for [T] {
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()> {
        for (idx, item) in self.iter().enumerate() {
            if idx == self.len() - 1 {
                p.mark_last();
            }
            item.print_tree(p)?;
        }
        Ok(())
    }
}

impl<T: PrintTree> PrintTree for Rc<T> {
    #[inline]
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()> {
        self.as_ref().print_tree(p)
    }
}

impl<T: PrintTree> PrintTree for RefCell<T> {
    #[inline]
    fn print_tree(&self, p: &mut ASTPrinter) -> std::io::Result<()> {
        self.borrow().print_tree(p)
    }
}
