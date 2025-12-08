use crate::ast::{Expr, FuncCall, Literal, Module, Stmt};
use std::cell::RefCell;
use std::fmt::Arguments;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;

pub struct TreePrinter {
    sinks: Vec<Box<dyn Write>>,
    indent: Vec<bool>,
}

impl TreePrinter {
    pub fn new() -> Self {
        Self {
            sinks: Vec::new(),
            indent: Vec::new(),
        }
    }

    pub fn add_sink<O: Write + 'static>(mut self, out: O) -> Self {
        self.sinks.push(Box::new(out));
        self
    }

    pub fn add_stdout(self) -> Self {
        self.add_sink(std::io::stdout())
    }

    pub fn add_file<P: AsRef<Path>>(self, filepath: P) -> std::io::Result<Self> {
        let file = std::fs::File::create(filepath.as_ref())?;
        Ok(self.add_sink(file))
    }

    #[inline]
    pub fn push_indent(&mut self) {
        self.indent.push(true);
    }

    #[inline]
    pub fn pop_indent(&mut self) {
        self.indent.pop();
    }

    pub fn mark_last(&mut self) {
        if let Some(last) = self.indent.last_mut() {
            *last = false;
        }
    }

    fn indent_str(&self) -> String {
        if self.indent.is_empty() {
            return String::new();
        }

        self.indent
            .iter()
            .enumerate()
            .map(|(i, tail)| {
                let is_last = i == self.indent.len() - 1;
                match tail {
                    true if is_last => "├─ ",
                    true => "|  ",
                    false if is_last => "└─ ",
                    false => "   ",
                }
            })
            .collect::<String>()
    }
}

impl Write for TreePrinter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        for sink in &mut self.sinks {
            sink.write_all(buf)?;
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        for sink in &mut self.sinks {
            sink.flush()?;
        }
        Ok(())
    }

    fn write_fmt(&mut self, args: Arguments<'_>) -> std::io::Result<()> {
        let indent = self.indent_str();
        for sink in &mut self.sinks {
            sink.write_all(indent.as_bytes())?;
            sink.write_fmt(args)?;
        }
        Ok(())
    }
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

/// -----------------------------------------------------------------
/// ---- AST Traversal

pub trait PrintTree {
    fn print_tree(&self, p: &mut TreePrinter) -> std::io::Result<()>;
}

impl PrintTree for Module {
    fn print_tree(&self, p: &mut TreePrinter) -> std::io::Result<()> {
        writeln!(p, "Module: {} [{}]", self.origin, self.span)?;
        indent!(p, {
            self.stmts.print_tree(p)?;
        })
    }
}

impl PrintTree for Stmt {
    fn print_tree(&self, p: &mut TreePrinter) -> std::io::Result<()> {
        match self {
            Stmt::Expr(expr) => expr.borrow().print_tree(p),
        }
    }
}

impl PrintTree for Expr {
    fn print_tree(&self, p: &mut TreePrinter) -> std::io::Result<()> {
        match self {
            Expr::FuncCall(expr) => expr.print_tree(p),
            Expr::Lit(expr) => expr.print_tree(p),
        }
    }
}

impl PrintTree for FuncCall {
    fn print_tree(&self, p: &mut TreePrinter) -> std::io::Result<()> {
        writeln!(p, "FuncCall: {} [{}]", self.id.name, self.span)?;
        indent!(p, {
            self.args.print_tree(p)?;
        })
    }
}

impl PrintTree for Literal {
    fn print_tree(&self, p: &mut TreePrinter) -> std::io::Result<()> {
        match self {
            Literal::Integer { val, span } => writeln!(p, "Integer: {} [{}]", val, span),
        }
    }
}

/// -----------------------------------------------------------------
/// ---- AST Traversal Helpers

impl<T: PrintTree> PrintTree for [T] {
    fn print_tree(&self, p: &mut TreePrinter) -> std::io::Result<()> {
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
    fn print_tree(&self, p: &mut TreePrinter) -> std::io::Result<()> {
        self.as_ref().print_tree(p)
    }
}

impl<T: PrintTree> PrintTree for RefCell<T> {
    #[inline]
    fn print_tree(&self, p: &mut TreePrinter) -> std::io::Result<()> {
        self.borrow().print_tree(p)
    }
}
