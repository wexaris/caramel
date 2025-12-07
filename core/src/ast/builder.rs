use crate::ast::Module;

pub trait ASTBuilder {
    /// Construct the module's AST.
    fn build_module(self) -> Module;
}
