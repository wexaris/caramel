use crate::llvm::codegen::LLVMCodegen;
use inkwell::context::Context;
use inkwell::module::Module;

/// Thread local LLVM context.
pub struct LLVMContext {
    inner: Context,
}

impl LLVMContext {
    pub fn new() -> Self {
        Self {
            inner: Context::create(),
        }
    }

    pub fn generate_module(&self, module: crate::ast::Module) -> Module {
        LLVMCodegen::parse_module(&self.inner, module)
    }
}
