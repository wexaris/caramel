use crate::ast::{ASTPrinter, PrintTree};
use crate::build::config::BuildConfig;
use crate::error::{CompileError, CompileResult};
use crate::llvm::LLVMContext;
use crate::parse::parser::SourceParser;
use crate::parse::token::LiveTokenizer;
use crate::source::code_source::source_file::SourceFile;
use crate::source::reader::SourceReader;
use std::path::{Path, PathBuf};
use std::rc::Rc;

/// The driver for the build process.
pub struct BuildDriver {
    config: BuildConfig,
}

impl BuildDriver {
    const OUT_AST_FILE_EXT: &'static str = "ast";
    const OUT_LL_FILE_EXT: &'static str = "ll";

    pub fn new(config: BuildConfig) -> Self {
        Self { config }
    }

    /// Builds the source files provided in the build configuration.
    pub fn build(self) -> CompileResult<()> {
        for input in &self.config.input {
            let source_file = Rc::new(SourceFile::read(&input)?);
            let source_reader = SourceReader::new(source_file);
            let tokenizer = LiveTokenizer::new(source_reader);
            let mut parser = SourceParser::new(Box::new(tokenizer));

            let ast = parser.parse_module();

            if self.config.print_ast {
                Self::print_ast(&input, &ast)?;
            }

            let llvm_context = LLVMContext::new();
            let llvm_module = Self::generate_llvm_module(&llvm_context, ast)?;

            if self.config.print_ll {
                Self::print_ll(&input, &llvm_module)?;
            }
        }
        Ok(())
    }

    fn generate_llvm_module(
        context: &LLVMContext,
        ast: crate::ast::Module,
    ) -> CompileResult<inkwell::module::Module> {
        let module = context.generate_module(ast);
        module.verify().map_err(|e| CompileError::LLVMError(e))?;
        Ok(module)
    }

    fn print_ast<P: AsRef<Path>>(input: P, module: &crate::ast::Module) -> CompileResult<()> {
        let ast_filepath = Self::tmp_input_out_filepath(&input, Self::OUT_AST_FILE_EXT);

        let mut printer = ASTPrinter::new()
            .add_stdout()
            .add_file(ast_filepath)
            .map_err(|e| CompileError::OutputFileError(e))?;

        module
            .print_tree(&mut printer)
            .map_err(|e| CompileError::OutputFileError(e))
    }

    fn print_ll<P: AsRef<Path>>(input: P, module: &inkwell::module::Module) -> CompileResult<()> {
        let ll_filepath = Self::tmp_input_out_filepath(&input, Self::OUT_LL_FILE_EXT);

        let ll_string = module.print_to_string();

        println!("\n{}", ll_string.to_str().unwrap());
        std::fs::write(ll_filepath, ll_string.to_bytes())
            .map_err(|e| CompileError::OutputFileError(e))?;
        Ok(())
    }

    fn tmp_input_out_filepath<P: AsRef<Path>>(input: P, ext: &str) -> PathBuf {
        assert!(input.as_ref().is_file());
        let mut out_ast_filepath = input.as_ref().to_owned();
        out_ast_filepath.set_extension(ext);
        out_ast_filepath
    }
}
