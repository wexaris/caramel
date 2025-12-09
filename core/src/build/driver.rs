use crate::ast::{ASTBuilder, ASTPrinter, Module, PrintTree};
use crate::build::config::BuildConfig;
use crate::error::{CompileError, CompileResult};
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

    pub fn new(config: BuildConfig) -> Self {
        Self { config }
    }

    /// Builds the source files provided in the build configuration.
    pub fn build(self) -> CompileResult<()> {
        for input in &self.config.input {
            let source_file = Rc::new(SourceFile::read(&input)?);
            let source_reader = SourceReader::new(source_file);
            let tokenizer = LiveTokenizer::new(source_reader);
            let parser = SourceParser::new(Box::new(tokenizer));

            let ast = parser.build_module();

            if self.config.print_ast {
                self.print_ast(&input, &ast)?;
            }
        }
        Ok(())
    }

    fn print_ast<P: AsRef<Path>>(&self, input: P, ast: &Module) -> CompileResult<()> {
        let ast_filepath = Self::output_ast_filepath(&input);

        let mut printer = ASTPrinter::new()
            .add_stdout()
            .add_file(ast_filepath)
            .map_err(|e| CompileError::OutputFileError(e))?;

        ast.print_tree(&mut printer)
            .map_err(|e| CompileError::OutputFileError(e))
    }

    fn output_ast_filepath<P: AsRef<Path>>(input: P) -> PathBuf {
        assert!(input.as_ref().is_file());
        let mut out_ast_filepath = input.as_ref().to_owned();
        out_ast_filepath.set_extension(Self::OUT_AST_FILE_EXT);
        out_ast_filepath
    }
}
