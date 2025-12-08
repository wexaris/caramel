use crate::ast::{ASTBuilder, ASTPrinter, PrintTree};
use crate::build::config::BuildConfig;
use crate::parse::parser::SourceParser;
use crate::parse::token::ListTokenizer;
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
    pub fn build(self) {
        for input in self.config.input {
            let source_file = Rc::new(SourceFile::read(&input).unwrap());
            let source_reader = SourceReader::new(source_file);
            let tokenizer = ListTokenizer::from_source(source_reader);
            let parser = SourceParser::new(Box::new(tokenizer));

            let ast = parser.build_module();

            if self.config.print_ast {
                let ast_filepath = Self::output_ast_filepath(&input);

                let mut tp = ASTPrinter::new()
                    .add_stdout()
                    .add_file(ast_filepath)
                    .expect("Failed to create AST printer");

                ast.print_tree(&mut tp).expect("Failed to print AST");
            }
        }
    }

    fn output_ast_filepath<P: AsRef<Path>>(input: P) -> PathBuf {
        assert!(input.as_ref().is_file());
        let mut out_ast_filepath = input.as_ref().to_owned();
        out_ast_filepath.set_extension(Self::OUT_AST_FILE_EXT);
        out_ast_filepath
    }
}
