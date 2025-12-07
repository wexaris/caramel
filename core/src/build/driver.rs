use crate::ast::ASTBuilder;
use crate::build::config::BuildConfig;
use crate::parse::parser::SourceParser;
use crate::parse::token::ListTokenizer;
use crate::source::code_source::source_file::SourceFile;
use crate::source::reader::SourceReader;
use std::rc::Rc;

/// The driver for the build process.
pub struct BuildDriver {
    config: BuildConfig,
}

impl BuildDriver {
    pub fn new(config: BuildConfig) -> Self {
        Self { config }
    }

    /// Builds the source files provided in the build configuration.
    pub fn build(self) {
        for input in self.config.input_files {
            let source_file = Rc::new(SourceFile::read(input).unwrap());
            let source_reader = SourceReader::new(source_file);
            let tokenizer = ListTokenizer::from_source(source_reader);
            let parser = SourceParser::new(Box::new(tokenizer));

            let ast = parser.build_module();
            println!("{}: {:?}", ast.origin, ast.stmts);
        }
    }
}
