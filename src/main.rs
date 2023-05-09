mod ast;
mod parse;
mod translate;

use std::path::PathBuf;

use clap::Parser as CliParser;

use crate::ast::Interpreter;
use crate::ast::validate::r#type::TypeValidator;
use crate::parse::{Parser, Tokenizer};
use crate::translate::AMTranslator;

#[derive(Debug, CliParser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Input file
    input: Option<PathBuf>,

    /// Translate input AM code
    #[arg(long)]
    translate_am: bool,

    /// Output file
    #[arg(short, long)]
    out: Option<PathBuf>
}

impl Cli {
    pub fn validate(&mut self) {
        if self.input.is_none() {
            println!("No input specified; fallback to `input.txt`");
            self.input = Some(PathBuf::from("input.txt"));
        }

        if self.out.is_none() {
            self.out = Some(PathBuf::from("out.txt"));
        }
    }
}

fn main() {
    // Parse CLI arguments
    let mut args = Cli::parse();
    args.validate();

    let input = args.input.as_ref().unwrap();
    let output = args.out.as_ref().unwrap();

    if !input.is_file() {
        println!("Input file does not exist! Make sure it's in the same directory!");
        return;
    }

    // Tokenize
    let mut tokenizer = match Tokenizer::from_file(&input) {
        Ok(ts) => ts,
        Err(e) => {
            println!("Failed to read input; {}", e);
            return;
        }
    };
    let ts = tokenizer.tokenize();

    if tokenizer.has_errors() {
        println!("\nTokenization failed; input invalid!");
        return;
    }

    // Parse
    let mut parser = Parser::new(ts);
    let ast = parser.parse();

    if parser.has_errors() {
        println!("\nParsing failed; input invalid!");
        return;
    }

    // Validate
    let mut validator = TypeValidator::new();
    validator.validate(&ast);

    if validator.has_errors() {
        println!("\nValidation failed; input invalid!");
        return;
    }

    // Translate to AM code
    if args.translate_am {
        let mut translator = match AMTranslator::new(&output) {
            Ok(tr) => tr,
            Err(e) => {
                println!("Failed to translate to AM code; {}", e);
                return;
            }
        };

        if let Err(e) = translator.run(&ast) {
            println!("{}", e);
        }

        return;
    }

    // Run interpreter
    let mut interpreter = Interpreter::new();
    if let Err(e) = interpreter.run(&ast) {
        println!("{}", e);
        return;
    }
}
