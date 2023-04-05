mod ast;
mod parse;

use std::path::Path;
use crate::ast::Interpreter;
use crate::ast::validate::r#type::TypeValidator;
use crate::parse::{Parser, Tokenizer};

fn main() {
    let file = Path::new("input.txt");
    if !file.is_file() {
        println!("Input file does not exist! Make sure it's in the same directory!");
        return;
    }

    // Tokenize
    let mut tokenizer = match Tokenizer::from_file(file) {
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
    if let Err(e) = validator.validate(&ast) {
        println!("{}", e);
        println!("\nValidation failed; input invalid!");
        return;
    }

    // Run interpreter
    let mut interpreter = Interpreter::new();
    if let Err(e) = interpreter.run(&ast) {
        println!("{}", e);
        return;
    }
}
