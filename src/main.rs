mod parse;

use std::path::Path;
use crate::parse::{ASTPrinter, Parser, Tokenizer};

fn main() {
    let file = Path::new("input.txt");
    if !file.is_file() {
        println!("Input file does not exist!");
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

    // Print AST
    let mut printer = ASTPrinter::new();
    printer.print_root(&ast);
}