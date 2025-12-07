use std::fmt::{Debug, Display};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TokenType {
    Unknown,
    Eof,

    Ident,

    Fn,
    Var,

    Dot,       // .
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;

    ParenOpen,    // (
    ParenClose,   // )
    BracketOpen,  // [
    BracketClose, // ]
    BraceOpen,    // {
    BraceClose,   // }

    // Binary Operators
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %

    // Unary Operators
    Excl, // !

    // Assignment Operators
    Assign, // =

    // Comparison operators
    Equal,        // ==
    NotEqual,     // !=
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=

    Literal(Literal),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Literal {
    Integer,    // int/uint
    Real,       // float/double
    String,     // string
    Char,       // char
    Bool(bool), // bool
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
