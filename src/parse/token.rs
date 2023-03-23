#![allow(dead_code)]

use crate::parse::Span;

#[derive(Default, Debug, Clone)]
pub struct Token {
    pub tt: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(tt: TokenType, span: Span) -> Self {
        Token { tt, span }
    }

    pub fn is_valid(&self) -> bool {
        self.tt != TokenType::Invalid
    }
}

impl PartialEq<TokenType> for Token {
    fn eq(&self, tt: &TokenType) -> bool {
        self.tt == *tt
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenType {
    Invalid,

    Ident(String),
    Integer(i32),
    Boolean(bool),

    Skip,
    Read,
    Write,

    If,
    Then,
    Else,
    Fi,

    While,
    Do,
    Od,

    Assign,     // :=

    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /

    Eq,         // =
    Neq,        // !=
    LessEq,     // <=
    MoreEq,     // >=
    Less,       // <
    More,       // >
    Or,         // or
    And,        // and
    Neg,        // !

    Semi,       // ;
    Comma,      // ,
    LParen,     // (
    RParen,     // )
}

impl Default for TokenType {
    fn default() -> Self {
        TokenType::Invalid
    }
}

pub type TokenStream = Vec<Token>;
