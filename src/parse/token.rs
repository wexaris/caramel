#![allow(dead_code)]

use std::fmt::{Display, Formatter};
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
        self.tt != TokenType::EOF
    }
}

impl PartialEq<TokenType> for Token {
    fn eq(&self, tt: &TokenType) -> bool {
        self.tt == *tt
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenType {
    EOF,

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
        TokenType::EOF
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::EOF => write!(f, "EOF"),
            TokenType::Ident(name) => write!(f, "{}", name),
            TokenType::Integer(val) => write!(f, "{}", val),
            TokenType::Boolean(val) => write!(f, "{}", val),
            TokenType::Skip     => write!(f, "skip"),
            TokenType::Read     => write!(f, "read"),
            TokenType::Write    => write!(f, "write"),
            TokenType::If       => write!(f, "if"),
            TokenType::Then     => write!(f, "then"),
            TokenType::Else     => write!(f, "else"),
            TokenType::Fi       => write!(f, "fi"),
            TokenType::While    => write!(f, "while"),
            TokenType::Do       => write!(f, "do"),
            TokenType::Od       => write!(f, "od"),
            TokenType::Assign   => write!(f, ":="),
            TokenType::Plus     => write!(f, "+"),
            TokenType::Minus    => write!(f, "-"),
            TokenType::Star     => write!(f, "*"),
            TokenType::Slash    => write!(f, "/"),
            TokenType::Eq       => write!(f, "="),
            TokenType::Neq      => write!(f, "!="),
            TokenType::LessEq   => write!(f, "<="),
            TokenType::MoreEq   => write!(f, ">="),
            TokenType::Less     => write!(f, "<"),
            TokenType::More     => write!(f, ">"),
            TokenType::Or       => write!(f, "or"),
            TokenType::And      => write!(f, "and"),
            TokenType::Neg      => write!(f, "!"),
            TokenType::Semi     => write!(f, ";"),
            TokenType::Comma    => write!(f, ","),
            TokenType::LParen   => write!(f, "("),
            TokenType::RParen   => write!(f, ")"),
        }
    }
}

pub type TokenStream = Vec<Token>;
