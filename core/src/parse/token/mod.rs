mod keywords;
mod list;
mod tokenizer;
mod r#type;

pub use crate::parse::token::{
    list::TokenList, tokenizer::Tokenizer, tokenizer::list_tokenizer::ListTokenizer,
    tokenizer::live_tokenizer::LiveTokenizer, tokenizer::raw::RawTokenizer, r#type::*,
};

use crate::parse::span::Span;
use std::fmt::{Display, Formatter};

/// A token representing a single source keyword, identifier, literal, or symbol.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType, span: Span) -> Self {
        Self { token_type, span }
    }

    /// Get the raw bytes used to construct this token.
    #[inline]
    pub fn get_raw(&self) -> &str {
        let source_bytes = self.span.origin.source_bytes();
        let token_bytes = &source_bytes[self.span.idx..self.span.idx + self.span.len];
        str::from_utf8(token_bytes).expect("Invalid UTF-8 in token source")
    }

    /// Returns true if this token represents the end of the source file.
    #[inline]
    pub fn is_eof(&self) -> bool {
        self.token_type == TokenType::Eof
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.token_type, self.span.position())
    }
}
