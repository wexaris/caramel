mod keywords;
mod list;
mod tokenizer;
mod r#type;

pub use crate::parse::token::{
    list::TokenList, tokenizer::Tokenizer, tokenizer::list_tokenizer::ListTokenizer,
    tokenizer::live_tokenizer::LiveTokenizer, tokenizer::raw::RawTokenizer, r#type::*,
};

#[cfg(test)]
pub use tokenizer::mock_tokenizer::MockTokenizer;

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

    /// Returns true if this token represents the end of the source file.
    #[inline]
    pub fn is_eof(&self) -> bool {
        matches!(self.token_type, TokenType::Eof)
    }

    /// Returns true if this token represents a binary operator.
    #[inline]
    pub fn is_binary_op(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::Plus
                | TokenType::Minus
                | TokenType::Star
                | TokenType::Slash
                | TokenType::Percent
        )
    }

    /// Returns true if this token comes from an unrecognized symbol.
    #[inline]
    pub fn is_invalid(&self) -> bool {
        matches!(self.token_type, TokenType::Unknown)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.token_type, self.span.position())
    }
}
