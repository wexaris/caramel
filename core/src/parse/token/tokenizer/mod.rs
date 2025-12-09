use crate::parse::token::Token;
use crate::parse::token::list::TokenList;
use crate::source::code_source::CodeSource;
use std::rc::Rc;

pub mod list_tokenizer;
pub mod live_tokenizer;
pub mod raw;

#[cfg(test)]
pub mod mock_tokenizer;

pub trait Tokenizer: Sized {
    /// Returns the origin code source being tokenized.
    fn origin(&self) -> &Rc<dyn CodeSource>;

    /// Returns all the tokens from the source.
    /// The returned list is guaranteed to end with an `Eof` token.
    fn tokenize_all(self) -> TokenList;

    /// Returns the next token from the source.
    /// Skips whitespace and comments.
    /// Returns an unknown token for any unrecognized characters.
    /// Continuously returns EOF once the end of the file has been reached.
    fn next_token(&mut self) -> Token;

    /// Returns the next token without advancing the tokenizer position.
    fn peek_token(&mut self) -> Token;
}
