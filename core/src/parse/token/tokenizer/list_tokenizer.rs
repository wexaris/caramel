use crate::parse::span::Span;
use crate::parse::token::Token;
use crate::parse::token::list::TokenList;
use crate::parse::token::tokenizer::Tokenizer;
use crate::parse::token::tokenizer::live_tokenizer::LiveTokenizer;
use crate::parse::token::r#type::TokenType;
use crate::source::code_source::CodeSource;
use crate::source::reader::SourceReader;
use std::rc::Rc;

/// Tokenizer that traverses a token list in a sequential fashion.
pub struct ListTokenizer {
    token_list: TokenList,
    index: usize,
}

impl ListTokenizer {
    pub fn new(token_list: TokenList) -> Self {
        Self {
            token_list,
            index: 0,
        }
    }

    pub fn from_source(reader: SourceReader) -> Self {
        Self::new(LiveTokenizer::new(reader).tokenize_all())
    }

    fn get_token(&self, idx: usize) -> Token {
        assert!(idx <= self.token_list.len());
        self.token_list
            .get(idx)
            .cloned()
            .unwrap_or_else(|| match idx {
                0 => Token::new(TokenType::Eof, Span::default()),
                _ => {
                    let span = self.token_list.get(idx - 1).map_or_else(
                        || Span::default(),
                        |t| Span::from_start(&t.span.next_position(), 0),
                    );
                    Token::new(TokenType::Eof, span)
                }
            })
    }
}

impl Tokenizer for ListTokenizer {
    #[inline]
    fn origin(&self) -> &Rc<dyn CodeSource> {
        &self.token_list.origin
    }

    #[inline]
    fn tokenize_all(self) -> TokenList {
        self.token_list
    }

    fn next_token(&mut self) -> Token {
        assert!(self.index <= self.token_list.len());
        let token = self.get_token(self.index);
        if !token.is_eof() {
            self.index += 1;
        }
        token
    }

    fn peek_token(&mut self) -> Token {
        assert!(self.index <= self.token_list.len());
        let token = self.get_token(self.index);
        token
    }
}

impl Iterator for ListTokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token())
    }
}
