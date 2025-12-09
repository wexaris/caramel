use crate::parse::token::Token;
use crate::parse::token::list::TokenList;
use crate::parse::token::tokenizer::Tokenizer;
use crate::parse::token::tokenizer::raw::RawTokenizer;
use crate::parse::token::r#type::TokenType;
use crate::source::code_source::CodeSource;
use crate::source::reader::SourceReader;
use std::rc::Rc;

/// Tokenizer that tokenizes source code in a sequential fashion.
pub struct LiveTokenizer {
    // The active tokenizer
    raw: RawTokenizer,
    tmp_next: Option<(RawTokenizer, Token)>,
}

impl LiveTokenizer {
    pub fn new(reader: SourceReader) -> Self {
        Self {
            raw: RawTokenizer::new(reader),
            tmp_next: None,
        }
    }

    fn tmp_state<T, F: FnOnce(&mut Self) -> T>(&mut self, f: F) -> T {
        let raw = self.raw.clone();
        let ret = f(self);
        self.raw = raw;
        ret
    }
}

impl Tokenizer for LiveTokenizer {
    #[inline]
    fn origin(&self) -> &Rc<dyn CodeSource> {
        &self.raw.origin()
    }

    fn tokenize_all(mut self) -> TokenList {
        let mut token_list = TokenList::new(self.raw.origin().clone());

        loop {
            match self.next() {
                Some(token @ Token { token_type, .. }) => {
                    token_list.push(token);

                    if token_type == TokenType::Eof {
                        break;
                    }
                }
                None => unreachable!("TokenIter.next() should never return None"),
            };
        }

        token_list
    }

    fn next_token(&mut self) -> Token {
        // If a token is cached, return it immediately.
        // Otherwise, read the next token and cache it.
        match self.tmp_next.take() {
            Some((tokenizer, token)) => {
                self.raw = tokenizer;
                token
            }
            None => self.raw.next_token(),
        }
    }

    fn peek_token(&mut self) -> Token {
        // If a token is cached, return it immediately.
        // Otherwise, read the next token and cache it.
        match &self.tmp_next {
            Some((_, token)) => token.clone(),
            None => self.tmp_state(|tk| {
                let token = tk.next_token();
                let tokenizer = tk.raw.clone();
                tk.tmp_next = Some((tokenizer, token.clone()));
                token
            }),
        }
    }
}

impl Iterator for LiveTokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token())
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::span::Span;
    use crate::parse::token::Token;
    use crate::parse::token::tokenizer::Tokenizer;
    use crate::parse::token::tokenizer::live_tokenizer::LiveTokenizer;
    use crate::parse::token::r#type::{Literal, TokenType};
    use crate::source::code_source::CodeSource;
    use crate::source::reader::SourceReader;
    use std::rc::Rc;

    #[test]
    fn new() {
        let source = Rc::new("source") as Rc<dyn CodeSource>;
        let tokenizer = LiveTokenizer::new(SourceReader::new(source.clone()));

        assert!(Rc::ptr_eq(tokenizer.raw.origin(), &source));
    }

    #[test]
    fn tokenize_span_token_first_line() {
        let source = Rc::new("42");
        let tokenizer = LiveTokenizer::new(SourceReader::new(source.clone()));

        let token_list = tokenizer.tokenize_all();

        assert_eq!(token_list.len(), 2, "Token list: {:?}", token_list);
        assert_eq!(
            token_list[0],
            Token::new(
                TokenType::Literal(Literal::Integer),
                Span {
                    idx: 0,
                    len: 2,
                    line: 1,
                    col: 1
                }
            )
        );
        assert_eq!(
            token_list[1],
            Token::new(
                TokenType::Eof,
                Span {
                    idx: 2,
                    len: 0,
                    line: 1,
                    col: 3
                }
            )
        );
    }

    #[test]
    fn tokenize_span_token_last_line() {
        let source = Rc::new("\n\n42");
        let tokenizer = LiveTokenizer::new(SourceReader::new(source.clone()));

        let token_list = tokenizer.tokenize_all();

        assert_eq!(token_list.len(), 2, "Token list: {:?}", token_list);
        assert_eq!(
            token_list[0],
            Token::new(
                TokenType::Literal(Literal::Integer),
                Span {
                    idx: 2,
                    len: 2,
                    line: 3,
                    col: 1
                }
            )
        );
        assert_eq!(
            token_list[1],
            Token::new(
                TokenType::Eof,
                Span {
                    idx: 4,
                    len: 0,
                    line: 3,
                    col: 3
                }
            )
        );
    }

    #[test]
    fn tokenize_span_eof_first_line() {
        let source = Rc::new("");
        let tokenizer = LiveTokenizer::new(SourceReader::new(source.clone()));

        let token_list = tokenizer.tokenize_all();

        assert_eq!(token_list.len(), 1, "Token list: {:?}", token_list);
        assert_eq!(
            token_list[0],
            Token::new(
                TokenType::Eof,
                Span {
                    idx: 0,
                    len: 0,
                    line: 1,
                    col: 1
                }
            )
        );
    }

    #[test]
    fn tokenize_span_eof_last_line() {
        let source = Rc::new("42\n ");
        let tokenizer = LiveTokenizer::new(SourceReader::new(source.clone()));

        let token_list = tokenizer.tokenize_all();

        assert_eq!(token_list.len(), 2, "Token list: {:?}", token_list);
        assert_eq!(
            token_list[1],
            Token::new(
                TokenType::Eof,
                Span {
                    idx: 4,
                    len: 0,
                    line: 2,
                    col: 2
                }
            )
        );
    }

    #[test]
    fn tokenize_span_expr_simple() {
        let source = Rc::new("42+69");
        let tokenizer = LiveTokenizer::new(SourceReader::new(source.clone()));

        let token_list = tokenizer.tokenize_all();

        assert_eq!(token_list.len(), 4, "Token list: {:?}", token_list);
        assert_eq!(
            token_list[0],
            Token::new(
                TokenType::Literal(Literal::Integer),
                Span {
                    idx: 0,
                    len: 2,
                    line: 1,
                    col: 1
                }
            )
        );
        assert_eq!(
            token_list[1],
            Token::new(
                TokenType::Plus,
                Span {
                    idx: 2,
                    len: 1,
                    line: 1,
                    col: 3
                }
            )
        );
        assert_eq!(
            token_list[2],
            Token::new(
                TokenType::Literal(Literal::Integer),
                Span {
                    idx: 3,
                    len: 2,
                    line: 1,
                    col: 4
                }
            )
        );
        assert_eq!(
            token_list[3],
            Token::new(
                TokenType::Eof,
                Span {
                    idx: 5,
                    len: 0,
                    line: 1,
                    col: 6
                }
            )
        );
    }

    #[test]
    fn tokenize_span_expr_commented() {
        let source = Rc::new("//comment\n 42\n // @comment\r\n+/* \n420 */ 69");
        let tokenizer = LiveTokenizer::new(SourceReader::new(source.clone()));

        let token_list = tokenizer.tokenize_all();

        assert_eq!(token_list.len(), 4, "Token list: {:?}", token_list);
        assert_eq!(
            token_list[0],
            Token::new(
                TokenType::Literal(Literal::Integer),
                Span {
                    idx: 11,
                    len: 2,
                    line: 2,
                    col: 2
                }
            )
        );
        assert_eq!(
            token_list[1],
            Token::new(
                TokenType::Plus,
                Span {
                    idx: 28,
                    len: 1,
                    line: 4,
                    col: 1
                }
            )
        );
        assert_eq!(
            token_list[2],
            Token::new(
                TokenType::Literal(Literal::Integer),
                Span {
                    idx: 40,
                    len: 2,
                    line: 5,
                    col: 8
                }
            )
        );
        assert_eq!(
            token_list[3],
            Token::new(
                TokenType::Eof,
                Span {
                    idx: 42,
                    len: 0,
                    line: 5,
                    col: 10
                }
            )
        );
    }

    #[test]
    fn tokenize_span_expr_multiline() {
        let source = Rc::new("42\n+/* \n420 */ 69");
        let tokenizer = LiveTokenizer::new(SourceReader::new(source.clone()));

        let token_list = tokenizer.tokenize_all();

        assert_eq!(
            token_list[0],
            Token::new(
                TokenType::Literal(Literal::Integer),
                Span {
                    idx: 0,
                    len: 2,
                    line: 1,
                    col: 1
                }
            )
        );
        assert_eq!(
            token_list[1],
            Token::new(
                TokenType::Plus,
                Span {
                    idx: 3,
                    len: 1,
                    line: 2,
                    col: 1
                }
            )
        );
        assert_eq!(
            token_list[2],
            Token::new(
                TokenType::Literal(Literal::Integer),
                Span {
                    idx: 15,
                    len: 2,
                    line: 3,
                    col: 8
                }
            )
        );
        assert_eq!(
            token_list[3],
            Token::new(
                TokenType::Eof,
                Span {
                    idx: 17,
                    len: 0,
                    line: 3,
                    col: 10
                }
            )
        )
    }
}
