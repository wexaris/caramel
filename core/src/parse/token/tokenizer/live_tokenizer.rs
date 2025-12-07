use crate::parse::token::Token;
use crate::parse::token::list::TokenList;
use crate::parse::token::tokenizer::Tokenizer;
use crate::parse::token::tokenizer::raw::RawTokenizer;
use crate::parse::token::r#type::TokenType;
use crate::source::code_source::CodeSource;
use crate::source::reader::SourceReader;
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;

/// Tokenizer that tokenizes source code in a sequential fashion.
pub struct LiveTokenizer {
    // The active tokenizer
    raw: RawTokenizer,
    // Stack of pinned token iterators used for backtracking
    pin_stack: Rc<RefCell<Vec<RawTokenizer>>>,

    tmp_next: Option<(RawTokenizer, Token)>,
}

impl LiveTokenizer {
    pub fn new(reader: SourceReader) -> Self {
        Self {
            raw: RawTokenizer::new(reader),
            pin_stack: Rc::new(RefCell::new(Vec::new())),
            tmp_next: None,
        }
    }
}

impl Tokenizer for LiveTokenizer {
    type Pin = ScopedTokenLiveReaderPin;

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
            None => {
                // Pin the current tokenizer position to prevent it from advancing.
                self.push_pin();

                let token = self.next_token();
                let tokenizer = self.raw.clone();
                self.tmp_next = Some((tokenizer, token.clone()));

                // Unpin the previous tokenizer position.
                self.pop_pin();
                token
            }
        }
    }

    fn push_pin(&mut self) -> Self::Pin {
        debug!(
            "TokenLiveReader.push_pin pin_stack[{}] : {}",
            self.pin_stack.borrow().len(),
            self.raw.get_pos()
        );
        self.pin_stack.borrow_mut().push(self.raw.clone());
        ScopedTokenLiveReaderPin::new(self)
    }

    fn pop_pin(&mut self) {
        assert!(
            !self.pin_stack.borrow().is_empty(),
            "TokenLiveReader.pop_pin() called without a matching push_pin()"
        );
        debug!(
            "TokenLiveReader.pop_pin pin_stack[{}] : {}",
            self.pin_stack.borrow().len() - 1,
            self.raw.get_pos()
        );
        self.raw = self.pin_stack.borrow_mut().pop().unwrap();
    }

    fn ack_pin(&mut self) {
        assert!(
            !self.pin_stack.borrow().is_empty(),
            "TokenLiveReader.ack_pin() called without a matching push_pin()"
        );
        debug!(
            "TokenLiveReader.ack_pin pin_stack[{}] : {}",
            self.pin_stack.borrow().len() - 1,
            self.raw.get_pos()
        );
        self.pin_stack.borrow_mut().pop();
    }
}

impl Iterator for LiveTokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token())
    }
}

/// A scope guard that commits the tokenizer position when the end of the scope is reached.
pub struct ScopedTokenLiveReaderPin {
    pin_stack: Rc<RefCell<Vec<RawTokenizer>>>,
    pinned_addr: usize,
}

impl ScopedTokenLiveReaderPin {
    pub fn new(iter: &LiveTokenizer) -> Self {
        assert!(!iter.pin_stack.borrow().is_empty());
        assert_eq!(
            iter.pin_stack.borrow().last().unwrap().get_pos(),
            iter.raw.get_pos()
        );
        let pin_stack = iter.pin_stack.clone();
        let pinned_addr = iter.pin_stack.borrow().last().unwrap() as *const RawTokenizer as usize;
        Self {
            pin_stack,
            pinned_addr,
        }
    }
}

impl Drop for ScopedTokenLiveReaderPin {
    fn drop(&mut self) {
        let mut pin_stack = self.pin_stack.borrow_mut();
        assert!(
            !pin_stack.is_empty(),
            "ScopedTokenLiveReaderPin.drop() called without a matching push_pin()"
        );

        let pinned_stack_addr = pin_stack.last().unwrap() as *const RawTokenizer as usize;
        if self.pinned_addr != pinned_stack_addr {
            debug!(
                "ScopedTokenLiveReaderPin.drop @{} already acknowledged",
                self.pinned_addr
            );
            return;
        }

        debug!(
            "ScopedTokenLiveReaderPin.drop pin_stack[{}] @{} : {}",
            pin_stack.len() - 1,
            self.pinned_addr,
            self.pin_stack.borrow().last().unwrap().get_pos()
        );
        pin_stack.pop();
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
        assert!(tokenizer.pin_stack.borrow().is_empty());
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
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
                    origin: source.clone(),
                    idx: 17,
                    len: 0,
                    line: 3,
                    col: 10
                }
            )
        )
    }
}
