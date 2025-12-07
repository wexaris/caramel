use crate::parse::span::Span;
use crate::parse::token::Token;
use crate::parse::token::list::TokenList;
use crate::parse::token::tokenizer::Tokenizer;
use crate::parse::token::tokenizer::live_tokenizer::LiveTokenizer;
use crate::parse::token::r#type::TokenType;
use crate::source::code_source::CodeSource;
use crate::source::reader::SourceReader;
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;

/// Tokenizer that traverses a token list in a sequential fashion.
pub struct ListTokenizer {
    token_list: TokenList,
    index: usize,

    // Stack of pinned token indexes used for backtracking
    pin_stack: Rc<RefCell<Vec<usize>>>,
}

impl ListTokenizer {
    pub fn new(reader: SourceReader) -> Self {
        Self {
            token_list: LiveTokenizer::new(reader).tokenize_all(),
            index: 0,
            pin_stack: Rc::new(RefCell::new(Vec::new())),
        }
    }

    fn get_token(&self, idx: usize) -> Token {
        assert!(idx <= self.token_list.len());
        self.token_list.get(idx).cloned().unwrap_or_else(|| {
            let span = self.token_list.get(idx - 1).map_or_else(
                || Span::new(self.token_list.origin.clone(), 0, 0, 1, 1),
                |t| Span::from_start(&t.span.next_position(), 0),
            );
            Token::new(TokenType::Eof, span)
        })
    }
}

impl Tokenizer for ListTokenizer {
    type Pin = ScopedTokenListReaderPin;

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

    fn push_pin(&mut self) -> Self::Pin {
        debug!(
            "TokenListReader.push_pin pin_stack[{}] @{}",
            self.pin_stack.borrow().len(),
            self.get_token(self.index).span.position()
        );
        self.pin_stack.borrow_mut().push(self.index);
        ScopedTokenListReaderPin::new(self)
    }

    fn pop_pin(&mut self) {
        assert!(
            !self.pin_stack.borrow().is_empty(),
            "TokenListReader.pop_pin() called without a matching push_pin()"
        );
        debug!(
            "TokenListReader.pop_pin pin_stack[{}] @{}",
            self.pin_stack.borrow().len() - 1,
            self.get_token(self.index).span.position()
        );
        self.index = self.pin_stack.borrow_mut().pop().unwrap();
    }

    fn ack_pin(&mut self) {
        assert!(
            !self.pin_stack.borrow().is_empty(),
            "TokenListReader.ack_pin() called without a matching push_pin()"
        );
        debug!(
            "TokenListReader.ack_pin pin_stack[{}] @{}",
            self.pin_stack.borrow().len() - 1,
            self.get_token(self.index).span.position()
        );
        self.pin_stack.borrow_mut().pop();
    }
}

impl Iterator for ListTokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token())
    }
}

/// A scope guard that commits the tokenizer position when the end of the scope is reached.
pub struct ScopedTokenListReaderPin {
    pin_stack: Rc<RefCell<Vec<usize>>>,
    pinned_idx: usize,
}

impl ScopedTokenListReaderPin {
    pub fn new(iter: &ListTokenizer) -> Self {
        let pin_stack = iter.pin_stack.clone();
        Self {
            pin_stack,
            pinned_idx: iter.index,
        }
    }
}

impl Drop for ScopedTokenListReaderPin {
    fn drop(&mut self) {
        let mut pin_stack = self.pin_stack.borrow_mut();
        assert!(
            !pin_stack.is_empty(),
            "ScopedTokenListReaderPin.drop() called without a matching push_pin()"
        );

        let pinned_stack_idx = *pin_stack.last().unwrap();
        if self.pinned_idx != pinned_stack_idx {
            debug!(
                "ScopedTokenListReaderPin.drop @{} already acknowledged",
                self.pinned_idx
            );
            return;
        }

        debug!(
            "ScopedTokenListReaderPin.drop pin_stack[{}] @{}",
            pin_stack.len() - 1,
            self.pinned_idx
        );
        pin_stack.pop();
    }
}
