use crate::parse::span::Span;
use crate::parse::token::{ListTokenizer, Token, TokenList, TokenType, Tokenizer};
use crate::source::code_source::CodeSource;
use std::rc::Rc;

pub struct MockTokenizer {
    inner: ListTokenizer,
}

impl MockTokenizer {
    pub fn new(tts: Vec<TokenType>) -> Self {
        let token_list = Self::build_token_list(tts);
        Self {
            inner: ListTokenizer::new(token_list),
        }
    }

    fn build_token_list(tts: Vec<TokenType>) -> TokenList {
        let origin = Rc::new("mock".to_string());
        let mut token_list = TokenList::new(origin.clone());
        for tt in tts {
            token_list.push(Token::new(tt, Span::default()));
        }
        token_list
    }
}

impl Tokenizer for MockTokenizer {
    fn origin(&self) -> &Rc<dyn CodeSource> {
        self.inner.origin()
    }

    fn tokenize_all(self) -> TokenList {
        self.inner.tokenize_all()
    }

    fn next_token(&mut self) -> Token {
        self.inner.next_token()
    }

    fn peek_token(&mut self) -> Token {
        self.inner.peek_token()
    }

    fn push_pin(&mut self) -> impl Drop + 'static {
        self.inner.push_pin()
    }

    fn pop_pin(&mut self) {
        self.inner.pop_pin()
    }

    fn ack_pin(&mut self) {
        self.inner.ack_pin()
    }
}
