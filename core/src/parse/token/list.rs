use crate::parse::token::Token;
use crate::source::code_source::CodeSource;
use std::fmt::Display;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct TokenList {
    pub origin: Rc<dyn CodeSource>,
    tokens: Vec<Token>,
}

impl TokenList {
    pub fn new(origin: Rc<dyn CodeSource>) -> Self {
        Self {
            origin,
            tokens: Vec::new(),
        }
    }

    pub fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }
}

impl IntoIterator for TokenList {
    type Item = Token;
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.tokens.into_iter()
    }
}

impl Deref for TokenList {
    type Target = [Token];
    fn deref(&self) -> &Self::Target {
        &self.tokens
    }
}

impl Display for TokenList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r"TokenList ({}) [{}]",
            self.origin,
            self.tokens
                .iter()
                .map(|t| t.token_type.to_string())
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
