use crate::parse::token::Literal;
use crate::parse::token::r#type::TokenType;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = HashMap::from([
        ("fn", TokenType::Fn),
        ("var", TokenType::Var),
        ("true", TokenType::Literal(Literal::Bool(true))),
        ("false", TokenType::Literal(Literal::Bool(false))),
    ]);
}

pub fn try_get_keyword(ident: &str) -> Option<TokenType> {
    KEYWORDS.get(&ident).copied()
}
