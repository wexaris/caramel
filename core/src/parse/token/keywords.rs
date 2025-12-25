use crate::parse::token::Literal;
use crate::parse::token::r#type::TokenType;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = HashMap::from([
        ("fn", TokenType::Fn),
        ("var", TokenType::Var),
        ("ret", TokenType::Return),
        ("true", TokenType::Literal(Literal::Bool(true))),
        ("false", TokenType::Literal(Literal::Bool(false))),
        ("i64", TokenType::I64),
        ("i32", TokenType::I32),
        ("i16", TokenType::I16),
        ("i8", TokenType::I8),
        ("u64", TokenType::U64),
        ("u32", TokenType::U32),
        ("u16", TokenType::U16),
        ("u8", TokenType::U8),
        ("f64", TokenType::F64),
        ("f32", TokenType::F32),
        ("string", TokenType::String),
        ("char", TokenType::Char),
        ("bool", TokenType::Bool),
    ]);
}

pub fn try_get_keyword(ident: &str) -> Option<TokenType> {
    KEYWORDS.get(&ident).copied()
}
