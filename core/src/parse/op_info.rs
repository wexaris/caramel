use crate::ast::BinaryOpType;
use crate::parse::token::TokenType;
use lazy_static::lazy_static;
use std::collections::HashMap;

/// Operator precedence and associativity information.
#[derive(Debug, Clone)]
pub struct OpInfo {
    pub ty: BinaryOpType,
    pub precedence: u32,
    pub associativity: Associativity,
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum Associativity {
    Left,
    Right,
}

impl OpInfo {
    pub fn new(op_type: BinaryOpType, precedence: u32, associativity: Associativity) -> Self {
        Self {
            ty: op_type,
            precedence,
            associativity,
        }
    }

    pub fn for_token(token_type: TokenType) -> &'static Self {
        lazy_static! {
            static ref OP_INFO_MAP: HashMap<TokenType, OpInfo> = {
                let mut m = HashMap::new();
                m.insert(
                    TokenType::Star,
                    OpInfo::new(BinaryOpType::Mul, 10, Associativity::Left),
                );
                m.insert(
                    TokenType::Slash,
                    OpInfo::new(BinaryOpType::Div, 10, Associativity::Left),
                );
                m.insert(
                    TokenType::Percent,
                    OpInfo::new(BinaryOpType::Mod, 10, Associativity::Left),
                );
                m.insert(
                    TokenType::Plus,
                    OpInfo::new(BinaryOpType::Sum, 9, Associativity::Left),
                );
                m.insert(
                    TokenType::Minus,
                    OpInfo::new(BinaryOpType::Sub, 9, Associativity::Left),
                );
                m
            };
        }
        match OP_INFO_MAP.get(&token_type) {
            Some(op_info) => op_info,
            None => unreachable!("Unrecognised binary operator: {}", token_type),
        }
    }
}
