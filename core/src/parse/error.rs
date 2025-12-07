use crate::parse::token::Token;
use thiserror::Error;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Invalid symbol: '{}'", .0.get_raw())]
    InvalidSymbol(Token),

    #[error("{}", unexpected_token_str(.0, .1))]
    UnexpectedToken(Token, Vec<String>),
}

fn unexpected_token_str(token: &Token, expected: &[String]) -> String {
    assert!(
        !expected.is_empty(),
        "missing valid token types for unexpected token error message"
    );
    format!(
        "Unexpected token: {}; expected {}",
        token.get_raw(),
        expected_tts_str(expected)
    )
}

fn expected_tts_str(expected: &[String]) -> String {
    match expected.len() {
        0 => unreachable!(),
        1 => format!("'{:?}'", expected[0]),
        2 => format!("'{:?}' or '{:?}'", expected[0], expected[1]),
        _ => format!(
            "one of '{:?}', {}",
            expected.first().unwrap(),
            expected_tts_str(&expected[1..])
        ),
    }
}
