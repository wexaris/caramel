use crate::parse::span::Span;
use crate::parse::token::{Literal, Token, TokenType, keywords};
use crate::source::code_source::CodeSource;
use crate::source::code_source::source_pos::SourcePos;
use crate::source::reader::SourceReader;
use std::rc::Rc;

/// The core tokenizer that parses raw source characters into tokens.
#[derive(Debug, Clone)]
pub struct RawTokenizer {
    reader: SourceReader,
}

impl RawTokenizer {
    pub fn new(reader: SourceReader) -> Self {
        Self { reader }
    }

    /// Returns the source origin.
    #[inline]
    pub fn origin(&self) -> &Rc<dyn CodeSource> {
        self.reader.origin()
    }

    /// Returns the current position within the source.
    #[inline]
    pub fn get_pos(&self) -> &SourcePos {
        self.reader.position()
    }

    /// Parses and returns the next token from the source.
    /// Skips whitespace and comments.
    /// Returns an unknown token for any unrecognized characters.
    /// Continuously returns EOF once the end of the file has been reached.
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        // Tokenize EOF
        if self.reader.is_eof() {
            return Token::new(TokenType::Eof, Span::from_start(self.get_pos(), 0));
        }

        // Tokenize identifiers
        if let Some(token) = self.tokenize_keyword_or_ident() {
            return token;
        }

        // Tokenize numbers
        if let Some(token) = self.tokenize_number() {
            return token;
        }

        // Tokenize symbols
        if let Some(token) = self.tokenize_symbol() {
            return token;
        }

        // Tokenize unknown characters
        let token = Token::new(TokenType::Unknown, Span::from_start(self.get_pos(), 1));
        self.reader.bump();
        token
    }

    fn skip_whitespace_and_comments(&mut self) {
        // Skip whitespace
        self.skip_while(|reader| Self::is_whitespace(reader.curr));

        // Skip comments
        match (self.reader.curr, self.reader.next) {
            (b'/', b'/') => {
                self.skip_while(|reader| reader.curr != b'\n');

                self.reader.bump();
                self.skip_whitespace_and_comments(); // try to skip again
            }
            (b'/', b'*') => {
                self.reader.bump();
                self.reader.bump();

                self.skip_while(|reader| reader.curr != b'*' && reader.next != b'/');

                self.reader.bump();
                self.reader.bump();
                self.skip_whitespace_and_comments(); // try to skip again
            }
            _ => {}
        }
    }

    fn skip_while(&mut self, predicate: impl Fn(&SourceReader) -> bool) {
        while predicate(&self.reader) && !self.reader.is_eof() {
            self.reader.bump();
        }
    }

    fn tokenize_keyword_or_ident(&mut self) -> Option<Token> {
        let start_pos = self.reader.position().clone();

        if Self::is_ident_start(self.reader.curr) {
            self.reader.bump();

            while Self::is_ident_cont(self.reader.curr) {
                self.reader.bump();
            }

            let span = self.get_span_from(&start_pos);
            let raw_str = span.raw_str();
            let token_type = keywords::try_get_keyword(&raw_str).unwrap_or(TokenType::Ident);
            return Some(Token::new(token_type, span));
        }

        None
    }

    fn tokenize_number(&mut self) -> Option<Token> {
        let start_pos = self.reader.position().clone();

        if Self::is_digit(self.reader.curr) {
            while Self::is_digit(self.reader.curr) {
                self.reader.bump();
            }

            let span = self.get_span_from(&start_pos);
            return Some(Token::new(TokenType::Literal(Literal::Integer), span));
        }

        None
    }

    fn tokenize_symbol(&mut self) -> Option<Token> {
        let start_pos = self.reader.position().clone();

        let token_type = match self.reader.curr {
            b'.' => {
                self.reader.bump();
                TokenType::Dot
            }
            b',' => {
                self.reader.bump();
                TokenType::Comma
            }
            b':' => {
                self.reader.bump();
                TokenType::Colon
            }
            b';' => {
                self.reader.bump();
                TokenType::Semicolon
            }

            b'(' => {
                self.reader.bump();
                TokenType::ParenOpen
            }
            b')' => {
                self.reader.bump();
                TokenType::ParenClose
            }
            b'[' => {
                self.reader.bump();
                TokenType::BracketOpen
            }
            b']' => {
                self.reader.bump();
                TokenType::BracketClose
            }
            b'{' => {
                self.reader.bump();
                TokenType::BraceOpen
            }
            b'}' => {
                self.reader.bump();
                TokenType::BraceClose
            }

            b'+' => {
                self.reader.bump();
                TokenType::Plus
            }
            b'-' => {
                self.reader.bump();
                TokenType::Minus
            }
            b'*' => {
                self.reader.bump();
                TokenType::Star
            }
            b'/' => {
                self.reader.bump();
                TokenType::Slash
            }
            b'%' => {
                self.reader.bump();
                TokenType::Percent
            }

            b'!' => {
                self.reader.bump();
                match self.reader.curr {
                    b'=' => {
                        self.reader.bump();
                        TokenType::NotEqual
                    }
                    _ => TokenType::Excl,
                }
            }

            b'=' => {
                self.reader.bump();
                match self.reader.curr {
                    b'=' => {
                        self.reader.bump();
                        TokenType::Equal
                    }
                    _ => TokenType::Assign,
                }
            }

            b'<' => {
                self.reader.bump();
                match self.reader.curr {
                    b'=' => {
                        self.reader.bump();
                        TokenType::LessEqual
                    }
                    _ => TokenType::Less,
                }
            }
            b'>' => {
                self.reader.bump();
                match self.reader.curr {
                    b'=' => {
                        self.reader.bump();
                        TokenType::GreaterEqual
                    }
                    _ => TokenType::Greater,
                }
            }

            _ => return None,
        };

        Some(Token::new(token_type, self.get_span_from(&start_pos)))
    }

    fn get_span_from(&self, start: &SourcePos) -> Span {
        Span::from_range(&start, &self.reader.position())
    }

    fn is_whitespace(c: u8) -> bool {
        c.is_ascii_whitespace()
    }

    fn is_ident_start(c: u8) -> bool {
        c.is_ascii_alphabetic() || c == b'_'
    }

    fn is_ident_cont(c: u8) -> bool {
        c.is_ascii_alphanumeric() || c == b'_'
    }

    fn is_digit(c: u8) -> bool {
        c.is_ascii_digit()
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::token::tokenizer::raw::RawTokenizer;
    use crate::source::code_source::CodeSource;
    use crate::source::reader::SourceReader;
    use std::rc::Rc;

    #[test]
    fn new() {
        let src = Rc::new("source") as Rc<dyn CodeSource>;
        let tokenizer = RawTokenizer::new(SourceReader::new(src.clone()));

        assert!(Rc::ptr_eq(tokenizer.origin(), &src));
    }
}
