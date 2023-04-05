use std::path::Path;
use std::str::Chars;
use crate::parse::{FilePos, Token, TokenStream, TokenType};

pub struct Tokenizer {
    source: String,
    error_count: u32,

    is_num: bool,
    num_builder: i32,

    is_ident: bool,
    ident_builder: String,

    span_start: FilePos,
}

impl Tokenizer {
    pub fn from_file(path: &Path) -> std::io::Result<Self> {
        let source = std::fs::read_to_string(path)?;
        Ok(Self::from_source(source))
    }

    pub fn from_source(source: String) -> Self {
        Self {
            source,
            error_count: 0,
            is_num: false,
            num_builder: 0,
            is_ident: false,
            ident_builder: String::new(),
            span_start: FilePos::default(),
        }
    }

    pub fn tokenize(&mut self) -> TokenStream {
        let mut ts = TokenStream::new();

        let mut chars = CharPairIter::from(&self.source);

        while let Some((cc, cn)) = chars.next() {
            let start = chars.pos_curr;

            // Identifier starts
            if !self.is_ident && Self::is_ident_start(cc) {
                self.is_ident = true;
                self.ident_builder.push(cc);
                self.span_start = start;
                continue;
            }
            // Identifier continues
            else if self.is_ident && Self::is_ident_cont(cc) {
                self.ident_builder.push(cc);
                continue;
            }
            // Identifier ended
            else if self.is_ident {
                self.is_ident = false;
                let ident_str = std::mem::replace(&mut self.ident_builder, String::new());

                let tt = match KEYWORD_MAP.get(&ident_str) {
                    Some(keyword) => keyword.clone(),
                    None => TokenType::Ident(ident_str),
                };

                ts.push(Token::new(tt, start - self.span_start));
            }

            // Number starts or continues
            if cc.is_numeric() {
                self.is_num = true;
                self.num_builder *= 10;
                self.num_builder += (cc as i32) - ('0' as i32);
                self.span_start = start;
                continue;
            }
            // Number ended
            else if self.is_num {
                self.is_num = false;
                let num_val = std::mem::replace(&mut self.num_builder, 0);

                ts.push(Token::new(TokenType::Integer(num_val), start - self.span_start));
            }

            // Skip whitespace
            // Only done after ident and literal, since skipping whitespace will merge them
            if cc.is_whitespace() {
                continue;
            }

            // Symbols
            let token = match cc {
                ':' if cn == '=' => {
                    chars.next().unwrap(); // Bump
                    Token::new(TokenType::Assign, chars.pos_next - start)
                },

                '+' => Token::new(TokenType::Plus, chars.pos_next - start),
                '-' => Token::new(TokenType::Minus, chars.pos_next - start),
                '*' => Token::new(TokenType::Star, chars.pos_next - start),
                '/' => Token::new(TokenType::Slash, chars.pos_next - start),

                '=' => Token::new(TokenType::Eq, chars.pos_next - start),
                '!' => {
                    if cn == '=' {
                        chars.next().unwrap(); // Bump
                        Token::new(TokenType::Neq, chars.pos_next - start)
                    }
                    else {
                        Token::new(TokenType::Neg, chars.pos_next - start)
                    }
                },
                '<' => {
                    if cn == '=' {
                        chars.next().unwrap(); // Bump
                        Token::new(TokenType::LessEq, chars.pos_next - start)
                    }
                    else {
                        Token::new(TokenType::Less, chars.pos_next - start)
                    }
                },
                '>' => {
                    if cn == '=' {
                        chars.next().unwrap(); // Bump
                        Token::new(TokenType::MoreEq, chars.pos_next - start)
                    }
                    else {
                        Token::new(TokenType::More, chars.pos_next - start)
                    }
                },

                ';' => Token::new(TokenType::Semi, chars.pos_next - start),
                ',' => Token::new(TokenType::Comma, chars.pos_next - start),
                '(' => Token::new(TokenType::LParen, chars.pos_next - start),
                ')' => Token::new(TokenType::RParen, chars.pos_next - start),

                _ => {
                    self.error_count += 1;
                    println!("{}: invalid symbol: {}", chars.pos_curr, cc);
                    continue;
                }
            };

            ts.push(token);
        }

        ts
    }

    fn is_ident_start(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_ident_cont(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    pub fn has_errors(&self) -> bool {
        self.error_count != 0
    }
}


struct CharPairIter<'a> {
    chars: Chars<'a>,
    next: char,
    pub pos_curr: FilePos,
    pub pos_next: FilePos,
}

impl<'a> CharPairIter<'a> {
    pub fn from(source: &'a str) -> Self {
        let mut chars = source.chars();
        let next = chars.next().unwrap_or('\0');

        Self {
            chars,
            next,
            pos_curr: FilePos::default(),
            pos_next: FilePos::default(),
        }
    }
}

impl<'a> Iterator for CharPairIter<'a> {
    type Item = (char, char);
    fn next(&mut self) -> Option<Self::Item> {
        self.pos_curr = self.pos_next;

        let curr = self.next;
        self.next = self.chars.next()?;

        if curr == '\n' {
            self.pos_next.line += 1;
            self.pos_next.col = 1;
        }
        else if curr != '\0' {
            self.pos_next.col += 1;
        }

        Some((curr, self.next))
    }
}


static KEYWORD_MAP: phf::Map<&'static str, TokenType> = phf::phf_map! {
    "true" => TokenType::Boolean(true),
    "false" => TokenType::Boolean(false),

    "skip" => TokenType::Skip,
    "read" => TokenType::Read,
    "write" => TokenType::Write,

    "if" => TokenType::If,
    "then" => TokenType::Then,
    "else" => TokenType::Else,
    "fi" => TokenType::Fi,

    "while" => TokenType::While,
    "do" => TokenType::Do,
    "od" => TokenType::Od,

    "or" => TokenType::Or,
    "and" => TokenType::And,
};
