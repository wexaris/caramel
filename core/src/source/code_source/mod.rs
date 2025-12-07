pub mod source_file;
pub mod source_pos;
pub mod source_str;

use std::fmt::{Debug, Display, Formatter};
use std::path::Path;

pub trait CodeSource: Debug {
    /// Returns the type of the source.
    fn source_type(&self) -> CodeSourceType<'_>;

    /// Returns the raw bytes of the source.
    fn source_bytes(&self) -> &[u8];

    /// Returns a substring of the source.
    fn get_substr(&self, start: usize, len: usize) -> &str {
        assert!(
            start <= self.source_bytes().len(),
            "substring index out of bounds"
        );
        assert!(
            start + len <= self.source_bytes().len(),
            "substring length out of bounds"
        );
        let bytes = &self.source_bytes()[start..start + len];
        std::str::from_utf8(bytes).unwrap()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum CodeSourceType<'a> {
    File(&'a Path),
    String,
}

impl Display for dyn CodeSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.source_type())
    }
}

impl Display for CodeSourceType<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CodeSourceType::File(filepath) => write!(f, "{}", filepath.display()),
            CodeSourceType::String => write!(f, "string"),
        }
    }
}
