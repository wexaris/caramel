use crate::source::code_source::{CodeSource, CodeSourceType};

impl CodeSource for String {
    fn source_type(&self) -> CodeSourceType<'_> {
        CodeSourceType::String
    }
    fn source_bytes(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl CodeSource for &str {
    fn source_type(&self) -> CodeSourceType<'_> {
        CodeSourceType::String
    }
    fn source_bytes(&self) -> &[u8] {
        self.as_bytes()
    }
}
