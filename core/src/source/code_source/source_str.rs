use crate::source::code_source::{CodeSource, CodeSourceType};

impl CodeSource for String {
    fn get_type(&self) -> CodeSourceType<'_> {
        CodeSourceType::String
    }
    fn get_bytes(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl CodeSource for &str {
    fn get_type(&self) -> CodeSourceType<'_> {
        CodeSourceType::String
    }
    fn get_bytes(&self) -> &[u8] {
        self.as_bytes()
    }
}
