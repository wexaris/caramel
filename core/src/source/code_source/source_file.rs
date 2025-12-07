use crate::source::code_source::{CodeSource, CodeSourceType};
use crate::source::error::SourceError;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SourceFile {
    pub filepath: PathBuf,
    content: Vec<u8>,
}

impl SourceFile {
    pub fn read<P: AsRef<Path>>(filepath: P) -> Result<Self, SourceError> {
        let filepath = filepath.as_ref();
        let content = std::fs::read(filepath)
            .map_err(|e| SourceError::FailedToReadFile(filepath.to_owned(), e))?;

        Ok(Self {
            filepath: filepath.to_owned(),
            content,
        })
    }
}

impl CodeSource for SourceFile {
    fn source_type(&self) -> CodeSourceType<'_> {
        CodeSourceType::File(self.filepath.as_path())
    }
    fn source_bytes(&self) -> &[u8] {
        &self.content
    }
}
