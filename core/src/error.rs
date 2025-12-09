use crate::source::error::SourceError;
use thiserror::Error;

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error(transparent)]
    SourceError(#[from] SourceError),

    #[error("Failed to write output file: {0}")]
    OutputFileError(#[from] std::io::Error),

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}
