use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SourceError {
    #[error("Failed to read file: {}; {}", .0.display(), .1)]
    FailedToReadFile(PathBuf, #[source] std::io::Error),
}
