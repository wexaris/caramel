#![allow(dead_code)]

use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Span {
    pub lo: FilePos,
    pub hi: FilePos,
}

impl Span {
    pub fn new(lo: FilePos, hi: FilePos) -> Self {
        Span { lo, hi }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            lo: FilePos::default(),
            hi: FilePos::default(),
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.lo, self.hi)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct FilePos {
    pub line: u32,
    pub col: u32,
}

impl Default for FilePos {
    fn default() -> Self {
        Self { line: 1, col: 1 }
    }
}

impl std::ops::Sub for FilePos {
    type Output = Span;
    fn sub(self, rhs: Self) -> Self::Output {
        Span { lo: rhs, hi: self }
    }
}

impl Display for FilePos {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}
