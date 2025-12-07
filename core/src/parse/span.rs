use crate::source::code_source::source_pos::SourcePos;

#[derive(Debug, Clone)]
pub struct Span {
    pub idx: usize,
    pub len: usize,
    pub line: usize,
    pub col: usize,
}

impl Default for Span {
    fn default() -> Self {
        Self::new(0, 0, 1, 1)
    }
}

impl Span {
    pub fn new(idx: usize, len: usize, line: usize, col: usize) -> Self {
        assert!(line >= 1);
        assert!(col >= 1);
        Self {
            idx,
            len,
            line,
            col,
        }
    }

    pub fn from_start(start: &SourcePos, len: usize) -> Self {
        Self::new(start.idx, len, start.line, start.col)
    }

    pub fn from_range(start: &SourcePos, end: &SourcePos) -> Self {
        debug_assert!(start.idx <= end.idx);
        debug_assert!(start.line <= end.line);
        Self::from_start(start, end.idx - start.idx)
    }

    pub fn position(&self) -> SourcePos {
        SourcePos {
            idx: self.idx,
            line: self.line,
            col: self.col,
        }
    }

    pub fn next_position(&self) -> SourcePos {
        let mut pos = self.position();
        pos.idx += self.len;
        pos.col += self.len;
        pos
    }
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
            && self.len == other.len
            && self.line == other.line
            && self.col == other.col
    }
}
impl Eq for Span {}
