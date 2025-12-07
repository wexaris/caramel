use crate::source::code_source::CodeSource;
use crate::source::code_source::source_pos::SourcePos;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Span {
    pub origin: Rc<dyn CodeSource>,
    pub idx: usize,
    pub len: usize,
    pub line: usize,
    pub col: usize,
}

impl Span {
    pub fn new(
        origin: Rc<dyn CodeSource>,
        idx: usize,
        len: usize,
        line: usize,
        col: usize,
    ) -> Self {
        assert!(line >= 1);
        assert!(col >= 1);
        Self {
            origin,
            idx,
            len,
            line,
            col,
        }
    }

    pub fn from_start(start: &SourcePos, len: usize) -> Self {
        Self::new(start.origin.clone(), start.idx, len, start.line, start.col)
    }

    pub fn from_range(start: &SourcePos, end: &SourcePos) -> Self {
        debug_assert!(Rc::ptr_eq(&start.origin, &end.origin));
        debug_assert!(start.idx <= end.idx);
        debug_assert!(start.line <= end.line);
        Self::from_start(start, end.idx - start.idx)
    }

    pub fn get_pos(&self) -> SourcePos {
        SourcePos {
            origin: self.origin.clone(),
            idx: self.idx,
            line: self.line,
            col: self.col,
        }
    }

    pub fn get_pos_after(&self) -> SourcePos {
        let mut pos = self.get_pos();
        pos.idx += self.len;
        pos.col += self.len;
        pos
    }

    pub fn get_raw_str(&self) -> &str {
        self.origin.get_substr(self.idx, self.len)
    }
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.origin, &other.origin)
            && self.idx == other.idx
            && self.len == other.len
            && self.line == other.line
            && self.col == other.col
    }
}
impl Eq for Span {}
