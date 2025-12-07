use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct SourcePos {
    pub idx: usize,
    pub line: usize,
    pub col: usize,
}

impl SourcePos {
    pub fn new(index: usize, line: usize, column: usize) -> Self {
        assert!(line >= 1);
        assert!(column >= 1);
        Self {
            idx: index,
            line,
            col: column,
        }
    }
}

impl Default for SourcePos {
    fn default() -> Self {
        Self::new(0, 1, 1)
    }
}

impl Display for SourcePos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Eq for SourcePos {}
impl PartialEq for SourcePos {
    fn eq(&self, other: &Self) -> bool {
        let eq_pos = self.idx == other.idx;
        assert!(if eq_pos {
            self.line == other.line
        } else {
            true
        });
        assert!(if eq_pos { self.col == other.col } else { true });
        eq_pos
    }
}
