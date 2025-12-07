use crate::source::code_source::{CodeSource, CodeSourceType};
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct SourcePos {
    pub origin: Rc<dyn CodeSource>,
    pub idx: usize,
    pub line: usize,
    pub col: usize,
}

impl SourcePos {
    pub fn new(origin: Rc<dyn CodeSource>, index: usize, line: usize, column: usize) -> Self {
        Self {
            origin: origin.clone(),
            idx: index,
            line,
            col: column,
        }
    }
}

impl Display for SourcePos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.origin.get_type() {
            CodeSourceType::File(filepath) => {
                write!(f, "{}:{}:{}", filepath.display(), self.line, self.col)
            }
            CodeSourceType::String => write!(f, "{}:{}", self.line, self.col),
        }
    }
}

impl Eq for SourcePos {}
impl PartialEq for SourcePos {
    fn eq(&self, other: &Self) -> bool {
        let eq_pos = Rc::ptr_eq(&self.origin, &other.origin) && self.idx == other.idx;
        assert!(if eq_pos {
            self.line == other.line
        } else {
            true
        });
        assert!(if eq_pos { self.col == other.col } else { true });
        eq_pos
    }
}
