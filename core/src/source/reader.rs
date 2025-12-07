use crate::source::code_source::CodeSource;
use crate::source::code_source::source_pos::SourcePos;
use std::rc::Rc;

/// A source tokenizer that tracks line/column position within a code source.
#[derive(Debug, Clone)]
pub struct SourceReader {
    origin: Rc<dyn CodeSource>,
    pos: SourcePos, // Position within the source file
    pub prev: u8,   // Previous symbol
    pub curr: u8,   // Current symbol
    pub next: u8,   // Next symbol
}

impl SourceReader {
    pub fn new(origin: Rc<dyn CodeSource>) -> Self {
        Self {
            prev: 0,
            curr: origin.source_bytes().get(0).cloned().unwrap_or(0),
            next: origin.source_bytes().get(1).cloned().unwrap_or(0),
            pos: SourcePos::default(),
            origin,
        }
    }

    /// Returns the source origin.
    #[inline]
    pub fn origin(&self) -> &Rc<dyn CodeSource> {
        &self.origin
    }

    /// Returns the current position within the source.
    #[inline]
    pub fn position(&self) -> &SourcePos {
        &self.pos
    }

    /// Returns true if the reader is at the end of the source.
    #[inline]
    pub fn is_eof(&self) -> bool {
        self.pos.idx == self.origin.source_bytes().len()
    }

    /// Advances the tokenizer by one symbol.
    /// Provides line/colum tracking within the source file.
    pub fn bump(&mut self) {
        if self.is_eof() {
            assert_eq!(
                self.pos.idx,
                self.origin.source_bytes().len(),
                "SourceReader.index should be the source length at EOF"
            );
            assert_ne!(self.prev, 0, "SourceReader.prev should not be 0 at EOF");
            assert_eq!(self.curr, 0, "SourceReader.curr should be 0 at EOF");
            assert_eq!(self.next, 0, "SourceReader.next should be 0 at EOF");
            return;
        }

        match self.curr {
            b'\n' => {
                self.pos.line += 1;
                self.pos.col = 1;
            }
            b'\r' if self.next != b'\n' => {
                self.pos.line += 1;
                self.pos.col = 1;
            }
            _ => {
                self.pos.col += 1;
            }
        }

        self.bump_index();
    }

    /// Advances the tokenizer by one character without tracking position.
    fn bump_index(&mut self) {
        assert!(
            self.pos.idx < self.origin.source_bytes().len(),
            "SourceReader.index bumping past source length"
        );

        self.pos.idx += 1;
        self.prev = self.curr;
        self.curr = self.next;

        // Index is the current byte, so add 1 to get the next byte
        self.next = self.get_byte_at(self.pos.idx + 1);
    }

    /// Returns the byte at the given index.
    fn get_byte_at(&self, index: usize) -> u8 {
        let bytes = self.origin.source_bytes();
        bytes.get(index).cloned().unwrap_or(0)
    }
}

#[cfg(test)]
mod tests {
    use crate::source::code_source::CodeSource;
    use crate::source::reader::SourceReader;
    use std::rc::Rc;
    use test_case::test_case;

    #[test]
    fn new() {
        const SOURCE: &str = "print!(\"Hello, World!\")";
        let source = Rc::new(SOURCE) as Rc<dyn CodeSource>;
        let reader = SourceReader::new(source.clone());

        assert!(Rc::ptr_eq(&reader.origin, &source));
        assert_eq!(reader.pos.idx, 0);
        assert_eq!(reader.pos.line, 1);
        assert_eq!(reader.pos.col, 1);
        assert_eq!(reader.prev, 0);
        assert_eq!(reader.curr, SOURCE.as_bytes()[0]);
        assert_eq!(reader.next, SOURCE.as_bytes()[1]);
        assert!(!reader.is_eof());
    }

    #[test]
    fn new_empty_source() {
        const SOURCE: &str = "";
        let source = Rc::new(SOURCE) as Rc<dyn CodeSource>;
        let reader = SourceReader::new(source.clone());

        assert!(Rc::ptr_eq(&reader.origin, &source));
        assert_eq!(reader.pos.idx, 0);
        assert_eq!(reader.pos.line, 1);
        assert_eq!(reader.pos.col, 1);
        assert_eq!(reader.prev, 0);
        assert_eq!(reader.curr, 0);
        assert_eq!(reader.next, 0);
        assert!(reader.is_eof());
    }

    #[test_case("\n")]
    #[test_case("\r")]
    #[test_case("\r\n")]
    fn bump_eof(newline: &str) {
        let src = multiline_source(newline);
        let source = Rc::new(src.clone()) as Rc<dyn CodeSource>;
        let mut reader = SourceReader::new(source.clone());

        // Bump the tokenizer to the end of the source
        let source_len = src.len();
        for _ in 0..source_len {
            assert!(!reader.is_eof());
            reader.bump();
        }

        // Should be at EOF
        assert!(reader.is_eof());
        assert_eq!(reader.pos.idx, source_len);

        // Bumping again should do nothing
        reader.bump();
        assert!(reader.is_eof());
        assert_eq!(reader.pos.idx, source_len);
    }

    #[test_case("\n")]
    #[test_case("\r")]
    #[test_case("\r\n")]
    fn bump_bytes(newline: &str) {
        let src = multiline_source(newline);
        let source = Rc::new(src.clone()) as Rc<dyn CodeSource>;
        let mut reader = SourceReader::new(source);

        let mut idx = 0usize;
        while !reader.is_eof() {
            assert_eq!(
                reader.pos.idx, idx,
                "reader position doesn't match source index"
            );
            assert_eq!(
                reader.curr,
                src.as_bytes()[idx],
                "reconstructed code doesn't match source code"
            );
            reader.bump();
            idx += 1;
        }
    }

    #[test_case("\n")]
    #[test_case("\r")]
    #[test_case("\r\n")]
    fn bump_position(newline: &str) {
        let src = multiline_source(newline);
        let source = Rc::new(src.clone()) as Rc<dyn CodeSource>;
        let mut reader = SourceReader::new(source.clone());

        let mut idx = 0usize;
        while !reader.is_eof() {
            assert!(Rc::ptr_eq(&reader.origin, &source));
            assert_eq!(reader.pos.idx, idx);
            validate_position(&reader, newline);

            reader.bump();
            idx += 1;
        }

        assert!(Rc::ptr_eq(&reader.origin, &source));
        assert_eq!(reader.pos.idx, src.len());
        validate_position(&reader, newline);
    }

    fn validate_position(reader: &SourceReader, newline: &str) {
        let source_bytes = reader.origin.source_bytes();
        let current_source_bytes = &source_bytes[..reader.pos.idx];

        let line_count = current_source_bytes
            .windows(newline.len())
            .filter(|&w| w == newline.as_bytes())
            .count();

        assert_eq!(
            reader.pos.line,
            line_count + 1, // we start from line 1
            "wrong line at index {}",
            reader.pos.idx
        );

        let last_line_idx = current_source_bytes
            .windows(newline.len())
            .rposition(|w| w == newline.as_bytes())
            .map_or(0, |idx| idx + newline.len());
        let last_line = &current_source_bytes[last_line_idx..];
        let last_line_len = last_line.len();

        assert_eq!(
            reader.pos.col,
            last_line_len + 1, // we start from column 1
            "wrong column at index {}; processed source text: {}",
            reader.pos.idx,
            str::from_utf8(last_line).unwrap()
        );
    }

    fn multiline_source(newline: &str) -> String {
        format!("{{{newline}\tprint!({newline}\t\"Hello, World!\"\t{newline})}}\t{newline}")
    }
}
