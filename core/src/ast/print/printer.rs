use std::fmt::Arguments;
use std::io::Write;
use std::path::Path;

/// A printer that writes ASTs to multiple sinks.
pub struct ASTPrinter {
    sinks: Vec<Box<dyn Write>>,
    indent: Vec<bool>, // levels of indentation (true = trailing line)
}

impl ASTPrinter {
    pub fn new() -> Self {
        Self {
            sinks: Vec::new(),
            indent: Vec::new(),
        }
    }

    /// Adds an output sink to the printer.
    pub fn add_sink<O: Write + 'static>(mut self, out: O) -> Self {
        self.sinks.push(Box::new(out));
        self
    }

    /// Adds a sink that writes to stdout.
    pub fn add_stdout(self) -> Self {
        self.add_sink(std::io::stdout())
    }

    /// Adds a sink that writes to a file.
    pub fn add_file<P: AsRef<Path>>(self, filepath: P) -> std::io::Result<Self> {
        let file = std::fs::File::create(filepath.as_ref())?;
        Ok(self.add_sink(file))
    }

    /// Pushes a new indentation level.
    #[inline]
    pub fn push_indent(&mut self) {
        self.indent.push(true);
    }

    /// Pops an indentation level.
    #[inline]
    pub fn pop_indent(&mut self) {
        self.indent.pop();
    }

    /// Stop drawing tailing lines after the last item.
    pub fn mark_last(&mut self) {
        // stop drawing the trailing line after the last item
        if let Some(last) = self.indent.last_mut() {
            *last = false;
        }
    }

    fn indent_str(&self) -> String {
        if self.indent.is_empty() {
            return String::new();
        }

        self.indent
            .iter()
            .enumerate()
            .map(|(i, is_tail)| {
                let is_end = i == self.indent.len() - 1;
                match is_end {
                    true if *is_tail => "├─ ",
                    true => "└─ ",
                    false if *is_tail => "|  ",
                    false => "   ",
                }
            })
            .collect::<String>()
    }
}

impl Write for ASTPrinter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        for sink in &mut self.sinks {
            sink.write_all(buf)?;
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        for sink in &mut self.sinks {
            sink.flush()?;
        }
        Ok(())
    }

    fn write_fmt(&mut self, args: Arguments<'_>) -> std::io::Result<()> {
        let indent = self.indent_str();
        for sink in &mut self.sinks {
            sink.write_all(indent.as_bytes())?;
            sink.write_fmt(args)?;
        }
        Ok(())
    }
}
