use std::path::PathBuf;

/// Configuration for the build process.
pub struct BuildConfig {
    pub input_files: Vec<PathBuf>,
    pub output_filepath: PathBuf,

    pub write_ast: bool,
}

impl BuildConfig {
    const DEFAULT_BIN_OUTPUT_FILENAME: &'static str = "out.txt";

    pub fn new() -> Self {
        Self {
            input_files: Vec::new(),
            output_filepath: std::env::current_dir()
                .unwrap_or_default()
                .join(Self::DEFAULT_BIN_OUTPUT_FILENAME),

            write_ast: false,
        }
    }

    /// Adds an input file to the build configuration.
    pub fn input_file(mut self, filepath: PathBuf) -> Self {
        self.input_files.push(filepath);
        self
    }

    /// Sets the output file path for the build configuration.
    pub fn output_file(mut self, filepath: PathBuf) -> Self {
        self.output_filepath = filepath;
        self
    }

    /// Sets whether to write the AST to intermediate files for debugging purposes, e.g. `input.car.ast`
    pub fn write_ast(mut self, val: bool) -> Self {
        self.write_ast = val;
        self
    }
}
