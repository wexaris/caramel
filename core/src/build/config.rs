use std::path::PathBuf;

/// Configuration for the build process.
pub struct BuildConfig {
    pub input: Vec<PathBuf>,
    pub output: PathBuf,

    pub print_ast: bool,
    pub print_ll: bool,
}

impl BuildConfig {
    #[cfg(target_os = "windows")]
    const DEFAULT_OUTPUT_BIN: &str = "out.exe";
    #[cfg(not(target_os = "windows"))]
    const DEFAULT_OUTPUT_BIN: &str = "out";

    pub fn new() -> Self {
        Self {
            input: Vec::new(),
            output: std::env::current_dir()
                .unwrap_or_default()
                .join(Self::DEFAULT_OUTPUT_BIN),

            print_ast: false,
            print_ll: false,
        }
    }

    /// Adds an input file to the build configuration.
    pub fn input_file(mut self, filepath: PathBuf) -> Self {
        self.input.push(filepath);
        self
    }

    /// Sets the output file path for the build configuration.
    pub fn output_file(mut self, filepath: PathBuf) -> Self {
        self.output = filepath;
        self
    }

    /// Sets whether to print the AST to intermediate files for debugging purposes, e.g. `input.ast`
    pub fn print_ast(mut self, val: bool) -> Self {
        self.print_ast = val;
        self
    }

    /// Sets whether to print the LLVM intermediate IR files for debugging purposes, e.g. `input.ll`
    pub fn print_ll(mut self, val: bool) -> Self {
        self.print_ll = val;
        self
    }
}
