use caramel::build::BuildConfig;
use clap::Parser;
use std::path::PathBuf;

const ABOUT: &str = "Compiler for the Caramel programming language";

#[cfg(target_os = "windows")]
const DEFAULT_OUTPUT_BIN: &str = "out.exe";
#[cfg(not(target_os = "windows"))]
const DEFAULT_OUTPUT_BIN: &str = "out";

#[derive(Parser, Debug)]
#[command(version, about, long_about = ABOUT)]
pub struct Args {
    #[clap(required = true)]
    pub input: Vec<PathBuf>,

    #[clap(short, long, default_value = DEFAULT_OUTPUT_BIN)]
    pub output: PathBuf,

    #[clap(long, default_value_t = false)]
    pub print_ast: bool,
}

impl From<Args> for BuildConfig {
    fn from(args: Args) -> Self {
        BuildConfig {
            input: args.input,
            output: args.output,
            print_ast: args.print_ast,
        }
    }
}
