mod cli;

use crate::cli::Args;
use caramel::build::{BuildConfig, BuildDriver};
use clap::Parser;

fn main() {
    let args = Args::parse();
    let config = BuildConfig::from(args);

    let driver = BuildDriver::new(config);

    driver.build();
}
