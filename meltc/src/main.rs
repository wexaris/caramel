use caramel;
use caramel::build::{BuildConfig, BuildDriver};
use std::path::PathBuf;

fn main() {
    let input: PathBuf = std::env::current_dir()
        .unwrap()
        .join("test")
        .join("input.car");

    let config = BuildConfig::new().input_file(input).write_ast(true);

    let driver = BuildDriver::new(config);

    driver.build();
}
