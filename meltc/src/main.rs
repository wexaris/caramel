mod cli;

use crate::cli::Args;
use caramel::build::{BuildConfig, BuildDriver};
use caramel::error::CompileResult;
use clap::Parser;
use log::LevelFilter;

#[cfg(debug_assertions)]
const LOG_LEVEL: spdlog::Level = spdlog::Level::Debug;
#[cfg(not(debug_assertions))]
const LOG_LEVEL: spdlog::Level = spdlog::Level::Info;

fn main() -> CompileResult<()> {
    init_logger();

    let args = Args::parse();
    let config = BuildConfig::from(args);

    let driver = BuildDriver::new(config);

    driver.build()
}

fn init_logger() {
    let logger = spdlog::default_logger();
    logger.set_level_filter(spdlog::LevelFilter::MoreSevereEqual(LOG_LEVEL));

    spdlog::init_log_crate_proxy().expect("Failed to initialize logger");
    spdlog::log_crate_proxy().set_logger(Some(logger));

    log::set_max_level(LevelFilter::Trace);
}
