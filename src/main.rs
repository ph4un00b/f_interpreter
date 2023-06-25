use std::{
    env::{self},
    process,
};

mod cli;
mod lexer_06_scanner;
mod scanner_01;
//? usage: unix - cargo run -- ./codes/first.monkey
//? usage: ps - cargo run -- ./codes/first.monkey
fn main() {
    let config = cli::parse_configs(env::args())
        //
        .unwrap_or_else(|err| {
            eprintln!("😱 Problem parsing arguments: {err}");
            process::exit(1)
        });

    if let Err(e) = cli::run(config) {
        eprintln!("❌❌ Application error: {e}");
        process::exit(1);
    }
}
