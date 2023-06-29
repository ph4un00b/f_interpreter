use std::{
    env::{self},
    process,
};

mod cli;
// mod lexer_06_scanner;
// mod lexer_07_emoji_as_token;
// mod lexer_08_emojis_strings;
mod lexer_09_iterator;
// mod scanner_02_track_lines;
// mod scanner_03_emoji_as_token;
mod ast_01;
mod parser_01;
mod scanner_04_emojis_strings;
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
