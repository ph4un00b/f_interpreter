use std::{
    error::Error,
    fs,
    io::{self, BufRead, Write},
    process::{self, ExitCode},
};

// use crate::lexer_06_scanner::Lexer;
use crate::lexer_07_emoji_as_token::Lexer;

pub struct Config {
    file_path: String,
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    // todo: find out a better api!
    let mut l = Lexer::new("".to_string());
    l.run(config)?;
    Ok(())
}

pub fn parse_configs(mut args: impl Iterator<Item = String>) -> Result<Config, &'static str> {
    let _app_name = args.next().unwrap();

    let file_path = match args.next() {
        Some(arg) => arg,
        None => "".to_string(),
    };

    Ok(Config { file_path })
}

impl Lexer {
    fn run(&mut self, config: Config) -> Result<(), Box<dyn Error>> {
        if config.file_path.is_empty() {
            let input = io::stdin();
            let reader = input.lock();
            let mut lines = reader.lines();

            loop {
                print!("> ");
                io::stdout().flush()?;
                let line = lines.next().transpose()?;
                if let Some(line) = line {
                    println!("👀 \n\n{line}");
                    //? We need to reset this flag in the interactive loop.
                    //? If the user makes a mistake,
                    //? it shouldn’t kill their entire session.
                    self.had_error = false;
                } else {
                    break;
                }
            }
        } else {
            let contents = fs::read_to_string(config.file_path)?;
            println!("👀 \n\n{contents}");
            if self.had_error {
                //? Ideally, we would have an actual abstraction,
                //? some kind of “ErrorReporter” interface that gets
                //? passed to the scanner and parser so that we can
                //? swap out different reporting strategies.
                process::exit(1);
            }
        };

        Ok(())
    }
}
