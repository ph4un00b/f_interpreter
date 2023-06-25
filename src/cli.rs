use std::{
    error::Error,
    fs,
    io::{self, BufRead, Write},
};

pub struct Config {
    file_path: String,
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
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
            } else {
                break;
            }
        }
    } else {
        let contents = fs::read_to_string(config.file_path)?;
        println!("👀 \n\n{contents}");
    };

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
