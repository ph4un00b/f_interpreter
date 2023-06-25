use std::{error::Error, fs, io};

pub struct Config {
    file_path: String,
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    if config.file_path.is_empty() {
        let mut tokens = String::new();
        io::stdin().read_line(&mut tokens)?;
        println!("👀 \n\n{tokens}");
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
