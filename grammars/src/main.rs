use std::fs;

use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "csv2.pest"] //? cargo doc
pub struct CSVParser;

//* if you run `cargo doc`, it generates
//* CSVParser::parse and an enum called Rule with a single variant Rule::field
fn main() {
    let successful_parse = CSVParser::parse(Rule::field, "1,2").unwrap();
    let tokens = successful_parse.clone().tokens();

    for token in tokens {
        println!("{:?}", token);
    }

    let successful_parse = CSVParser::parse(Rule::record, "2,3").unwrap();
    let tokens = successful_parse.clone().tokens();

    for token in tokens {
        println!("{:?}", token);
    }

    let unsuccessful_parse: Result<pest::iterators::Pairs<'_, Rule>, pest::error::Error<Rule>> =
        CSVParser::parse(
            Rule::file,
            r#"1,2
3,4
5,6
7,8
"#, //? last line needed
        );
    println!("{:?}", unsuccessful_parse);
}
