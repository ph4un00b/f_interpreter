#![allow(unused)]

mod ast;
mod ast_expression;
mod ast_statements;
mod bind_statement;
mod block_stmt;
mod bool_expr;
mod caller_expr;
mod expr_statement;
mod func_expr;
mod group_expr;
mod id_expr;
mod if_expr;
mod infix_expr;
mod int_expr;
mod lexer;
mod parser;
mod parser_test;
mod prefix_expr;
mod program_node;
mod return_statement;
mod scanner;
mod shunting_yard_algorithm;
use clap::Parser;

#[derive(Parser)]
#[command(name = "f_interpreter")]
#[command(author = "phau")]
#[command(version = "1.0")]
#[command(about = "Does awesome things❗", long_about = None)]
struct Cli {
    #[arg(long)]
    parser: String,
    #[arg(long)]
    one: String,
}

fn main() {
    let cli = Cli::parse();

    println!("two: {:?}", cli.parser);
    println!("one: {:?}", cli.one);
}
