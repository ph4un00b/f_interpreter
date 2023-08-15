use crate::{
    ast::{Name, ToLiteral, V},
    ast_expression::Expr,
    lexer::Lexer,
    parser::{Errors, Parser, Parsing},
};

#[allow(unused)]
pub fn assert_identifier(
    expr: crate::ast_expression::Expr,
    expected_expr: &str,
    expected_literal: &str,
    expected_string: &str,
) {
    assert_eq!(expr.name(), expected_expr);
    assert_eq!(expr.to_literal(), expected_literal);
    assert_eq!(expr.to_string(), expected_string);
}

#[allow(unused)]
pub fn parse_program(input: &str) -> crate::program_node::Program {
    let lex = Lexer::new(input.into());
    let mut p = Parser::new(lex);
    let program = p.parse_program();
    for err in p.errors() {
        println!("🎈 {err}");
    }
    assert_eq!(p.errors().len(), 0);
    program
}

#[allow(unused)]
pub fn assert_literal_expression(expr: Box<Expr>, expected_value: V) {
    let literal = expr.to_literal();
    let val = match *expr {
        Expr::Literal { token: _, value } => value,
        _ => unreachable!("not *ast.LiteralExpression. got {expr}"),
    };
    assert_eq!(val, expected_value, "Value not {expected_value}. got {val}");
    assert_eq!(
        literal,
        expected_value.to_string(),
        "TokenLiteral not {expected_value}. got {literal}"
    );
}

#[allow(unused)]
fn assert_literal(expr: Box<Expr>) {
    let value = match *expr {
        Expr::Literal { token: _, value: _ } => true,
        _ => unreachable!("not *ast.LiteralExpression. got {expr}"),
    };
    assert!(value);
}
