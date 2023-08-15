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
pub fn assert_literal_expr(expr: Box<Expr>, expected: V) {
    match expected {
        V::I64(_) => assert_int_literal(expr, expected),
        V::Done
        | V::Return(_)
        | V::Instance(_)
        | V::Row(_)
        | V::Func(_)
        | V::NativeFunc(_)
        | V::I32(_)
        | V::F64(_)
        | V::String(_)
        | V::Bool(_) => todo!(),
    }
}

#[allow(unused)]
pub fn assert_int_literal(expr: Box<Expr>, expected: V) {
    let literal = expr.to_literal();
    let value = match *expr {
        Expr::Literal { token: _, value } => value,
        _ => unreachable!("not *ast.IntegerLiteral. got {expr}"),
    };
    assert_eq!(value, expected, "Value not {expected}. got {value}");
    assert_eq!(
        literal,
        expected.to_string(),
        "TokenLiteral not {expected}. got {literal}"
    );
}
