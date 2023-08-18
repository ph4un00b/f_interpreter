use crate::{
    ast::{Name, ToLiteral, V},
    ast_expression::Expr,
    ast_statements::Statement,
    lexer::Lexer,
    parser::{Errors, Parser, Parsing},
};

#[allow(unused)]
pub fn assert_id_expression(left: Box<Expr>, expected_value: &str) {
    let literal = left.to_literal();
    let val = match *left {
        Expr::Ident(name) => name,
        _ => unreachable!("not *ast.Ident. got {left}"),
    };
    assert_eq!(
        val.to_string(),
        expected_value,
        "Value not {expected_value}. got {val}"
    );
    assert_eq!(
        literal, expected_value,
        "TokenLiteral not {expected_value}. got {literal}"
    );
}

#[allow(unused)]
pub fn assert_infix_stmt(stmt: Statement, expected_left: V, expected_op: &str, expected_right: V) {
    println!("> {stmt}");
    if let Statement::Expr {
        first_token: _,
        expr: Expr::Binary { op, right, left },
    } = stmt
    {
        assert_literal_boxed_expression(left, expected_left);
        assert_eq!(
            &op.to_string(),
            expected_op,
            "exp.Operator is not '{expected_op}'. got {op}",
        );
        assert_literal_boxed_expression(right, expected_right);
    } else {
        unreachable!("not *ast.Statement::Expr. got {stmt:?}")
    };
}

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
pub fn assert_literal_boxed_expression(expr: Box<Expr>, expected_value: V) {
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
