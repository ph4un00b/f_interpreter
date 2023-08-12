use crate::{
    ast::FnType, ast_expression::Expr, bind_statement::LetStatement, parser::Parser, scanner::Tk,
};

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Statement {
    None(String),
    Expr(Expr),
    Print(Expr),
    Block(Vec<Statement>),
    Row {
        name: Tk,
        super_expr: Expr,
        columns: Vec<Statement>,
    },
    Return {
        keyword: Tk,
        value: Expr,
    },
    Func {
        kind: FnType,
        name: Tk,
        parameters: Vec<Tk>,
        body: Vec<Statement>,
    },
    While {
        condition: Expr,
        body: Box<Statement>,
    },
    Cond {
        condition: Expr,
        then_statement: Box<Statement>,
        else_statement: Box<Statement>,
    },
    Bind {
        token: Tk,
        identifier: Tk,
        initializer: Expr,
    },
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::None(_) => todo!(),
            Statement::Expr(_) => todo!(),
            Statement::Print(_) => todo!(),
            Statement::Block(_) => todo!(),
            Statement::Row {
                name: _,
                super_expr: _,
                columns: _,
            } => todo!(),
            Statement::Return {
                keyword: _,
                value: _,
            } => todo!(),
            Statement::Func {
                kind: _,
                name: _,
                parameters: _,
                body: _,
            } => {
                todo!()
            }
            Statement::While {
                condition: _,
                body: _,
            } => todo!(),
            Statement::Cond {
                condition: _,
                then_statement: _,
                else_statement: _,
            } => todo!(),
            Statement::Bind {
                token,
                identifier,
                initializer,
            } => LetStatement::literal(token, identifier, initializer),
        }
    }
}

impl Statement {
    pub fn parse(p: &mut Parser) -> Option<Statement> {
        match &p.current_token {
            Tk::Let => LetStatement::parse(p),
            Tk::Assign
            | Tk::Plus
            | Tk::Sub
            | Tk::Bang
            | Tk::Mul
            | Tk::Div
            | Tk::LT
            | Tk::GT
            | Tk::LP
            | Tk::RP
            | Tk::LB
            | Tk::RB
            | Tk::Comma
            | Tk::Semi
            | Tk::End
            | Tk::String(_, _)
            | Tk::Ident(_, _)
            | Tk::Num(_, _)
            | Tk::None
            | Tk::Func
            | Tk::If
            | Tk::Else
            | Tk::True
            | Tk::False
            | Tk::Return
            | Tk::EQ
            | Tk::NotEq
            | Tk::Comment
            | Tk::EMOJI4(_, _)
            | Tk::EMOJI3(_, _)
            | Tk::EMOJI2(_, _)
            | Tk::EMOJI1(_, _) => None,
        }
    }
}
