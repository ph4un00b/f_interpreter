use crate::{ast::V, id_expr::IdentExpr, scanner::Tk};

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Expr::None => todo!(),
            Expr::Ident(name) => IdentExpr::literal(name),
            Expr::This(_) => todo!(),
            Expr::Literal(_) => todo!(),
            Expr::Grouping(_) => todo!(),
            Expr::Unary { op: _, right: _ } => todo!(),
            Expr::Binary {
                left: _,
                op: _,
                right: _,
            } => todo!(),
            Expr::Call {
                callee: _,
                paren: _,
                arguments: _,
            } => todo!(),
            Expr::Logical {
                left: _,
                op: _,
                right: _,
            } => todo!(),
            Expr::Assign {
                identifier: _,
                value: _,
            } => todo!(),
            Expr::GetProp {
                identifier: _,
                prop: _,
            } => todo!(),
            Expr::SetProp {
                identifier: _,
                prop_id: _,
                value: _,
            } => todo!(),
            Expr::Super {
                keyword: _,
                behavior: _,
            } => todo!(),
        }
    }
}
#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Expr {
    None,
    Ident(Tk),
    This(Tk),
    Literal(V),
    Grouping(Box<Expr>),
    Unary {
        op: Tk,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: Tk,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Tk,
        arguments: Vec<Expr>,
    },
    Logical {
        left: Box<Expr>,
        op: Tk,
        right: Box<Expr>,
    },
    Assign {
        identifier: Tk,
        value: Box<Expr>,
    },
    GetProp {
        identifier: Box<Expr>,
        prop: Tk,
    },
    SetProp {
        identifier: Box<Expr>,
        prop_id: Tk,
        value: Box<Expr>,
    },
    Super {
        keyword: Tk,
        behavior: Tk,
    },
}
