use crate::{
    ast::{ToLiteral, V},
    id_expr::IdentExpr,
    infix_expr::InfixExpr,
    int_expr::IntegerExpr,
    prefix_expr::PrefixExpr,
    scanner::Tk,
};

impl ToLiteral for Expr {
    fn to_literal(&self) -> String {
        match self {
            Expr::None => todo!(),
            Expr::Ident(name) => IdentExpr::literal(name),
            Expr::Literal { token, value: _ } => IntegerExpr::literal(token),
            Expr::Unary { op, right: _ } => PrefixExpr::literal(op),
            Expr::Binary { left, op, right } => InfixExpr::literal(left, op, right),
            Expr::This(_) => todo!(),
            Expr::Grouping(_) => todo!(),
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

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::None => todo!(),
            Expr::Ident(name) => IdentExpr::display(f, name),
            Expr::Literal { token: _, value } => match value {
                V::I64(val) => IntegerExpr::display(f, val),
                V::Done
                | V::Return(_)
                | V::Instance(_)
                | V::Row(_)
                | V::Func(_)
                | V::NativeFunc(_)
                | V::I32(_)
                | V::F64(_)
                | V::String(_)
                | V::Bool(_) => unreachable!(),
            },
            Expr::Unary { op, right } => PrefixExpr::display(f, op, right),
            Expr::Binary { left, op, right } => InfixExpr::display(f, left, op, right),
            Expr::This(_) => todo!(),
            Expr::Grouping(_) => todo!(),
            Expr::Call {
                callee: _,
                paren: _,
                arguments: _,
            } => {
                todo!()
            }
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
            } => {
                todo!()
            }
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
    Literal {
        token: Tk,
        value: V,
    },
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
