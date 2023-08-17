use crate::{
    ast::{FnKind, Name, ToLiteral, V},
    ast_statements::Statement,
    bool_expr::BooleanExpr,
    func_expr::FunctionExpr,
    group_expr::GroupExpr,
    id_expr::IdentExpr,
    if_expr::IfExpr,
    infix_expr::InfixExpr,
    int_expr::IntegerExpr,
    prefix_expr::PrefixExpr,
    scanner::Tk,
};

impl Name for Expr {
    fn name(&self) -> String {
        match self {
            Expr::None => todo!(),
            Expr::Ident(_) => IdentExpr::name(),
            Expr::Literal { token: _, value: _ } => BooleanExpr::name(),
            Expr::If {
                token: _,
                condition: _,
                then: _,
                alternative: _,
            } => IfExpr::name(),
            Expr::Func {
                token: _,
                kind: _,
                params: _,
                body: _,
            } => FunctionExpr::name(),
            Expr::Binary {
                left: _,
                op: _,
                right: _,
            } => todo!(),
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
            Expr::Unary { op: _, right: _ } => todo!(),
        }
    }
}
impl ToLiteral for Expr {
    fn to_literal(&self) -> String {
        match self {
            Expr::None => todo!(),
            Expr::Ident(name) => IdentExpr::literal(name),
            Expr::Literal { token, value: _ } => IntegerExpr::literal(token),
            Expr::Unary { op, right: _ } => PrefixExpr::literal(op),
            Expr::Binary { left, op, right } => InfixExpr::literal(left, op, right),
            Expr::If {
                token,
                condition: _,
                then: _,
                alternative: _,
            } => IfExpr::literal(token),
            Expr::Func {
                token,
                kind: _,
                params: _,
                body: _,
            } => FunctionExpr::literal(token),
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
                V::Bool(val) => BooleanExpr::display(f, *val),
                V::Done
                | V::Return(_)
                | V::Instance(_)
                | V::Row(_)
                | V::Func(_)
                | V::NativeFunc(_)
                | V::I32(_)
                | V::F64(_)
                | V::String(_) => unreachable!(),
            },
            Expr::Unary { op, right } => PrefixExpr::display(f, op, right),
            Expr::Binary { left, op, right } => InfixExpr::display(f, left, op, right),
            Expr::Grouping(expr) => GroupExpr::display(f, expr),
            Expr::If {
                token: _,
                condition,
                then,
                alternative,
            } => IfExpr::display(f, condition, then, alternative.as_deref()),
            Expr::Func {
                token,
                kind,
                params,
                body,
            } => FunctionExpr::display(f, token, kind, params, body),
            Expr::This(_) => todo!(),
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
    Func {
        kind: FnKind,
        //? The 'fn' token
        token: Tk,
        //?  []*Identifier
        params: Vec<Tk>,
        //? *BlockStatement
        body: Box<Statement>,
    },
    Literal {
        token: Tk,
        value: V,
    },
    Grouping(Box<Expr>),
    If {
        token: Tk,
        condition: Box<Expr>,
        //? block-stmt
        then: Box<Statement>,
        //? block-stmt
        alternative: Option<Box<Statement>>,
    },
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
