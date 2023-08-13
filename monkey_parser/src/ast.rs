#[derive(Debug, Clone)]
pub struct InstanceCallable {}
#[derive(Debug, Clone)]
pub struct RowCallable {}
#[derive(Debug, Clone)]
pub struct FuncCallable {}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum V {
    Done,
    Return(Box<V>),
    Instance(Box<InstanceCallable>),
    Row(Box<RowCallable>),
    Func(Box<FuncCallable>),
    NativeFunc(String),
    I32(i32),
    F64(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum FnType {
    // None,
    // Constructor,
    // Function,
    // Column,
}

pub trait ToLiteral {
    fn to_literal(&self) -> String;
}

#[cfg(test)]
mod tests {
    use crate::{
        ast_expression::Expr, ast_statements::Statement, program_node::Program, scanner::Tk,
    };

    #[test]
    fn test_return_to_string() {
        let mut program = Program::new();
        program.append(Statement::Return {
            token: Tk::Return,
            value: Expr::None,
        });
        assert_eq!(program.to_string().as_str(), "return;\n");
    }

    #[test]
    fn test_bind_to_string() {
        let mut program = Program::new();
        program.append(Statement::Bind {
            token: Tk::Let,
            identifier: Tk::Ident("myVar".into(), 1),
            initializer: Expr::Ident(Tk::Ident("anotherVar".into(), 1)),
        });
        assert_eq!(program.to_string().as_str(), "let myVar = anotherVar;\n");
    }
}
