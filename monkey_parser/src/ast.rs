use crate::scanner::Tk;

#[derive(PartialEq, PartialOrd)]
pub enum P {
    Lowest,
    //?     public static final int ASSIGNMENT  = 1;
    Equals,
    LessGreater,
    //?     public static final int CONDITIONAL = 2;
    // Cond,
    //?     public static final int SUM         = 3;
    Sum,
    //?     public static final int PRODUCT     = 4;
    Product,
    //?     public static final int EXPONENT    = 5;
    // Pow,
    //?     public static final int PREFIX      = 6;
    Prefix,
    //?     public static final int POSTFIX     = 7;
    // Post,
    //?     public static final int CALL        = 8;
    // Call,
}

impl From<&Tk> for P {
    fn from(value: &Tk) -> P {
        match value {
            Tk::EQ => P::Equals,
            Tk::NotEq => P::Equals,
            Tk::LT => P::LessGreater,
            Tk::GT => P::LessGreater,
            Tk::Plus => P::Sum,
            Tk::Sub => P::Sum,
            Tk::Div => P::Product,
            Tk::Mul => P::Product,
            _ => P::Lowest,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstanceCallable {}
#[derive(Debug, Clone, PartialEq)]
pub struct RowCallable {}
#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallable {}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum V {
    Done,
    Return(Box<V>),
    Instance(Box<InstanceCallable>),
    Row(Box<RowCallable>),
    Func(Box<FuncCallable>),
    NativeFunc(String),
    I32(i32),
    I64(i64),
    F64(f64),
    String(String),
    Bool(bool),
}

impl std::fmt::Display for V {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            V::Done => write!(f, "done!")?,
            V::Return(val) => write!(f, "ret {:?}", val)?,
            // V::Func(function) => write!(f, "{}", function)?,
            V::NativeFunc(fn_name) => match fn_name.as_str() {
                "clock" => write!(f, "<native {}>", fn_name)?,
                _ => write!(f, "#{}", fn_name)?,
            },
            V::I64(val) => write!(f, "{}", val)?,
            V::I32(val) => write!(f, "{}", val)?,
            V::F64(val) => write!(f, "{}", val)?,
            V::String(val) => write!(f, "{}", val)?,
            V::Bool(val) => write!(f, "{}", val)?,
            V::Instance(_) => todo!(),
            V::Row(_) => todo!(),
            V::Func(_) => todo!(),
            // V::Row(obj) => write!(f, "{}", obj)?,
            // V::Instance(obj) => write!(f, "{}", obj)?,
        }
        Ok(())
    }
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

pub trait Name {
    fn name(&self) -> String;
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
        assert_eq!(program.to_string().as_str(), "return;");
    }

    #[test]
    fn test_bind_to_string() {
        let mut program = Program::new();
        program.append(Statement::Bind {
            token: Tk::Let,
            identifier: Tk::Ident("myVar".into(), 1),
            initializer: Expr::Ident(Tk::Ident("anotherVar".into(), 1)),
        });
        assert_eq!(program.to_string().as_str(), "let myVar = anotherVar;");
    }
}
