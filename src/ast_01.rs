use std::fmt::{self};

use crate::scanner_04_emojis_strings::Tk;

trait ASTNode {
    /*
     * TokenLiteral() will be used only
     *  for debugging and testing.
     */
    fn token_literal(&self) -> String;
}

/*
 * These interfaces only contain dummy methods called statementNode and
 * expressionNode respectively. They are not strictly necessary
 * but help us by guiding the compiler and possibly causing
 * it to throw errors when we use a Statement where an Expression
 * should’ve been used, and vice versa
 */

pub trait Statement: ASTNode {
    fn statement_node(&self);
}

pub trait Expression: ASTNode {
    fn expression_node(&self);
}

// #[derive(Debug)]
pub struct ProgramNode {
    // todo: habrá una mejor forma sin BOX ❓👀
    pub statements: Vec<Stt>,
}

impl fmt::Display for ProgramNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in &self.statements {
            writeln!(f, "{}", s)?;
        }
        Ok(())
    }
}

impl fmt::Debug for ProgramNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in &self.statements {
            writeln!(f, "Stt {:?}", s)?;
        }
        Ok(())
    }
}

impl ProgramNode {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }
}

/*
 * This Program node is going to be the root node of every AST our
 * parser produces.
 * Every valid Monkey program is a series of statements.
 * These statements are contained in the Program.Statements,
 * which is just a slice of AST nodes that implement the Statement
 * interface.
 */
impl ASTNode for ProgramNode {
    fn token_literal(&self) -> String {
        if self.statements.is_empty() {
            String::new()
        } else {
            // return self.statements[0].token_literal();
            //? si ya sabemos que no esta empty
            //? vale la pena el unwrap❓
            //? cuánto afecta❓
            self.statements.first().unwrap().token_literal()
        }
    }
}

/*
 * let <identifier> = <expression>;
 */

#[derive(Debug, PartialEq)]
pub enum Stt {
    LET { token: Tk, name: Tk },
    RET { token: Tk },
}

#[derive(Debug, PartialEq)]
pub enum Exp {
    //? token: the first token of the expression
    LET { token: Tk, name: Tk },
}

impl ASTNode for Stt {
    fn token_literal(&self) -> String {
        match self {
            Stt::LET { token, name } => String::from(token.clone()),
            Stt::RET { token } => String::from(token.clone()),
        }
    }
}

impl fmt::Display for Stt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stt::LET { token, name } => {
                writeln!(f, "{} = jamon;", String::from(token.clone()))?;
            }
            Stt::RET { token } => {
                writeln!(f, "{} jamon;", String::from(token.clone()))?;
            }
        };

        Ok(())
    }
}

impl Statement for Stt {
    fn statement_node(&self) {
        todo!()
    }
}

pub struct Identifier {
    token: Tk,
    value: String,
}

impl ASTNode for Identifier {
    fn token_literal(&self) -> String {
        // self.token.
        todo!()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {
        todo!()
    }
}
