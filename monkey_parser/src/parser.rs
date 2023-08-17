use crate::{
    ast::P, ast_expression::Expr, ast_statements::Statement, bool_expr::BooleanExpr,
    func_expr::FunctionExpr, group_expr::GroupExpr, id_expr::IdentExpr, if_expr::IfExpr,
    infix_expr::InfixExpr, int_expr::IntegerExpr, lexer::Lexer, prefix_expr::PrefixExpr,
    program_node::Program, scanner::Tk,
};

pub(crate) trait Parsing {
    fn next_token(&mut self);
    fn parse_program(&mut self) -> Program;
}

pub(crate) trait Assertions {
    /*
     * “assertion functions” nearly all parsers share.
     * Their primary purpose is to enforce
     * the correctness of the order of tokens by checking the
     * type of the next token. Our expectPeek
     * here checks the type of the peekToken and only if the
     *  type is correct does it advance the tokens
     * by calling nextToken.
     *
     * As you’ll see, this is something a parser does a lot.
     */
    fn current_token_isnt_semi(&self) -> bool;
    fn peek_token_isnt_semi(&self) -> bool;
    fn expect_peek(&mut self, expected_token: Tk) -> bool;
    fn expect_peek_identifier(&mut self) -> bool;
    fn optional_semi(&mut self) -> bool;
}

impl Assertions for Parser {
    fn expect_peek_identifier(&mut self) -> bool {
        if let Tk::Ident(_, _) = self.peek_token {
            self.next_token();
            true
        } else {
            self.peek_error(Tk::Ident("name".into(), 0));
            false
        }
    }

    fn expect_peek(&mut self, expected_token: Tk) -> bool {
        if expected_token == self.peek_token {
            self.next_token();
            true
        } else {
            self.peek_error(expected_token);
            false
        }
    }

    fn current_token_isnt_semi(&self) -> bool {
        self.current_token != Tk::Semi
    }

    fn optional_semi(&mut self) -> bool {
        /*
         * If the peekToken is a token.SEMICOLON, we advance so
         * it’s the curToken. If it’s not there, that’s okay too, we don’t add an error to the parser if it’s
         * not there. That’s because we want expression statements to have optional semicolons (which
         * makes it easier to type something like 5 + 5 into the REPL later on).
         */
        self.peek_token == Tk::Semi
    }

    fn peek_token_isnt_semi(&self) -> bool {
        self.peek_token != Tk::Semi
    }
}

pub(crate) trait Errors {
    fn errors(&self) -> Vec<String>;
    /*
     * It doesn’t exit on the first one, potentially saving us the
     * grunt work of rerunning the parsing process again and again to catch all of the syntax errors.
     * That’s pretty helpful - even with line and column numbers missing.
     */
    fn peek_error(&mut self, expected_token: Tk);
    fn append_error(&mut self, msg: String);
}

impl Errors for Parser {
    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, expected_token: Tk) {
        let msg = format!(
            "expected next token to be {expected_token:?}, got {:?} instead",
            self.peek_token
        );
        self.errors.push(msg);
    }

    fn append_error(&mut self, msg: String) {
        self.errors.push(msg);
    }
}
pub struct Parser {
    lex: Lexer,
    pub current_token: Tk,
    /*
     * we need peekToken to decide whether we are at the end
     * of the line or if we are at just the start of an arithmetic expression.
     */
    pub peek_token: Tk,
    errors: Vec<String>,
}

#[allow(unused)]
impl Parser {
    pub fn new(lex: Lexer) -> Self {
        let mut p = Parser {
            lex,
            current_token: Tk::None,
            peek_token: Tk::None,
            errors: vec![],
        };
        p.next_token();
        p.next_token();
        p
    }

    /*
     *  Suppose we’re parsing the following expression statement:
     *
     * 1 + 2 + 3;
     *
     * The big challenge here is not to represent every operator and operand
     * in the resulting AST, but to nest the nodes of the AST correctly.
     * What we want is an AST that (serialized as a string)
     * looks like this:
     * ((1 + 2) + 3)
     *
     * The goal is to have expressions involving operators with a higher precedence
     * to be deeper in the tree than expressions with lower precedence operators.
     * This is accomplished by the precedence value (the argument) in parseExpression
     */
    pub(crate) fn parse_expression(&mut self, precedence: crate::ast::P) -> Option<Expr> {
        let mut left = match &self.current_token {
            Tk::Func => FunctionExpr::parse(self),
            Tk::If => IfExpr::parse(self),
            Tk::OpenParen => GroupExpr::parse(self),
            Tk::False | Tk::True => BooleanExpr::parse(self),
            Tk::Sub | Tk::Bang => PrefixExpr::parse(self),
            Tk::Ident(_, _) => IdentExpr::parse(self),
            //todo: v.clone().as_str() seems ugly 💩❗
            Tk::Num(v, _) => IntegerExpr::parse(self, v.clone().as_str()),
            _ => {
                self.append_error(format!(
                    "no prefix parse function for {} found",
                    self.current_token.clone()
                ));
                //todo: test err❗
                return None;
            }
        };
        /*
         * This condition checks if the left-binding power of the next operator/token
         * is higher than our current right-binding power.
         *
         * If it is, what we parsed so far gets “sucked in” by the next operator,
         * from left to right, and ends up being passed to
         * the infixParseFn of the next operator
         */
        //? while RIGHT_BINDING_POWER < LEFT_BINDING_POWER
        //? THEN suck it to the LEFT SIDE
        while self.peek_token_isnt_semi() && precedence < P::from(&self.peek_token) {
            self.next_token();
            //? in this case the format is lit with if-let 😏
            left = if let Tk::Plus
            | Tk::Sub
            | Tk::Div
            | Tk::Mul
            | Tk::EQ
            | Tk::NotEq
            | Tk::LT
            | Tk::GT = &self.current_token
            {
                //?✅ unwrap is guaranteed❗
                InfixExpr::parse(self, left.unwrap())
            } else {
                return left;
            }
        }
        left
    }
}

impl Parsing for Parser {
    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        if let Some(next_token) = self.lex.next() {
            self.peek_token = next_token;
        } else {
            self.peek_token = Tk::None;
        };
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();
        while self.current_token != Tk::End {
            println!("{:?}", self.current_token.clone());
            if let Some(stmt) = Statement::parse(self) {
                program.append(stmt);
            }
            self.next_token();
        }
        program
    }
}
