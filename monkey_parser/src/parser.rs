use crate::{ast_statements::Statement, lexer::Lexer, program_node::Program, scanner::Tk};

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
    fn expect_peek(&mut self, expected_token: Tk) -> bool;
    fn expect_peek_identifier(&mut self) -> bool;
}
pub struct Parser {
    lex: Lexer,
    pub current_token: Tk,
    /*
     * we need peekToken to decide whether we are at the end
     * of the line or if we are at just the start of an arithmetic expression.
     */
    pub peek_token: Tk,
}

impl Parsing for Parser {
    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        if let Some(next_token) = self.lex.next() {
            self.peek_token = next_token;
        } else {
            self.peek_token = Tk::None;
        }
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();
        while self.current_token != Tk::End {
            if let Some(stmt) = Statement::parse(self) {
                program.append(stmt);
            }
            self.next_token();
        }
        program
    }
}

impl Assertions for Parser {
    fn expect_peek_identifier(&mut self) -> bool {
        if let Tk::Ident(_, _) = self.peek_token {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn expect_peek(&mut self, expected_token: Tk) -> bool {
        if expected_token == self.peek_token {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn current_token_isnt_semi(&self) -> bool {
        self.current_token == Tk::Semi
    }
}
#[allow(unused)]
impl Parser {
    pub fn new(lex: Lexer) -> Self {
        let mut p = Parser {
            lex,
            current_token: Tk::None,
            peek_token: Tk::None,
        };
        p.next_token();
        p.next_token();
        p
    }
}
