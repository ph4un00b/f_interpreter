use crate::{
    ast_01::{Exp, ProgramNode, Stt},
    lexer_09_iterator::Lexer,
    scanner_04_emojis_strings::Tk,
};

struct Parser<'a> {
    lex: &'a mut Lexer,
    current_token: Tk,
    next_token: Tk,
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lex: &'a mut Lexer) -> Self {
        let mut parser = Parser {
            lex,
            current_token: Tk::ILEGAL,
            next_token: Tk::ILEGAL,
            errors: Vec::new(),
        };

        //? Read two tokens, so current_token and peek_token are both set
        parser.next_token();
        parser.next_token();

        parser
    }

    fn peek_error(&mut self, tk: Tk) {
        let message = format!(
            "expected next token to be {:?}, got {:?} instead",
            tk, self.next_token
        );
        self.errors.push(message);
    }

    fn next_token(&mut self) {
        // todo: cuál es la diferencia entre #to_owned y #clone() ❓👀
        self.current_token = self.next_token.to_owned();
        // println!("curr {:?}", self.current_token);
        self.next_token = self.lex.next().unwrap_or(Tk::ILEGAL);
        // println!("peek {:?}", self.peek_token);
    }

    fn parse_program(&mut self) -> ProgramNode {
        let mut root_node = ProgramNode::new();

        //? esto puede ser un iterador❓👀
        while self.current_token != Tk::EOF {
            if let Some(stmt) = self.parse_statement() {
                root_node.statements.push(stmt);
            }
            self.next_token();
        }

        root_node
    }

    fn parse_statement(&mut self) -> Option<Stt> {
        match self.current_token {
            Tk::LET => self.parse_let_statement(),
            Tk::RETURN => self.parse_return_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Stt> {
        /*
         * let <identifier> = <expression>;
         */
        // println!("c {:?}, n {:?}", self.current_token, self.next_token);
        //? son necesarios estos #clone ❓👀
        let result_token = self.current_token.clone();

        match self.next_token {
            Tk::IDENT(_, _) => self.next_token(),
            _ => {
                self.peek_error(Tk::IDENT("".to_string(), 0));
                return None;
            }
        };

        let result_name = self.current_token.clone();

        match self.next_token {
            Tk::ASSIGN => self.next_token(),
            _ => {
                self.peek_error(Tk::ASSIGN);
                return None;
            }
        };

        // TODO: We're skipping the expressions until we encounter a semicolon
        match self.next_token {
            Tk::IDENT(_, _) => self.next_token(),
            Tk::INT(_, _) => self.next_token(),
            _ => {
                // todo: quizá aceptar un vector de tokens, para generar los errores
                // todo: quizá usar un default
                /*
                 * idealmente, buscaría algo como =>
                 * self.peek_error([Tk::IDENT, Tk::INT])
                 *
                 * output =>
                 * expected next token to be IDENT or INT, got STATEMENT instead
                 * o quizá más general
                 *
                 * self.peek_error(EXP)
                 * expected next token to be EXPRESSION, got STATEMENT instead
                 */
                self.peek_error(Tk::IDENT("something".to_string(), 0));
                self.peek_error(Tk::INT("something".to_string(), 0));
                return None;
            }
        };

        let result_id = self.current_token.clone();

        while self.current_token != Tk::SEMI {
            self.next_token();
        }

        Some(Stt::LET {
            token: result_token,
            name: result_name,
            value: Exp::ID { token: result_id },
        })
    }

    fn parse_return_statement(&mut self) -> Option<Stt> {
        /*
         * return <expression>;
         */
        let result_token = self.current_token.clone();

        // TODO: We're skipping the expressions until we encounter a semicolon

        while self.current_token != Tk::SEMI {
            self.next_token();
        }

        Some(Stt::RET {
            token: result_token,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    // #[test]
    // fn test_let_statements_errors() {
    //     // Initialize your Parser and other necessary variables
    //     let input = "
    //     let x 5;
    //     let = 10;
    //     let 838383;
    // ";
    //     // todo: el mut lexer y el &mut lexer, esta medio rraro ❓👀
    //     let mut lexer = Lexer::new(input.to_string());
    //     let mut parser = Parser::new(&mut lexer);

    //     parser.parse_program();

    //     check_errors(parser);
    //     // Rest of your test logic
    // }

    fn check_errors(parser: Parser<'_>) {
        let errors = parser.errors;

        /*
         * It doesn’t exit on the first one, potentially saving us the
         * grunt work of rerunning the parsing process again and again
         *  to catch all of the syntax errors.
         * That’s pretty helpful -
         * even with line and column numbers missing.
         */
        if !errors.is_empty() {
            eprintln!("Parser has {} errors", errors.len());
            for msg in errors {
                eprintln!("Parser error: {}", msg);
            }
            panic!("Test failed");
        }
    }

    // fn check_parser_errors(p: &Parser) {
    //     let errors = p;
    //     if !errors.is_empty() {
    //         eprintln!("Parser has {} errors", errors.len());
    //         for msg in errors {
    //             eprintln!("Parser error: {}", msg);
    //         }
    //         panic!("Test failed");
    //     }
    // }

    #[test]
    fn test_return_statements() {
        let input = "
            return 5;
            return 10;
            return 993322;
        ";

        // todo: el mut lexer y el &mut lexer, esta medio rraro ❓👀
        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        check_errors(parser);
        println!("😀\n{}", program);
        // println!("🧐\n{:?}", program);

        assert_eq!(
            program.statements.len(),
            3,
            "program.Statements does not contain 3 statements"
        );

        let mut statements = program.statements.iter();

        for expected_statement in statements {
            assert_eq!(&Stt::RET { token: Tk::RETURN }, expected_statement);
        }
    }

    #[test]
    fn test_let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";

        // todo: el mut lexer y el &mut lexer, esta medio rraro ❓👀
        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        check_errors(parser);
        // println!("😀\n{}", program);
        println!("🧐\n{:?}", program);
        assert_eq!(
            program.statements.len(),
            3,
            "program.Statements does not contain 3 statements"
        );

        let tests = [
            (Tk::IDENT("x".to_string(), 2), Tk::INT("5".to_string(), 2)),
            (Tk::IDENT("y".to_string(), 3), Tk::INT("10".to_string(), 3)),
            (
                Tk::IDENT("foobar".to_string(), 4),
                Tk::INT("838383".to_string(), 4),
            ),
        ];

        let mut statements = program.statements.iter();

        for (expected_identifier, expected_value) in tests {
            assert_eq!(
                Some(&Stt::LET {
                    token: Tk::LET,
                    name: expected_identifier,
                    value: Exp::ID {
                        token: expected_value
                    }
                }),
                statements.next()
            );
        }
    }
}
