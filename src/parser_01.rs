use crate::{
    ast_01::{LetStatement, ProgramNode},
    lexer_09_iterator::Lexer,
    scanner_04_emojis_strings::Tk,
};

struct Parser<'a> {
    lex: &'a mut Lexer,
    current_token: Tk,
    peek_token: Tk,
}

impl<'a> Parser<'a> {
    fn new(lex: &'a mut Lexer) -> Self {
        let mut parser = Parser {
            lex,
            current_token: Tk::ILEGAL,
            peek_token: Tk::ILEGAL,
        };

        //? Read two tokens, so current_token and peek_token are both set
        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        // todo: cuál es la diferencia entre #to_owned y #clone() ❓👀
        self.current_token = self.peek_token.to_owned();
        // println!("curr {:?}", self.current_token);
        self.peek_token = self.lex.next().unwrap_or(Tk::ILEGAL);
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

    fn parse_statement(&mut self) -> Option<LetStatement> {
        match self.current_token {
            Tk::LET => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        /*
         * let <identifier> = <expression>;
         */
        println!("c {:?}, n {:?}", self.current_token, self.peek_token);
        //? son necesarios estos #clone ❓👀
        let mut result = LetStatement {
            token: self.current_token.clone(),
            name: Tk::ILEGAL,
        };

        match self.peek_token {
            Tk::IDENT(_, _) => self.next_token(),
            _ => return None,
        };

        result.name = self.current_token.clone();

        match self.peek_token {
            Tk::ASSIGN => self.next_token(),
            _ => return None,
        };

        // TODO: We're skipping the expressions until we encounter a semicolon
        while self.current_token != Tk::SEMI {
            self.next_token();
        }

        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

        assert_eq!(
            program.statements.len(),
            3,
            "program.Statements does not contain 3 statements"
        );

        let tests = [
            Tk::IDENT("x".to_string(), 2),
            Tk::IDENT("y".to_string(), 3),
            Tk::IDENT("foobar".to_string(), 4),
        ];

        let mut statements = program.statements.iter();

        for expected_identifier in tests {
            assert_eq!(
                Some(&LetStatement {
                    token: Tk::LET,
                    name: expected_identifier,
                }),
                statements.next()
            );
        }
    }
}
