use crate::ast_statements::Statement;

pub struct Program {
    statements: Vec<Statement>,
    position: usize,
}

impl Iterator for Program {
    type Item = Statement;

    fn next(&mut self) -> Option<Self::Item> {
        //todo: is there a way to reuse the VEC iterator❓
        //todo: what would be better to use❓
        if self.position < self.statements.len() {
            let statement = self.statements[self.position].clone();
            self.position += 1;
            Some(statement)
        } else {
            None
        }
    }
}

impl Program {
    pub(crate) fn new() -> Self {
        Self {
            statements: vec![],
            position: 0,
        }
    }

    pub(crate) fn append(&mut self, stmt: Statement) {
        self.statements.push(stmt)
    }

    #[allow(unused)]
    pub(crate) fn len(&self) -> usize {
        self.statements.len()
    }
}

impl ToString for Program {
    fn to_string(&self) -> String {
        if self.statements.is_empty() {
            String::from("")
        } else {
            self.statements.first().unwrap().to_string()
        }
    }
}
