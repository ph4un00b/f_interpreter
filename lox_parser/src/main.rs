use std::fmt::{self, Display};
// mod env_01_recursive;
mod env_02_vector;

#[allow(unused)]
fn string_err(expected_token: Tk, token: Tk, context_message: &str) -> String {
    if token == Tk::End {
        format!(
            "❌ expected next token to be {:?} at end, {}, got {:?} instead",
            expected_token, context_message, token
        )
    } else if expected_token == Tk::Default {
        format!("❌ {}, got {:?} instead", context_message, token)
    } else {
        format!(
            "❌ expected next token to be {:?}, {}, got {:?} instead",
            expected_token, context_message, token
        )
    }
}

fn report_err(expected_token: Tk, token: Tk, context_message: &str) {
    /*
     * Another way to handle common syntax errors is with error productions.
     * You augment the grammar with a rule that
     * successfully matches the erroneous syntax. The parser
     * safely parses it but then reports it as an error instead
     * of producing a syntax tree.
     *
     * unary → ( "!" | "-" | -> "+" <- ) unary | primary ;
     *
     * This lets the parser consume + without going into
     * panic mode or leaving the parser in a weird state
     *
     * This lets the parser consume + without going into panic
     * mode or leaving the parser in a weird state
     */

    //? Mature parsers tend to accumulate error productions
    //? like barnacles since they help users fix common mistakes.
    if token == Tk::End {
        eprintln!(
            "❌ expected next token to be {:?} at end, {}, got {:?} instead",
            expected_token, context_message, token
        );
    } else {
        eprintln!(
            "❌ expected next token to be {:?}, {}, got {:?} instead",
            expected_token, context_message, token
        );
    }
}
/*
 * minimal parser and token for
 * 1 - (2 * 3) < 4 == false
 */
#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
enum Expr {
    Unary {
        operator: Tk,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Tk,
        right: Box<Expr>,
    },
    /*
     * We could reuse the existing Expr.Binary class for these two new
     * expressions since they have the same fields.
     * But then visitBinaryExpr() would have to check to see if the
     * operator is one of the logical operators and use a different
     * code path to handle the short circuiting.
     * I think it’s cleaner to define a new class for these
     * operators so that they get their own visit method.
     */
    Logical(Box<Expr>, Tk, Box<Expr>),
    Literal(V),
    Grouping(Box<Expr>),
    Let(Tk),
    /*
     * We want the syntax tree to reflect that an l-value isn’t evaluated
     * like a normal expression. That’s why the Expr.Assign node has a
     * Token for the left-hand side, not an Expr.
     * The problem is that the parser doesn’t know it’s parsing
     * an l-value until it hits the =. In a complex l-value,
     * that may occur many tokens later.
     */
    //? makeList().head.next = node;
    /*
     * Since the receiver of a field assignment can be any expression,
     * and expressions can be as long as you want to make them,
     * it may take an unbounded number of tokens of lookahead to find the =.
     */
    Assign(Tk, Box<Expr>),
    None,
}

#[derive(Debug, PartialEq, Clone)]
pub enum V {
    I32(i32),
    F64(f64),
    String(String),
    Bool(bool),
}

impl fmt::Display for V {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            V::I32(val) => write!(f, "{}", val)?,
            V::F64(val) => write!(f, "{}", val)?,
            V::String(val) => write!(f, "{}", val)?,
            V::Bool(val) => write!(f, "{}", val)?,
        }
        Ok(())
    }
}

impl From<Tk> for String {
    fn from(value: Tk) -> String {
        match value {
            Tk::Num(val) => val.to_string(),
            Tk::Float(val) => val.to_string(),
            Tk::Identifier(val) => val,
            Tk::Sub => "-".to_string(),
            Tk::Lpar => "(".to_string(),
            Tk::Mul => "*".to_string(),
            Tk::Rpar => ")".to_string(),
            Tk::LT => "<".to_string(),
            Tk::EQ => "==".to_string(),
            Tk::False => "false".to_string(),
            Tk::Default => unreachable!(),
            Tk::End => unreachable!(),
            Tk::Bang => "!".to_string(),
            Tk::Semi => ";".to_string(),
            Tk::Class => "class".to_string(),
            Tk::Fn => "fn".to_string(),
            Tk::Var => "let".to_string(),
            Tk::For => "for".to_string(),
            Tk::If => "if".to_string(),
            Tk::Or => "or".to_string(),
            Tk::And => "and".to_string(),
            Tk::Else => "else".to_string(),
            Tk::While => "while".to_string(),
            Tk::Print => "put".to_string(),
            Tk::Return => "ret".to_string(),
            Tk::Assign => "=".to_string(),
            Tk::LBrace => "{".to_string(),
            Tk::RBrace => "}".to_string(),
        }
    }
}

//? 1 - (2 * 3) < 4 == false
#[allow(dead_code)]
#[derive(Clone, PartialEq, Debug)]
pub enum Tk {
    Default,
    Num(i32),
    Float(f64),
    Identifier(String),
    Assign,
    Sub,
    Lpar,
    Mul,
    Rpar,
    LT,
    EQ,
    False,
    Bang,
    End,
    Semi,
    Class,
    Fn,
    Var,
    For,
    If,
    While,
    Print,
    Return,
    LBrace,
    RBrace,
    Else,
    Or,
    And,
}

trait ToLiteral<TValue>
where
    TValue: Display,
{
    fn literal(val: TValue) -> Expr;
}

impl ToLiteral<bool> for Expr {
    fn literal(val: bool) -> Self {
        Expr::Literal(V::Bool(val))
    }
}

impl ToLiteral<i32> for Expr {
    fn literal(val: i32) -> Self {
        Expr::Literal(V::I32(val))
    }
}

impl ToLiteral<String> for Expr {
    fn literal(val: String) -> Self {
        Expr::Literal(V::String(val))
    }
}

impl ToLiteral<&str> for Expr {
    fn literal(val: &str) -> Self {
        Expr::Literal(V::String(val.to_string()))
    }
}

impl ToLiteral<f64> for Expr {
    fn literal(val: f64) -> Self {
        Expr::Literal(V::F64(val))
    }
}

impl Expr {
    fn binary_op(left: Expr, operator: Tk, right: Expr) -> Self {
        Expr::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn unary_op(operator: Tk, right: Expr) -> Self {
        Expr::Unary {
            operator,
            right: Box::new(right),
        }
    }

    fn grouping(expression: Expr) -> Self {
        Expr::Grouping(Box::new(expression))
    }

    // Other methods...
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Unary { operator, right } => {
                let token = operator.clone();
                write!(f, "({}{})", String::from(token), right)?
            }
            Expr::Logical(left, operator, right) => {
                write!(f, "({} {} {})", String::from(operator.clone()), left, right)?
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", String::from(operator.clone()), left, right)?,
            Expr::Literal(val) => write!(f, "{}", val)?,
            Expr::Grouping(expr) => write!(f, "(group {})", expr)?,
            Expr::Let(name) => write!(f, "@{}", String::from(name.clone()))?,
            Expr::Assign(identifier, expr) => {
                write!(f, "@{} = {}", String::from(identifier.clone()), expr)?
            }
            Expr::None => write!(f, "❌ algún error!")?,
        }

        Ok(())
    }
}

// todo: can it be automated❓👀
#[derive(Debug, PartialEq, Clone)]
enum Statement {
    None,
    Expr(Expr),
    /*
     * The node stores the condition and body.
     * Here you can see why it’s nice to have separate base
     * classes for expressions and statements.
     * The field declarations make it clear that the condition
     * is an expression and the body is a statement.
     */
    While {
        condition: Expr,
        body: Box<Statement>,
    },
    Cond {
        condition: Expr,
        then_statement: Box<Statement>,
        else_statement: Box<Statement>,
    },
    Print(Expr),
    Let {
        name: Tk,
        initializer: Expr,
    },
    //* The curly-braced block statement that defines a local scope
    Block(Vec<Statement>),
}

#[allow(dead_code)]
struct Parser {
    tokens: Vec<Tk>,
    current_position: usize,
    current_token: Tk,
    prev_token: Tk,
}

#[allow(dead_code)]
struct Interpreter {
    runtime_error: bool,
    global_env: Vec<Env>,
    // todo: como definer sólo unas strings ala typescript❓👀
    // todo: tipo -> results: Vec<("let" | "print", V)>
    results: Vec<(String, V)>,
}

//? use std::mem;

#[allow(dead_code)]
impl Interpreter {
    fn new(initial_env: Env) -> Self {
        Self {
            runtime_error: false,
            global_env: vec![initial_env],
            results: vec![],
        }
    }

    fn eval(&mut self, statements: Vec<Statement>) {
        for state in statements {
            self.eval_statement(state);
        }
    }

    fn eval_block(&mut self, statements: Vec<Statement>) {
        /*
         * Another classic approach is to explicitly pass the environment
         * as a parameter to each visit method. To “change” the environment,
         * you pass a different one as you recurse down the tree.
         * You don’t have to restore the old one, since the new one
         * lives on the Java stack and is implicitly discarded when
         * the interpreter returns from the block’s visit method.
         */
        // todo: do it in a recursive way❗
        // todo:  let previous_env = std::mem::replace(&mut self.global_env, local_env);
        self.global_env.push(Env::new());
        println!(">>> new block {:?}", self.global_env);
        for state in statements {
            self.eval_statement(state);
        }
        /*
         * To execute code within a given scope, this method updates the
         * interpreter’s environment field, visits all of the statements,
         * and then restores the previous value.
         */
        self.global_env.pop();
        println!("<<< block out {:?}", self.global_env);
        // todo: it gets restored even if an exception is thrown.
    }

    fn eval_statement(&mut self, state: Statement) {
        match state {
            Statement::While { condition, body } => {
                //? este el truco, evaluar en cada loop❗
                while self.eval_expr(condition.clone()) == Ok(V::Bool(true)) {
                    // todo: improve allocations❓👀
                    self.eval_statement(*body.clone());
                }
            }
            Statement::Cond {
                condition,
                then_statement,
                else_statement,
            } => {
                let maybe_condition = self.eval_expr(condition);
                let Ok(evaluated_condition) = maybe_condition else {
                    todo!()
                };
                /*
                 * Most other syntax trees always evaluate their subtrees.
                 * Here, we may not evaluate the then or else statement.
                 * If either of those has a side effect,
                 * the choice not to evaluate it becomes user visible.
                 */
                match evaluated_condition {
                    V::Bool(true) => self.eval_statement(*then_statement),
                    V::Bool(false) => self.eval_statement(*else_statement),
                    _ => unreachable!(),
                }
            }
            Statement::Block(statements) => self.eval_block(statements),
            Statement::Print(expr) => {
                let value = self.eval_expr(expr);
                match value {
                    Ok(val) => {
                        self.results.push(("print".into(), val.clone()));
                        println!("🎈 {val}")
                    }
                    Err(err) => match err {
                        RE::MustBeNumber(_) => todo!(),
                        RE::NotNumber(_, _, _) => todo!(),
                        RE::MustBeBoolean(_) => todo!(),
                        RE::UndefinedVariable(_) => todo!(),
                    },
                }
            }
            Statement::Expr(expr) => {
                let _ = self.eval_expr(expr);
            }
            Statement::Let { name, initializer } => {
                /*
                 * If the variable has an initializer, we evaluate it.
                 * If not, we have another choice to make.
                 * We could have made this a syntax error in
                 * the parser by requiring an initializer.
                 * Most languages don’t, though, so it feels
                 * a little harsh to do so in Lox.
                 */
                let maybe_val = self.eval_expr(initializer);
                if let Ok(val) = maybe_val {
                    self.define_variable(name, val);
                } else {
                    // todo: err❓
                    todo!()
                }
            }
            Statement::None => unreachable!(),
        }
    }

    fn define_variable(&mut self, name: Tk, val: V) {
        /*
        todo: test ->
        * var a;
        * print a; // "nil".
        todo: test ->
        * print a;
        * var a = "too late!";
        */
        self.results.push(("let".into(), val.clone()));
        let name = String::from(name);
        if let Some(local_values) = self.global_env.last_mut() {
            println!("{:?}", local_values);
            local_values.define(name, val);
        }
        println!("{:?}", self.global_env);
    }

    fn eval_expr(&mut self, expr: Expr) -> RValue {
        println!("{:?}", expr);
        match expr {
            Expr::Assign(to_identifier, right) => {
                let value = self.eval_expr(*right)?;
                /*
                 * The last thing the visit() method does is return the
                 * assigned value. That’s because assignment is
                 * an expression that can be nested inside other
                 * expressions, like so:
                 */
                //? var a = 1;
                self.assign_variable(value, to_identifier)
            }
            Expr::Let(identifier) => {
                if let Some(local_values) = self.global_env.last_mut() {
                    println!("{:?}", local_values);
                    if let Some(value) = local_values.fetch(identifier.clone()) {
                        return Ok(value);
                    }
                    let mut iter = self.global_env.iter_mut();
                    loop {
                        match iter.next() {
                            Some(outer_env) => {
                                if let Some(value) = outer_env.fetch(identifier.clone()) {
                                    return Ok(value);
                                }
                            }
                            None => return Err(RE::UndefinedVariable(identifier.into())),
                        }
                    }
                } else {
                    unreachable!("not initial global env!");
                }
            }
            Expr::Unary { operator, right } => {
                /*
                 * First, we evaluate the operand expression.
                 * Then we apply the unary operator itself to
                 * the result of that. There are two different
                 * unary expressions, identified by the type of
                 * the operator token.
                 */
                let right = self.eval_expr(*right)?;
                match (operator, right) {
                    (Tk::Sub, V::I32(r)) => Ok(V::I32(-r)),
                    (Tk::Sub, V::F64(r)) => Ok(V::F64(-r)),
                    (Tk::Sub, value) => Err(RE::MustBeNumber(value)),
                    (Tk::Bang, V::Bool(false)) => Ok(V::Bool(true)),
                    (Tk::Bang, V::Bool(true)) => Ok(V::Bool(false)),
                    (Tk::Bang, value) => Err(RE::MustBeBoolean(value)),
                    /*
                     * You’re probably wondering what happens
                     * if the cast fails❓.
                     * Fear not, we’ll get into that soon.
                     */
                    _ => unreachable!(),
                }
            }
            /*
             * If you compare this to the earlier chapter’s
             * Expr::Binary  method, you can see the difference.
             * Here, we evaluate the left operand first.
             * We look at its value to see if we can short-circuit.
             * If not, and only then, do we evaluate the right operand.
             */
            Expr::Logical(l, Tk::Or, r) => {
                let left = self.eval_expr(*l)?;
                if left == V::Bool(true) {
                    return Ok(left);
                }
                self.eval_expr(*r)
            }
            Expr::Logical(l, Tk::And, r) => {
                let left = self.eval_expr(*l)?;
                if left == V::Bool(false) {
                    return Ok(left);
                }
                self.eval_expr(*r)
            }
            Expr::Logical(_, _, _) => unreachable!(),
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                /*
                 * Did you notice we pinned down a subtle corner of the
                 * language semantics here? In a binary expression,
                 * we evaluate the operands in left-to-right order.
                 * If those operands have side effects, that choice
                 * is user visible, so this isn’t simply an implementation
                 * detail.
                 */
                //? Another subtle semantic choice: We evaluate both
                //? operands before checking the type of either.
                //? Imagine we have a function say() that prints its
                //? argument then returns it.
                //? We could have instead specified that the left operand
                //? is checked before even evaluating the right.
                let left = self.eval_expr(*left)?;
                println!("left: {:?}", left);
                let right = self.eval_expr(*right)?;
                println!("right: {:?}", right);
                match (left, operator, right) {
                    // todo: +, /
                    /*
                     * The + operator can also be used to concatenate two strings.
                     * To handle that, we don’t just assume the operands are
                     * a certain type and cast them, we dynamically check the
                     * type and choose the appropriate operation.
                     */
                    (V::I32(l), Tk::Sub, V::I32(r)) => Ok(V::I32(l - r)),
                    (V::F64(l), Tk::Sub, V::F64(r)) => Ok(V::F64(l - r)),
                    (l, Tk::Sub, r) => Err(RE::NotNumber(l, Tk::Sub, r)),
                    (V::I32(l), Tk::Mul, V::I32(r)) => Ok(V::I32(l * r)),
                    (V::F64(l), Tk::Mul, V::F64(r)) => Ok(V::F64(l * r)),
                    (l, Tk::Mul, r) => Err(RE::NotNumber(l, Tk::Mul, r)),
                    // todo: What happens right now if you divide a number by zero? What do you think should happen?
                    /*
                     * comparisons:
                     * They are basically the same as arithmetic.
                     * The only difference is that where the arithmetic
                     * operators produce a value whose type is the same as \
                     * the operands (numbers or strings), the comparison
                     * operators always produce a Boolean.
                     */
                    (V::I32(l), Tk::EQ, V::I32(r)) => Ok(V::Bool(l.eq(&r))),
                    (V::F64(l), Tk::EQ, V::F64(r)) => Ok(V::Bool(l.eq(&r))),
                    (l, Tk::EQ, r) => Err(RE::NotNumber(l, Tk::EQ, r)),
                    (V::I32(l), Tk::LT, V::I32(r)) => Ok(V::Bool(l.lt(&r))),
                    (V::F64(l), Tk::LT, V::F64(r)) => Ok(V::Bool(l.lt(&r))),
                    (l, Tk::LT, r) => Err(RE::NotNumber(l, Tk::LT, r)),
                    /*
                     * Allowing comparisons on types other than numbers
                     * could be useful. The operators might have a reasonable
                     * interpretation for strings. Even comparisons among
                     * mixed types, like 3 < "pancake" could be handy to
                     * enable things like ordered collections of heterogeneous
                     * types. Or it could simply lead to bugs and confusion.
                     */
                    // todo: + handle the case for string concatenation "'a' + 'a'"❗
                    /*
                     * Many languages define + such that if either operand
                     * is a string, the other is converted to a string and
                     * the results are then concatenated. For example,
                     * "scone" + 4 would yield scone4.
                     *
                     * como lo vió en javascript 👀❗
                     */
                    _ => unreachable!(),
                }
            }
            Expr::Literal(value) => Ok(value),
            Expr::Grouping(expr) => self.eval_expr(*expr),
            Expr::None => unreachable!(),
        }
    }

    fn assign_variable(&mut self, value: V, to_identifier: Tk) -> StdResult<V, RE> {
        // todo: print a = 2; // "2".
        if let Some(local_values) = self.global_env.last_mut() {
            println!("{:?}", local_values);
            if let Some(value) = local_values.assign(to_identifier.clone(), value.clone()) {
                return Ok(value);
            }
            let mut iter = self.global_env.iter_mut();
            loop {
                /*
                 * If the variable isn’t found in this environment,
                 * we simply try the enclosing one. That in turn does
                 * the same thing recursively, so this will ultimately
                 * walk the entire chain. If we reach an environment
                 * with no enclosing one and still don’t find the variable,
                 * then we give up and report an error as before.
                 */
                match iter.next() {
                    /*
                     * It’s likely faster to iteratively walk the chain,
                     * but I think the recursive solution is prettier.
                     * We’ll do something much faster in clox.
                     */
                    Some(outer_env) => {
                        if let Some(value) = outer_env.assign(to_identifier.clone(), value.clone())
                        {
                            return Ok(value);
                        }
                    }
                    None => return Err(RE::UndefinedVariable(to_identifier.into())),
                }
            }
        } else {
            unreachable!();
        }
    }
}

enum ParserAy {
    BadExpression(Tk),
}

#[derive(PartialEq)]
pub enum RE {
    /*
     * @see: https://craftinginterpreters.com/evaluating-expressions.html#design-note
     * It turns out even most statically typed
     * languages do some type checks at runtime.
     * The type system checks most type rules
     * statically, but inserts runtime checks in
     * the generated code for other operations.
     * For example, in Java, the static type system
     * assumes a cast expression will always safely
     * succeed. After you cast some value, you can
     * statically treat it as the destination type
     * and not get any compile errors.
     * But downcasts can fail, obviously.
     * The only reason the static checker can
     * presume that casts always succeed without
     * violating the language’s soundness
     * guarantees, is because the cast is
     * checked at runtime and throws an exception
     * on failure.
     * A more subtle example is covariant arrays
     * in Java and C#. The static sub-typing rules
     * for arrays allow operations that are not sound.
     *
     * Consider:
     * Object[] stuff = new Integer[1];
     * stuff[0] = "not an int!";
     *
     * @phau: también pasa algo similar en typescript❗
     *
     * Even Haskell will let you run code with
     * non-exhaustive matches. If you find yourself
     * designing a statically typed language, keep in
     * mind that you can sometimes give users more
     * flexibility without sacrificing too many of the
     * benefits of static safety by deferring some type
     * checks until runtime.
     */
    MustBeNumber(V),
    NotNumber(V, Tk, V),
    MustBeBoolean(V),
    UndefinedVariable(String),
}

use std::result::Result as StdResult;

// use env_01_recursive::Env;
use env_02_vector::Env;
type Result<T> = StdResult<T, ParserAy>;
type RStatement = StdResult<Statement, ParserAy>;
type RValue = StdResult<V, RE>;

#[allow(dead_code)]
impl Parser {
    fn new(tokens: Vec<Tk>) -> Self {
        let mut p = Parser {
            tokens,
            current_position: 0,
            current_token: Tk::Default,
            prev_token: Tk::Default,
        };
        p.init_tokens();
        p
    }

    /*
     * Each method for parsing a grammar rule
     * produces a syntax tree for that rule and
     * returns it to the caller. When the body of
     * the rule contains a non-terminal—a reference
     * to another rule—we call that other rule’s method.
     */
    //? This is why left recursion is problematic for
    //? recursive descent. The function for a
    //? left-recursive rule immediately calls itself,
    //? which calls itself again, and so on, until
    //? the parser hits a stack overflow and dies.
    // * equality  → comparison ( ( "!=" | "==" ) comparison )* ;
    fn init_tokens(&mut self) {
        self.current_token = self.tokens[self.current_position].clone();
        self.prev_token = Tk::Default;
    }

    fn consume_token(&mut self) {
        self.current_position += 1;
        self.current_token = self.tokens[self.current_position].clone();
        self.prev_token = self.tokens[self.current_position - 1].clone();
        println!("tk: {:?}", self.current_token);
    }
    /*
     * GRAMAR:
     *
     * program        → declaration* EOF ;
     * declaration    → varDecl | statement ;
     * statement      → exprStmt
     *                  | ifStmt
     *                  | printStmt
     *                  | whileStmt
     *                  | block ;
     * exprStmt       → expression ";"
     * ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
     * printStmt      → "print" expression ";"
     * whileStmt      → "while" "(" expression ")" statement ;
     * block          → "{" declaration* "}" ;
     */

    fn parse(&mut self) -> Vec<Statement> {
        /*
         * Now that our grammar has the correct
         * starting rule, program, we can turn parse()
         * into the real deal.
         */
        let mut result = vec![];

        while self.current_token != Tk::End {
            let statement = self.variable_or_statement().unwrap_or_else(|err| {
                match err {
                    ParserAy::BadExpression(bad_token) => {
                        println!(
                            "❌ desde parse!, expected expression got {:?}😱❗",
                            bad_token
                        )
                    }
                };

                Statement::None
            });
            result.push(statement);
        }

        result
    }

    //* let-Decl → "let" IDENTIFIER ( "=" expression )? ";" ;
    fn variable_or_statement(&mut self) -> RStatement {
        let result = if self.current_token == Tk::Var {
            self.variable_statement()?
        } else {
            self.statement()?
        };

        Ok(result)
    }

    fn variable_statement(&mut self) -> RStatement {
        /*
         * As always, the recursive descent code follows
         * the grammar rule. The parser has already
         * matched the var token, so next it requires
         * and consumes an identifier token for the
         * variable name.
         *
         * "let" IDENTIFIER ( "=" expression )? ";" ;
         */
        self.consume_token();
        let result_token = if let Tk::Identifier(_) = self.current_token {
            self.current_token.clone()
        } else {
            report_err(
                Tk::Identifier("name".into()),
                self.current_token.clone(),
                "👀 expected 'identifier'",
            );
            Tk::Default
        };

        /*
         * Then, if it sees an = token, it knows there is an
         * initializer expression and parses it. Otherwise,
         * it leaves the initializer null.
         */
        self.consume_token();
        let initializer = if let Tk::Assign = self.current_token {
            self.consume_token();
            self.expression()?
        } else {
            Expr::None
        };

        /*
         * Finally, it consumes the required semicolon at the end
         * of the statement. All this gets wrapped in a Stmt.Var
         * syntax tree node and we’re groovy
         */
        self.report_and_consume(Tk::Semi, "👀 expected ';' after expression");

        Ok(Statement::Let {
            name: result_token,
            initializer,
        })
    }

    /*
     * statement      → exprStmt
     *                  | forStmt
     *                  | ifStmt
     *                  | printStmt
     *                  | whileStmt
     *                  | block ;
     */
    fn statement(&mut self) -> RStatement {
        /*
         * When a syntax error does occur, this method returns null.
         * That’s OK. The parser promises not to crash or hang on
         * invalid syntax, but it doesn’t promise to return a
         * usable syntax tree if an error is found. As soon as
         * the parser reports an error, hadError gets set, and
         * subsequent phases are skipped.
         */
        let stt = match self.current_token {
            Tk::While => self.while_statement()?,
            Tk::If => self.conditional_statement()?,
            Tk::Print => self.print_statement()?,
            /*
             * Having block() return the raw list of statements and
             * leaving it to statement() to wrap the list in a Stmt.Block
             * looks a little odd.
             *
             * I did it that way because we’ll
             * reuse block() later for parsing function bodies and we
             * don’t want that body wrapped in a Stmt.Block.
             */
            Tk::LBrace => Statement::Block(self.block_statement()?),
            _ => self.expr_statement()?,
        };

        Ok(stt)
    }

    fn while_statement(&mut self) -> RStatement {
        self.consume_token();
        // consume(LEFT_PAREN, "Expect '(' after 'while'.");
        self.report_and_consume(Tk::Lpar, "Expect '(' after 'while'");
        // Expr condition = expression();
        let condition = self.expression()?;
        // consume(RIGHT_PAREN, "Expect ')' after condition.");
        self.report_and_consume(Tk::Rpar, "Expect ')' after 'while'");
        // Stmt body = statement();
        let body = self.statement()?;
        // return new Stmt.While(condition, body);
        Ok(Statement::While {
            condition,
            body: Box::new(body),
        })
    }

    // * ifStmt  → "if" "(" expression ")" statement ( "else" statement )? ;
    /*
     * The semicolons in the rules aren’t quoted, which means
     * they are part of the grammar meta-syntax, not Lox’s syntax.
     * A block does not have a ; at the end and an if statement doesn’t
     * either, unless the then or else statement happens to be one
     * that ends in a semicolon.
     */
    fn conditional_statement(&mut self) -> RStatement {
        // consume(LEFT_PAREN, "Expect '(' after 'if'.");
        self.report_and_consume(Tk::Lpar, "Expect '(' after 'if'");
        // Expr condition = expression();
        self.consume_token();
        let condition = self.expression()?;
        /*
         * Other languages like Lua and some BASICs use a keyword
         * like then as the ending delimiter and don’t have anything
         * before the condition. Go and Swift instead require
         * the statement to be a braced block.
         * That lets them use the { at the beginning of the
         * statement to tell when the condition is done.
         */
        // consume(RIGHT_PAREN, "Expect ')' after if condition.");
        self.report_and_consume(Tk::Rpar, "Expect ')' after if condition");
        // Stmt thenBranch = statement();
        let then_statement = Box::new(self.statement()?);
        // Stmt elseBranch = null;
        // if (match(ELSE)) {
        //   elseBranch = statement();
        // }
        // todo: test if (first) if (second) whenTrue(); else whenFalse();
        /*
         * Here’s the riddle: Which if statement does that else
         * clause belong to? This isn’t just a theoretical question
         * about how we notate our grammar. It actually affects how
         * the code executes:
         */
        //? If we attach the else to the first if statement,
        //? then whenFalse() is called if first is falsey,
        //? regardless of what value second has.
        //? If we attach it to the second if statement,
        //? then whenFalse() is only called if first is truthy
        //? and second is falsey.
        /*
         * Since else clauses are optional, and there is no explicit
         * delimiter marking the end of the if statement,
         * the grammar is ambiguous when you nest ifs in this way.
         * This classic pitfall of syntax is called the dangling
         * else problem.
         * @see https://en.wikipedia.org/wiki/Dangling_else
         */
        let else_statement = if let Tk::Else = self.current_token {
            self.consume_token();
            Box::new(self.statement()?)
        } else {
            Box::new(Statement::None)
        };
        /*
         * Instead, most languages and parsers avoid the problem
         * in an ad hoc way. No matter what hack they use to get
         * themselves out of the trouble, they always choose the
         * same interpretation—the else is bound to the nearest
         * if that precedes it.
         *
         * Our parser conveniently does that already.
         * Since ifStatement() eagerly looks for an else before
         * returning, the innermost call to a nested series will
         * claim the else clause for itself before returning to
         * the outer if statements.
         */
        // return new Stmt.If(condition, thenBranch, elseBranch);
        Ok(Statement::Cond {
            condition,
            then_statement,
            else_statement,
        })
    }

    fn print_statement(&mut self) -> RStatement {
        self.consume_token();
        // Expr value = expression();
        let value = self.expression()?;
        println!("{:?}", value);
        // consume(SEMICOLON, "Expect ';' after value.");
        self.report_and_consume(Tk::Semi, "👀 expected ';' after value");
        // return new Stmt.Print(value);
        Ok(Statement::Print(value))
    }

    fn expr_statement(&mut self) -> RStatement {
        // Expr expr = expression();
        let value = self.expression()?;
        // consume(SEMICOLON, "Expect ';' after expression.");
        self.report_and_consume(Tk::Semi, "👀 expected ';' after expression");
        // return new Stmt.Expression(expr);
        Ok(Statement::Expr(value))
    }

    //* block     → "{" declaration* "}" ;
    fn block_statement(&mut self) -> StdResult<Vec<Statement>, ParserAy> {
        self.consume_token();
        println!("{:?}", self.current_token);
        let mut result = vec![];
        /*
         * We create an empty list and then parse statements and
         * add them to the list until we reach the end of the block,
         * marked by the closing }.
         *
         * Note that the loop also has an
         * explicit check for isAtEnd(). We have to be careful
         * to avoid infinite loops, even when parsing invalid code.
         * If the user forgets a closing }, the parser needs to not get stuck
         */
        while self.current_token != Tk::RBrace && self.current_token != Tk::End {
            result.push(self.variable_or_statement()?);
        }

        self.report_and_consume(Tk::RBrace, "Expect '}' after block 👀❗");
        Ok(result)
    }

    // * expression → assignment ;
    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    //* assignment → IDENTIFIER "=" assignment | logic_or ;
    /*
     * Instead of falling back to equality, assignment now cascades to
     * logic_or. The two new rules, logic_or and logic_and, are
     * similar to other binary operators.
     * Then logic_and calls out to equality for its operands,
     * and we chain back to the rest of the expression rules.
     *
     * The syntax doesn’t care that they short-circuit.
     * That’s a semantic concern.
     */
    fn assignment(&mut self) -> Result<Expr> {
        /*
         * Here is where it gets tricky.
         * A single token lookahead recursive descent parser can’t
         * see far enough to tell that it’s parsing an assignment
         * until after it has gone through the left-hand side and
         * stumbled onto the =. You might wonder why it even needs to.
         * After all, we don’t know we’re parsing a + expression until
         * after we’ve finished parsing the left operand.
         *
         * The difference is that the left-hand side of an assignment
         * isn’t an expression that evaluates to a value.
         * It’s a sort of pseudo-expression that evaluates to
         * a “thing” you can assign to. Consider:
         */
        //? var a = "before";
        //? a = "value";
        /*
         * On the second line, we don’t evaluate a (which would return
         * the string “before”). We figure out what variable a refers
         * to so we know where to store the right-hand side expression’s value.
         *
         * The classic terms for these two constructs are l-value and r-value.
         * All of the expressions that we’ve seen so far that produce values
         * are r-values.
         *
         * An l-value “evaluates” to a storage location that you can assign into.
         */
        //? We have only a single token of lookahead, so what do we do?
        //? We use a little trick, and it looks like this:
        //* We parse the left-hand side, which can be any expression of higher precedence.
        let left = self.logic_or()?;

        if self.current_token == Tk::Assign {
            self.consume_token();
            let symbol = self.prev_token.clone();
            /*
             * One slight difference from binary operators is that we don’t loop
             * to build up a sequence of the same operator. Since assignment is
             * right-associative, we instead recursively call assignment() to
             * parse the right-hand side.
             */
            //? We convert the r-value expression node into an l-value representation.
            //* This conversion works because it turns out that every valid
            //* assignment target happens to also be valid syntax as a normal expression.
            //? newPoint(x + 2, 0).y = 3;
            //* The left-hand side of that assignment could also work as a valid expression.
            //? newPoint(x + 2, 0).y;
            //* The first example sets the field, the second gets it.
            let right = self.assignment()?;

            if let Expr::Let(let_token) = left {
                /*
                 * If we find an =, we parse the right-hand side and then
                 * wrap it all up in an assignment expression tree node.
                 */
                return Ok(Expr::Assign(let_token, Box::new(right)));
            } else {
                /*
                 * We report an error if the left-hand side isn’t a valid
                 * assignment target, but we don’t throw it because the
                 * parser isn’t in a confused state where we need to go
                 * into panic mode and synchronize.
                 */
                /*
                 * This means we can parse the left-hand side as if it were an
                 * expression and then after the fact produce a syntax tree that
                 * turns it into an assignment target. If the left-hand side
                 * expression isn’t a valid assignment target, we fail with a
                 * syntax error. That ensures we report an error on code like this:
                 */
                // todo: a + b = c;
                /*
                 * Way back in the parsing chapter, I said we represent parenthesized
                 * expressions in the syntax tree because we’ll need them later.
                 * This is why. We need to be able to distinguish these cases:
                 */
                //? a = 3;   // OK.
                // todo: (a) = 3; // Error.
                report_err(Tk::Semi, symbol, "Invalid assignment target.")
            }
        }
        Ok(left)
    }

    /*
     * For an and expression to evaluate to something truthy,
     * both operands must be truthy.
     * We can see as soon as we evaluate the left false
     * operand that that isn’t going to be the case,
     * so there’s no need to evaluate sideEffect() and it gets skipped.
     *
     * This is why we didn’t implement the logical
     * operators with the other binary operators. Now we’re ready.
     * The two new operators are low in the precedence table.
     * Similar to || and && in C, they each have their own precedence
     * with or lower than and.
     * We slot them right between assignment and equality.
     */

    // * logic_or → logic_and ( "or" logic_and )* ;
    fn logic_or(&mut self) -> Result<Expr> {
        //? The code to parse a series of or expressions mirrors other binary operators.
        let mut expr = self.logic_and()?;
        while self.current_token == Tk::Or {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.logic_and()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // * logic_and  → equality ( "and" equality )* ;
    fn logic_and(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;
        while self.current_token == Tk::And {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // * eq  = _{ comparison ~ (("!=" | "==") ~ comparison)* }
    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;
        while self.current_token == Tk::EQ {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.comparison()?;
            expr = Expr::binary_op(expr, operator, right);
        }
        Ok(expr)
    }

    // * comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;
        while self.current_token == Tk::LT {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.term()?;
            expr = Expr::binary_op(expr, operator, right);
        }
        Ok(expr)
    }

    // * term       = _{ factor ~ ((sub | add) ~ factor)* }
    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;
        while self.current_token == Tk::Sub || self.current_token == Tk::Mul {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.factor()?;
            expr = Expr::binary_op(expr, operator, right);
        }
        Ok(expr)
    }

    // * factor     = _{ unary ~ ((div | mul) ~ unary)* }
    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;
        while self.current_token == Tk::Mul {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.unary()?;
            expr = Expr::binary_op(expr, operator, right);
        }
        Ok(expr)
    }

    // * unary      = _{ (bang | neg) ~ unary | primary }
    fn unary(&mut self) -> Result<Expr> {
        if self.current_token == Tk::Bang || self.current_token == Tk::Sub {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.unary()?;
            return Ok(Expr::unary_op(operator, right));
        }
        self.primary()
    }

    // todo: Add support for comma expressions
    // todo: add support for the C-style conditional or “ternary” operator ?:.
    // What precedence level is allowed between the ? and :?
    // Is the whole operator left-associative or right-associative?
    // todo: Add error productions to handle each binary operator appearing without a left-hand operand.
    // In other words, detect a binary operator appearing at the beginning of an expression
    //* primary → "true" | "false" | NUMBER | STRING | "(" expression ")" | IDENTIFIER ;
    fn primary(&mut self) -> Result<Expr> {
        match self.current_token {
            Tk::False => {
                self.consume_token();
                Ok(Expr::literal(false))
            }
            Tk::Num(val) => {
                self.consume_token();
                Ok(Expr::literal(val))
            }
            Tk::Float(val) => {
                self.consume_token();
                Ok(Expr::literal(val))
            }
            Tk::Identifier(_) => {
                self.consume_token();
                Ok(Expr::Let(self.prev_token.clone()))
            }
            Tk::Lpar => {
                self.consume_token();
                let expr = self.expression()?;
                self.report_and_consume(Tk::Rpar, "')' after EXPRESSION");
                Ok(Expr::grouping(expr))
            }
            _ => Err(ParserAy::BadExpression(self.current_token.clone())),
        }
    }

    fn report_and_consume(&mut self, expected_token: Tk, expected_message: &str) {
        if self.current_token != expected_token && self.current_token != Tk::End {
            report_err(expected_token, self.current_token.clone(), expected_message);
        }
        self.consume_token();
    }

    // private void synchronize() {
    //     advance();

    //     while (!isAtEnd()) {
    //       if (previous().type == SEMICOLON) return;

    //       switch (peek().type) {
    //         case CLASS:
    //         case FUN:
    //         case VAR:
    //         case FOR:
    //         case IF:
    //         case WHILE:
    //         case PRINT:
    //         case RETURN:
    //           return;
    //       }

    //       advance();
    //     }
    //   }

    fn synchronize(&mut self) {
        self.consume_token();

        while self.current_token != Tk::End {
            if self.prev_token == Tk::Semi {
                return;
            }

            match self.current_token {
                Tk::Class
                | Tk::Fn
                | Tk::Var
                | Tk::For
                | Tk::If
                | Tk::While
                | Tk::Print
                | Tk::Return => return,
                _ => self.consume_token(),
            }
        }
    }
}

#[allow(unused)]
struct Lox {
    had_error: bool,
    had_runtime_error: bool,
}

#[allow(unused)]
impl Lox {
    fn new() -> Self {
        Self {
            had_error: false,
            had_runtime_error: false,
        }
    }

    fn run(&self, tokens_instead_of_source_code: Vec<Tk>) {
        let mut p = Parser::new(tokens_instead_of_source_code);
        p.parse();
        //*  Stop if there was a syntax error.
        if self.had_error {}
    }
}

fn main() {
    /*
     * A parser really has two jobs:
     *
     * - Given a valid sequence of tokens, produce a corresponding syntax tree.
     *
     * - Given an invalid sequence of tokens, detect any errors and tell
     * the user about their mistakes.
     */
    //? When the user doesn’t realize the syntax is wrong,
    //? it is up to the parser to help guide them back onto the right path.
    //? The parser can’t read your mind❓
    //? With the way things are going in machine learning these days,
    //? who knows what the future will bring?
    let expression = Expr::binary_op(
        Expr::unary_op(Tk::Sub, Expr::literal(123)),
        Tk::Mul,
        Expr::grouping(Expr::literal(45.65)),
    );
    println!("{:?}", expression.to_string());
}

#[cfg(test)]
mod tests {
    use super::*;

    //* var i = 10;
    //* while (0 < i) {
    //*     print i;
    //*     i = i - 1;
    //* }
    #[test]
    fn test_while() {
        let tokens = vec![
            Tk::While,
            Tk::Lpar,
            Tk::Num(0),
            Tk::LT,
            Tk::Identifier("i".into()),
            Tk::Rpar,
            Tk::LBrace,
            Tk::Print,
            Tk::Identifier("i".into()),
            Tk::Semi,
            Tk::Identifier("i".into()),
            Tk::Assign,
            Tk::Identifier("i".into()),
            Tk::Sub,
            Tk::Num(1),
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut environment = Env::new();
        environment.define("i".into(), V::I32(10));
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);
        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        // println!("{:?}", inter.results);
        assert_eq!(last, ("print".into(), V::I32(1)));
    }

    #[test]
    fn it_works() {
        let expression = Expr::binary_op(
            Expr::unary_op(Tk::Sub, Expr::literal(123)),
            Tk::Mul,
            Expr::grouping(Expr::literal(45.65)),
        );

        assert_eq!(
            expression.to_string(),
            "(* (-123) (group 45.65))".to_string()
        );
    }

    //* print 2 < 1 and 1 < 2;
    #[test]
    fn test_logical_and() {
        let tokens = vec![
            Tk::Print,
            Tk::Num(2),
            Tk::LT,
            Tk::Num(1),
            Tk::And,
            Tk::Num(1),
            Tk::LT,
            Tk::Num(2),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut inter = Interpreter::new(Env::new());
        inter.eval(statements);
        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        assert_eq!(last, ("print".into(), V::Bool(false)));
    }

    //* print 2 < 1 or 1 < 2;
    #[test]
    fn test_logical_or() {
        let tokens = vec![
            Tk::Print,
            Tk::Num(2),
            Tk::LT,
            Tk::Num(1),
            Tk::Or,
            Tk::Num(1),
            Tk::LT,
            Tk::Num(2),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut inter = Interpreter::new(Env::new());
        inter.eval(statements);
        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        assert_eq!(last, ("print".into(), V::Bool(true)));
    }

    //* if (!false) if (false) whenTrue(); else print 111;
    #[test]
    fn test_dangling_else_control_flow() {
        let tokens = vec![
            Tk::If,
            Tk::Lpar,
            Tk::Bang,
            Tk::False,
            Tk::Rpar,
            Tk::If,
            Tk::Lpar,
            Tk::False,
            Tk::Rpar,
            Tk::Print,
            Tk::Num(100),
            Tk::Semi,
            Tk::Else,
            Tk::Print,
            Tk::Num(111),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut inter = Interpreter::new(Env::new());
        inter.eval(statements);
        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        assert_eq!(last, ("print".into(), V::I32(111)));
    }

    //* if (!!false) print 100; else print 42;
    #[test]
    fn test_else_control_flow() {
        let tokens = vec![
            Tk::If,
            Tk::Lpar,
            Tk::Bang,
            Tk::Bang,
            Tk::False,
            Tk::Rpar,
            Tk::Print,
            Tk::Num(100),
            Tk::Semi,
            Tk::Else,
            Tk::Print,
            Tk::Num(42),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut inter = Interpreter::new(Env::new());
        inter.eval(statements);
        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        assert_eq!(last, ("print".into(), V::I32(42)));
    }

    //* if (!false) print 100;
    #[test]
    fn test_control_flow() {
        let tokens = vec![
            Tk::If,
            Tk::Lpar,
            Tk::Bang,
            Tk::False,
            Tk::Rpar,
            Tk::Print,
            Tk::Num(100),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut inter = Interpreter::new(Env::new());
        inter.eval(statements);
        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        assert_eq!(last, ("print".into(), V::I32(100)));
    }

    //* let a = 3;
    //* {
    //*     let a = a - 1;
    //*     print a;
    //* }
    /*
     * Over time, the languages I know with implicit variable declaration
     * ended up adding more features and complexity to deal with these problems.
     *
     * Implicit declaration of global variables in JavaScript is
     * universally considered a mistake today.
     * “Strict mode” disables it and makes it a compile error.
     *
     * Python added a global statement to let you explicitly assign
     * to a global variable from within a function.
     * Later, as functional programming and nested functions
     * became more popular, they added a similar nonlocal statement
     * to assign to variables in enclosing functions.
     *
     * Ruby extended its block syntax to allow declaring certain
     * variables to be explicitly local to the block even if the
     * same name exists in an outer scope.
     */
    //* @see https://learn.microsoft.com/en-us/previous-versions/visualstudio/visual-studio-2010/xe53dz5w(v=vs.100)?redirectedfrom=MSDN
    #[test]
    fn test_block_variable_declaration() {
        let tokens = vec![
            Tk::LBrace,
            Tk::Var,
            Tk::Identifier("a".into()),
            Tk::Assign,
            Tk::Identifier("a".into()),
            Tk::Sub,
            Tk::Num(1),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let mut environment = Env::new();
        environment.define("a".into(), V::I32(3));
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);

        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        assert_eq!(last, ("print".into(), V::I32(2)));
    }

    //* let a = 1;
    //* let b = 2;
    //* let c = 3;
    //* {
    //*     let a = 10;
    //*     print a;
    //*     print b;
    //*     print c;
    //* }
    #[test]
    fn test_block_eval() {
        let tokens = vec![
            Tk::LBrace,
            Tk::Var,
            Tk::Identifier("a".into()),
            Tk::Assign,
            Tk::Num(10),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into()),
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let mut environment = Env::new();
        environment.define("a".into(), V::I32(1));
        environment.define("b".into(), V::I32(2));
        environment.define("c".into(), V::I32(3));
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);

        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        let penultimate = inter.results.get(inter.results.len() - 2).unwrap().clone();
        let antepenultimate = inter.results.get(inter.results.len() - 3).unwrap().clone();
        assert_eq!(last, ("print".into(), V::I32(3)));
        assert_eq!(penultimate, ("print".into(), V::I32(2)));
        assert_eq!(antepenultimate, ("print".into(), V::I32(10)));
    }

    //* let a = 1;
    //* let b = 2;
    //* let c = 3;
    //* {
    //*     let a = 100;
    //*     let b = 200;
    //*     {
    //*         let a = 10;
    //*         print a;
    //*         print b;
    //*         print c;
    //*     }
    //*     print a;
    //*     print b;
    //*     print c;
    //* }
    //* print a;
    //* print b;
    //* print c;
    #[test]
    fn test_block_eval_3() {
        let tokens = vec![
            Tk::LBrace,
            Tk::Var,
            Tk::Identifier("a".into()),
            Tk::Assign,
            Tk::Num(100),
            Tk::Semi,
            Tk::Var,
            Tk::Identifier("b".into()),
            Tk::Assign,
            Tk::Num(200),
            Tk::Semi,
            Tk::LBrace,
            Tk::Var,
            Tk::Identifier("a".into()),
            Tk::Assign,
            Tk::Num(10),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into()),
            Tk::Semi,
            Tk::RBrace,
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into()),
            Tk::Semi,
            Tk::RBrace,
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into()),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let mut environment = Env::new();
        environment.define("a".into(), V::I32(1));
        environment.define("b".into(), V::I32(2));
        environment.define("c".into(), V::I32(3));
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);

        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        let penultimate = inter.results.get(inter.results.len() - 2).unwrap().clone();
        let antepenultimate = inter.results.get(inter.results.len() - 3).unwrap().clone();
        assert_eq!(last, ("print".into(), V::I32(3)));
        assert_eq!(penultimate, ("print".into(), V::I32(2)));
        assert_eq!(antepenultimate, ("print".into(), V::I32(1)));
    }

    //* let a = 1;
    //* let b = 2;
    //* let c = 3;
    //* {
    //*     let a = 100;
    //*     let b = 200;
    //*     {
    //*         let a = 10;
    //*         print a;
    //*         print b;
    //*         print c;
    //*     }
    //*     print a;
    //*     print b;
    //*     print c;
    //* }
    #[test]
    fn test_block_eval_2() {
        let tokens = vec![
            Tk::LBrace,
            Tk::Var,
            Tk::Identifier("a".into()),
            Tk::Assign,
            Tk::Num(100),
            Tk::Semi,
            Tk::Var,
            Tk::Identifier("b".into()),
            Tk::Assign,
            Tk::Num(200),
            Tk::Semi,
            Tk::LBrace,
            Tk::Var,
            Tk::Identifier("a".into()),
            Tk::Assign,
            Tk::Num(10),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into()),
            Tk::Semi,
            Tk::RBrace,
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into()),
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let mut environment = Env::new();
        environment.define("a".into(), V::I32(1));
        environment.define("b".into(), V::I32(2));
        environment.define("c".into(), V::I32(3));
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);

        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        let penultimate = inter.results.get(inter.results.len() - 2).unwrap().clone();
        let antepenultimate = inter.results.get(inter.results.len() - 3).unwrap().clone();
        assert_eq!(last, ("print".into(), V::I32(3)));
        assert_eq!(penultimate, ("print".into(), V::I32(200)));
        assert_eq!(antepenultimate, ("print".into(), V::I32(100)));
    }

    //* {
    //*     let a = 10;
    //*     print a;
    //*     print b;
    //*     print c;
    //* }
    #[test]
    fn test_block_tree() {
        let tokens = vec![
            Tk::LBrace,
            Tk::Var,
            Tk::Identifier("a".into()),
            Tk::Assign,
            Tk::Num(10),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into()),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into()),
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let expression = parser.statement().unwrap_or_else(|_| Statement::None);
        assert_eq!(
            expression,
            Statement::Block(vec![
                Statement::Let {
                    name: Tk::Identifier("a".into()),
                    initializer: Expr::Literal(V::I32(10))
                },
                Statement::Print(Expr::Let(Tk::Identifier("a".into()))),
                Statement::Print(Expr::Let(Tk::Identifier("b".into()))),
                Statement::Print(Expr::Let(Tk::Identifier("c".into())))
            ])
        );
    }
    // #[test]
    // fn test_assignment_tree_err() {
    //     let tokens = vec![
    //         Tk::Identifier("a".into()),
    //         Tk::Sub,
    //         Tk::Identifier("b".into()),
    //         Tk::Assign,
    //         Tk::Identifier("c".into()),
    //         Tk::Semi,
    //         Tk::End,
    //     ];
    //     let mut parser = Parser::new(tokens);
    //     let expression = parser.expression().unwrap_or_else(|_| Expr::None);

    //     assert_eq!(
    //         expression,
    //         Expr::Assign(
    //             Tk::Identifier("a".into()),
    //             Box::new(Expr::Literal(V::I32(3)))
    //         )
    //     );
    // }

    //* print a = 2;
    #[test]
    fn test_assign_evaluation() {
        let tokens = vec![
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Assign,
            Tk::Num(2),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut environment = Env::new();
        environment.define("a".into(), V::I32(20));
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);

        // todo: assert stdout
        let data = inter.results.last().unwrap().clone();
        assert_eq!(data, ("print".into(), V::I32(2)));
    }

    //* print a = 2;
    #[test]
    #[should_panic = "not yet implemented"]
    fn test_assign_evaluation_runtime_err() {
        let tokens = vec![
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Assign,
            Tk::Num(2),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let mut inter = Interpreter::new(Env::new());
        inter.eval(statements);

        // todo: assert stdout
        let data = inter.results.last().unwrap().clone();
        assert_eq!(data, ("print".into(), V::I32(2)));
    }

    //* ->  a = 3;
    #[test]
    fn test_assignment_tree() {
        let tokens = vec![
            Tk::Identifier("a".into()),
            Tk::Assign,
            Tk::Num(3),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let expression = parser.expression().unwrap_or_else(|_| Expr::None);

        assert_eq!(
            expression,
            Expr::Assign(
                Tk::Identifier("a".into()),
                Box::new(Expr::Literal(V::I32(3)))
            )
        );
    }

    //* let a = 2;
    //* let b = 4;
    //* print a - b;
    #[test]
    fn test_let_evaluation() {
        let tokens = vec![
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Sub,
            Tk::Identifier("b".into()),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut environment = Env::new();
        environment.define("a".into(), V::I32(2));
        environment.define("b".into(), V::I32(4));
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);

        // todo: assert stdout
        let data = inter.results.last().unwrap().clone();
        assert_eq!(data, ("print".into(), V::I32(-2)));
    }

    //* ->  let jamon = 100;
    //* ->  let jamon = lol;
    #[test]
    fn test_let_declarations() {
        let tokens = vec![
            Tk::Var,
            Tk::Identifier("jamon".into()),
            Tk::Assign,
            Tk::Num(100),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let expression = parser
            .variable_or_statement()
            .unwrap_or_else(|_| Statement::None);

        assert_eq!(
            expression,
            Statement::Let {
                name: Tk::Identifier("jamon".into()),
                initializer: Expr::Literal(V::I32(100))
            }
        );

        let tokens = vec![
            Tk::Var,
            Tk::Identifier("jamon".into()),
            Tk::Assign,
            Tk::Identifier("lol".into()),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let expression = parser
            .variable_or_statement()
            .unwrap_or_else(|_| Statement::None);

        assert_eq!(
            expression,
            Statement::Let {
                name: Tk::Identifier("jamon".into()),
                initializer: Expr::Let(Tk::Identifier("lol".into()))
            }
        );
    }

    //* put !42 ;
    //* "throws" not a boolean❗
    #[test]
    #[should_panic = "not yet implemented"]
    fn test_runtime_error_on_unary_bang() {
        let tokens = vec![Tk::Print, Tk::Bang, Tk::Num(42), Tk::Semi, Tk::End];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let mut inter = Interpreter::new(Env::new());
        inter.eval(statements);

        // todo: assert stdout
    }

    //* ->  !false
    #[test]
    fn test_bool_expressions() {
        let tokens = vec![Tk::Bang, Tk::False, Tk::End];
        let mut parser = Parser::new(tokens);
        let expression = parser.expression().unwrap_or_else(|_| Expr::None);

        assert_eq!(
            expression,
            Expr::Unary {
                operator: Tk::Bang,
                right: Box::new(Expr::Literal(V::Bool(false)))
            }
        );
    }

    //* put 2-3 ;
    #[test]
    fn test_run_interpreter() {
        let tokens = vec![
            Tk::Print,
            Tk::Num(2),
            Tk::Sub,
            Tk::Num(3),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut inter = Interpreter::new(Env::new());
        inter.eval(statements);

        // todo: assert stdout
        let data = inter.results.last().unwrap().clone();
        assert_eq!(data, ("print".into(), V::I32(-1)));
    }

    //* put 2 - false ;
    //* "throws" not a number❗
    #[test]
    #[should_panic = "not yet implemented"]
    fn test_runtime_error() {
        let tokens = vec![Tk::Print, Tk::Num(2), Tk::Sub, Tk::False, Tk::Semi, Tk::End];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        println!("{:?}", statements);
        let mut inter = Interpreter::new(Env::new());
        inter.eval(statements);

        // todo: assert stdout
        //? assert_eq!(expr, Statement::Print(Expr::Literal(Value::I32(1))));
    }

    //* put 1;
    #[test]
    fn test_interpreter() {
        let tokens = vec![Tk::Print, Tk::Num(1), Tk::Semi, Tk::End];
        let mut parser = Parser::new(tokens);
        let expression = parser.parse();

        for expr in expression {
            assert_eq!(expr, Statement::Print(Expr::Literal(V::I32(1))));
        }
    }

    //* put 1;
    #[test]
    fn test_print() {
        let tokens = vec![Tk::Print, Tk::Num(1), Tk::Semi, Tk::End];
        let mut parser = Parser::new(tokens);
        let expression = parser.parse();

        for expr in expression {
            assert_eq!(expr, Statement::Print(Expr::Literal(V::I32(1))));
        }
    }

    //* ->  1 - (2 * 3) < 4 == false
    #[test]
    #[should_panic]
    fn parser_works_with_err() {
        let tokens = vec![
            Tk::Num(1),
            Tk::Sub,
            Tk::Lpar,
            Tk::Class,
            Tk::Mul,
            Tk::Num(3),
            Tk::Rpar,
            Tk::LT,
            Tk::Num(4),
            Tk::EQ,
            Tk::False,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let expression = parser.expression().unwrap_or_else(|_| Expr::None);

        assert_eq!(
            //*  "(- 1 (* (group ❌ algún error!) 3))"
            expression.to_string(),
            "(== (< (- 1 (group (* 2 3))) 4) false)".to_string()
        );
    }

    //* ->  1 - (2 * 3( < 4 == false
    #[test]
    fn parser_works() {
        let tokens = vec![
            Tk::Num(1),
            Tk::Sub,
            Tk::Lpar,
            Tk::Num(2),
            Tk::Mul,
            Tk::Num(3),
            Tk::Lpar,
            Tk::LT,
            Tk::Num(4),
            Tk::EQ,
            Tk::False,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let expression = parser.expression().unwrap_or_else(|_| Expr::None);

        assert_eq!(
            expression.to_string(),
            "(== (< (- 1 (group (* 2 3))) 4) false)".to_string()
        );
    }

    //* ->  -123 * (45.65)
    #[test]
    fn parser_works_2() {
        let tokens = vec![
            Tk::Sub,
            Tk::Num(123),
            Tk::Mul,
            Tk::Lpar,
            Tk::Float(45.65),
            Tk::Rpar,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let expression = parser.expression().unwrap_or_else(|_| Expr::None);

        assert_eq!(
            expression.to_string(),
            "(* (-123) (group 45.65))".to_string()
        );
    }
}
