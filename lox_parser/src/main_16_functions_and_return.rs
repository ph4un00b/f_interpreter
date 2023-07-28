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
     * It stores the callee expression and a list of expressions for the arguments.
     * It also stores the token for the closing parenthesis.
     * We’ll use that token’s location when we report a runtime error
     * caused by a function call.
     */
    FunctionCall {
        callee: Box<Expr>,
        paren: Tk,
        arguments: Vec<Expr>,
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
    Done,
    Return(Box<V>),
    Callable(Box<FunctionObject>),
    NativeCallable(String),
    I32(i32),
    F64(f64),
    String(String),
    Bool(bool),
}

trait Arity {
    fn arity(&self) -> usize;
}

impl Arity for V {
    fn arity(&self) -> usize {
        match self {
            V::NativeCallable(fn_name) => match fn_name.as_str() {
                //todo: maybe use an enum to have compile time checks ❓
                "clock" => 0,
                _ => panic!(),
            },
            _ => {
                // Handle other variants if needed
                // For example, return a default arity value or throw an error
                panic!()
            }
        }
    }
}
trait FnCallable {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<V>) -> RValue;
}

impl FnCallable for V {
    fn call(&self, _interpreter: &mut Interpreter, _arguments: Vec<V>) -> RValue {
        match self {
            V::NativeCallable(fn_name) => match fn_name.as_str() {
                "clock" => {
                    let current_time = std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .expect("Failed to get current time");

                    Ok(V::F64(current_time.as_secs() as f64))
                }
                _ => panic!(),
            },
            _ => {
                // Handle other variants if needed
                // For example, return a default arity value or throw an error
                panic!()
            }
        }
    }
}

impl fmt::Display for V {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            V::Done => write!(f, "done!")?,
            V::Return(val) => write!(f, "ret {:?}", val)?,
            V::Callable(function) => write!(f, "#{:?}", function)?,
            V::NativeCallable(fn_name) => match fn_name.as_str() {
                "clock" => write!(f, "#<native-{}>", fn_name)?,
                _ => write!(f, "#{}", fn_name)?,
            },
            // V::Callable(id) => write!(f, "#{}", id)?,
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
            Tk::Comma => ",".to_string(),
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
    Comma,
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
            Expr::FunctionCall {
                callee,
                paren: _,
                arguments,
            } => write!(f, "{}::(arguments{:?})", callee, arguments)?,
        }

        Ok(())
    }
}

// todo: can it be automated❓👀
#[derive(Debug, PartialEq, Clone)]
enum Statement {
    None(String),
    Expr(Expr),
    /*
     * This means every Lox function must return something,
     * even if it contains no return statements at all.
     *
     * We use nil for this, which is why LoxFunction’s implementation of call()
     * returns null at the end.
     * In that same vein, if you omit the value in a return statement,
     * we simply treat it as equivalent to:
     *
     * return nil;
     */
    Return {
        keyword: Tk,
        value: Expr,
    },
    /*
     * Function declarations, like variables, bind a new name.
     * That means they are allowed only in places where a declaration is permitted.
     *
     * A named function declaration isn’t really a single primitive operation.
     * It’s syntactic sugar for two distinct steps:
     *
     * (1) creating a new function object, and
     * (2) binding a new variable to it.
     *
     * If Lox had syntax for anonymous functions, we wouldn’t need
     * function declaration statements.
     */
    FunctionDeclaration {
        name: Tk,
        parameters: Vec<Tk>,
        body: Vec<Statement>,
    },
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
    LetDeclaration {
        name: Tk,
        initializer: Expr,
    },
    //* The curly-braced block statement that defines a local scope
    Block(Vec<Statement>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionObject {
    //* Stmt.Function
    declaration: Statement,
}

impl fmt::Display for FunctionObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // todo: look for refactors❗
        match self.declaration.clone() {
            Statement::FunctionDeclaration {
                name,
                parameters: _,
                body: _,
            } => Ok(write!(f, "<fn {}>", String::from(name))?),
            _ => unreachable!("only fn declaration!"),
        }
    }
}

impl Arity for FunctionObject {
    fn arity(&self) -> usize {
        // todo: look for refactors❗
        /*
         * Note when we bind the parameters,
         * we assume the parameter and argument lists have the same length.
         * This is safe because CallExpr() checks the arity before calling call().
         * It relies on the function reporting its arity to do that.
         */
        match self.declaration.clone() {
            Statement::FunctionDeclaration {
                name: _,
                parameters,
                body: _,
            } => parameters.len(),
            _ => unreachable!("only fn declaration!"),
        }
    }
}

// impl From<Statement> for Vec<Tk> {
//     fn from(value: Statement) -> Self {
//         match value {
//             Statement::FnDeclaration {
//                 name: _,
//                 parameters,
//                 body: _,
//             } => parameters,
//             _ => unreachable!("no parameters!"),
//         }
//     }
// }

// impl From<Statement> for Vec<Statement> {
//     fn from(value: Statement) -> Self {
//         match value {
//             Statement::FnDeclaration {
//                 name: _,
//                 parameters: _,
//                 body,
//             } => body,
//             _ => unreachable!("no body!"),
//         }
//     }
// }

impl FnCallable for FunctionObject {
    /*
     * Mechanically, the code is pretty simple.
     *
     * Walk a couple of lists.
     * Bind some new variables.
     * Call a method.
     *
     * But this is where the crystalline code of the function declaration
     * becomes a living, breathing invocation.
     *
     * Feel free to take a moment to meditate on it if you’re so inclined.
     */
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<V>) -> RValue {
        // todo: look for refactors❗
        let (_, fn_params, fn_body) = match self.declaration.clone() {
            Statement::FunctionDeclaration {
                name,
                parameters,
                body,
            } => (name, parameters, body),
            _ => unreachable!("only fn declaration!"),
        };
        /*
         * This handful of lines of code is one of the most fundamental❗
         *
         * At the beginning of the call, it creates a new environment.
         * Then it walks the parameter and argument lists in lockstep.
         * For each pair, it creates a new variable with the parameter’s
         * name and binds it to the argument’s value.
         */
        let mut params_env = Env::new(EnvKind::Call);
        for (index, param) in fn_params.into_iter().enumerate() {
            let param_name = String::from(param);
            let argument_val = arguments.get(index).unwrap();
            /*
             * Parameters are core to functions, especially the fact that
             * a function encapsulates its parameters—no other code outside
             * of the function can see them.
             * This means each function gets its own environment where it
             * stores those variables.
             *
             * Further, this environment must be created dynamically.
             * Each function call gets its own environment.
             * Otherwise, recursion would break.
             *
             * If there are multiple calls to the same function
             * in play at the same time, each needs its own environment,
             * even though they are all calls to the same function.
             */
            //?  fn count(n) {
            //?     if (n > 1) count(n - 1);
            //?     print n;
            //?   }
            //?   count(3);
            /*
             * Imagine we pause the interpreter right at the point where
             * it’s about to print 1 in the innermost nested call.
             * The outer calls to print 2 and 3 haven’t printed their values yet,
             * so there must be environments somewhere in memory that still
             *
             * store the fact that n is bound to 3 in one context,
             * count(3) -> n = 3
             *
             * 2 in another,
             * count(2) -> n = 2
             *
             * and 1 in the innermost
             * count(1) -> n = 1
             *
             * That’s why we create a new environment at each call,
             * not at the function declaration.
             */
            params_env.define(param_name, argument_val.clone());
        }
        /*
         * Once the body of the function has finished executing,
         * executeBlock() discards that function-local environment
         * and restores the previous one that was active back at the callsite.
         * Finally, call() returns null, which returns nil to the caller.
         * (We’ll add return values later.)
         */
        interpreter.global_env.push(params_env);
        let returned = interpreter.eval_block(fn_body)?;
        interpreter.global_env.pop();

        match returned {
            V::Return(value) => Ok(*value),
            _ => Ok(V::Done),
        }
    }
}

impl FunctionObject {
    fn new(declaration: Statement) -> Self {
        //todo: this should be only for Statement::Function❗
        Self { declaration }
    }
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
        /*
         * In Lox, functions and variables occupy the same
         * namespace. In Common Lisp, the two live in their
         * own worlds.
         * A function and variable with the same name don’t
         * collide. If you call the name, it looks up the
         * function. If you refer to it, it looks up the
         * variable.
         * This does require jumping through some hoops when
         * you do want to refer to a function as a first-class
         * value.
         *
         * Richard P. Gabriel and Kent Pitman coined the
         * terms “Lisp-1” to refer to languages like
         * Scheme that put functions and variables in
         * the same namespace, and “Lisp-2” for languages
         * like Common Lisp that partition them.
         * Despite being totally opaque, those names have
         * since stuck. Lox is a Lisp-1.
         */
        let mut preludio = Env::new(EnvKind::Prelude);
        preludio.define("clock".to_string(), V::NativeCallable("clock".to_string()));
        Self {
            runtime_error: false,
            global_env: vec![preludio, initial_env],
            results: vec![],
        }
    }

    fn eval(&mut self, statements: Vec<Statement>) {
        for state in statements {
            let value = self.eval_statement(state);
            match value {
                Ok(_val) => (),
                Err(err) => match err {
                    RE::MustBeNumber(_) => todo!(),
                    RE::NotNumber(_, _, _) => todo!(),
                    RE::MustBeBoolean(_) => todo!(),
                    RE::UndefinedVariable(v) => todo!("undefined variable {}", v),
                    RE::NotCallableValue(_) => todo!(),
                    RE::WrongCallableArity(_, _, _) => todo!(),
                },
            }
        }
    }

    fn eval_block(&mut self, statements: Vec<Statement>) -> RValue {
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
        self.global_env.push(Env::new(EnvKind::Block));
        // println!(">>> new block {:?}", self.global_env);
        for state in statements {
            let returned = self.eval_statement(state)?;
            if let V::Return(value) = returned {
                self.global_env.pop();
                return Ok(V::Return(value));
            }
        }
        /*
         * To execute code within a given scope, this method updates the
         * interpreter’s environment field, visits all of the statements,
         * and then restores the previous value.
         */
        self.global_env.pop();
        // println!("<<< block out {:?}", self.global_env);
        Ok(V::Done)
        // todo: it gets restored even if an exception is thrown.
    }

    fn eval_statement(&mut self, state: Statement) -> RValue {
        // println!("{:?}", state);
        match state {
            Statement::Return { keyword: _, value } => {
                let evaluated_value = self.eval_expr(value)?;
                self.results.push(("ret".into(), evaluated_value.clone()));
                Ok(V::Return(Box::new(evaluated_value)))
            }
            Statement::FunctionDeclaration {
                name,
                parameters,
                body,
            } => {
                /*
                 * This is similar to how we interpret other literal expressions.
                 * We take a function syntax node—a compile-time representation
                 * of the function—and convert it to its runtime representation.
                 * Here, that’s a CallableObject that wraps the syntax node.
                 */
                let function = FunctionObject::new(Statement::FunctionDeclaration {
                    name: name.clone(),
                    parameters,
                    body,
                });
                /*
                 * Function declarations are different from other literal nodes
                 * in that the declaration also binds the resulting object to
                 * a new variable. So, after creating the LoxFunction,
                 * we create a new binding in the current environment and
                 * store a reference to it there.
                 */
                self.define_variable(name, V::Callable(Box::new(function)));
                Ok(V::Done)
            }
            Statement::While { condition, body } => {
                //? este el truco, evaluar en cada loop❗
                while self.eval_expr(condition.clone()) == Ok(V::Bool(true)) {
                    // todo: improve allocations❓👀
                    self.eval_statement(*body.clone())?;
                }
                Ok(V::Done)
            }
            Statement::Cond {
                condition,
                then_statement,
                else_statement,
            } => {
                let evaluated_condition = self.eval_expr(condition)?;
                println!("{}", evaluated_condition);
                /*
                 * Most other syntax trees always evaluate their subtrees.
                 * Here, we may not evaluate the then or else statement.
                 * If either of those has a side effect,
                 * the choice not to evaluate it becomes user visible.
                 */
                if let Statement::None(_) = *else_statement {
                    match evaluated_condition {
                        V::Bool(true) => self.eval_statement(*then_statement),
                        V::Bool(false) => Ok(V::Done),
                        _ => unreachable!(),
                    }
                } else {
                    match evaluated_condition {
                        V::Bool(true) => self.eval_statement(*then_statement),
                        V::Bool(false) => self.eval_statement(*else_statement),
                        _ => unreachable!(),
                    }
                }
            }
            Statement::Block(statements) => self.eval_block(statements),
            /*
             * Once we have functions, we could simplify the language by
             * tearing out the old print syntax and replacing it with a
             * native function.
             * But that would mean that examples early in the book
             * wouldn’t run on the interpreter from later chapters
             * and vice versa. So, for the book, I’ll leave it alone.
             *
             * If you’re building an interpreter for your own language,
             * though, you may want to consider it.
             */
            Statement::Print(expr) => {
                let value = self.eval_expr(expr)?;
                self.results.push(("print".into(), value.clone()));
                println!("🎈 {value}");
                Ok(V::Done)
            }
            Statement::Expr(expr) => self.eval_expr(expr),
            Statement::LetDeclaration { name, initializer } => {
                /*
                 * If the variable has an initializer, we evaluate it.
                 * If not, we have another choice to make.
                 * We could have made this a syntax error in
                 * the parser by requiring an initializer.
                 * Most languages don’t, though, so it feels
                 * a little harsh to do so in Lox.
                 */
                let val = self.eval_expr(initializer)?;
                self.define_variable(name, val);
                Ok(V::Done)
            }
            Statement::None(msg) => unreachable!("{}", msg),
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
        // self.results.push(("let".into(), val.clone()));
        let name = String::from(name);
        if let Some(local_values) = self.global_env.last_mut() {
            println!("{:#?}", local_values);
            local_values.define(name, val);
        }
        println!("{:?}", self.global_env);
    }

    fn eval_expr(&mut self, expr: Expr) -> RValue {
        // println!("{:?}", expr);
        match expr {
            Expr::FunctionCall {
                callee,
                paren,
                arguments,
            } => {
                /*
                 * First, we evaluate the expression for the callee.
                 * Typically, this expression is just an identifier
                 * that looks up the function by its name, but it
                 * could be anything. Then we evaluate each of the
                 * argument expressions in order and store the resulting
                 * values in a list.
                 */
                let function_value = self.eval_expr(*callee)?;
                let mut argument_list = vec![];
                for argument_expr in arguments {
                    /*
                     * This is another one of those subtle semantic choices.
                     * Since argument expressions may have side effects,
                     * the order they are evaluated could be user visible.
                     *
                     * Even so, some languages like Scheme and C don’t specify
                     * an order.
                     * This gives compilers freedom to reorder them
                     * for efficiency, but means users may be unpleasantly
                     * surprised if arguments aren’t evaluated in the order
                     * they expect.
                     */
                    let fn_arg = self.eval_expr(argument_expr)?;
                    argument_list.push(fn_arg);
                }

                /*
                 * PROBLEM #1:
                 *
                 * "totally not a function"();
                 *
                 * Strings aren’t callable in Lox.
                 * The runtime representation of a Lox string is a
                 * Java string, so when we cast that to LoxCallable,
                 * the JVM will throw a ClassCastException.
                 * We don’t want our interpreter to vomit out some nasty
                 * Java stack trace and die.
                 * Instead, we need to check the type ourselves first.
                 */
                // todo: test err❗

                /*
                 * PROBLEM #2
                 *
                 * add(1, 2, 3, 4); // Too many.
                 * add(1, 2);       // Too few.
                 *
                 * Different languages take different approaches to this
                 * problem. Of course, most statically typed languages
                 * check this at compile time and refuse to compile the
                 * code if the argument count doesn’t match the function’s
                 * arity. JavaScript discards any extra arguments you pass.
                 *
                 * If you don’t pass enough, it fills in the missing
                 * parameters with the magic
                 * sort-of-like-null-but-not-really value undefined.
                 *
                 * Python is stricter. It raises a runtime error if the
                 * argument list is too short or too long.
                 */
                println!("\nFN ARG {:?}", argument_list);
                // todo: test err❗
                let function = match function_value {
                    V::Callable(function) => function,
                    // V::NativeCallable(_) => self.call(function_value, argument_values),
                    /*
                     * We still throw an exception,
                     * but now we’re throwing our own exception type,
                     * one that the interpreter knows to catch and
                     * report gracefully.
                     */
                    _ => return Err(RE::NotCallableValue(paren)),
                };

                if argument_list.len() != function.arity() {
                    return Err(RE::WrongCallableArity(
                        paren,
                        function.arity(),
                        argument_list.len(),
                    ));
                }
                println!("\n WiLL CALL {}", function);
                function.call(self, argument_list)
            }
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
                // println!("\n>>> searching: {:?}", self.global_env);
                // todo: verify this refactor on assignment
                let mut iter = self.global_env.iter_mut();
                loop {
                    match iter.next_back() {
                        Some(outer_env) => {
                            println!("{:?} for {:?}", outer_env, identifier);
                            if let Some(value) = outer_env.fetch(identifier.clone()) {
                                return Ok(value);
                            }
                        }
                        None => return Err(RE::UndefinedVariable(identifier.into())),
                    }
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
    NotCallableValue(Tk),
    WrongCallableArity(Tk, usize, usize),
}

enum Kind {
    Function,
}

use std::result::Result as StdResult;

// use env_01_recursive::Env;
use env_02_vector::Env;

use crate::env_02_vector::EnvKind;
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
     *
     * declaration    → function Declaration
     *                  | variable Declaration
     *                  | statement ;
     *
     * statement      → exprStmt
     *                  | ifStmt
     *                  | printStmt
     *                  | whileStmt
     *                  | block ;
     *
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
            let statement = self.declaration_or_statement().unwrap_or_else(|err| {
                match err {
                    ParserAy::BadExpression(bad_token) => {
                        println!(
                            "❌ desde parse!, expected expression got {:?}😱❗",
                            bad_token
                        )
                    }
                };

                Statement::None("#parse".into())
            });
            result.push(statement);
        }

        result
    }

    //* declaration    → function Declaration
    //*                  | variable Declaration
    //*                  | statement ;
    fn declaration_or_statement(&mut self) -> RStatement {
        let result = match self.current_token {
            Tk::Fn => self.function_declaration(Kind::Function)?,
            Tk::Var => self.variable_declaration()?,
            _ => self.statement()?,
        };

        Ok(result)
    }
    // * funDecl        → "fun" function ;
    //? It’s like the earlier arguments rule,
    //? except that each parameter is an identifier, not an expression.
    // * function       → IDENTIFIER "(" parameters? ")" block ;
    // * parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
    /*
     * The main funDecl rule uses a separate helper rule function.
     * A function declaration statement is the fun keyword followed
     * by the actual function-y stuff.
     *
     * When we get to classes, we’ll reuse that function rule for declaring methods.
     * Those look similar to function declarations, but aren’t preceded by fun.
     */
    fn function_declaration(&mut self, kind: Kind) -> RStatement {
        let kind_string = match kind {
            Kind::Function => "function",
        };
        self.consume_token();
        let name = if let Tk::Identifier(_) = self.current_token {
            self.current_token.clone()
        } else {
            self.report_and_consume(
                Tk::Lpar,
                format!("Expected {} name!.", kind_string).as_str(),
            );
            Tk::Default
        };
        // consume(LEFT_PAREN, "Expect '(' after " + kind + " name.");
        self.consume_token();
        self.report_and_consume(
            Tk::Lpar,
            format!("Expect '(' after {} name.", kind_string).as_str(),
        );
        // List<Token> parameters = new ArrayList<>();
        let mut parameters: Vec<Tk> = vec![];
        // if (!check(RIGHT_PAREN)) {
        if self.current_token != Tk::Rpar {
            loop {
                /*
                 * This is like the code for handling arguments in a call,
                 * except not split out into a helper method.
                 * The outer if statement handles the zero parameter case,
                 * and the inner while loop parses parameters as long as we
                 * find commas to separate them.
                 * The result is the list of tokens for each parameter’s name.
                 */
                if parameters.len() >= 255 {
                    // todo: test err❗"
                    report_err(
                        Tk::Default,
                        self.current_token.clone(),
                        "Can't have more than 255 parameters.",
                    );
                }
                // self.consume_token();
                let parameter_identifier = if let Tk::Identifier(_) = self.current_token {
                    self.current_token.clone()
                } else {
                    self.report_and_consume(
                        Tk::Identifier("parameter".to_string()),
                        "Expect parameter name",
                    );
                    Tk::Default
                };
                parameters.push(parameter_identifier);
                self.consume_token();
                if self.current_token != Tk::Comma {
                    break;
                }
                self.consume_token();
            }
        };
        // consume(RIGHT_PAREN, "Expect ')' after parameters.");
        self.report_and_consume(Tk::Rpar, "Expect ')' after parameters.");
        /*
         * Note that we consume the { at the beginning of the body here before calling block().
         * That’s because block() assumes the brace token has already been matched.
         * Consuming it here lets us report a more precise error message
         * if the { isn’t found since we know it’s in the context of a function declaration.
         */
        // consume(LEFT_BRACE, "Expect '{' before " + kind + " body.");
        // List<Stmt> body = block();
        let body = match self.current_token {
            Tk::LBrace => self.block_statement()?,
            _ => {
                report_err(
                    Tk::LBrace,
                    self.current_token.clone(),
                    format!("Expect '{{' before {} body.", kind_string).as_str(),
                );
                self.block_statement()?
            }
        };
        // return new Stmt.Function(name, parameters, body);
        Ok(Statement::FunctionDeclaration {
            name,
            parameters,
            body,
        })
    }

    //* "let" IDENTIFIER ( "=" expression )? ";" ;
    fn variable_declaration(&mut self) -> RStatement {
        /*
         * As always, the recursive descent code follows
         * the grammar rule. The parser has already
         * matched the var token, so next it requires
         * and consumes an identifier token for the
         * variable name.
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

        Ok(Statement::LetDeclaration {
            name: result_token,
            initializer,
        })
    }

    /*
     * statement      → exprStmt
     *                  | forStmt
     *                  | ifStmt
     *                  | returnStmt
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
            Tk::Return => self.return_statement()?,
            Tk::For => self.for_statement()?,
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

    /*
     * The return value is optional to support exiting early from a
     * function that doesn’t return a useful value.
     *
     * In statically typed languages, “void” functions don’t return a
     * value and non-void ones do. Since Lox is dynamically typed,
     * there are no true void functions.
     *
     * The compiler has no way of preventing you from taking the
     * result value of a call to a function that doesn’t contain a return statement.
     */
    //* returnStmt     → "return" expression? ";" ;
    fn return_statement(&mut self) -> RStatement {
        self.consume_token();
        // Token keyword = previous();
        let keyword = self.prev_token.clone();
        // Expr value = null;
        // if (!check(SEMICOLON)) {
        //   value = expression();
        // }
        let value = if self.current_token != Tk::Semi || self.current_token != Tk::End {
            /*
             * After snagging the previously consumed return keyword,
             * we look for a value expression.
             * Since many different tokens can potentially start an expression,
             * it’s hard to tell if a return value is present.
             * Instead, we check if it’s absent.
             * Since a semicolon can’t begin an expression, if the next token is that,
             * we know there must not be a value.
             */
            self.expression()?
        } else {
            Expr::None
        };
        // consume(SEMICOLON, "Expect ';' after return value.");
        self.report_and_consume(Tk::Semi, "Expect ';' after return value.");
        // return new Stmt.Return(keyword, value);
        Ok(Statement::Return { keyword, value })
    }

    /*
     * forStmt      → "for"
     *                "("
     *                      ( varDecl | exprStmt | ";" )
     *                      expression? ";"
     *                      expression?
     *                ")"
     *                statement ;
     */
    fn for_statement(&mut self) -> RStatement {
        /*
         * We’re going to desugar for loops to the while loops
         * and other statements the interpreter already handles.
         * In our simple interpreter, desugaring really doesn’t
         * save us much work, but it does give me an excuse
         * to introduce you to the technique.
         * So, unlike the previous statements, we won’t add
         * a new syntax tree node. Instead, we go straight to parsing. First, add an import we’ll need soon.
         */
        self.consume_token();
        // consume(LEFT_PAREN, "Expect '(' after 'for'.");
        self.report_and_consume(Tk::Lpar, "Expect '(' after 'for'");
        // Stmt initializer;
        // if (match(SEMICOLON)) {
        //   initializer = null;
        // } else if (match(VAR)) {
        //   initializer = varDeclaration();
        // } else {
        //   initializer = expressionStatement();
        // }
        let initializer = match self.current_token {
            //? If the token following the ( is a semicolon then the initializer has been omitted.
            Tk::Semi => Statement::None("omitted initializer in 'for'".into()),
            //? Otherwise, we check for a var keyword to see if it’s a variable declaration.
            Tk::Var => self.variable_declaration()?,
            //? If neither of those matched, it must be an expression.
            //? We parse that and wrap it in an expression statement
            //? so that the initializer is always of type Stmt
            _ => self.expr_statement()?,
        };
        // Expr condition = null;
        // if (!check(SEMICOLON)) {
        //   condition = expression();
        // }
        let condition = if self.current_token != Tk::Semi {
            self.expression()?
        } else {
            Expr::None
        };
        // consume(SEMICOLON, "Expect ';' after loop condition.");
        self.report_and_consume(Tk::Semi, "Expect ';' after 'loop condition'");
        //? Again, we look for a semicolon to see if the clause has been omitted.
        //? The last clause is the increment.
        // Expr increment = null;
        // if (!check(RIGHT_PAREN)) {
        //   increment = expression();
        // }
        let increment = if self.current_token != Tk::Rpar {
            self.expression()?
        } else {
            Expr::None
        };
        // consume(RIGHT_PAREN, "Expect ')' after for clauses.");
        self.report_and_consume(Tk::Rpar, "Expect ')' after 'for clauses'");
        //? It’s similar to the condition clause except this one is
        //? terminated by the closing parenthesis.
        //? All that remains is the body. 😏
        // Stmt body = statement();
        let body = self.statement()?;

        /*
         * We’ve parsed all of the various pieces of the for loop and
         * the resulting AST nodes are sitting in a handful of Java local variables.
         * This is where the desugaring comes in.
         * We take those and use them to synthesize syntax tree nodes that
         * express the semantics of the for loop, like the
         * hand-desugar example I showed you earlier.
         *
         * The code is a little simpler if we work backward,
         * so we start with the increment clause.
         */
        // if (increment != null) {
        //     body = new Stmt.Block(
        //         Arrays.asList(
        //             body,
        //             new Stmt.Expression(increment)));
        let body = if let Expr::None = increment {
            Statement::None("no expr in 'for'".into())
        } else {
            Statement::Block(vec![body, Statement::Expr(increment)])
        };

        /*
         * The increment, if there is one, executes after the body in
         * each iteration of the loop.
         * We do that by replacing the
         * body with a little block that contains the original body
         * followed by an expression statement that evaluates the increment.
         */
        let body = if let Expr::None = condition {
            /*
             * Next, we take the condition and the body and build the loop
             * using a primitive while loop.
             * If the condition is omitted, we jam in true to make an infinite loop.
             */
            Statement::While {
                condition: Expr::Literal(V::Bool(true)),
                body: Box::new(body),
            }
        } else {
            Statement::While {
                condition,
                body: Box::new(body),
            }
        };

        //  if (initializer != null) {
        //     body = new Stmt.Block(Arrays.asList(initializer, body));
        //   }
        let body = if let Statement::None(_msg) = initializer {
            Statement::None("no initializer in 'for'".into())
        } else {
            /*
             * Finally, if there is an initializer, it runs once before the entire loop.
             * We do that by, again, replacing the whole statement with a block
             * that runs the initializer and then executes the loop.
             */
            Statement::Block(vec![initializer, body])
        };

        /*
         * That’s it. Our interpreter now supports C-style for loops
         * and we didn’t have to touch the Interpreter class at all.
         * Since we desugar to nodes the interpreter already knows how to visit,
         * there is no more work to do.
         */
        Ok(body)
        // return body;
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
        self.consume_token();
        // consume(LEFT_PAREN, "Expect '(' after 'if'.");
        self.report_and_consume(Tk::Lpar, "Expect '(' after 'if'");
        // Expr condition = expression();
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
            Box::new(Statement::None("no else in 'if'".into()))
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
            result.push(self.declaration_or_statement()?);
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

    // * unary      = _{ (bang | neg) ~ unary | call }
    // * call       → primary ( "(" arguments? ")" )* ;
    fn unary(&mut self) -> Result<Expr> {
        if self.current_token == Tk::Bang || self.current_token == Tk::Sub {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.unary()?;
            return Ok(Expr::unary_op(operator, right));
        }
        self.call()
    }

    // * call       → primary ( "(" arguments? ")" )* ;
    // * arguments  → expression ( "," expression )* ;
    /*
     * The rule uses * to allow matching a series of calls
     * like fn(1)(2)(3). Code like that isn’t common in C-style languages,
     * but it is in the family of languages derived from ML.
     * There, the normal way of defining a function that takes
     * multiple arguments is as a series of nested functions.
     * Each function takes one argument and returns a new function.
     */
    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;
        /*
         * This code would be simpler as while (match(LEFT_PAREN))
         * instead of the silly while (true) and break. Don’t worry,
         * it will make sense when we expand the parser later
         * to handle properties on objects.
         */
        loop {
            // self.consume_token();
            if self.current_token == Tk::Lpar {
                /*
                 * The code here doesn’t quite line up with the
                 * grammar rules. I moved a few things around to make
                 * the code cleaner—one of the luxuries we have with
                 * a handwritten parser. But it’s roughly similar to
                 * how we parse infix operators. First, we parse a
                 * primary expression, the “left operand” to the call.
                 *
                 * Then, each time we see a (, we call finishCall()
                 * to parse the call expression using the previously
                 * parsed expression as the callee.
                 * The returned expression becomes the new expr and
                 * we loop to see if the result is itself called.
                 */
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /*
     * Right now, the loop where we parse arguments has no bound.
     * If you want to call a function and pass a million arguments to it,
     * the parser would have no problem with it.
     *
     * Do we want to limit that❓
     */
    fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
        /*
         *  This is more or less the arguments grammar rule translated
         *  to code, except that we also handle the zero-argument case.
         *
         *  We check for that case first by seeing if the next token is ).
         *  If it is, we don’t try to parse any arguments.
         */
        // List<Expr> arguments = new ArrayList<>();
        let mut arguments: Vec<Expr> = vec![];
        // if (!check(RIGHT_PAREN)) {
        self.consume_token();
        // todo: test end case❗
        if self.current_token != Tk::Rpar && self.current_token != Tk::End {
            //   do {
            //     arguments.add(expression());
            //   } while (match(COMMA));
            // }
            /*
             * Otherwise, we parse an expression, then look for a comma
             * indicating that there is another argument after that.
             *
             * We keep doing that as long as we find commas after each
             * expression.
             */
            loop {
                /*
                 * Other languages have various approaches.
                 * The C standard says a conforming implementation has to support
                 * at least 127 arguments to a function, but doesn’t say there’s
                 * any upper limit.
                 * The Java specification says a method can accept no more than
                 * 255 arguments.
                 *
                 * The limit is 254 arguments if the method is an instance method.
                 * That’s because this—the receiver of the method—works
                 * like an argument that is implicitly passed to the method,
                 * so it claims one of the slots.
                 */
                // if (arguments.size() >= 255) {
                //     error(peek(), "Can't have more than 255 arguments.");
                //   }
                if arguments.len() >= 255 {
                    // todo: test err❗"
                    /*
                     * Note that the code here reports an error if
                     * it encounters too many arguments, but it doesn’t
                     * throw the error.
                     * Throwing is how we kick into panic mode which
                     * is what we want if the parser is in a confused
                     * tate and doesn’t know where it is in the
                     * grammar anymore. But here, the parser is still
                     * in a perfectly valid state—it just found too
                     * many arguments.
                     * So it reports the error and keeps on keeping on.
                     */
                    report_err(
                        Tk::Default,
                        self.current_token.clone(),
                        "Can't have more than 255 arguments.",
                    );
                }
                // Code that will be executed at least once
                let argument_expr = self.expression()?;
                arguments.push(argument_expr);
                // Conditional break to exit the loop
                if self.current_token != Tk::Comma {
                    break;
                }
                // Increment
                self.consume_token();
            }
        }
        /*
         * When we don’t find a comma,
         * then the argument list must be done and we consume the
         * expected closing parenthesis.
         *
         * Finally, we wrap the callee and those arguments up
         * into a call AST node.
         */
        // Token paren = consume(RIGHT_PAREN,
        //               "Expect ')' after arguments.");
        self.report_and_consume(Tk::Rpar, "Expect ')' after arguments.");
        // return new Expr.Call(callee, paren, arguments);
        Ok(Expr::FunctionCall {
            callee: Box::new(callee),
            paren: self.prev_token.clone(),
            arguments,
        })
    }

    // * arguments  → expression ( "," expression )* ;
    /*
     * This rule requires at least one argument expression,
     * followed by zero or more other expressions, each preceded by a comma.
     * To handle zero-argument calls, the call rule itself considers the entire
     * arguments production to be optional.
     *
     * I admit, this seems more grammatically awkward than you’d expect
     * for the incredibly common “zero or more comma-separated things” pattern.
     * There are some sophisticated metasyntaxes that handle this better,
     * ut in our BNF and in many language specs I’ve seen, it is this cumbersome.
     */
    fn arguments(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;
        while self.current_token == Tk::Lpar {
            self.consume_token();
            let operator = self.prev_token.clone();
            let right = self.arguments()?;
            expr = Expr::binary_op(expr, operator, right);
        }
        Ok(expr)
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

    // #[test]
    fn test_closure() {
        let tokens = vec![
            //? fun makeCounter() {
            //?     var i = 0;
            //?     fun count() {
            //?       i = i + 1;
            //?       print i;
            //?     }
            //?     return count;
            //?   }
            //?   var counter = makeCounter();
            //?   counter(); // "1".
            //?   counter(); // "2".
            Tk::Fn,
            Tk::Identifier("makeCounter".into()),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            //? var i = 0;
            Tk::Var,
            Tk::Identifier("i".into()),
            Tk::Assign,
            Tk::Num(0),
            Tk::Semi,
            //?     fun count() {
            //?       i = i + 1;
            //?       print i;
            //?     }
            Tk::Fn,
            Tk::Identifier("count".into()),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            Tk::Identifier("i".into()),
            Tk::Assign,
            Tk::Identifier("i".into()),
            Tk::Sub,
            Tk::Num(1),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("i".into()),
            Tk::Semi,
            Tk::RBrace,
            //?     return count;
            //?   }
            Tk::Return,
            Tk::Identifier("count".into()),
            Tk::Semi,
            Tk::RBrace,
            //?   var counter = makeCounter();
            Tk::Var,
            Tk::Identifier("counter".into()),
            Tk::Assign,
            Tk::Identifier("makeCounter".into()),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            //?   counter(); // "1".
            Tk::Identifier("counter".into()),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            // //?   counter(); // "2".
            // Tk::Identifier("counter".into()),
            // Tk::Lpar,
            // Tk::Rpar,
            // Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let environment = Env::new(EnvKind::Global);
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);

        // todo: assert stdout
        let last = inter.results.last();
        assert_eq!(last, Some(&("print".into(), V::I32(3))));
    }

    #[test]
    #[should_panic = "not yet implemented: undefined variable makeCounter"]
    fn test_call_with_arguments() {
        let tokens = vec![
            //?   var counter = makeCounter(3);
            Tk::Var,
            Tk::Identifier("counter".into()),
            Tk::Assign,
            Tk::Identifier("makeCounter".into()),
            Tk::Lpar,
            Tk::Num(3),
            Tk::Rpar,
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let environment = Env::new(EnvKind::Global);
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);
    }

    #[test]
    #[should_panic = "not yet implemented: undefined variable makeCounter"]
    fn test_call_with_no_arguments() {
        let tokens = vec![
            //?   var counter = makeCounter();
            Tk::Var,
            Tk::Identifier("counter".into()),
            Tk::Assign,
            Tk::Identifier("makeCounter".into()),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let environment = Env::new(EnvKind::Global);
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);
    }

    // * ->  fun fib(n) {
    // * ->     if (n < 1 or n == 1) return n;
    // * ->     return fib(n - 2) - fib(n - 1);
    // * ->  }
    // * ->  for (var i = 10; 0 < i; i = i - 1) {
    // * ->     print fib(i);
    // * ->  }
    #[test]
    fn test_recursion() {
        let tokens = vec![
            Tk::Fn,
            Tk::Identifier("fibo".into()),
            Tk::Lpar,
            Tk::Identifier("n".into()),
            Tk::Rpar,
            Tk::LBrace,
            // * ->     if (n < 1 or n == 1) return n;
            Tk::If,
            Tk::Lpar,
            Tk::Identifier("n".into()),
            Tk::LT,
            Tk::Num(1),
            Tk::Or,
            Tk::Identifier("n".into()),
            Tk::EQ,
            Tk::Num(1),
            Tk::Rpar,
            Tk::Return,
            Tk::Identifier("n".into()),
            Tk::Semi,
            // * ->     return fib(n - 2) - fib(n - 1);
            Tk::Return,
            Tk::Identifier("fibo".into()),
            Tk::Lpar,
            Tk::Identifier("n".into()),
            Tk::Sub,
            Tk::Num(2),
            Tk::Rpar,
            Tk::Sub,
            Tk::Identifier("fibo".into()),
            Tk::Lpar,
            Tk::Identifier("n".into()),
            Tk::Sub,
            Tk::Num(1),
            Tk::Rpar,
            Tk::Semi,
            Tk::RBrace,
            // * ->  for (let i = 11; 9 < i; i = i - 1) {
            Tk::For,
            Tk::Lpar,
            Tk::Var,
            Tk::Identifier("i".into()),
            Tk::Assign,
            Tk::Num(11),
            Tk::Semi,
            Tk::Num(9),
            Tk::LT,
            Tk::Identifier("i".into()),
            Tk::Semi,
            Tk::Identifier("i".into()),
            Tk::Assign,
            Tk::Identifier("i".into()),
            Tk::Sub,
            Tk::Num(1),
            Tk::Rpar,
            Tk::LBrace,
            // * ->     print fib(i);
            Tk::Print,
            Tk::Identifier("fibo".into()),
            Tk::Lpar,
            Tk::Identifier("i".into()),
            Tk::Rpar,
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
            // * ->     print fib(1);
            // Tk::Print,
            // Tk::Identifier("fibo".into()),
            // Tk::Lpar,
            // Tk::Num(10),
            // Tk::Rpar,
            // Tk::Semi,
            // Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
        inter.eval(statements);

        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        assert_eq!(last, ("print".into(), V::I32(-55)));
    }

    #[test]
    fn test_count() {
        let tokens = vec![
            //?  function count(n) {
            //?     if (1 < n) count(n - 1);
            //?     print n;
            //?   }
            //?   count(3);
            Tk::Fn,
            Tk::Identifier("count".into()),
            Tk::Lpar,
            Tk::Identifier("n".into()),
            Tk::Rpar,
            Tk::LBrace,
            // * -> if (1 < n) count(n - 1);
            Tk::If,
            Tk::Lpar,
            Tk::Num(1),
            Tk::LT,
            Tk::Identifier("n".into()),
            Tk::Rpar,
            Tk::Identifier("count".into()),
            Tk::Lpar,
            Tk::Identifier("n".into()),
            Tk::Sub,
            Tk::Num(1),
            Tk::Rpar,
            Tk::Semi,
            // * ->   print n;
            Tk::Print,
            Tk::Identifier("n".into()),
            Tk::Semi,
            Tk::RBrace,
            //?  } count(3);
            Tk::Identifier("count".into()),
            Tk::Lpar,
            Tk::Num(3),
            Tk::Rpar,
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let environment = Env::new(EnvKind::Global);
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);

        // todo: assert stdout
        let last = inter.results.last();
        assert_eq!(last, Some(&("print".into(), V::I32(3))));
    }

    #[test]
    fn test_return_in_conditional() {
        let tokens = vec![
            // * ->     if (n < 1 or n == 1) return 2;
            Tk::If,
            Tk::Lpar,
            Tk::Identifier("n".into()),
            Tk::LT,
            Tk::Num(1),
            Tk::Or,
            Tk::Identifier("n".into()),
            Tk::EQ,
            Tk::Num(1),
            Tk::Rpar,
            Tk::Return,
            Tk::Num(2),
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();

        let mut environment = Env::new(EnvKind::Global);
        environment.define("n".into(), V::I32(10));
        let mut inter = Interpreter::new(environment);

        inter.eval(statements);
        // todo: assert stdout
        let last = inter.results.last();
        assert_eq!(last, None);
    }

    // * ->  fun sayHi(a, b) {
    // * ->     return a - b;
    // * ->  }
    // * ->  sayHi(1, 3);
    #[test]
    fn test_return() {
        let tokens = vec![
            Tk::Fn,
            Tk::Identifier("sayHi".into()),
            Tk::Lpar,
            Tk::Identifier("a".into()),
            Tk::Comma,
            Tk::Identifier("b".into()),
            Tk::Rpar,
            Tk::LBrace,
            Tk::Return,
            Tk::Identifier("a".into()),
            Tk::Sub,
            Tk::Identifier("b".into()),
            Tk::Semi,
            Tk::RBrace,
            Tk::Identifier("sayHi".into()),
            Tk::Lpar,
            Tk::Num(1),
            Tk::Comma,
            Tk::Num(3),
            Tk::Rpar,
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
        inter.eval(statements);
        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        println!("{:?}", inter.results);
        assert_eq!(last, ("ret".into(), V::I32(-2)));
    }

    // * ->  fun sayHi(a, b) {
    // * ->     print a - b;
    // * ->  }
    // * ->  sayHi(1, 3);
    #[test]
    fn parser_procedure() {
        let tokens = vec![
            Tk::Fn,
            Tk::Identifier("sayHi".into()),
            Tk::Lpar,
            Tk::Identifier("a".into()),
            Tk::Comma,
            Tk::Identifier("b".into()),
            Tk::Rpar,
            Tk::LBrace,
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Sub,
            Tk::Identifier("b".into()),
            Tk::Semi,
            Tk::RBrace,
            Tk::Identifier("sayHi".into()),
            Tk::Lpar,
            Tk::Num(1),
            Tk::Comma,
            Tk::Num(3),
            Tk::Rpar,
            Tk::Semi,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
        inter.eval(statements);
        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        println!("{:?}", inter.results);
        assert_eq!(last, ("print".into(), V::I32(-2)));
    }

    // * ->  1 - (2 * 3( < 4 == false
    // * this will try parse a function call due to '('
    // todo❓: js: Uncaught SyntaxError: Unexpected token '<'
    #[test]
    fn parser_works() {
        let tokens = vec![
            Tk::Num(1),
            Tk::Sub,
            Tk::Lpar,
            Tk::Num(2),
            Tk::Mul,
            Tk::Num(3),
            //todo: Tk::Lpar,
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
            expression.to_string(),
            "(== (< (- 1 (group (* 2 3))) 4) false)".to_string()
        );
    }

    //* let a = 0;
    //* let temp = 0;
    //* for (let b = 1; -10_000 < a; b = temp - b) {
    //*     print a;
    //*     temp = a;
    //*     a = b;
    //* }
    //* -> 10_946
    #[test]
    fn test_for() {
        let tokens = vec![
            Tk::For,
            Tk::Lpar,
            Tk::Var,
            Tk::Identifier("b".into()),
            Tk::Assign,
            Tk::Num(1),
            Tk::Semi,
            Tk::Num(-10_000),
            Tk::LT,
            Tk::Identifier("a".into()),
            Tk::Semi,
            Tk::Identifier("b".into()),
            Tk::Assign,
            Tk::Identifier("temp".into()),
            Tk::Sub,
            Tk::Identifier("b".into()),
            Tk::Rpar,
            Tk::LBrace,
            Tk::Print,
            Tk::Identifier("a".into()),
            Tk::Semi,
            Tk::Identifier("temp".into()),
            Tk::Assign,
            Tk::Identifier("a".into()),
            Tk::Semi,
            Tk::Identifier("a".into()),
            Tk::Assign,
            Tk::Identifier("b".into()),
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut environment = Env::new(EnvKind::Global);
        environment.define("a".into(), V::I32(0));
        environment.define("temp".into(), V::I32(0));
        let mut inter = Interpreter::new(environment);
        inter.eval(statements);
        // todo: assert stdout
        let last = inter.results.last().unwrap().clone();
        // println!("{:#?}", inter.results);
        assert_eq!(last, ("print".into(), V::I32(10_946)));
    }

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
        let mut environment = Env::new(EnvKind::Global);
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
        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
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
        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
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
        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
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
        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
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
        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
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

        let mut environment = Env::new(EnvKind::Global);
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

        let mut environment = Env::new(EnvKind::Global);
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

        let mut environment = Env::new(EnvKind::Global);
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

        let mut environment = Env::new(EnvKind::Global);
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
        let expression = parser
            .statement()
            .unwrap_or_else(|_| Statement::None("parse err".into()));
        assert_eq!(
            expression,
            Statement::Block(vec![
                Statement::LetDeclaration {
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
        let mut environment = Env::new(EnvKind::Global);
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

        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
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
        let mut environment = Env::new(EnvKind::Global);
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
            .declaration_or_statement()
            .unwrap_or_else(|_| Statement::None("parse err".into()));

        assert_eq!(
            expression,
            Statement::LetDeclaration {
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
            .declaration_or_statement()
            .unwrap_or_else(|_| Statement::None("parse err".into()));

        assert_eq!(
            expression,
            Statement::LetDeclaration {
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

        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
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
        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
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
        let mut inter = Interpreter::new(Env::new(EnvKind::Global));
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
