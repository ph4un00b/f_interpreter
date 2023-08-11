use std::collections::HashMap;
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
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
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
    GetProp(Box<Expr>, Tk),
    SetProp {
        identifier_expr: Box<Expr>,
        prop_name: Tk,
        expr_value: Box<Expr>,
    },
    This(Tk),
    None,
}

#[derive(Debug, Clone)]
pub enum V {
    Done,
    Return(Box<V>),
    Instance(Box<InstanceObject>),
    Row(Box<RowObject>),
    Func(Box<FunctionObject>),
    NativeFunc(String),
    I32(i32),
    F64(f64),
    String(String),
    Bool(bool),
}

impl std::hash::Hash for V {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            V::Done => V::Done.hash(state),
            V::Return(val) => {
                state.write_u8(1);
                val.hash(state);
            }
            V::Func(func) => {
                state.write_u8(2);
                func.as_ref().hash(state);
            }
            V::NativeFunc(name) => {
                state.write_u8(3);
                name.hash(state);
            }
            V::I32(num) => {
                state.write_u8(4);
                num.hash(state);
            }
            V::F64(num) => {
                state.write_u8(5);
                //todo: test especial cases❗
                num.to_bits().hash(state);
            }
            V::String(str) => {
                state.write_u8(6);
                str.hash(state);
            }
            V::Bool(val) => {
                state.write_u8(7);
                val.hash(state);
            }
            V::Row(obj) => {
                state.write_u8(8);
                obj.hash(state);
            }
            V::Instance(obj) => {
                state.write_u8(9);
                obj.hash(state);
            }
        }
    }
}

impl PartialEq for V {
    fn eq(&self, other: &Self) -> bool {
        use V::*;

        match (self, other) {
            (Done, Done) => true,
            (Return(a), Return(b)) => *a == *b,
            (Func(a), Func(b)) => *a == *b,
            (NativeFunc(a), NativeFunc(b)) => a == b,
            (I32(a), I32(b)) => a == b,
            (F64(a), F64(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Bool(a), Bool(b)) => a == b,
            (Row(a), Row(b)) => a == b,
            (Instance(a), Instance(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for V {}

trait Callable {
    fn call(&mut self, interpreter: &mut Interpreter, arguments: Vec<V>) -> RValue;
    fn arity(&self) -> usize;
}

impl Callable for V {
    fn call(&mut self, interpreter: &mut Interpreter, arguments: Vec<V>) -> RValue {
        match self {
            V::Func(callable) => callable.call(interpreter, arguments),
            V::NativeFunc(fn_name) => match fn_name.as_str() {
                "clock" => {
                    let current_time = std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .expect("Failed to get current time");

                    Ok(V::F64(current_time.as_secs() as f64))
                }
                _ => panic!(),
            },
            //? Handle other variants if needed
            //? For example, return a default arity value or throw an error
            V::Done => Err(Runtime::NonCallable),
            V::Return(_) => Err(Runtime::NonCallable),
            V::Instance(_) => Err(Runtime::NonCallable),
            V::Row(_) => Err(Runtime::NonCallable),
            V::I32(_) => Err(Runtime::NonCallable),
            V::F64(_) => Err(Runtime::NonCallable),
            V::String(_) => Err(Runtime::NonCallable),
            V::Bool(_) => Err(Runtime::NonCallable),
        }
    }

    fn arity(&self) -> usize {
        match self {
            V::NativeFunc(fn_name) => match fn_name.as_str() {
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

impl fmt::Display for V {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            V::Done => write!(f, "done!")?,
            V::Return(val) => write!(f, "ret {:?}", val)?,
            V::Func(function) => write!(f, "{}", function)?,
            V::NativeFunc(fn_name) => match fn_name.as_str() {
                "clock" => write!(f, "<native {}>", fn_name)?,
                _ => write!(f, "#{}", fn_name)?,
            },
            // V::Callable(id) => write!(f, "#{}", id)?,
            V::I32(val) => write!(f, "{}", val)?,
            V::F64(val) => write!(f, "{}", val)?,
            V::String(val) => write!(f, "{}", val)?,
            V::Bool(val) => write!(f, "{}", val)?,
            V::Row(obj) => write!(f, "{}", obj)?,
            V::Instance(obj) => write!(f, "{}", obj)?,
        }
        Ok(())
    }
}

impl From<Tk> for String {
    fn from(value: Tk) -> String {
        match value {
            Tk::Num(val) => val.to_string(),
            Tk::Float(val) => val.to_string(),
            Tk::Identifier(val, _line) => val,
            Tk::Return(_line) => "ret".to_string(),
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
            Tk::Assign => "=".to_string(),
            Tk::LBrace => "{".to_string(),
            Tk::RBrace => "}".to_string(),
            Tk::Comma => ",".to_string(),
            Tk::Dot => ".".to_string(),
            Tk::ThisTk => "SELF".to_string(),
        }
    }
}

//? 1 - (2 * 3) < 4 == false
#[allow(dead_code)]
#[derive(Clone, PartialEq, Debug, Eq, Hash)]
pub enum Tk {
    Default,
    Num(i32),
    Float(String),
    Identifier(String, u32),
    Return(u32),
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
    LBrace,
    RBrace,
    Else,
    Or,
    And,
    Dot,
    //? due to logging clarity
    // todo: find out a better name❓
    ThisTk,
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
            Expr::GetProp(obj, name) => write!(f, "< {} GET {:?}", obj, name)?,
            Expr::SetProp {
                identifier_expr: expr,
                prop_name: name,
                expr_value: value,
            } => write!(f, "> {expr} SET {name:?} = {value}")?,
            Expr::This(_) => write!(f, "@")?,
        }

        Ok(())
    }
}

// todo: can it be automated❓👀
#[derive(Debug, PartialEq, Clone)]
enum Statement {
    None(String),
    Row {
        name: Tk,
        /*
         * You might be surprised that we store the superclass name as
         * an Expr.Variable, not a Token.
         * The grammar restricts the superclass clause to a single identifier,
         * but at runtime, that identifier is evaluated as a variable access.
         * Wrapping the name in an Expr.Variable early on in the parser gives us
         * an object that the resolver can hang the resolution information off of.
         */
        //? Expr.Variable
        super_expr: Expr,
        //? Stmt.Function
        columns: Vec<Statement>,
    },
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
        //? para detectar en runtime que clase es❗
        kind: FnType,
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
pub struct InstanceObject {
    row: RowObject,
    /*
     * Doing a hash table lookup for every field access
     * is fast enough for many language implementations,
     * but not ideal. High performance VMs for languages
     * like JavaScript use sophisticated optimizations like
     * “hidden classes” to avoid that overhead.
     *
     * @see https://richardartoul.github.io/jekyll/update/2015/04/26/hidden-classes.html
     */
    state_fields: HashMap<String, V>,
}

impl std::hash::Hash for InstanceObject {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.row.hash(state);
    }
}

impl fmt::Display for InstanceObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<instance of {}>", String::from(self.row.name.clone()))?;
        Ok(())
    }
}

impl InstanceObject {
    fn new(row: RowObject) -> Self {
        Self {
            row,
            state_fields: HashMap::new(),
        }
    }

    fn mut_prop(&mut self, name: Tk, value: V) {
        let key = String::from(name.clone());
        // self.fields.entry(key).and_modify(|val| *val = value);
        // println!("\n----- will MUT {value} in {self}");
        self.state_fields.insert(key, value);
        // println!("\n----- instance {self:?}");
    }

    /*
     * Most modern languages support “getters” and “setters”—members on a
     * class that look like field reads and writes but that actually execute
     * user-defined code.
     *
     * These are declared without a parameter list. T
     * he body of the getter is executed when a property with that name is accessed.
     */
    // todo: Extend Lox to support getter methods.
    //? class Circle {
    //?     init(radius) {
    //?       this.radius = radius;
    //?     }
    //?     area {
    //?       return 3.141592653 * this.radius * this.radius;
    //?     }
    //?   }
    //?   var circle = Circle(4);
    //?   print circle.area; // Prints roughly "50.2655".
    fn get_prop(self, interpreter: &mut Interpreter, name: Tk) -> RValue {
        let key = String::from(name.clone());
        /*
         * Note how I switched from talking about “properties”
         * to “fields”. There is a subtle difference between the two.
         * Fields are named bits of state stored directly
         * in an instance. Properties are the named, uh,
         * things, that a get expression may return.
         *
         * Every field is a property, but as we’ll see later,
         * not every property is a field.
         */
        if self.state_fields.contains_key(&key) {
            //todo: is this clone ok❓
            Ok(self.state_fields.get(&key).unwrap().clone())
        } else if self.row.behaviors.contains_key(&key) {
            /*
             * Looking for a field first implies that fields shadow methods,
             * a subtle but important semantic point.
             */
            // todo: test shadowing❗
            let function = self.row.behaviors.get(&key).unwrap();
            let val = function.bind_instance(interpreter, &V::Instance(Box::new(self.clone())));
            Ok(val)
        } else {
            /*
            * An interesting edge case we need to handle
            the given name. We could
            * s * is what happens if the instance doesn’t have
            * a property withilently return some dummy value like nil,
            * but my experience with languages like JavaScript
            * is that this behavior masks bugs more often than
            * it does anything useful. Instead, we’ll make it a
            * runtime error.
            */
            Err(Runtime::UndefinedProperty(name.clone()))
        }
    }
}

/*
 * Python and JavaScript allow you to freely access an object’s fields from outside of
 * its own methods. Ruby and Smalltalk encapsulate instance state.
 * Only methods on the class can access the raw fields, and it is up to the
 * class to decide which state is exposed. Most statically typed languages offer
 * modifiers like private and public to control which parts of a
 * class are externally accessible on a per-member basis.
 *
 * ❓❓
 * @see https://craftinginterpreters.com/classes.html#design-note
 * What are the trade-offs between these approaches and
 * why might a language prefer one or the other?
 */
#[derive(Debug, Clone, PartialEq)]
pub struct RowObject {
    name: Tk,
    /*
     * Where an instance stores state, the class stores behavior.
     * LoxInstance has its map of fields, and LoxClass gets a map of methods.
     * Even though methods are owned by the class,
     * they are still accessed through instances of that class.
     */
    behaviors: HashMap<String, FunctionObject>,
    //? V::Row
    super_row: Option<V>,
    /*
     * We have methods on instances, but there is no way to define “static” methods
     * that can be called directly on the class object itself.
     */
    // todo: Add support for them.
    //? class Math {
    //?     class square(n) {
    //?         return n * n;
    //?     }
    //? }
    //? print Math.square(3); // Prints "9".
    /*
     * You can solve this however you like, but the “meta-classes” used by Smalltalk
     * and Ruby are a particularly elegant approach.
     *
     * Hint: Make LoxClass extend LoxInstance and go from there.
     */
}

impl Callable for RowObject {
    fn call(&mut self, interpreter: &mut Interpreter, arguments: Vec<V>) -> RValue {
        let value_instance = V::Instance(Box::new(InstanceObject::new(self.clone())));
        let init = self.behaviors.get(&"new".to_string());
        if let Some(constructor) = init {
            /*
             * When a class is called, after the LoxInstance is created,
             * we look for an “init” method. If we find one,
             * we immediately bind and invoke it just like a normal method call.
             * The argument list is forwarded along.
             */
            constructor
                .bind_instance(interpreter, &value_instance)
                .call(interpreter, arguments)?;
        };
        Ok(value_instance)
    }

    fn arity(&self) -> usize {
        /*
         * If there is an initializer, that method’s arity determines how many
         * arguments you must pass when you call the class itself.
         * We don’t require a class to define an initializer, though, as a convenience.
         * If you don’t have an initializer, the arity is still zero.
         *
         * That’s basically it. Since we bind the init() method before we call it,
         * it has access to this inside its body. That, along with the arguments
         * passed to the class, are all you need to be able to set up the new
         * instance however you desire.
         */
        let init = self.behaviors.get(&"new".to_string());
        if let Some(constructor) = init {
            constructor.arity()
        } else {
            0
        }
    }
}

impl RowObject {
    fn new(name: Tk, behaviors: HashMap<String, FunctionObject>, super_row: Option<V>) -> Self {
        Self {
            name,
            behaviors,
            super_row,
        }
    }
}

impl std::hash::Hash for RowObject {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl fmt::Display for RowObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<row {}>", String::from(self.name.clone()))?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionObject {
    //? Stmt.Function
    declaration: Statement,
    // * If the function is an initializer,
    // * we override the actual return value and forcibly return it's instance.
    is_constructor: bool,
}

impl std::hash::Hash for FunctionObject {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
        // self.declaration.hash(state);
    }
}

impl FunctionObject {
    fn new(declaration: Statement, is_constructor: bool) -> Self {
        //todo: this should be only for Statement::Function❗
        Self {
            declaration,
            is_constructor,
        }
    }

    fn bind_instance(&self, interpreter: &mut Interpreter, val: &V) -> V {
        let fn_name = match self.declaration.clone() {
            Statement::FunctionDeclaration {
                kind: _,
                name,
                parameters: _,
                body: _,
            } => name,
            _ => todo!(),
        };
        println!("CREATE CALL ENV FOR {fn_name:?}");
        let mut call_env = Env::new(EnvKind::Call(fn_name.clone()));
        call_env.define(Tk::ThisTk.into(), val.clone());
        interpreter.global_env.push(call_env);
        /*
         * And then in bind() where we create the closure that binds this to
         * a method, we pass along the original method’s value.
         */
        //todo: add closure
        V::Func(Box::new(FunctionObject::new(
            self.declaration.clone(),
            self.is_constructor,
        )))
    }
}

impl fmt::Display for FunctionObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // todo: look for refactors❗
        match self.declaration.clone() {
            Statement::FunctionDeclaration {
                kind: _,
                name,
                parameters: _,
                body: _,
            } => Ok(write!(f, "<fn {}>", String::from(name))?),
            _ => unreachable!("only fn declaration!"),
        }
    }
}

impl Drop for FunctionObject {
    fn drop(&mut self) {
        // todo: si quiero modificar interpreter#global_env
        // todo: hasta donde sé, se vana introducir un montón
        // todo:  de anotaciones lifetime de forma recursiva
        // todo:  V, EXpr, Statements
        // todo:  buscar otra forma❗
        println!("function-obj drop: {}", self);
    }
}

impl Callable for FunctionObject {
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
    fn call(&mut self, interpreter: &mut Interpreter, arguments: Vec<V>) -> RValue {
        // todo: look for refactors❗
        let (fn_kind, fn_name, fn_params, fn_body) = match self.declaration.clone() {
            Statement::FunctionDeclaration {
                kind,
                name,
                parameters,
                body,
            } => (kind, name, parameters, body),
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

        interpreter.current_fn_call = fn_name.clone();
        interpreter.current_fn_type = fn_kind.clone();
        interpreter
            .closures
            .insert(fn_name.clone(), Env::new(EnvKind::Closure(fn_name.clone())));

        // let closure_env = interpreter.closures.last_mut().unwrap();
        // let mut params_env = &self.closure;
        let last_env = if let Some(env) = interpreter.global_env.last() {
            env
        } else {
            unreachable!("there is no global env defined!")
        };

        if last_env.kind != EnvKind::Call(fn_name.clone()) {
            //? this mean verify if before entering this call
            //? we already have a call env defined with SELF instance❗
            interpreter
                .global_env
                .push(Env::new(EnvKind::Call(fn_name.clone())));
        }

        let call_env = interpreter.global_env.last_mut().unwrap();
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
            //todo: remove duplication❗
            call_env.define(param_name.clone(), argument_val.clone());
            interpreter
                .closures
                .entry(fn_name.clone())
                .and_modify(|closure_env| closure_env.define(param_name, argument_val.clone()));
            // closure_env.define(param_name, argument_val.clone());
        }
        /*
         * Once the body of the function has finished executing,
         * executeBlock() discards that function-local environment
         * and restores the previous one that was active back at the callsite.
         * Finally, call() returns null, which returns nil to the caller.
         * (We’ll add return values later.)
         */

        // interpreter.global_env.push(call_env);
        let returned = interpreter.eval_block(fn_body)?;
        let Some(call_to_be_dropped) = interpreter.global_env.pop() else {
            unreachable!("call env not defined before!")
        };
        println!("<<< call OFF {call_to_be_dropped}");
        //todo: add instance to closure❗❓
        println!("closures {:?}", interpreter.closures);
        for env in interpreter.global_env.iter() {
            println!("globals: {}", env);
        }
        //? finishing call
        //? an issue can arise on recursive calls
        //? maybe use an stack or the technique in the resolver❓
        // interpreter.current_function_call = Tk::Default;
        // interpreter.current_function_type = FunType::None;
        match returned {
            V::Return(value) => Ok(*value),
            _ => {
                /*
                 * Can you “re-initialize” an object by directly
                 * calling its new() method?
                 * If you do, what does it return? A reasonable answer
                 * would be nil since that’s what it appears the body returns.
                 *
                 * However—and I generally dislike compromising to satisfy the
                 * implementation—it will make clox’s implementation of constructors
                 * much easier if we say that init() methods always return this,
                 * even when directly called. In order to keep jlox
                 * compatible with that, we add a little special case code in LoxFunction.
                 *
                 * It’s reasonable to have the constraints and resources of your
                 * implementation affect the design of the language.
                 * There are only so many hours in the day, and if a
                 * cut corner here or there lets you get more features to
                 * users in less time, it may very well be a net win for their
                 * happiness and productivity.
                 *
                 * The trick is figuring out which corners to cut that
                 * won’t cause your users and future self to curse your shortsightedness.
                 */
                if self.is_constructor {
                    //? aquí usamos la call env, antes de que se libere
                    //? idealmente deberia usarse desde el closure
                    //? pero por ahora lo dejamos así
                    // todo: hacer un test para que falle desde el closure❗
                    let Some(this) = call_to_be_dropped.fetch(Tk::ThisTk) else {
                        unreachable!("instance should be defined!");
                    };
                    Ok(this)
                } else {
                    Ok(V::Done)
                }
            }
        }
    }

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
                kind: _,
                name: _,
                parameters,
                body: _,
            } => parameters.len(),
            _ => unreachable!("only fn declaration!"),
        }
    }
}

#[allow(dead_code)]
struct Parser {
    tokens: Vec<Tk>,
    current_position: usize,
    current_token: Tk,
    prev_token: Tk,
}

trait ResolutionPass<TValue>
where
    TValue: std::fmt::Debug,
{
    fn resolve(&mut self, val: TValue);
}

/*
 * Lexical scopes nest in both the interpreter and the resolver.
 * They behave like a stack.
 * The interpreter implements that stack using a linked list—the
 * chain of Environment objects.
 * In the resolver, we use an actual Java Stack.
 *
 * The value associated with a key in the scope map represents
 * whether or not we have finished resolving that variable’s initializer.
 */

#[derive(Debug, Clone, PartialEq)]
enum FnType {
    None,
    Constructor,
    Function,
    Column,
}

#[derive(Debug, Clone, PartialEq)]
enum RowType {
    None,
    Row,
}

type LocalBlockScope = HashMap<String, bool>;
struct Resolver<'a> {
    /*
     * The scope stack is only used for local block scopes.
     * Variables declared at the top level in the global scope
     * are not tracked by the resolver since they are more dynamic in Lox.
     * When resolving a variable, if we can’t find it in the stack
     * of local scopes, we assume it must be global.
     */
    /*
     * Our resolver calculates which environment the variable is found in,
     * but it’s still looked up by name in that map.
     * A more efficient environment representation would store local
     * variables in an array and look them up by index.
     */
    // todo:
    /*
     * Extend the resolver to associate a unique index for each local
     * variable declared in a scope. When resolving a variable access,
     * look up both the scope the variable is in and its index and store that.
     * In the interpreter, use that to quickly access a variable by its
     * index instead of using a map.
     */
    scopes: Vec<LocalBlockScope>,
    current_fn_type: FnType,
    /*
     * Yes, it could be a Boolean. When we get to inheritance,
     * it will get a third value, hence the enum right now.
     * We also add a corresponding field, currentClass.
     * Its value tells us if we are currently inside a
     * class declaration while traversing the syntax tree.
     * It starts out NONE which means we aren’t in one.
     */
    current_row_type: RowType,
    interpreter: &'a mut Interpreter,
}

#[allow(unused)]
impl<'a> Resolver<'a> {
    fn new(interpreter: &'a mut Interpreter) -> Resolver<'a> {
        Self {
            scopes: vec![],
            interpreter,
            current_fn_type: FnType::None,
            current_row_type: RowType::None,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        /*
         * Since scopes are stored in an explicit stack,
         * exiting one is straightforward.
         */
        self.scopes.pop();
    }

    fn declare_variable(&mut self, name: Tk) {
        //? var a = "outer";
        //? {
        //?   var a = a;
        //? }
        /*
         * What happens when the initializer for a local variable
         * refers to a variable with the same name as the variable
         * being declared? We have a few options:
         *
         * 1. Run the initializer, then put the new variable in scope.
         *
         * Here, the new local a would be initialized with “outer”,
         * the value of the global one.
         * In other words, the previous declaration would desugar to:
         */
        //? var temp = a; // Run the initializer.
        //? var a;        // Declare the variable.
        //? a = temp;     // Initialize it.
        /*
         * 2. Put the new variable in scope, then run the initializer.
         *
         * This means you could observe a variable before it’s initialized,
         * so we would need to figure out what value it would have then.
         * Probably nil.
         * That means the new local a would be re-initialized to its own implicitly
         * initialized value, nil.
         * Now the desugaring would look like:
         */
        //? var a; // Define the variable.
        //? a = a; // Run the initializer.
        /*
         * 3. Make it an error to reference a variable in its initializer.
         *
         * Have the interpreter fail either at compile time or runtime
         * if an initializer mentions the variable being initialized.
         *
         * Since the first two options are likely to mask user errors,
         * we’ll take the third. Further, we’ll make it a compile error
         * instead of a runtime one.
         * That way, the user is alerted to the problem before any code is run.
         */
        /*
         * In order to do that, as we visit expressions,
         * we need to know if we’re inside the initializer for some variable.
         * We do that by splitting binding into two steps.`
         *
         * The first is declaring it.
         */
        if self.scopes.is_empty() {
            return;
        }
        if let Some(scope) = self.scopes.last_mut() {
            let key = String::from(name.clone());
            if scope.contains_key(&key) {
                report_err(
                    Tk::Identifier("another_name".into(), 0),
                    name.clone(),
                    "Already a variable with this name in this scope.",
                );
                //todo: remove panic, use static LOX::had_Error❗
                panic!("Already a variable with this name in this scope.");
            }
            /*
             * Declaration adds the variable to the innermost scope so that it
             * shadows any outer one and so that we know the variable exists.
             *
             * We mark it as “not ready yet” by binding
             * its name to false in the scope map.
             */
            scope.insert(name.into(), false);
        }
    }

    fn define_variable(&mut self, name: Tk) {
        /*
         * After declaring the variable, we resolve its initializer expression
         * in that same scope where the new variable now exists but is unavailable.
         * Once the initializer expression is done, the variable is ready for prime time.
         * We do that by defining it.
         */
        if self.scopes.is_empty() {
            return;
        }

        if let Some(scope) = self.scopes.last_mut() {
            /*
             * We set the variable’s value in the scope map to true to mark it
             * as fully initialized and available for use.
             */
            scope.entry(name.into()).and_modify(|value| *value = true);
        }
        println!("\nscopes: {:?}", self.scopes);
    }

    fn resolve_local_variable(&mut self, expr: Expr, name: Tk) {
        let mut local_blocks = self.scopes.iter().enumerate();
        /*
         * This looks, for good reason, a lot like the code in Environment for evaluating a variable.
         * We start at the innermost scope and work outwards,
         * looking in each map for a matching name.
         */
        println!(" {expr}, resolving: {name:?} in {:?}", self.scopes);
        while let Some((scope_idx, scope)) = local_blocks.next_back() {
            println!(">> scope_idx: {scope_idx}, len: {}", self.scopes.len());
            if scope.contains_key(&String::from(name.clone())) {
                /*
                 * If we find the variable, we resolve it, passing in the number
                 * of scopes between the current innermost scope and the
                 * scope where the variable was found.
                 */
                self.interpreter
                    //? we are in backwards mode
                    //? - we don't need to adjusts the index,
                    //? - we can reuse the scope index❗
                    //? - in the interpreter we will have this structure
                    //? - [PRELUDE, GLOBALS, scope 0, scope 1, scope innermost 2....]
                    .resolve_variable(expr, scope_idx);
                return;
            }
        }
        /*
         * If we walk through all of the block scopes and never find the variable,
         * we leave it unresolved and assume it’s global.
         */
        println!("resolving name {name:?} should be @GLOBALS!");
    }

    /*
     * It’s a separate method since we will also use it
     * for resolving Lox methods when we add classes later.
     * It creates a new scope for the body and then binds variables
     * for each of the function’s parameters.
     */
    fn resolve_behavior(&mut self, declaration: Statement, kind: FnType) {
        if let Statement::FunctionDeclaration {
            kind: _,
            name: _,
            parameters,
            body,
        } = declaration
        {
            // * We stash the previous value of the field in a local variable first.
            // * Remember, Lox has local functions, so you can nest function declarations
            // * arbitrarily deeply. We need to track not just that we’re in a function,
            // * but how many we’re in.
            // todo: check for a swap alternative❗
            let enclosing_function = self.current_fn_type.clone();
            self.current_fn_type = kind;

            self.begin_scope();
            for param in parameters {
                self.declare_variable(param.clone());
                self.define_variable(param);
            }
            /*
             * Once that’s ready, it resolves the function body in that scope.
             * This is different from how the interpreter handles function declarations.
             * At runtime, declaring a function doesn’t do anything with the function’s body.
             * The body doesn’t get touched until later when the function is called.
             *
             * In a static analysis, we immediately traverse into the body right then and there.
             */
            for stt in body {
                self.resolve(stt);
            }
            self.end_scope();
            /*
             * We could use an explicit stack of FunctionType values for that,
             * but instead we’ll piggyback on the JVM.
             * We store the previous value in a local on the Java stack.
             *
             * When we’re done resolving
             * the function body, we restore the field to that value.
             */
            self.current_fn_type = enclosing_function;
        } else {
            unreachable!("should be function_declaration")
        }
    }
}

impl ResolutionPass<Expr> for Resolver<'_> {
    fn resolve(&mut self, val: Expr) {
        match val.clone() {
            Expr::This(keyword) => {
                if self.current_row_type == RowType::None {
                    /*
                     * That should help users use this correctly,
                     * and it saves us from having to
                     * handle misuse at runtime in the interpreter.
                     */
                    panic!("Can't use '{}' outside of a Row.", String::from(Tk::ThisTk));
                }
                self.resolve_local_variable(val, keyword)
            }
            /*
             * Again, like Expr.Get, the property itself is dynamically evaluated,
             * so there’s nothing to resolve there. All we need to do is recurse
             * into the two sub-expressions of Expr.Set,
             * the object whose property is being set,
             * and the value it’s being set to.
             */
            Expr::SetProp {
                identifier_expr,
                prop_name: _,
                expr_value,
            } => {
                self.resolve(*identifier_expr);
                self.resolve(*expr_value);
            }
            /*
             * Since properties are looked up dynamically,
             * they don’t get resolved. During resolution,
             * we recurse only into the expression to the left
             * of the dot.
             *
             * The actual property access happens
             * in the interpreter.
             *
             */
            Expr::GetProp(parent_id, _) => self.resolve(*parent_id),
            /*
             * Variable declarations—and function declarations,
             * which we’ll get to—write to the scope maps.
             * Those maps are read when we resolve variable expressions.
             */
            Expr::Let(name) => {
                if !self.scopes.is_empty() {
                    if let Some(scope) = self.scopes.last() {
                        if let Some(peek) = scope.get(&String::from(name.clone())) {
                            if !(*peek) {
                                /*
                                 * First, we check to see if the variable is being accessed
                                 * inside its own initializer.
                                 * This is where the values in the scope map come into play.
                                 * If the variable exists in the current scope but its value is false,
                                 * that means we have declared it but not yet defined it.
                                 *
                                 * We report that error.
                                 */
                                let msg = format!(
                                    "{:?} Can't read local variable in its own initializer.",
                                    name
                                );
                                panic!("{msg}");
                            }
                        }
                    }
                }
                /*
                 * After that check, we actually resolve the variable itself using this helper:
                 */
                self.resolve_local_variable(val, name);
            }
            Expr::Assign(to_identifier, value_expression) => {
                /*
                 * First, we resolve the expression for the assigned value in case
                 * it also contains references to other variables.
                 */
                self.resolve(*value_expression);
                self.resolve_local_variable(val, to_identifier);
            }
            /*
             * We traverse into and resolve both operands.
             */
            Expr::Binary {
                left,
                operator: _,
                right,
            } => {
                self.resolve(*left);
                self.resolve(*right);
            }
            /*
             * we walk the argument list and resolve them all.
             * The thing being called is also an expression (usually a variable expression),
             * so that gets resolved too.
             */
            Expr::FunctionCall {
                callee,
                paren: _,
                arguments,
            } => {
                self.resolve(*callee);
                for expr in arguments {
                    self.resolve(expr)
                }
            }
            /*
             * Parentheses are easy.
             */
            Expr::Grouping(expr) => self.resolve(*expr),
            /*
             * Literals are easiest of all.
             *
             * A literal expression doesn’t mention any variables and
             * doesn’t contain any sub-expressions so there is no work to do.
             */
            Expr::Literal(_) => (),
            /*
             * Since a static analysis does no control flow or short-circuiting,
             * logical expressions are exactly the same as other binary operators.
             */
            Expr::Logical(left, _, right) => {
                self.resolve(*left);
                self.resolve(*right);
            }
            Expr::Unary { operator: _, right } => self.resolve(*right),
            Expr::None => unreachable!("should be a valid expression"),
        }
    }
}

impl ResolutionPass<Statement> for Resolver<'_> {
    /*
     * Variable resolution touches each node once, so its performance is O(n)
     * where n is the number of syntax tree nodes.
     * More sophisticated analyses may have greater complexity,
     * but most are carefully designed to be linear or not far from it.
     * It’s an embarrassing faux pas if your compiler gets exponentially
     * slower as the user’s program grows.
     */
    fn resolve(&mut self, val: Statement) {
        match val.clone() {
            Statement::Row {
                name,
                super_expr,
                columns,
            } => {
                /*
                 * As with currentFunction, we store the previous value of
                 * the field in a local variable.
                 *
                 * This lets us piggyback onto the JVM to keep a stack of
                 * currentClass values.
                 * That way we don’t lose track of the previous value
                 * if one class nests inside another.
                 */
                let enclosing_row = self.current_row_type.clone();
                self.current_row_type = RowType::Row;
                /*
                 * It’s not common to declare a class as a local variable,
                 * but Lox permits it, so we need to handle it correctly.
                 */
                self.declare_variable(name.clone());
                self.define_variable(name.clone());
                println!("{super_expr:?}");
                match &super_expr {
                    Expr::Let(super_name) => {
                        if String::from(super_name.clone()) == String::from(name.clone()) {
                            // Lox.error(stmt.superclass.name, "A class can't inherit from itself.");
                            panic!("{super_name:?} A row can't inherit from itself.")
                        }
                    }
                    Expr::None => (),
                    _ => unreachable!("can be only a variable expression!"),
                }

                match &super_expr {
                    Expr::None => (),
                    /*
                     * The class declaration AST node has a new sub-expression,
                     * so we traverse into and resolve that.
                     *
                     * Since classes are usually declared at the top level,
                     * the superclass name will most likely be a global variable,
                     * so this doesn’t usually do anything useful.
                     *
                     * However, Lox allows class declarations even inside blocks,
                     * so it’s possible the superclass name refers to a local variable.
                     * In that case, we need to make sure it’s resolved.
                     */
                    _ => self.resolve(super_expr),
                }
                /*
                 * Before we step in and start resolving the method bodies,
                 * we push a new scope and define “this” in it as if it were a variable.
                 *
                 * Then, when we’re done, we discard that surrounding scope.
                 *
                 * Now, whenever a this expression is encountered (at least inside a method)
                 * it will resolve to a “local variable” defined in an implicit scope just
                 * outside of the block for the method body.
                 */
                self.begin_scope();
                if let Some(scope) = self.scopes.last_mut() {
                    scope.insert(Tk::ThisTk.into(), true);
                }
                //? resolviendo methods aka column...
                /*
                 * Storing the function type in a local variable is pointless right now,
                 * but we’ll expand this code before too long and it will make more sense.
                 *
                 * "FunType::Column", That’s going to be important when we resolve this expressions.
                 */
                println!("{:?}", columns);
                for behavior in columns {
                    let kind = match &behavior {
                        Statement::FunctionDeclaration {
                            kind,
                            name: _,
                            parameters: _,
                            body: _,
                        } => kind,
                        _ => unreachable!("should be function!"),
                    };
                    match kind {
                        FnType::Constructor => self.resolve_behavior(behavior, FnType::Constructor),
                        FnType::Column => self.resolve_behavior(behavior, FnType::Column),
                        FnType::None | FnType::Function => {
                            unreachable!("should be column or constructor!")
                        }
                    }
                }

                self.end_scope();
                /*
                 * Once the methods have been resolved,
                 * we “pop” that stack by restoring the old value.
                 */
                self.current_row_type = enclosing_row;
            }
            /*
             * A block statement introduces a new scope for the statements it contains.
             */
            Statement::Block(statements) => {
                /*
                 * We start with blocks since they create the local scopes
                 * where all the magic happens.
                 */
                self.begin_scope();
                /*
                 * This begins a new scope, traverses into the statements inside the block,
                 * and then discards the scope. The fun stuff lives in those helper methods.
                 * We start with the simple one.
                 */
                for stt in statements {
                    // todo: These methods are similar to the evaluate() and execute()
                    self.resolve(stt);
                }
                self.end_scope();
            }
            /*
             * A function declaration introduces a new scope for its body and binds its parameters in that scope.
             */
            Statement::FunctionDeclaration {
                kind: _,
                name,
                parameters: _,
                body: _,
            } => {
                self.declare_variable(name.clone());
                self.define_variable(name);
                self.resolve_behavior(val, FnType::Function);
            }
            /*
             * A variable declaration adds a new variable to the current scope.
             */
            Statement::LetDeclaration { name, initializer } => {
                /*
                 * Resolving a variable declaration adds a new entry to
                 * the current innermost scope’s map.
                 * That seems simple, but there’s a little dance we need to do.
                 */
                self.declare_variable(name.clone());
                if initializer != Expr::None {
                    println!("{initializer:?}");
                    self.resolve(initializer);
                }
                self.define_variable(name);
            }
            /*
             * Variable and assignment expressions need to have their variables resolved.
             *
             * An expression statement contains a single expression to traverse.
             */
            Statement::Expr(expr) => self.resolve(expr),
            /*
             * An if statement has an expression for its condition
             * and one or two statements for the branches.
             */
            Statement::Cond {
                condition,
                then_statement,
                else_statement,
            } => {
                /*
                 * Here, we see how resolution is different from interpretation.
                 * When we resolve an if statement, there is no control flow.
                 *
                 * We resolve the condition and both branches.
                 * Where a dynamic execution steps only into the branch that is run,
                 * a static analysis is conservative—it analyzes any branch that could be run.
                 * Since either one could be reached at runtime, we resolve both.
                 */
                self.resolve(condition);
                self.resolve(*then_statement);
                match *else_statement {
                    Statement::None(_) => {}
                    _ => self.resolve(*else_statement),
                }
            }
            /*
             * Like expression statements, a print statement contains a single sub-expression.
             */
            Statement::Print(expr) => {
                print!("print");
                self.resolve(expr)
            }
            /*
             * Same deal for return.
             */
            Statement::Return { keyword, value } => {
                if self.current_fn_type == FnType::None {
                    // report_err(stmt.keyword, "Can't return from top-level code.");
                    //todo: use lox::error_error instead of panic
                    /*
                     * You could imagine doing lots of other analysis in here.
                     * For example,
                     * if we added break statements to Lox,
                     * we would probably want to ensure they are only used inside loops.
                     */
                    //todo: add break❗
                    /*
                     * many IDEs will warn if you have unreachable code after a
                     * return statement, or a local variable whose value is never read.
                     * All of that would be pretty easy to add to our static
                     * visiting pass, or as separate passes.❗
                     *
                     * However, there is a real runtime cost to traversing the
                     * syntax tree itself, so bundling multiple analyses into a
                     * single pass is usually faster.
                     */
                    //todo: Extend the resolver to report an error if a local variable is never used.
                    panic!("{keyword:?}, Can't return from top-level code.");
                } else if self.current_fn_type == FnType::Constructor {
                    /*
                     * We’re still not done. We statically disallow returning a
                     * value from an initializer, but you can still use an empty
                     * early return.
                     */
                    // Lox.error(stmt.keyword, "Can't return a value from an initializer.");
                    panic!("{keyword:?}, Can't return a value from an initializer.");
                };

                match value {
                    Expr::None => {}
                    _ => self.resolve(value),
                }
            }
            /*
             * As in if statements, with a while statement,
             * we resolve its condition and resolve the body exactly once.
             */
            Statement::While { condition, body } => {
                self.resolve(condition);
                self.resolve(*body);
            }
            Statement::None(_) => unreachable!(),
        }
    }
}

type LocalSideTable = HashMap<Expr, usize>;
#[allow(dead_code)]
#[derive(Debug)]
struct Interpreter {
    runtime_error: bool,
    global_env: Vec<Env>,
    current_fn_call: Tk,
    current_fn_type: FnType,
    //? this is for keeping track the parent of function declaration
    functions_map: HashMap<Tk, Tk>,
    closures: HashMap<Tk, Env>,
    /*
     * Interactive tools like IDEs often incrementally re-parse and re-resolve
     * parts of the user’s program.
     * It may be hard to find all of the bits of state that need
     * recalculating when they’re hiding in the foliage of the syntax tree.
     * A benefit of storing this data outside of the nodes is that
     * it makes it easy to discard it—simply clear the map.
     */
    locals: LocalSideTable,
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
        preludio.define("clock".to_string(), V::NativeFunc("clock".to_string()));
        Self {
            runtime_error: false,
            global_env: vec![preludio, initial_env],
            closures: HashMap::new(),
            functions_map: HashMap::new(),
            current_fn_call: Tk::Default,
            current_fn_type: FnType::None,
            results: vec![],
            locals: HashMap::new(),
        }
    }

    fn eval(&mut self, statements: Vec<Statement>) {
        for (pos, state) in statements.iter().enumerate() {
            println!("\n{pos} - Statement: {state:?}");
            let value = self.eval_statement(state.clone());
            match value {
                Ok(_val) => (),
                Err(err) => match err {
                    Runtime::MustBeNumber(_) => todo!(),
                    Runtime::NotNumber(_, _, _) => todo!(),
                    Runtime::MustBeBoolean(_) => todo!(),
                    Runtime::UndefinedVariable(v) => todo!("undefined variable {}", v),
                    Runtime::NotCallableValue(_) => todo!(),
                    Runtime::WrongCallableArity(_, _, _) => todo!(),
                    Runtime::NotInstance(_) => todo!(),
                    Runtime::UndefinedProperty(_) => todo!(),
                    Runtime::NonCallable => todo!("non callable value"),
                    Runtime::MustBeRow() => todo!("debe ser Row!"),
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
        for (pos, state) in statements.iter().enumerate() {
            println!("\n{pos} - BLOCK IN: {state:?}");
            let returned = self.eval_statement(state.clone())?;
            if let V::Return(value) = returned {
                self.global_env.pop();
                println!("<<< BLOCK out RETURN");
                for env in self.global_env.iter() {
                    println!("globals: {}", env);
                }
                return Ok(V::Return(value));
            }
        }
        /*
         * To execute code within a given scope, this method updates the
         * interpreter’s environment field, visits all of the statements,
         * and then restores the previous value.
         */
        self.global_env.pop();
        println!("<<< BLOCK out NO RETURN");
        for env in self.global_env.iter() {
            println!("globals: {}", env);
        }
        Ok(V::Done)
        // todo: it gets restored even if an exception is thrown.
    }

    fn eval_statement(&mut self, state: Statement) -> RValue {
        // println!("{:?}", state);
        match state {
            Statement::Row {
                name,
                super_expr,
                columns,
            } => {
                let super_value = if super_expr.clone() != Expr::None {
                    /*
                     * If the class has a superclass expression, we evaluate it.
                     * Since that could potentially evaluate to some other kind
                     * of object, we have to check at runtime that the thing
                     * we want to be the superclass is actually a class.
                     * Bad things would happen❗. . .
                     */
                    Some(self.eval_expr(super_expr.clone())?)
                } else {
                    None
                };

                //? si sabemos que no hay super solo es un noop
                //? de otro modo verificamos si no es Row
                //? tiramos error right away mf❗
                //todo: huele a que hay un mejor refactor❗
                if super_expr.clone() == Expr::None {
                    ()
                } else {
                    let Some(V::Row(_)) = super_value else {
                        return Err(Runtime::MustBeRow());
                    };
                }

                self.define_declaration(name.clone(), V::Done);
                //? por que no lo definimos en seguida❓
                //? por que vamos a definir los métodos❗
                let mut behaviors = HashMap::new();
                for behavior_declaration in columns {
                    if let Statement::FunctionDeclaration {
                        kind: _,
                        name,
                        parameters: _,
                        body: _,
                    } = behavior_declaration.clone()
                    {
                        let behavior_name = String::from(name);
                        /*
                         * When we interpret a class declaration statement, we turn the syntactic
                         * representation of the class—its AST node—into its runtime representation.
                         * Now, we need to do that for the methods contained in the class as well.
                         * Each method declaration blossoms into a LoxFunction object.
                         *
                         * For actual function declarations, isInitializer is always false.
                         * For methods, we check the name.
                         */
                        let function =
                            FunctionObject::new(behavior_declaration, behavior_name.eq(&"new"));
                        behaviors.insert(behavior_name, function);
                    } else {
                        unreachable!("not a function declaration!")
                    }
                }
                let row = V::Row(Box::new(RowObject::new(
                    name.clone(),
                    behaviors,
                    super_value,
                )));
                self.assign_variable(name, row, Expr::None)?;
                Ok(V::Done)
            }
            Statement::Return { keyword: _, value } => {
                let evaluated_value = self.eval_expr(value)?;
                self.results.push(("ret".into(), evaluated_value.clone()));
                Ok(V::Return(Box::new(evaluated_value)))
            }
            Statement::FunctionDeclaration {
                kind,
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
                let function = FunctionObject::new(
                    Statement::FunctionDeclaration {
                        kind,
                        name: name.clone(),
                        parameters,
                        body,
                    },
                    false,
                );
                /*
                 * Function declarations are different from other literal nodes
                 * in that the declaration also binds the resulting object to
                 * a new variable. So, after creating the LoxFunction,
                 * we create a new binding in the current environment and
                 * store a reference to it there.
                 */
                self.functions_map
                    .insert(name.clone(), self.current_fn_call.clone());
                self.define_declaration(name.clone(), V::Func(Box::new(function)));
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
                let evaluated_condition = self.eval_expr(condition.clone())?;
                println!("{} FOR {condition:?}", evaluated_condition);
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
                //? detalles, como el scope es similar a
                //? una lista enlazada, hay que tener en cuenta
                //? que hay dos env temporales por bloque
                //? la del block y la de call
                //? y parece que lo que queremos es
                //? usar el env closure, para los dos, sera❓
                //todo: unir call y block, lo mejor seria solo usar el closure
                //todo: por ahora hay duplicación❗
                self.define_declaration(name, val);

                Ok(V::Done)
            }
            Statement::None(msg) => unreachable!("{}", msg),
        }
    }

    fn define_declaration(&mut self, name: Tk, val: V) {
        /*
        todo: test ->
        * var a;
        * print a; // "nil".
        todo: test ->
        * print a;
        * var a = "too late!";
        */
        let name = String::from(name);
        let size = self.global_env.len() - 2;
        if let Some(call_environment) = self.global_env.get_mut(size) {
            if let EnvKind::Call(closure_name) = &call_environment.kind {
                //? el detalle es que BLOCK
                //? puede ser de otro statement
                //? tipo for o while
                //? para asegurarme, debido a la implementación
                //? que no es recursiva
                //? necesito los últimos dos environments
                //? si es BLOCK Y CALL, es para la closure
                //? si no, va al global❗
                /*
                 * in our implementation, environments do act like the entire block is one scope,
                 * just a scope that changes over time. Closures do not like that.
                 *
                 * The function should capture a frozen snapshot of the environment
                 * as it existed at the moment the function was declared.
                 * But instead, in the Java code, it has a reference to the
                 * actual mutable environment object. When a variable is later
                 * declared in the scope that environment corresponds to,
                 * the closure sees the new variable, even though the declaration does not precede the function.
                 */
                //todo: remove duplication❗
                self.closures
                    .entry(closure_name.clone())
                    .and_modify(|closure_env| closure_env.define(name.clone(), val.clone()));
                call_environment.define(name.clone(), val.clone());
                // todo: remove logs
                for env in self.global_env.iter() {
                    println!("- G: {}", env);
                }
                return;
            }
        }

        if let Some(environment) = self.global_env.last_mut() {
            // println!("\ndefine declaration {name:?} in {:?}", environment.kind);
            environment.define(name, val);
        }
        for env in self.global_env.iter() {
            println!("- G: {}", env);
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> RValue {
        // println!("{:?}", expr);
        match expr.clone() {
            /*
             * The remaining task is interpreting those this expressions.
             * Similar to the resolver,
             * it is the same as interpreting a variable expression.
             */
            // Expr::This(keyword) => self.lookup_variable(keyword, expr),
            Expr::This(keyword) => {
                let value = self.lookup_variable(keyword, expr)?;
                // println!("looked_up SELF: {value}:?");
                Ok(value)
            }
            Expr::SetProp {
                identifier_expr,
                prop_name,
                expr_value,
            } => {
                /*
                 * This is another semantic edge case. There are three distinct operations:
                 *
                 * - Evaluate the object.
                 * - Raise a runtime error if it’s not an instance of a class.
                 * - Evaluate the value.
                 *
                 * The order that those are performed in could be user visible,
                 * which means we need to carefully specify it and ensure our
                 * implementations do these in the same order.
                 */
                let id = match *identifier_expr.clone() {
                    Expr::Let(to_identifier) => to_identifier,
                    _ => unreachable!(),
                };

                let value_instance = self.eval_expr(*identifier_expr)?;
                if let V::Instance(mut instance) = value_instance {
                    let val = self.eval_expr(*expr_value)?;
                    instance.mut_prop(prop_name, val.clone());
                    //? reasignamos por que hacemos muchos clones
                    //? y no mutamos una referencia❗
                    // todo: refactor como referencia❓
                    self.assign_variable(
                        id,
                        V::Instance(instance),
                        //? esto lo manda directo a buscar en global_env
                        // todo: esto se va romper si no esta en global❗
                        Expr::None,
                    )?;
                    Ok(val)
                } else {
                    //*  "Only instances have fields."
                    Err(Runtime::NotInstance(prop_name))
                }
            }
            Expr::GetProp(instance_expr, prop_name) => {
                /*
                 * You can literally see that property dispatch
                 * in Lox is dynamic since we don’t process
                 * the property name during the static resolution pass.
                 */
                let instance_value = self.eval_expr(*instance_expr)?;
                /*
                 * First, we evaluate the expression whose property
                 * is being accessed. In Lox, only instances of
                 * classes have properties.
                 * If the object is some other type like a number,
                 * invoking a getter on it is a runtime error.
                 */
                if let V::Instance(boxed_instance) = instance_value {
                    boxed_instance.get_prop(self, prop_name)
                } else {
                    Err(Runtime::NotInstance(prop_name))
                }
            }
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
                // todo: do it in a generic way❗❓
                let mut function: Box<dyn Callable> = match function_value.clone() {
                    V::Func(function) => function,
                    V::Row(function) => function,
                    // V::NativeCallable(_) => self.call(function_value, argument_values),
                    /*
                     * We still throw an exception,
                     * but now we’re throwing our own exception type,
                     * one that the interpreter knows to catch and
                     * report gracefully.
                     */
                    _ => return Err(Runtime::NotCallableValue(paren)),
                };

                if argument_list.len() != function.arity() {
                    return Err(Runtime::WrongCallableArity(
                        paren,
                        function.arity(),
                        argument_list.len(),
                    ));
                }

                //todo: how to implement display on a dyn trait❓
                // println!("\n {function_value} CALL WITH {argument_list:?}");

                let val = function.call(self, argument_list)?;

                Ok(val)
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
                // todo: print a = 2; // "2".
                self.assign_variable(to_identifier, value, expr)
            }
            Expr::Let(identifier) => {
                let value = self.lookup_variable(identifier, expr)?;
                // println!("looked_up_variable: {value}:?");
                Ok(value)
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
                    (Tk::Sub, value) => Err(Runtime::MustBeNumber(value)),
                    (Tk::Bang, V::Bool(false)) => Ok(V::Bool(true)),
                    (Tk::Bang, V::Bool(true)) => Ok(V::Bool(false)),
                    (Tk::Bang, value) => Err(Runtime::MustBeBoolean(value)),
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
                // println!("left: {:?}", left);
                let right = self.eval_expr(*right)?;
                // println!("right: {:?}", right);
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
                    (l, Tk::Sub, r) => Err(Runtime::NotNumber(l, Tk::Sub, r)),
                    (V::I32(l), Tk::Mul, V::I32(r)) => Ok(V::I32(l * r)),
                    (V::F64(l), Tk::Mul, V::F64(r)) => Ok(V::F64(l * r)),
                    (l, Tk::Mul, r) => Err(Runtime::NotNumber(l, Tk::Mul, r)),
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
                    (l, Tk::EQ, r) => Err(Runtime::NotNumber(l, Tk::EQ, r)),
                    (V::I32(l), Tk::LT, V::I32(r)) => Ok(V::Bool(l.lt(&r))),
                    (V::F64(l), Tk::LT, V::F64(r)) => Ok(V::Bool(l.lt(&r))),
                    (l, Tk::LT, r) => Err(Runtime::NotNumber(l, Tk::LT, r)),
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

    //todo: refactor❗
    fn assign_variable(&mut self, to_identifier: Tk, value: V, expr: Expr) -> RValue {
        if let Some(distance) = self.locals.get(&expr) {
            //? +1 PRELUDE +1 GLOBALS
            let index = 2 + (*distance);
            println!(
                "\ntotal: {}, distance: {distance}, idx: {index} for {expr:?}",
                self.global_env.len()
            );

            if let Some(env) = self.global_env.get_mut(index) {
                // println!("\nlooking for: {identifier:?} ? -> {:?}", environment.kind);
                // println!("\nenv: {env}");
                // todo: remove after depth testing
                if env.kind == EnvKind::Global {
                    panic!("should not be global!")
                }
                //? looking up in closure
                let current_fn_call = &self.current_fn_call;
                if env.kind == EnvKind::Call(current_fn_call.clone()) {
                    if let Some(closure_name) = self.functions_map.get(current_fn_call) {
                        // println!("\nShould look in Closure: {closure_name:?}");
                        if let Some(closure) = self.closures.get_mut(&closure_name.clone()) {
                            // println!("\nlooking for: {to_identifier:?} in {:?}", closure.kind);
                            if let Some(value) =
                                closure.assign(to_identifier.clone(), value.clone())
                            {
                                return Ok(value);
                            }
                        }
                    }
                }
                //? continue with global_env
                if let Some(value) = env.assign(to_identifier.clone(), value.clone()) {
                    Ok(value)
                } else {
                    Err(Runtime::UndefinedVariable(to_identifier.into()))
                }
            } else {
                unreachable!("an error in the resolver or the interpreter!")
            }
        } else {
            //? PRELUDE is At index: 0
            if let Some(globals) = self.global_env.get_mut(1) {
                // println!("\nassigning to: {to_identifier:?} ? -> {:?}", globals.kind);
                if let Some(value) = globals.assign(to_identifier.clone(), value) {
                    Ok(value)
                } else {
                    Err(Runtime::UndefinedVariable(to_identifier.into()))
                }
            } else {
                unreachable!("globals is not defined!")
            }
        }
    }

    //todo: refactor❗
    fn lookup_variable(&mut self, identifier: Tk, expr: Expr) -> RValue {
        // println!("- IN CALL {:?}", self.current_function_call);
        // println!("- TYPE {:?}", self.current_function_type);
        // println!("- FNS {:?}", self.functions_map);
        for (tk, env) in self.closures.iter() {
            println!("- CLOSURE: {tk:?} \n{env}");
        }

        match (&self.current_fn_type, &identifier) {
            (FnType::Column, Tk::ThisTk) => {
                let call_idx = self.global_env.len() - 2;
                if let Some(environment) = self.global_env.get_mut(call_idx) {
                    // println!("\nlooking for: {identifier:?} ? -> {:?}", environment.kind);
                    // println!("\nenv: {environment}");
                    // todo: remove after depth testing
                    if environment.kind == EnvKind::Global {
                        panic!("should not be global!")
                    }
                    //? continue with global_env
                    if let Some(value) = environment.fetch(identifier.clone()) {
                        return Ok(value);
                    } else {
                        unreachable!("SELF should be defined!")
                    }
                } else {
                    unreachable!("an error in the resolver or the interpreter!")
                }
            }
            (FnType::Column, _) => {
                if let Some(closure) = self.closures.get(&self.current_fn_call) {
                    // println!("\nLOOK: {identifier:?} in {closure}");
                    if let Some(value) = closure.fetch(identifier.clone()) {
                        return Ok(value);
                    }
                }
            }
            _ => (),
        }
        /*
         * The interpreter code trusts that the resolver did its job
         * and resolved the variable correctly.
         * This implies a deep coupling between these two classes. (interpreter & resolver)
         *
         * In the resolver, each line of code that touches a scope must have
         * its exact match in the interpreter for modifying an environment.
         */
        if let Some(distance) = self.locals.get(&expr) {
            // let size = self.global_env.len() - 2;
            //? +1 PRELUDE +1 GLOBALS
            let mut index = 2 + (*distance);
            println!(
                "\ncurrent call: {:?}, total: {}, distance: {distance}, idx: {index} for {expr:?}",
                self.current_fn_call,
                self.global_env.len()
            );
            /*
             * This walks a fixed number of hops up the parent chain and returns
             * the environment there. Once we have that, getAt() simply returns
             * the value of the variable in that environment’s map.
             * It doesn’t even have to check to see if the variable is
             * there—we know it will be because the resolver already found it before.
             */
            let mut tmp = index + 1;
            //todo: this seems brittle, find out a better strategy❗
            while let Some(env) = self.global_env.get(tmp) {
                // println!("env: {}", env);
                if let EnvKind::Call(fn_name) = env.kind.clone() {
                    //? if next CAll is the same function call
                    //? it is a recursive call
                    if fn_name == self.current_fn_call {
                        println!("tmp: {}", tmp - index);
                        index += tmp - index
                    } else {
                        //? hence not a recursive call❗
                        break;
                    }
                }
                tmp += 1;
            }
            // println!("idx: {index}");
            if let Some(env) = self.global_env.get_mut(index) {
                // println!("\nlooking for: {identifier:?} ? -> {:?}", environment.kind);
                // println!("\nenv: {environment}");
                // todo: remove after depth testing
                if env.kind == EnvKind::Global {
                    panic!("should not be global!")
                }
                //? looking up in closure
                let current_fn_call = &self.current_fn_call;
                if env.kind == EnvKind::Call(current_fn_call.clone()) {
                    // println!("\n fns: {:?}", self.functions_table);
                    // println!("\n closures: {:?}", self.closures);
                    if let Some(closure_name) = self.functions_map.get(current_fn_call) {
                        //? if tk:default -> then look up on GLOBALS
                        // println!("\nShould look in Closure: {closure_name:?}");
                        if let Some(closure) = self.closures.get(&closure_name.clone()) {
                            // println!("\nlooking for: {identifier:?} in {:?}", closure.kind);
                            if let Some(value) = closure.fetch(identifier.clone()) {
                                return Ok(value);
                            }
                        }
                    }
                }
                //? continue with global_env
                if let Some(value) = env.fetch(identifier.clone()) {
                    Ok(value)
                } else {
                    Err(Runtime::UndefinedVariable(identifier.into()))
                }
            } else {
                unreachable!("an error in the resolver or the interpreter!")
            }
        } else {
            //? PRELUDE is At index: 0
            if let Some(globals) = self.global_env.get_mut(1) {
                if let Some(value) = globals.fetch(identifier.clone()) {
                    Ok(value)
                } else {
                    Err(Runtime::UndefinedVariable(identifier.into()))
                }
            } else {
                unreachable!("globals is not defined!")
            }
        }
    }

    /*
     * Each time it visits a variable, it tells the interpreter
     * how many scopes there are between the current scope and the
     * scope where the variable is defined. At runtime,
     * this corresponds exactly to the number of environments
     * between the current one and the enclosing one where the
     * interpreter can find the variable’s value.
     *
     * The resolver hands that number to the interpreter by calling this:
     */
    fn resolve_variable(&mut self, expr: Expr, depth: usize) {
        self.locals.insert(expr, depth);
        //? - [PRELUDE, GLOBALS, scope 0, scope 1, scope innermost 2....]
        // println!("\nlocals(expr,depth): {:?}", self.locals);
    }
}

enum ParserAy {
    BadExpression(Tk),
}

#[derive(PartialEq)]
pub enum Runtime {
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
    NotInstance(Tk),
    UndefinedProperty(Tk),
    NonCallable,
    MustBeRow(),
}

use std::result::Result as StdResult;

// use env_01_recursive::Env;
use env_02_vector::Env;

use crate::env_02_vector::EnvKind;
type Result<T> = StdResult<T, ParserAy>;
type RStatement = StdResult<Statement, ParserAy>;
type RValue = StdResult<V, Runtime>;

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
     * declaration    → | class
     *                  | function
     *                  | variable
     *                  | statement ;
     *
     * statement      → | expr
     *                  | if
     *                  | print
     *                  | while
     *                  | block ;
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

    //* declaration    → class | function | variable | statement ;
    fn declaration_or_statement(&mut self) -> RStatement {
        let result = match self.current_token {
            Tk::Class => self.class_declaration()?,
            Tk::Fn => self.function_declaration(FnType::Function)?,
            Tk::Var => self.variable_declaration()?,
            _ => self.statement()?,
        };

        Ok(result)
    }

    /*
     * In plain English, a class declaration is the class keyword,
     * followed by the class’s name, then a curly-braced body.
     * Inside that body is a list of method declarations.
     * Unlike function declarations, methods don’t have a leading fun keyword.
     * Each method is a name, parameter list, and body.
     */
    /*
     * Unlike some other object-oriented languages like Java, Lox has no root “Object”
     * class that everything inherits from, so when you omit the superclass clause,
     * the class has no superclass, not even an implicit one.
     */
    // * classDecl → ""class" IDENTIFIER ( "<" IDENTIFIER ) ? "{" function* "}" ;
    fn class_declaration(&mut self) -> RStatement {
        // Token name = consume(IDENTIFIER, "Expect class name.");
        self.consume_token();
        let name = if let Tk::Identifier(_, _) = self.current_token {
            self.current_token.clone()
        } else {
            // todo: test err❗
            report_err(
                Tk::Identifier("class_name".into(), 0),
                self.current_token.clone(),
                "Expect class name.",
            );
            self.current_token.clone()
        };

        //? Expr.Variable superclass = null;
        //? if (match(LESS)) {
        //?   consume(IDENTIFIER, "Expect superclass name.");
        //?   superclass = new Expr.Variable(previous());
        //? }
        self.consume_token();
        // todo: esta feito, buscar un buen refactor❗
        let super_expr = if self.current_token == Tk::LT {
            self.consume_token();
            self.consume_token();
            if let Tk::Identifier(_, _) = self.prev_token {
                Expr::Let(self.prev_token.clone())
            } else {
                Expr::None
            }
        } else {
            Expr::None
        };

        //? consume(LEFT_BRACE, "Expect '{' before class body.");
        if self.current_token != Tk::LBrace {
            // todo: test err❗
            report_err(
                Tk::LBrace,
                self.current_token.clone(),
                "Expect '{' before class body.",
            );
        }
        // List<Stmt.Function> methods = new ArrayList<>();
        let mut columns = vec![];
        // while (!check(RIGHT_BRACE) && !isAtEnd()) {
        //   methods.add(function("method"));
        // }
        //todo: refactor as while let ❓
        /*
         * Like we do in any open-ended loop in the parser,
         * we also check for hitting the end of the file.
         *
         * it ensures the parser doesn’t get stuck in an infinite loop
         * if the user has a syntax error and forgets to correctly end the class body.
         */
        while self.current_token != Tk::RBrace && self.current_token != Tk::End {
            let function = self.function_declaration(FnType::Column)?;
            if let Statement::None(_) = function {
                //? Statement::None(_)
                //? this is for the special case:
                //? class Bagel {}
                break;
            } else {
                columns.push(function);
            }
        }

        println!("{:?}", self.current_token);
        // consume(RIGHT_BRACE, "Expect '}' after class body.");
        if self.current_token != Tk::RBrace {
            report_err(
                Tk::RBrace,
                self.current_token.clone(),
                "Expect '}' after class body.",
            );
        }
        self.consume_token();
        // return new Stmt.Class(name, methods);
        Ok(Statement::Row {
            name,
            columns,
            /*
             * If we didn’t parse a superclass clause, the superclass expression
             * will be Expr::None. We’ll have to make sure the later passes check for that.
             * The first of those is the resolver.
             */
            super_expr,
        })
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
    fn function_declaration(&mut self, kind: FnType) -> RStatement {
        self.consume_token();
        if self.current_token == Tk::RBrace {
            return Ok(Statement::None("non-function".into()));
        }

        let kind_string = match kind {
            FnType::Constructor => "initializer",
            FnType::Column => "column",
            FnType::Function => "function",
            FnType::None => unreachable!("not a function type!"),
        };

        let name = if let Tk::Identifier(_, _) = self.current_token {
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
                let parameter_identifier = if let Tk::Identifier(_, _) = self.current_token {
                    self.current_token.clone()
                } else {
                    self.report_and_consume(
                        Tk::Identifier("parameter".to_string(), 0),
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
        if String::from(name.clone()).as_str() == "new" {
            Ok(Statement::FunctionDeclaration {
                kind: FnType::Constructor,
                name,
                parameters,
                body,
            })
        } else {
            Ok(Statement::FunctionDeclaration {
                kind,
                name,
                parameters,
                body,
            })
        }
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
        let result_token = if let Tk::Identifier(_, _) = self.current_token {
            self.current_token.clone()
        } else {
            report_err(
                Tk::Identifier("name".into(), 0),
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
            Tk::Return(_) => self.return_statement()?,
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
        let value = if self.current_token != Tk::Semi && self.current_token != Tk::End {
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

    //* assignment → ( call "." )? IDENTIFIER "=" assignment | logic_or ;
    /*
     * Unlike getters, setters don’t chain.❗
     *
     * However, the reference to call allows any high-precedence expression
     * before the last dot, including any number of getters, as in:
     *
     * 🎈 breakfast.omelette.filling.meat = ham
     * Note here that only the last part, the .meat is the setter.
     * The .omelette and .filling parts are both get expressions.
     *
     * Instead of falling back to equality, assignment now cascades to
     * logic_or. The two new rules, logic_or and logic_and, are
     * similar to other binary operators.
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
            let right_value = self.assignment()?;

            if let Expr::Let(identifier) = left {
                /*
                 * If we find an =, we parse the right-hand side and then
                 * wrap it all up in an assignment expression tree node.
                 */
                return Ok(Expr::Assign(identifier, Box::new(right_value)));
            } else if let Expr::GetProp(get_expr, get_prop_name) = left {
                /*
                 * Instead, the trick we do is parse the left-hand side as a normal expression.
                 * Then, when we stumble onto the equal sign after it,
                 * we take the expression we already parsed and transform it
                 * into the correct syntax tree node for the assignment.
                 */
                return Ok(Expr::SetProp {
                    identifier_expr: get_expr,
                    prop_name: get_prop_name,
                    expr_value: Box::new(right_value),
                });
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

    // * call       → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
    /*
     * After a primary expression, we allow a series of any mixture of
     * parenthesized calls and dotted property accesses.
     * “Property access” is a mouthful, so from here on out,
     * we’ll call these “get expressions”.
     */
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
         *
         * loop there corresponds to the * in the grammar rule
         * . . . | "." IDENTIFIER )* <-
         *
         * example:
         * egg.scramble(3).with(cheddar)
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
            } else if self.current_token == Tk::Dot {
                self.consume_token();
                let name = if let Tk::Identifier(_, _) = self.current_token {
                    self.current_token.clone()
                } else {
                    // todo: test err❗
                    report_err(
                        Tk::Identifier("get property".into(), 0),
                        self.current_token.clone(),
                        "Expect property name after '.'.",
                    );
                    self.current_token.clone()
                };
                println!("{}", expr);
                expr = Expr::GetProp(Box::new(expr), name);
                self.consume_token();
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
        //todo: how this clone, can affect❓
        match self.current_token.clone() {
            Tk::ThisTk => {
                self.consume_token();
                Ok(Expr::This(self.prev_token.clone()))
            }
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
            Tk::Identifier(_, _) => {
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
                | Tk::Return(_) => return,
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
    // let expression = Expr::binary_op(
    //     Expr::unary_op(Tk::Sub, Expr::literal(123)),
    //     Tk::Mul,
    //     Expr::grouping(Expr::literal(45.65)),
    // );
    // println!("{:?}", expression.to_string());
    // let a = 3;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic = "debe ser Row!"]
    fn test_cannot_inherit_from_non_row_value() {
        let tokens = vec![
            //? var NotAClass = 1_000;
            //? class Subclass < NotAClass {}
            Tk::Var,
            Tk::Identifier("NotAClass".into(), 1),
            Tk::Assign,
            Tk::Num(1_000),
            Tk::Semi,
            //? class Subclass < NotAClass {}
            Tk::Class,
            Tk::Identifier("Subclass".into(), 2),
            Tk::LT,
            Tk::Identifier("NotAClass".into(), 2),
            Tk::LBrace,
            Tk::RBrace,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![]);
        let _ = test_run(environment, statements);
    }

    #[test]
    #[should_panic = "Identifier(\"Oops\", 1) A row can't inherit from itself."]
    fn test_cannot_inherit_itself() {
        let tokens = vec![
            //? class Oops < Oops {}
            Tk::Class,
            Tk::Identifier("Oops".into(), 1),
            Tk::LT,
            Tk::Identifier("Oops".into(), 1),
            Tk::LBrace,
            Tk::RBrace,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![]);
        let _ = test_run(environment, statements);
    }

    #[test]
    #[should_panic = "Return(3), Can't return a value from an initializer."]
    fn test_constructor_call_returns_instance_3() {
        //?1 class Foo {
        //?2        new() {
        //?3            return;
        //?4        }
        //?5  }
        //* Prints "The German chocolate cake is delicious!".
        let tokens = vec![
            //?1 class Foo {
            Tk::Class,
            Tk::Identifier("Foo".into(), 1),
            Tk::LBrace,
            //?2        new() {
            Tk::Identifier("new".into(), 2),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            //?3             return;
            Tk::Return(3),
            Tk::Semi,
            Tk::RBrace,
            Tk::RBrace,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![]);
        let inter = test_run(environment, statements);

        let mut row = RowObject::new(Tk::Identifier("Foo".into(), 1), HashMap::new(), None);
        row.behaviors.insert(
            String::from("new"),
            FunctionObject::new(
                Statement::FunctionDeclaration {
                    kind: FnType::Constructor,
                    name: Tk::Identifier("new".into(), 2),
                    parameters: vec![],
                    body: vec![Statement::Print(Expr::This(Tk::ThisTk))],
                },
                true,
            ),
        );
        let instance = V::Instance(Box::new(InstanceObject::new(row)));
        // todo: assert stdout
        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        // assert_eq!(iter.next_back().unwrap(), &("print".into(), V::Done));
        assert_eq!(
            iter.next_back().unwrap(),
            &("print".into(), instance.clone())
        );
    }

    #[test]
    #[should_panic = "Return(3), Can't return a value from an initializer."]
    fn test_constructor_call_returns_instance_2() {
        //?1 class Foo {
        //?2        new() {
        //?3            return 42;
        //?4        }
        //?5  }
        //* Prints "The German chocolate cake is delicious!".
        let tokens = vec![
            //?1 class Foo {
            Tk::Class,
            Tk::Identifier("Foo".into(), 1),
            Tk::LBrace,
            //?2        new() {
            Tk::Identifier("new".into(), 2),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            //?3             return 42;
            Tk::Return(3),
            Tk::Num(42),
            Tk::Semi,
            Tk::RBrace,
            Tk::RBrace,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![]);
        let _ = test_run(environment, statements);
    }

    #[test]
    fn test_constructor_call_returns_instance() {
        //?1 class Foo {
        //?2        new() {
        //?3            print this;
        //?4        }
        //?5  }
        //?6  var foo = Foo();
        //?7  print foo.new();
        //* Prints "The German chocolate cake is delicious!".
        let tokens = vec![
            //?1 class Foo {
            Tk::Class,
            Tk::Identifier("Foo".into(), 1),
            Tk::LBrace,
            //?2        new() {
            Tk::Identifier("new".into(), 2),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            //?3            print this;
            Tk::Print,
            Tk::ThisTk,
            Tk::Semi,
            Tk::RBrace,
            Tk::RBrace,
            //?6  var foo = Foo();
            Tk::Var,
            Tk::Identifier("foo".into(), 6),
            Tk::Assign,
            Tk::Identifier("Foo".into(), 6),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            //?7  print foo.init();
            Tk::Print,
            Tk::Identifier("foo".into(), 7),
            Tk::Dot,
            Tk::Identifier("new".into(), 7),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![]);
        let inter = test_run(environment, statements);

        let mut row = RowObject::new(Tk::Identifier("Foo".into(), 1), HashMap::new(), None);
        row.behaviors.insert(
            String::from("new"),
            FunctionObject::new(
                Statement::FunctionDeclaration {
                    kind: FnType::Constructor,
                    name: Tk::Identifier("new".into(), 2),
                    parameters: vec![],
                    body: vec![Statement::Print(Expr::This(Tk::ThisTk))],
                },
                true,
            ),
        );
        let instance = V::Instance(Box::new(InstanceObject::new(row)));
        // todo: assert stdout
        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        // assert_eq!(iter.next_back().unwrap(), &("print".into(), V::Done));
        assert_eq!(
            iter.next_back().unwrap(),
            &("print".into(), instance.clone())
        );
        assert_eq!(iter.next_back().unwrap(), &("print".into(), instance));
    }

    /*
     * There is no instance for this to point to if you’re not in a method.
     * We could give it some default value like nil or make it a runtime error,
     * but the user has clearly made a mistake.
     * The sooner they find and fix that mistake, the happier they’ll be.
     *
     * Our resolution pass is a fine place to detect this error statically.
     * It already detects return statements outside of functions.
     * We’ll do something similar for this.
     */
    #[test]
    #[should_panic = "Can't use 'SELF' outside of a Row."]
    fn test_invalid_this() {
        let tokens = vec![
            //? print this;
            Tk::Print,
            Tk::ThisTk,
            Tk::Semi,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![("a".into(), V::I32(22))]);
        let inter = test_run(environment, statements);

        // todo: assert stdout

        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(22)));
    }

    #[test]
    fn test_class_this() {
        //?1 class Cake {
        //?2        taste() {
        //?3            var adjective = 1000;
        //?4            print adjetive;
        //?5            print this.flavor;
        //?6        }
        //?7  }
        //?8  var cake = Cake();
        //?9  cake.flavor = 42;
        //?10 cake.taste();
        //* Prints "The German chocolate cake is delicious!".
        let tokens = vec![
            //?1 class Cake {
            Tk::Class,
            Tk::Identifier("Cake".into(), 1),
            Tk::LBrace,
            //?2        taste() {
            Tk::Identifier("taste".into(), 2),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            //?3            var adjective = 1000;
            Tk::Var,
            Tk::Identifier("adjetive".into(), 3),
            Tk::Assign,
            Tk::Num(1000),
            Tk::Semi,
            //?4            print adjetive;
            Tk::Print,
            Tk::Identifier("adjetive".into(), 4),
            Tk::Semi,
            //?5            print this.flavor;
            Tk::Print,
            Tk::ThisTk,
            Tk::Dot,
            Tk::Identifier("flavor".into(), 5),
            Tk::Semi,
            Tk::RBrace,
            Tk::RBrace,
            //?8  var cake = Cake();
            Tk::Var,
            Tk::Identifier("cake".into(), 8),
            Tk::Assign,
            Tk::Identifier("Cake".into(), 8),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            //?9  cake.flavor = 42;
            Tk::Identifier("cake".into(), 9),
            Tk::Dot,
            Tk::Identifier("flavor".into(), 9),
            Tk::Assign,
            Tk::Num(42),
            Tk::Semi,
            //?10 cake.taste();
            Tk::Identifier("cake".into(), 10),
            Tk::Dot,
            Tk::Identifier("taste".into(), 10),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![]);
        let inter = test_run(environment, statements);

        // todo: assert stdout
        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(42)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(1_000)));
    }

    #[test]
    fn test_class_methods() {
        //? class Bagel {
        //?     eat() {
        //?       print 42;
        //?     }
        //? }

        //? Bagel().eat();
        //* Prints "Crunch crunch crunch!".
        let tokens = vec![
            //? class Bagel {
            Tk::Class,
            Tk::Identifier("Bagel".into(), 1),
            Tk::LBrace,
            //? eat() {
            Tk::Identifier("eat".into(), 2),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            //?       print 42;
            Tk::Print,
            Tk::Num(42),
            Tk::Semi,
            Tk::RBrace,
            Tk::RBrace,
            //? Bagel().eat();
            Tk::Identifier("Bagel".into(), 5),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Dot,
            Tk::Identifier("eat".into(), 5),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![]);
        let inter = test_run(environment, statements);

        // todo: assert stdout
        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(42)));
    }

    #[test]
    fn test_class_instance() {
        //? class Bagel {}
        //? var bagel = Bagel();
        //? print bagel;
        //* Prints "Bagel instance".
        let tokens = vec![
            //? class Bagel {}
            Tk::Class,
            Tk::Identifier("Bagel".into(), 1),
            Tk::LBrace,
            Tk::RBrace,
            //? var bagel = Bagel();
            Tk::Var,
            Tk::Identifier("bagel".into(), 2),
            Tk::Assign,
            Tk::Identifier("Bagel".into(), 2),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            //? print bagel;
            Tk::Print,
            Tk::Identifier("bagel".into(), 3),
            Tk::Semi,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![]);
        let inter = test_run(environment, statements);

        // todo: assert stdout
        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(
            iter.next_back().unwrap(),
            &(
                "print".into(),
                V::Instance(Box::new(InstanceObject::new(RowObject::new(
                    Tk::Identifier("Bagel".into(), 1),
                    HashMap::new(),
                    None
                ))))
            )
        );
    }

    #[test]
    fn test_parse_a_class() {
        //? class DevonshireCream {
        //?     serveOn() {
        //?       return 42;
        //?     }
        //? }
        //?   print DevonshireCream;
        // * Prints "DevonshireCream".
        let tokens = vec![
            Tk::Class,
            Tk::Identifier("DevonshireCream".into(), 1),
            Tk::LBrace,
            //?     serveOn() {
            Tk::Identifier("serveOn".into(), 2),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            //?       return 42;
            Tk::Return(3),
            Tk::Num(42),
            Tk::Semi,
            Tk::RBrace,
            Tk::RBrace,
            //?   print DevonshireCream;
            Tk::Print,
            Tk::Identifier("DevonshireCream".into(), 6),
            Tk::Semi,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![]);
        let inter = test_run(environment, statements);

        // todo: assert stdout
        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        let mut behaviors = HashMap::new();
        behaviors.insert(
            "serveOn".to_string(),
            FunctionObject::new(
                Statement::FunctionDeclaration {
                    kind: FnType::Column,
                    name: Tk::Identifier("serveOn".into(), 2),
                    parameters: vec![],
                    body: vec![Statement::Return {
                        keyword: Tk::Return(3),
                        value: Expr::Literal(V::I32(42)),
                    }],
                },
                false,
            ),
        );

        assert_eq!(
            iter.next_back().unwrap(),
            &(
                "print".into(),
                V::Row(Box::new(RowObject {
                    name: Tk::Identifier("DevonshireCream".into(), 1),
                    behaviors,
                    super_row: None
                }))
            )
        );
    }

    #[test]
    #[should_panic = "Return(1), Can't return from top-level code."]
    fn test_invalid_return() {
        let tokens = vec![
            //? return "at top level";
            Tk::Return(1),
            Tk::Num(10),
            Tk::Semi,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![("a".into(), V::I32(22))]);
        let inter = test_run(environment, statements);

        // todo: assert stdout

        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(22)));
    }

    //? fun bad() {
    //?     var a = "first";
    //?     var a = "second";
    //? }
    //? on js❗
    //? Uncaught SyntaxError: Identifier 'a' has already been declared
    //? on rust❗
    //? there is no errors.
    #[test]
    #[should_panic = "Already a variable with this name in this scope."]
    fn test_resolver_errors() {
        let tokens = vec![
            Tk::Fn,
            Tk::Identifier("bad".into(), 1),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            //?  var a = "10;
            Tk::Var,
            Tk::Identifier("a".into(), 2),
            Tk::Assign,
            Tk::Num(10),
            Tk::Semi,
            //?  var a = "20;
            Tk::Var,
            Tk::Identifier("a".into(), 3),
            Tk::Assign,
            Tk::Num(10),
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![("a".into(), V::I32(22))]);
        let _inter = test_run(environment, statements);
    }

    //? var a = 22;
    //? {
    //?   fun showA() {
    //?     print a;
    //?   }
    //?   showA();
    //?   var a = 1_000;
    //?   showA();
    //? }
    #[test]
    fn test_avoid_dynamic_scope() {
        let tokens = vec![
            Tk::LBrace,
            Tk::Fn,
            Tk::Identifier("showA".into(), 3),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            Tk::Print,
            Tk::Identifier("a".into(), 4),
            Tk::Semi,
            Tk::RBrace,
            //?   showA();
            Tk::Identifier("showA".into(), 6),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            //?  var a = "10;
            Tk::Var,
            Tk::Identifier("a".into(), 7),
            Tk::Assign,
            Tk::Num(1_000),
            Tk::Semi,
            //?   showA();
            Tk::Identifier("showA".into(), 8),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![("a".into(), V::I32(22))]);
        let inter = test_run(environment, statements);

        // todo: assert stdout

        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(22)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(22)));
    }

    #[test]
    fn test_closure() {
        let tokens = vec![
            //? fun makeCounter() {
            //?     var i = 0;
            //?     fun count() {
            //?       i = i - 1;
            //?       print i;
            //?     }
            //?     return count;
            //?   }
            //?   var counter = makeCounter();
            //?   counter(); // "1".
            //?   counter(); // "2".
            Tk::Fn,
            Tk::Identifier("makeCounter".into(), 1),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            //? var i = 0;
            Tk::Var,
            Tk::Identifier("i".into(), 2),
            Tk::Assign,
            Tk::Num(0),
            Tk::Semi,
            //?     fun count() {
            //?       i = i + 1;
            //?       print i;
            //?     }
            Tk::Fn,
            Tk::Identifier("count".into(), 3),
            Tk::Lpar,
            Tk::Rpar,
            Tk::LBrace,
            Tk::Identifier("i".into(), 4),
            Tk::Assign,
            Tk::Identifier("i".into(), 4),
            Tk::Sub,
            Tk::Num(1),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("i".into(), 5),
            Tk::Semi,
            Tk::RBrace,
            //?     return count;
            //?   }
            Tk::Return(7),
            Tk::Identifier("count".into(), 7),
            Tk::Semi,
            Tk::RBrace,
            //?   var counter = makeCounter();
            Tk::Var,
            Tk::Identifier("counter".into(), 9),
            Tk::Assign,
            Tk::Identifier("makeCounter".into(), 9),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            //?   counter(); // "1".
            Tk::Identifier("counter".into(), 10),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            // //?   counter(); // "2".
            Tk::Identifier("counter".into(), 10),
            Tk::Lpar,
            Tk::Rpar,
            Tk::Semi,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![]);
        let inter = test_run(environment, statements);

        // todo: assert stdout

        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(-2)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(-1)));
    }

    #[test]
    #[should_panic = "not yet implemented: undefined variable makeCounter"]
    fn test_call_with_arguments() {
        let tokens = vec![
            //?   var counter = makeCounter(3);
            Tk::Var,
            Tk::Identifier("counter".into(), 1),
            Tk::Assign,
            Tk::Identifier("makeCounter".into(), 1),
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
            Tk::Identifier("counter".into(), 1),
            Tk::Assign,
            Tk::Identifier("makeCounter".into(), 1),
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
            Tk::Identifier("fibo".into(), 1),
            Tk::Lpar,
            Tk::Identifier("n".into(), 1),
            Tk::Rpar,
            Tk::LBrace,
            // * ->     if (n < 1 or n == 1) return n;
            Tk::If,
            Tk::Lpar,
            Tk::Identifier("n".into(), 2),
            Tk::LT,
            Tk::Num(1),
            Tk::Or,
            Tk::Identifier("n".into(), 2),
            Tk::EQ,
            Tk::Num(1),
            Tk::Rpar,
            Tk::Return(2),
            Tk::Identifier("n".into(), 2),
            Tk::Semi,
            // * ->     return fib(n - 2) - fib(n - 1);
            Tk::Return(3),
            Tk::Identifier("fibo".into(), 3),
            Tk::Lpar,
            Tk::Identifier("n".into(), 3),
            Tk::Sub,
            Tk::Num(2),
            Tk::Rpar,
            Tk::Sub,
            Tk::Identifier("fibo".into(), 3),
            Tk::Lpar,
            Tk::Identifier("n".into(), 3),
            Tk::Sub,
            Tk::Num(1),
            Tk::Rpar,
            Tk::Semi,
            Tk::RBrace,
            // * ->  for (let i = 11; 9 < i; i = i - 1) {
            Tk::For,
            Tk::Lpar,
            Tk::Var,
            Tk::Identifier("i".into(), 5),
            Tk::Assign,
            Tk::Num(11),
            Tk::Semi,
            Tk::Num(9),
            Tk::LT,
            Tk::Identifier("i".into(), 5),
            Tk::Semi,
            Tk::Identifier("i".into(), 5),
            Tk::Assign,
            Tk::Identifier("i".into(), 5),
            Tk::Sub,
            Tk::Num(1),
            Tk::Rpar,
            Tk::LBrace,
            // * ->     print fib(i);
            Tk::Print,
            Tk::Identifier("fibo".into(), 6),
            Tk::Lpar,
            Tk::Identifier("i".into(), 6),
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
        let (statements, environment) = test_setup(tokens, vec![]);
        let inter = test_run(environment, statements);

        // todo: assert stdout

        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(-55)));
        assert_eq!(iter.next_back().unwrap(), &("ret".into(), V::I32(-55)));
        assert_eq!(iter.next_back().unwrap(), &("ret".into(), V::I32(34)));
        assert_eq!(iter.next_back().unwrap(), &("ret".into(), V::I32(-21)));
        assert_eq!(iter.next_back().unwrap(), &("ret".into(), V::I32(13)));
        assert_eq!(iter.next_back().unwrap(), &("ret".into(), V::I32(-8)));
        assert_eq!(iter.next_back().unwrap(), &("ret".into(), V::I32(5)));
        assert_eq!(iter.next_back().unwrap(), &("ret".into(), V::I32(-3)));
        assert_eq!(iter.next_back().unwrap(), &("ret".into(), V::I32(2)));
        assert_eq!(iter.next_back().unwrap(), &("ret".into(), V::I32(-1)));
        assert_eq!(iter.next_back().unwrap(), &("ret".into(), V::I32(1)));
        assert_eq!(iter.next_back().unwrap(), &("ret".into(), V::I32(0)));
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
            Tk::Identifier("count".into(), 1),
            Tk::Lpar,
            Tk::Identifier("n".into(), 1),
            Tk::Rpar,
            Tk::LBrace,
            // * -> if (1 < n) count(n - 1);
            Tk::If,
            Tk::Lpar,
            Tk::Num(1),
            Tk::LT,
            Tk::Identifier("n".into(), 2),
            Tk::Rpar,
            Tk::Identifier("count".into(), 2),
            Tk::Lpar,
            Tk::Identifier("n".into(), 2),
            Tk::Sub,
            Tk::Num(1),
            Tk::Rpar,
            Tk::Semi,
            // * ->   print n;
            Tk::Print,
            Tk::Identifier("n".into(), 3),
            Tk::Semi,
            Tk::RBrace,
            //?  } count(3);
            Tk::Identifier("count".into(), 5),
            Tk::Lpar,
            Tk::Num(3),
            Tk::Rpar,
            Tk::Semi,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![]);
        let inter = test_run(environment, statements);

        // todo: assert stdout

        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(3)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(2)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(1)));
    }

    #[test]
    fn test_return_in_conditional() {
        let tokens = vec![
            // * ->     if (n < 1 or n == 1) return 2;
            Tk::If,
            Tk::Lpar,
            Tk::Identifier("n".into(), 1),
            Tk::LT,
            Tk::Num(1),
            Tk::Or,
            Tk::Identifier("n".into(), 1),
            Tk::EQ,
            Tk::Num(1),
            Tk::Rpar,
            Tk::Return(1),
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
            Tk::Identifier("sayHi".into(), 1),
            Tk::Lpar,
            Tk::Identifier("a".into(), 1),
            Tk::Comma,
            Tk::Identifier("b".into(), 1),
            Tk::Rpar,
            Tk::LBrace,
            Tk::Return(2),
            Tk::Identifier("a".into(), 2),
            Tk::Sub,
            Tk::Identifier("b".into(), 2),
            Tk::Semi,
            Tk::RBrace,
            Tk::Identifier("sayHi".into(), 4),
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
        let mut semantic_pass = Resolver::new(&mut inter);
        for stt in statements.clone() {
            println!(">>{:?}", stt);
            semantic_pass.resolve(stt);
        }
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
            Tk::Identifier("sayHi".into(), 1),
            Tk::Lpar,
            Tk::Identifier("a".into(), 1),
            Tk::Comma,
            Tk::Identifier("b".into(), 1),
            Tk::Rpar,
            Tk::LBrace,
            Tk::Print,
            Tk::Identifier("a".into(), 2),
            Tk::Sub,
            Tk::Identifier("b".into(), 2),
            Tk::Semi,
            Tk::RBrace,
            Tk::Identifier("sayHi".into(), 4),
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
        let mut semantic_pass = Resolver::new(&mut inter);
        for stt in statements.clone() {
            println!(">>{:?}", stt);
            semantic_pass.resolve(stt);
        }
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
            Tk::Identifier("b".into(), 3),
            Tk::Assign,
            Tk::Num(1),
            Tk::Semi,
            Tk::Num(-10_000),
            Tk::LT,
            Tk::Identifier("a".into(), 3),
            Tk::Semi,
            Tk::Identifier("b".into(), 3),
            Tk::Assign,
            Tk::Identifier("temp".into(), 3),
            Tk::Sub,
            Tk::Identifier("b".into(), 3),
            Tk::Rpar,
            Tk::LBrace,
            Tk::Print,
            Tk::Identifier("a".into(), 4),
            Tk::Semi,
            Tk::Identifier("temp".into(), 5),
            Tk::Assign,
            Tk::Identifier("a".into(), 5),
            Tk::Semi,
            Tk::Identifier("a".into(), 6),
            Tk::Assign,
            Tk::Identifier("b".into(), 6),
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
        let mut semantic_pass = Resolver::new(&mut inter);
        for stt in statements.clone() {
            println!(">>{:?}", stt);
            semantic_pass.resolve(stt);
        }

        // println!("{:?}", inter);
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
            Tk::Identifier("i".into(), 1),
            Tk::Rpar,
            Tk::LBrace,
            Tk::Print,
            Tk::Identifier("i".into(), 2),
            Tk::Semi,
            Tk::Identifier("i".into(), 3),
            Tk::Assign,
            Tk::Identifier("i".into(), 3),
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
    //? in modern javascript with let it is a runtime error❗:
    //? ReferenceError: Cannot access 'a' before initialization
    //? at <anonymous>:3:17
    //* let a = 3;
    //* {
    //*     let a = a - 1;
    //*     print a;
    //* }
    //? in Rust it is a valid code❗, it yields: 2
    //todo: do it as rust❓
    //? this is a compile time error❗
    #[test]
    #[should_panic = "Identifier(\"a\", 3) Can't read local variable in its own initializer."]
    fn test_block_variable_declaration() {
        let tokens = vec![
            Tk::LBrace,
            Tk::Var,
            Tk::Identifier("a".into(), 3),
            Tk::Assign,
            Tk::Identifier("a".into(), 3),
            Tk::Sub,
            Tk::Num(1),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("a".into(), 4),
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
        ];
        let (statements, environment) = test_setup(tokens, vec![("a".to_string(), V::I32(3))]);
        let _inter = test_run(environment, statements);
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
            //* block
            Tk::LBrace,
            Tk::Var,
            Tk::Identifier("a".into(), 5),
            Tk::Assign,
            Tk::Num(100),
            Tk::Semi,
            Tk::Var,
            Tk::Identifier("b".into(), 6),
            Tk::Assign,
            Tk::Num(200),
            Tk::Semi,
            Tk::LBrace,
            Tk::Var,
            Tk::Identifier("a".into(), 8),
            Tk::Assign,
            Tk::Num(10),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("a".into(), 9),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into(), 10),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into(), 11),
            Tk::Semi,
            Tk::RBrace,
            Tk::Print,
            Tk::Identifier("a".into(), 13),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into(), 14),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into(), 15),
            Tk::Semi,
            Tk::RBrace,
            Tk::Print,
            Tk::Identifier("a".into(), 17),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into(), 18),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into(), 19),
            Tk::Semi,
            Tk::End,
        ];
        let (statements, environment) = test_setup(
            tokens,
            vec![
                ("a".to_string(), V::I32(1)),
                ("b".to_string(), V::I32(2)),
                ("c".to_string(), V::I32(3)),
            ],
        );
        let inter = test_run(environment, statements);

        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(3)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(2)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(1)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(3)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(200)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(100)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(3)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(200)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(10)));
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
            //*     let a = 100;
            Tk::Var,
            Tk::Identifier("a".into(), 5),
            Tk::Assign,
            Tk::Num(100),
            Tk::Semi,
            //*     let b = 200;
            Tk::Var,
            Tk::Identifier("b".into(), 6),
            Tk::Assign,
            Tk::Num(200),
            Tk::Semi,
            //*     { block
            Tk::LBrace,
            //*         let a = 10;
            Tk::Var,
            Tk::Identifier("a".into(), 8),
            Tk::Assign,
            Tk::Num(10),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("a".into(), 9),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into(), 10),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into(), 11),
            Tk::Semi,
            Tk::RBrace,
            //*     } block
            Tk::Print,
            Tk::Identifier("a".into(), 13),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into(), 14),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into(), 15),
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
        ];
        let (statements, environment) = test_setup(
            tokens,
            vec![
                ("a".to_string(), V::I32(1)),
                ("b".to_string(), V::I32(2)),
                ("c".to_string(), V::I32(3)),
            ],
        );
        let inter = test_run(environment, statements);

        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(3)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(200)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(100)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(3)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(200)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(10)));
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
            //* {
            Tk::LBrace,
            //*     let a = 10;
            Tk::Var,
            Tk::Identifier("a".into(), 5),
            Tk::Assign,
            Tk::Num(10),
            Tk::Semi,
            //*     print a;
            Tk::Print,
            Tk::Identifier("a".into(), 6),
            Tk::Semi,
            //*     print b;
            Tk::Print,
            Tk::Identifier("b".into(), 7),
            Tk::Semi,
            //*     print c;
            Tk::Print,
            Tk::Identifier("c".into(), 8),
            Tk::Semi,
            Tk::RBrace,
            Tk::End,
        ];
        let (statements, environment) = test_setup(
            tokens,
            vec![
                ("a".to_string(), V::I32(1)),
                ("b".to_string(), V::I32(2)),
                ("c".to_string(), V::I32(3)),
            ],
        );
        let inter = test_run(environment, statements);

        let mut iter = inter.results.iter();
        //todo: look for ordering in reverse ti match the logical output
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(3)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(2)));
        assert_eq!(iter.next_back().unwrap(), &("print".into(), V::I32(10)));
    }

    fn test_setup(tokens: Vec<Tk>, globals: Vec<(String, V)>) -> (Vec<Statement>, Env) {
        let mut parser = Parser::new(tokens);
        let statements = parser.parse();
        let mut environment = Env::new(EnvKind::Global);
        for (key, val) in globals {
            environment.define(key, val);
        }
        (statements, environment)
    }

    fn test_run(environment: Env, statements: Vec<Statement>) -> Interpreter {
        let mut inter = Interpreter::new(environment);
        //? Stop if there was a syntax error.
        /*
         * We don’t run the resolver if there are any parse errors.
         * If the code has a syntax error, it’s never going to run,
         * so there’s little value in resolving it.
         * If the syntax is clean, we tell the resolver to do its thing.
         * The resolver has a reference to the interpreter and pokes the
         * resolution data directly into it as it walks over variables.
         * When the interpreter runs next, it has everything it needs.
         */
        //? if (hadError) return;
        let mut semantic_pass = Resolver::new(&mut inter);
        for stt in statements.clone() {
            // println!(">>{:?}", stt);
            semantic_pass.resolve(stt);
        }
        println!("\nSTATE: {:?}\n", inter.locals);
        /*
         * we are careful to not run the interpreter if any parse errors
         * are encountered.
         * That check runs before the resolver so that we don’t try to
         * resolve syntactically invalid code.
         *
         * But we also need to skip the interpreter if
         * there are resolution errors, so we add another check.
         */
        // Stop if there was a resolution error.
        // if (hadError) return;
        inter.eval(statements);

        println!("\n{:?}", inter.current_fn_call);
        println!("\n{:?}", inter.functions_map);
        inter
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
            Tk::Identifier("a".into(), 2),
            Tk::Assign,
            Tk::Num(10),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("a".into(), 3),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("b".into(), 4),
            Tk::Semi,
            Tk::Print,
            Tk::Identifier("c".into(), 5),
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
                    name: Tk::Identifier("a".into(), 2),
                    initializer: Expr::Literal(V::I32(10))
                },
                Statement::Print(Expr::Let(Tk::Identifier("a".into(), 3))),
                Statement::Print(Expr::Let(Tk::Identifier("b".into(), 4))),
                Statement::Print(Expr::Let(Tk::Identifier("c".into(), 5)))
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
            Tk::Identifier("a".into(), 1),
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
            Tk::Identifier("a".into(), 1),
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
            Tk::Identifier("a".into(), 1),
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
                Tk::Identifier("a".into(), 1),
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
            Tk::Identifier("a".into(), 3),
            Tk::Sub,
            Tk::Identifier("b".into(), 3),
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
            Tk::Identifier("jamon".into(), 1),
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
                name: Tk::Identifier("jamon".into(), 1),
                initializer: Expr::Literal(V::I32(100))
            }
        );

        let tokens = vec![
            Tk::Var,
            Tk::Identifier("jamon".into(), 1),
            Tk::Assign,
            Tk::Identifier("lol".into(), 1),
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
                name: Tk::Identifier("jamon".into(), 1),
                initializer: Expr::Let(Tk::Identifier("lol".into(), 1))
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
            Tk::Float("45.65".to_string()),
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
