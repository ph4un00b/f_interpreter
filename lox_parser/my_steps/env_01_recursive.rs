use crate::{Tk, RE, V};
use std::collections::HashMap;
use std::result::Result as StdResult;

#[allow(dead_code)]
#[derive(Clone)]
pub struct Env {
    enclosing: Option<Box<Env>>,
    values: HashMap<String, V>,
}

#[allow(dead_code)]
impl Env {
    pub fn new(enclosing: Option<Box<Env>>) -> Self {
        Self {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, val: V) {
        /*
         * @see https://craftinginterpreters.com/statements-and-state.html#environments
         * Not exactly brain surgery, but we have made one
         * interesting semantic choice. When we add the key
         * to the map, we don’t check to see if it’s already
         * present. That means that this program works:
         *
         * var a = "before";
         * print a; // "before".
         * var a = "after";
         * print a; // "after".
         *
         * A variable statement doesn’t just define a new variable,
         *  it can also be used to redefine an existing variable.
         * We could choose to make this an error instead.
         * The user may not intend to redefine an existing
         * variable. (If they did mean to, they probably
         * would have used assignment, not var.)
         * Making redefinition an error would help them find that bug.
         * However, doing so interacts poorly with the REPL.
         * In the middle of a REPL session, it’s nice to not have
         * to mentally track which variables you’ve already defined.
         * We could allow redefinition in the REPL but not in scripts,
         * but then users would have to learn two sets of rules,
         * and code copied and pasted from one form to the other might not work.
         *
         * My rule about variables and scoping is, “When in doubt, do what Scheme does”.
         * Scheme allows redefining variables at the top level.
         */
        self.values.insert(name, val);
    }

    pub fn get(&self, name: Tk) -> StdResult<V, RE> {
        /*
         * This is a little more semantically interesting.
         * If the variable is found, it simply returns the value bound to it.
         * But what if it’s not? Again, we have a choice:
         *
         * Make it a syntax error.
         * ✅ Make it a runtime error.
         * Allow it and return some default value like nil.
         */
        let identifier = String::from(name.clone());
        if self.values.contains_key(&identifier) {
            let value = self.values.get(&identifier).unwrap();
            Ok(value.clone())
        } else if self.enclosing.is_some() {
            /*
             * If the variable isn’t found in this environment,
             * we simply try the enclosing one. That in turn does
             * the same thing recursively, so this will ultimately
             * walk the entire chain. If we reach an environment
             * with no enclosing one and still don’t find the variable,
             * then we give up and report an error as before.
             */
            let env = self.enclosing.as_ref().unwrap();
            let value = env.get(name)?;
            Ok(value)
        } else {
            Err(RE::UndefinedVariable(identifier))
        }
    }

    pub fn assign(&mut self, name: Tk, value: V) -> StdResult<V, RE> {
        /*
         * The key difference between assignment and definition is that
         * assignment is not allowed to create a new variable. In terms
         * of our implementation, that means it’s a runtime error if the
         * key doesn’t already exist in the environment’s variable map.
         */
        //? Unlike Python and Ruby, Lox doesn’t do❗
        let identifier = String::from(name.clone());
        if self.values.contains_key(&identifier) {
            //? aquí clippy me obligo a cambiar el insert!
            //? @see https://rust-lang.github.io/rust-clippy/master/index.html#/map_entry
            self.values
                .entry(identifier)
                .and_modify(|val| *val = value.clone());
            Ok(value)
        } else if self.enclosing.is_some() {
            /*
             * It’s likely faster to iteratively walk the chain,
             * but I think the recursive solution is prettier.
             * We’ll do something much faster in clox.
             */
            let env = self.enclosing.as_mut().unwrap();
            env.assign(name, value)
        } else {
            Err(RE::UndefinedVariable(identifier))
        }
    }
}
