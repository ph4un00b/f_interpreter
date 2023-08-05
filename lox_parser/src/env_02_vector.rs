use crate::{Tk, V};
use std::collections::HashMap;

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    pub kind: EnvKind,
    values: HashMap<String, V>,
}

impl std::fmt::Display for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Env {:?},", self.kind)?;
        for (key, val) in self.values.iter() {
            write!(f, "  ({key}, {val})")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum EnvKind {
    Prelude,
    Global,
    Block,
    Call(Tk),
    Closure(Tk),
}

#[allow(dead_code)]
impl Env {
    pub fn new(kind: EnvKind) -> Self {
        Self {
            kind,
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

    pub fn fetch(&self, name: Tk) -> Option<V> {
        let identifier = String::from(name.clone());
        if self.values.contains_key(&identifier) {
            let value = self.values.get(&identifier).unwrap();
            Some(value.clone())
        } else {
            None
        }
    }

    pub fn get(&self, name: Tk) -> Option<V> {
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
            Some(value.clone())
        } else {
            None
        }
    }

    pub fn assign(&mut self, name: Tk, value: V) -> Option<V> {
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
            return Some(value);
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut scope = Env::new(EnvKind::Global);
        scope.define("a".into(), V::I32(1));
        scope.define("b".into(), V::I32(10));
        scope.define("c".into(), V::I32(100));

        let a = scope.get(Tk::Identifier("a".into(), 1));
        let b = scope.get(Tk::Identifier("b".into(), 1));
        let c = scope.get(Tk::Identifier("c".into(), 1));
        assert_eq!(a, Some(V::I32(1)));
        assert_eq!(b, Some(V::I32(10)));
        assert_eq!(c, Some(V::I32(100)));
    }

    #[test]
    fn it_errors() {
        let mut scope = Env::new(EnvKind::Global);
        scope.define("a".into(), V::I32(1));
        scope.define("b".into(), V::I32(10));
        scope.define("c".into(), V::I32(100));
        let err = scope.get(Tk::Identifier("non-existen".into(), 0));
        assert_eq!(err, None);
    }

    #[test]
    fn it_assigns() {
        let mut scope = Env::new(EnvKind::Global);
        scope.define("a".into(), V::I32(1));
        let a = scope.assign(Tk::Identifier("a".into(), 1), V::I32(22));
        assert_eq!(a, Some(V::I32(22)));
    }

    #[test]
    fn it_errors_assignment() {
        let mut scope = Env::new(EnvKind::Global);
        scope.define("a".into(), V::I32(1));
        let a = scope.assign(Tk::Identifier("b".into(), 1), V::I32(22));
        assert_eq!(a, None);
    }
}
