#![feature(linked_list_cursors)]
use std::collections::{HashMap, LinkedList};

use crate::ll01_simple::Scope;
mod iterable;
mod linked_list;
mod ll01_simple;

fn main() {
    let list: Scope<i32> = Scope::Local(1, Box::new(Scope::Local(2, Box::new(Scope::Nil))));
    println!("{:?}", list);

    //? BUILT-IN LINKED-LIST WITH CURSORS
    let mut global_env = LinkedList::new();
    global_env.push_front(HashMap::from([("global_counter".to_string(), 10)]));

    if let Some(node) = global_env.front_mut() {
        node.insert("mutado".into(), 1);
    }

    let mut counter = 10;
    while 0 < counter {
        global_env.push_front(HashMap::from([("temporal".to_string(), 0)]));
        if let Some(prev) = global_env.cursor_front_mut().peek_next() {
            prev.entry("global_counter".into())
                .and_modify(|node| *node -= 1);
        }
        counter -= 1;
        global_env.pop_front();
    }

    println!("{:?}", global_env);

    //? BUILT-IN LINKED-LIST WITH ITER-MUT && (DoubleEndedIterator)
    let mut global_env = LinkedList::new();
    global_env.push_front(HashMap::from([("global_counter_iter_mut".to_string(), 10)]));

    if let Some(node) = global_env.front_mut() {
        node.insert("mutado".into(), 1);
    }

    let mut counter = 10;
    while 0 < counter {
        global_env.push_front(HashMap::from([("temporal".to_string(), 0)]));
        let mut iter = global_env.iter_mut();
        if let Some(prev) = iter.next_back() {
            prev.entry("global_counter_iter_mut".into())
                .and_modify(|node| *node -= 1);
        }
        counter -= 1;
        global_env.pop_front();
    }

    println!("{:?}", global_env);

    //? BUILT-IN vec!
    let mut global_env = vec![HashMap::from([("global_counter_vec".to_string(), 10)])];

    if let Some(node) = global_env.last_mut() {
        node.insert("mutado".into(), 1);
    }

    let mut counter = 10;
    while 0 < counter {
        global_env.push(HashMap::from([("temporal".to_string(), 0)]));
        let mut iter = global_env.iter_mut();
        if let Some(prev) = iter.next() {
            prev.entry("global_counter_vec".into())
                .and_modify(|node| *node -= 1);
        }
        counter -= 1;
        global_env.pop();
    }

    println!("{:?}", global_env);
}
