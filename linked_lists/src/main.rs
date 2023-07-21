use crate::{linked_list::List, ll01_simple::Scope};
mod iterable;
mod linked_list;
mod ll01_simple;

fn main() {
    let list: Scope<i32> = Scope::Local(1, Box::new(Scope::Local(2, Box::new(Scope::Nil))));
    println!("{:?}", list);

    let mut list = List::new();
    list.push(1);
    list.push(2);
    list.push(3);

    let mut iter = list.iter();
    let x = iter.next().unwrap();
    let y = iter.next().unwrap();
    let z = iter.next().unwrap();
    println!("{:?}, {:?}, {:?}", x, y, z);
}
