use crate::{linked_list::List, ll01_simple::Scope};
mod linked_list;
mod ll01_simple;

fn main() {
    let list: Scope<i32> = Scope::Local(1, Box::new(Scope::Local(2, Box::new(Scope::Nil))));
    println!("{:?}", list);
    let mut list = List::new();

    // Check empty list behaves right
    assert_eq!(list.pop(), None);

    // Populate list
    list.push(1);
    list.push(2);
    list.push(3);

    // Check normal removal
    assert_eq!(list.pop(), Some(3));
    assert_eq!(list.pop(), Some(2));

    // Push some more just to make sure nothing's corrupted
    list.push(4);
    list.push(5);

    // Check normal removal
    assert_eq!(list.pop(), Some(5));
    assert_eq!(list.pop(), Some(4));

    // Check exhaustion
    assert_eq!(list.pop(), Some(1));
    assert_eq!(list.pop(), None);
}
