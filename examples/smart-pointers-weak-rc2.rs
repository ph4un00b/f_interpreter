use std::cell::RefCell;
use std::rc::{Rc, Weak};

#[derive(Debug)]
struct Node {
    value: i32,
    children: RefCell<Vec<Rc<Node>>>,
}

fn main() {
    let leaf = Rc::new(Node {
        value: 3,
        children: RefCell::new(vec![]),
    });

    let branch = Rc::new(Node {
        value: 5,
        children: RefCell::new(vec![Rc::clone(&leaf)]),
    });

    println!("{branch:?}");

    /*
     * Thinking about the relationships another way,
     * a parent node should own its children:
     *
     * if a parent node is dropped, its child nodes should be
     * dropped as well. However, a child should not own its parent:
     *
     * if we drop a child node, the parent should still exist.
     * This is a case for weak references!
     */
    #[derive(Debug)]
    struct NodeWithParent {
        value: i32,
        //? A node will be able to refer to its parent node
        //? but doesn’t own its parent.❗
        parent: RefCell<Weak<NodeWithParent>>,
        children: RefCell<Vec<Rc<NodeWithParent>>>,
    }

    let leaf = Rc::new(NodeWithParent {
        value: 3,
        parent: RefCell::new(Weak::new()),
        children: RefCell::new(vec![]),
    });

    println!("leaf parent = {:?}", leaf.parent.borrow().upgrade());
    println!(
        "leaf strong = {}, weak = {}",
        Rc::strong_count(&leaf),
        Rc::weak_count(&leaf),
    );

    {
        let branch = Rc::new(NodeWithParent {
            value: 5,
            parent: RefCell::new(Weak::new()),
            children: RefCell::new(vec![Rc::clone(&leaf)]),
        });

        *leaf.parent.borrow_mut() = Rc::downgrade(&branch);

        println!(
            "branch strong = {}, weak = {}",
            Rc::strong_count(&branch),
            Rc::weak_count(&branch),
        );

        println!(
            "leaf strong = {}, weak = {}",
            Rc::strong_count(&leaf),
            Rc::weak_count(&leaf),
        );
    }

    //? we also avoid the cycle that eventually ended in a
    //? stack overflow like we had
    //? the Weak<Node> references are printed as (Weak)
    println!("leaf parent = {:?}", leaf.parent.borrow().upgrade());
    println!(
        "leaf strong = {}, weak = {}",
        Rc::strong_count(&leaf),
        Rc::weak_count(&leaf),
    );
    // * https://doc.rust-lang.org/nomicon/index.html#the-dark-arts-of-unsafe-rust
}
