use std::rc::Rc;

/*
 * Rc’s count ensures that its contained value is valid for as
 * long as there are references.
 *
 * Rc in Rust is like std::shared_ptr in C++.
 *
 * Rc::clone is cheap: it creates a pointer to the same allocation
 * and increases the reference count. Does not make a deep clone and can generally be ignored when looking for performance issues in code.
 *
 * make_mut actually clones the inner value
 * if necessary (“clone-on-write”) and returns a mutable reference.
 *
 * Use Rc::strong_count to check the reference count.
 *
 * Rc::downgrade gives you a weakly reference-counted object
 * to create cycles that will be dropped properly
 * (likely in combination with RefCell, on the next slide).
 */
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let a = Rc::new(10);
    let b = Rc::clone(&a);

    println!("a: {a}");
    println!("b: {b}");

    let first_handle = Rc::new(Point { x: 1, y: 1 });
    // * When you do a clone() the count goes up by one
    // * When you drop it, the count goes down by one
    // * The memory isn't freed until the count hits zero
    let second_handle = first_handle.clone();
    let third_handle = second_handle.clone();
    // * There's a Weak version which will not keep the allocation alive - to break cycles
    /*
     * A Cycle
     *
     *  A cycle would be if you managed to construct two Rc
     * wrapped structs and had each one hold an Rc reference
     * to the other. Now neither can ever be freed, because
     * each will always have at least one owner (the other).
     */
}
