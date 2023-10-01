fn main() {
    enum L {
        Cons(i32, Box<L>),
        Nil,
    }
    let a = L::Cons(5, Box::new(L::Cons(10, Box::new(L::Nil))));
    let b = L::Cons(3, Box::new(a));
    //? cannot move again❗
    // let c = L::Cons(4, Box::new(a));

    // * with rc
    use std::rc::Rc;
    #[derive(Debug)]
    enum LRc {
        Cons(i32, Rc<LRc>),
        Nil,
    }
    let a = Rc::new(LRc::Cons(5, Rc::new(LRc::Cons(10, Rc::new(LRc::Nil)))));
    println!("count after creating a = {}", Rc::strong_count(&a));
    //? We could have called a.clone() rather than Rc::clone(&a),
    //? but Rust’s convention is to use Rc::clone in this case.
    let b = LRc::Cons(3, a.clone());
    println!("count after creating b = {}", Rc::strong_count(&a));
    //? The call to Rc::clone only increments the reference count,
    //? which doesn’t take much time. Deep copies of data can take
    //? a lot of time.
    {
        let c = LRc::Cons(4, Rc::clone(&a));
        println!("count after creating c = {}", Rc::strong_count(&a));
        println!("c {c:?}");
    }
    println!("count after dropping c = {}", Rc::strong_count(&a));
    println!("b {b:?}");
}
