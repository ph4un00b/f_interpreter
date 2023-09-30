use std::cell::UnsafeCell;

fn main() {
    let x: UnsafeCell<i32> = UnsafeCell::new(42);
    let (alias1, alias2) = (&x, &x);

    //? The UnsafeCell::get(&self) -> *mut T method is safe,
    //? but deferencing the pointer
    //? (or converting it to a &mut reference) is unsafe
    //? because a human must verify there is no aliasing.
    let p1_exclusive: &mut i32 = unsafe { &mut *alias1.get() };
    *p1_exclusive += 27;

    drop(p1_exclusive);
    let p2_shared: &i32 = unsafe { &*alias2.get() };
    assert_eq!(*p2_shared, 42 + 27);
    let p1_shared: &i32 = unsafe { &*alias1.get() };
    assert_eq!(*p1_shared, *p2_shared);

    // * refcell
    let x: std::cell::RefCell<i32> = std::cell::RefCell::new(42);
    let (alias1, alias2) = (&x, &x);

    let mut p1_exclusive = alias1.borrow_mut();
    *p1_exclusive += 27;
    drop(p1_exclusive);

    let p2_shared = alias2.borrow();
    assert_eq!(*p2_shared, 42 + 27);
    //? This isn't allowed here:
    // let p2_mutable = alias2.borrow_mut();
    let p1_shared = alias1.borrow();
    assert_eq!(*p1_shared, *p2_shared);
}
