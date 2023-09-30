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
}
