struct Square {
    width: f32,
}

fn make_stuff(want_integer: bool) -> Box<dyn std::fmt::Debug> {
    if want_integer {
        Box::new(42_i32)
    } else {
        Box::new("Hello".to_string())
    }
}

fn main() {
    let x: u64 = 0;
    let y = Square { width: 1.0 };
    let mut z: String = "Hello".to_string();
    z.push_str(", world!");
    println!("x @ {:p}", &x);
    println!("y @ {:p}", &y);
    println!("z @ {:p}", &z);
    println!("z @ {:p}", z.as_str());

    //? On macOS, you can run vmmap <pid> to print
    //? the addresses for each region. On Linux you
    //? can use pmap <pid>, or you could add something like:
    if let Ok(maps) = std::fs::read_to_string(format!("/proc/{}/maps", std::process::id())) {
        println!("{}", maps);
    }

    let x: Box<f64> = Box::new(1.0_f64);
    let y: f64 = x.sin() * 2.0;
    let z: &f64 = &x;
    println!("x={x}, y={y}, z={z}");
    /*
     *
     * When should I use a Box?
     *
     * Not very often - friendlier containers (like Vec<T>) exist
     *
     * If you have a large value that moves around a lot
     * Moving a Box<T> is cheap, because only the pointer moves,
     * not the contents
     *
     * To hide the size or type of a returned value...
     */

    //? hiding the size❗
    //? An i32 and a String are very different sizes, and a
    //? function must have a single fixed size for the return value.
    //? But it does - it returns a Box and the Box itself always
    //? has the same size. The thing that varies in size is the
    //? value inside the box and that lives somewhere else - on
    //? the heap in fact.
    println!("make_stuff(true): {:?}", make_stuff(true));
    println!("make_stuff(false): {:?}", make_stuff(false));
}
