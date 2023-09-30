use std::{cell::RefCell, collections::HashMap, rc::Rc};

fn main() {
    // * Rc allows sharing, but not mutability...❗
    use std::rc::Weak;

    struct Dog {
        name: String,
        owner: Weak<Human>,
    }
    struct Human {
        name: String,
        pet_dogs: Vec<Dog>,
    }

    let sam = Rc::new(Human {
        name: "Sam".to_string(),
        pet_dogs: Vec::new(),
    });
    let rover = Dog {
        name: "Rover".to_string(),
        owner: Rc::downgrade(&sam),
    };
    //? ❌ sam.pet_dogs.push(rover);
    //? This is not allowed, because `sam` is actually immutable

    let db = Rc::new(HashMap::from([
        ("jamon".to_string(), 1_000),
        ("jamon-2".to_string(), 2_000),
        ("jamon-3".to_string(), 3_000),
    ]));

    println!("{db:?}");

    //? ============== MUTATING RC
    #[derive(Debug)]
    struct Dog2 {
        name: String,
        owner: Weak<RefCell<Human2>>,
    }
    #[derive(Debug)]
    struct Human2 {
        name: String,
        pet_dogs: Vec<Dog2>,
    }

    let sam = Rc::new(RefCell::new(Human2 {
        name: "Sam".to_string(),
        pet_dogs: Vec::new(),
    }));
    let rover = Dog2 {
        name: "Rover".to_string(),
        owner: Rc::downgrade(&sam),
    };
    //? This is now allowed because `RefCell::borrow_mut`
    //? does a run-time borrow check
    sam.borrow_mut().pet_dogs.push(rover);
    println!("MUTABLE RC {sam:?}");

    let db = Rc::new(RefCell::new(HashMap::from([
        ("jamon".to_string(), 1_000),
        ("jamon-2".to_string(), 2_000),
        ("jamon-3".to_string(), 3_000),
    ])));

    db.borrow_mut().insert("hola".to_string(), 22);
    println!("MUT RC {db:?}");
}
