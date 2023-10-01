#[cfg(test)]
mod tests {
    pub trait MutableMsg {
        fn send(&mut self, msg: &str);
    }

    pub struct TrackerWithMutable<'a, TMessenger: MutableMsg> {
        messenger: &'a mut TMessenger,
        value: usize,
        max: usize,
    }

    impl<'a, TMessenger> TrackerWithMutable<'a, TMessenger>
    where
        TMessenger: MutableMsg,
    {
        pub fn new(
            messenger: &'a mut TMessenger,
            max: usize,
        ) -> TrackerWithMutable<'a, TMessenger> {
            TrackerWithMutable {
                messenger,
                value: 0,
                max,
            }
        }

        pub fn set_value(&mut self, value: usize) {
            self.value = value;

            let percentage_of_max = self.value as f64 / self.max as f64;

            if percentage_of_max >= 1.0 {
                self.messenger.send("Error: You are over your quota!");
                println!("Error: You are over your quota!");
            } else if percentage_of_max >= 0.9 {
                self.messenger
                    .send("Urgent warning: You've used up over 90% of your quota!");
                println!("Urgent warning: You've used up over 90% of your quota!");
            } else if percentage_of_max >= 0.75 {
                self.messenger
                    .send("Warning: You've used up over 75% of your quota!");
                println!("Warning: You've used up over 75% of your quota!");
            }
        }
    }

    struct MutableMock {
        sent_messages: Vec<String>,
    }

    impl MutableMock {
        fn new() -> MutableMock {
            MutableMock {
                sent_messages: vec![],
            }
        }
    }

    impl MutableMsg for MutableMock {
        //? este API te fuerza a propagar la mutabilidad
        fn send(&mut self, message: &str) {
            self.sent_messages.push(String::from(message));
        }
    }

    #[test]
    fn it_sends_an_over_75_percent_warning_message() {
        //? este API te fuerza a propagar la mutabilidad
        let mut mock_messenger = MutableMock::new();
        let mut limit_tracker = TrackerWithMutable::new(&mut mock_messenger, 100);

        limit_tracker.set_value(80);

        assert_eq!(mock_messenger.sent_messages.len(), 1);
    }

    // * ahora con refcell para hacer la API inmutable al consumidor❗
    use std::cell::RefCell;

    pub trait InmutableMessenger {
        fn send(&self, msg: &str);
    }

    pub struct Tracker<'a, TMessenger: InmutableMessenger> {
        messenger: &'a TMessenger,
        value: usize,
        max: usize,
    }

    impl<'a, TMessenger> Tracker<'a, TMessenger>
    where
        TMessenger: InmutableMessenger,
    {
        pub fn new(messenger: &'a TMessenger, max: usize) -> Tracker<'a, TMessenger> {
            Tracker {
                messenger,
                value: 0,
                max,
            }
        }

        pub fn set_value(&mut self, value: usize) {
            self.value = value;

            let percentage_of_max = self.value as f64 / self.max as f64;

            if percentage_of_max >= 1.0 {
                self.messenger.send("Error: You are over your quota!");
                println!("Error: You are over your quota!");
            } else if percentage_of_max >= 0.9 {
                self.messenger
                    .send("Urgent warning: You've used up over 90% of your quota!");
                println!("Urgent warning: You've used up over 90% of your quota!");
            } else if percentage_of_max >= 0.75 {
                self.messenger
                    .send("Warning: You've used up over 75% of your quota!");
                println!("Warning: You've used up over 75% of your quota!");
            }
        }
    }
    struct ImmutableMock {
        sent_messages: RefCell<Vec<String>>,
    }

    impl ImmutableMock {
        fn new() -> ImmutableMock {
            ImmutableMock {
                sent_messages: RefCell::new(vec![]),
            }
        }
    }

    impl InmutableMessenger for ImmutableMock {
        //? api inmutable❗
        fn send(&self, message: &str) {
            self.sent_messages.borrow_mut().push(String::from(message));
        }
    }

    #[test]
    fn it_sends_an_over_75_percent_warning_message_with_refcell() {
        //? mock inmutable❗
        let mock_messenger = ImmutableMock::new();
        let mut limit_tracker = Tracker::new(&mock_messenger, 100);

        limit_tracker.set_value(80);

        assert_eq!(mock_messenger.sent_messages.borrow().len(), 1);
    }

    struct ImmutableMockPanic {
        sent_messages: RefCell<Vec<String>>,
    }

    impl ImmutableMockPanic {
        fn new() -> ImmutableMockPanic {
            ImmutableMockPanic {
                sent_messages: RefCell::new(vec![]),
            }
        }
    }
    impl InmutableMessenger for ImmutableMockPanic {
        // * this wil compile
        // * this will panic at runtime❗
        /*
         * We create a variable one_borrow for the RefMut<T>
         * smart pointer returned from borrow_mut.
         * Then we create another mutable borrow in the same way
         * in the variable two_borrow.
         * This makes two mutable references in the
         * same scope, which isn’t allowed.
         */
        fn send(&self, message: &str) {
            let mut one_borrow = self.sent_messages.borrow_mut();
            let mut two_borrow = self.sent_messages.borrow_mut();

            one_borrow.push(String::from(message));
            two_borrow.push(String::from(message));
        }
    }
}
