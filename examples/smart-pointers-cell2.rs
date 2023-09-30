// * @see https://github.com/ferrous-systems/rust-training/blob/main/training-slides/src/shared-mutability.md

fn main() {
    #[derive(Debug, Default)]
    struct Post {
        content: String,
        viewed_times: u64,
    }

    impl Post {
        //? `mut` is a problem here!
        fn view(&mut self) -> &str {
            self.viewed_times += 1;
            &self.content
        }
    }

    let _post = Post {
        content: String::from("Blah"),
        ..Post::default()
    };
    //? This line is a compile error! if there is no mut❗
    // println!("{}", post.view());
    //? We need to make the entire struct mutable!
    //? hence public API is mutable❗
    let mut post = Post {
        content: String::from("Blah"),
        ..Post::default()
    };
    println!("{}", post.view());
    //? Now this is allowed too...
    post.content.push_str(" - extra content");
    println!("{}", post.view());

    // * with cell

    #[derive(Debug, Default)]
    struct PostCell {
        content: String,
        viewed_times: std::cell::Cell<u64>,
    }

    impl PostCell {
        fn view(&self) -> &str {
            //? Note how we are making a copy, then replacing the original.
            let current_views = self.viewed_times.get();
            self.viewed_times.set(current_views + 1);
            &self.content
        }
    }
    //? the public API is inmutable❗
    let post = PostCell {
        content: String::from("Blah"),
        ..PostCell::default()
    };
    println!("with cell: {}", post.view());

    // * with ref-cell
    /*
     * tradeoffs:
     *
     * Moving the borrow checking to run-time:
     *
     * Might make your program actually compile ✅
     *
     * Might cause your program to panic ❌
     *
     * interior mutability is something of a last resort
     * @see https://doc.rust-lang.org/std/cell/index.html#when-to-choose-interior-mutability
     */

    #[derive(Debug, Default)]
    struct PostRefCell {
        content: String,
        viewed_times: std::cell::RefCell<u64>,
    }

    impl PostRefCell {
        fn view(&self) -> &str {
            let mut view_count_ref = self.viewed_times.borrow_mut();
            *view_count_ref += 1;
            &self.content
        }
    }
    //? the public API is inmutable❗
    let post = PostRefCell {
        content: String::from("Blah"),
        ..PostRefCell::default()
    };
    println!("with ref-cell: {}", post.view());
}
