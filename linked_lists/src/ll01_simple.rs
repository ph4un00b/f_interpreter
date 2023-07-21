#[derive(Debug)]
pub enum Scope<TData> {
    // * @see https://doc.rust-lang.org/std/boxed/
    /*
     * Consider a list with two elements:
     * [] = Stack
     * () = Heap
     *
     * ❗ [Elem A, ptr] -> (Elem B, ptr) -> (Nil, *junk*)
     * There are two key issues:
     *
     * 👀 We're allocating a node that just says "I'm not actually a Node"
     * 👀 One of our nodes isn't heap-allocated at all.
     *
     * On the surface, these two seem to cancel each-other out.
     * We heap-allocate an extra node, but one of our nodes doesn't
     * need to be heap-allocated at all.
     * However, consider the following potential layout for our list:
     *
     * ❗ [ptr] -> (Elem A, ptr) -> (Elem B, *null*)
     *
     * In this layout we now unconditionally heap allocate our nodes.
     * The key difference is the absence of the junk from our first layout.
     * What is this junk? To understand that,
     * we'll need to look at how an enum is laid out in memory.
     */
    Local(TData, Box<Scope<TData>>),
    Nil,
}
//? The big takeaway here is that even though Nil is a single bit of information,
//? it necessarily consumes enough space for a pointer and an element,
//? because it has to be ready to become an Elem at any time.
//? Therefore the first layout heap allocates an extra element that's
//? just full of junk, consuming a bit more space than the second layout.
/*
* One of our nodes not being allocated at all is also, perhaps surprisingly,
* worse than always allocating it.
* This is because it gives us a non-uniform node layout.❗
* This doesn't have much of an appreciable effect on pushing and popping nodes,
* but it does have an effect on splitting and merging lists 👀.
*
* layout 1:
*
* [Elem A, ptr] -> (Elem B, ptr) -> (Elem C, ptr) -> (Nil *junk*)
* split off C:
*
* [Elem A, ptr] -> (Elem B, ptr) -> (Nil *junk*)
* [Elem C, ptr] -> (Nil *junk*)
*
* layout 2:
*
* [ptr] -> (Elem A, ptr) -> (Elem B, ptr) -> (Elem C, *null*)
* split off C:
*
* [ptr] -> (Elem A, ptr) -> (Elem B, *null*)
* [ptr] -> (Elem C, *null*)
*/
//? Layout 2's split involves just copying B's pointer to
//? the stack and nulling the old value out.
//? Layout 1 ultimately does the same thing,
//? but also has to copy C from the heap to the stack.
//? Merging is the same process in reverse.
/*
 * One of the few nice things about a linked list is that you can construct
 * the element in the node itself, and then freely shuffle it around
 * lists without ever moving it.
 * You just fiddle with pointers and stuff gets "moved".
 * Layout 1 trashes this property.❗
*/

//? While enums let us declare a type that can contain one of several values
// pub enum Scope2 {
enum Scope2 {
    Nil,
    Local(Box<Node>),
}
//? structs let us declare a type that contains many values at once.
//? Let's break our List into two types: A List, and a Node.
struct Node {
    elem: i32,
    next: Scope2,
}
/*
 * ✅ Tail of a list never allocates extra junk:
 * ✅ enum is in delicious null-pointer-optimized form:
 * ✅ All elements are uniformly allocated:
 *
 * Let's make List a struct, so that we can hide the implementation details:
 */

pub struct List {
    head: Scope3,
}

impl List {
    pub fn new() -> Self {
        Self { head: Scope3::Nil }
    }

    pub fn push(&mut self, elem: i32) {
        //? let new_node = Box::new(Node2 {
        //?     elem: elem,
        //?     next: self.head,
        //? });
        /*
         * ❗self.head => cannot move out of borrowed content
         *
         * We're trying to move the self.head field out to next,
         * but Rust doesn't want us doing that.
         * This would leave self only partially initialized when we end the
         * borrow and "give it back" to its rightful owner.
         * As we said before, that's the one thing you can't do with an &mut:
         * It would be super rude, and Rust is very polite
         * (it would also be incredibly dangerous, but surely that isn't why it cares).
         *
         * What if we put something back? Namely, the node that we're creating:
         */
        //? self.head = Scope3::Local(new_node);
        /*
         * ❗self.head => cannot move out of borrowed content
         * In principle, this is something Rust could actually accept,
         * @see: https://doc.rust-lang.org/nightly/nomicon/exception-safety.html
         * but it won't (for various reasons -- the most serious being exception safety).
         * We need some way to get the head without Rust noticing that it's gone.
         */
        let new_node = Box::new(Node2 {
            elem,
            // * We need some way to get the head without Rust noticing that it's gone.
            // * This incredibly useful function lets us steal a value out of a
            // * borrow by replacing it with another value.
            next: std::mem::replace(&mut self.head, Scope3::Nil),
        });

        self.head = Scope3::Local(new_node);
    }

    pub fn pop(&mut self) -> Option<i32> {
        // let result;
        // match &self.head {
        /*
         * The key insight is we want to remove things, which means we want to
         * get the head of the list by value.
         * We certainly can't do that through the shared reference we get
         * through &self.head. We also "only" have a mutable reference to self,
         * so the only way we can move stuff is to replace it.
         */
        match std::mem::replace(&mut self.head, Scope3::Nil) {
            //? Check if the list is empty.
            //? If it's empty, just return None
            Scope3::Nil => None,
            //? If it's not empty
            Scope3::Local(node) => {
                // result = Some(node.elem);
                //? remove the head of the list
                //? remove its elem
                //? replace the list's head with its next
                //? return Some(elem)
                self.head = node.next;
                Some(node.elem)
            }
        }
        // result
    }
}

impl Drop for List {
    fn drop(&mut self) {
        println!("dropping!");
        let mut cur_link = std::mem::replace(&mut self.head, Scope3::Nil);
        // `while let` == "do this thing until this pattern doesn't match"
        while let Scope3::Local(mut boxed_node) = cur_link {
            cur_link = std::mem::replace(&mut boxed_node.next, Scope3::Nil);
            // boxed_node goes out of scope and gets dropped here;
            // but its Node's `next` field has been set to Scope3::Nil
            // so no unbounded recursion occurs.
        }
    }
}
/*
 * If you wish to have the best of both implementations,
 * you could add a new method, fn pop_node(&mut self) -> Link,
 * from-which pop and drop can both be cleanly derived.
 */
enum Scope3 {
    Nil,
    Local(Box<Node2>),
}

struct Node2 {
    elem: i32,
    next: Scope3,
}

mod test {
    use super::*;

    #[test]
    fn basics() {
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
}
