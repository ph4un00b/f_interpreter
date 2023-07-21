use crate::linked_list::{List, Node};

//? Tuple structs are an alternative form of struct,
//? useful for trivial wrappers around other types.
pub struct IntoIter<TData>(List<TData>);

impl<TData> List<TData> {
    pub fn into_iter(self) -> IntoIter<TData> {
        IntoIter(self)
    }
}

impl<TData> Iterator for IntoIter<TData> {
    type Item = TData;
    fn next(&mut self) -> Option<Self::Item> {
        //? access fields of a tuple struct numerically
        self.0.pop()
    }
}

/*
 * The basic logic we want is to hold a pointer to the current
 * node we want to yield next.
 *
 * Because that node may not exist (the list is empty or
 * we're otherwise done iterating), we want that reference
 * to be an Option.
 *
 * When we yield an element, we want to proceed to the current node's next node.
 */
//? We need to add lifetimes only in function and type signatures:❗

//? Iter is generic over *some* lifetime, it doesn't care
pub struct Iter<'a, TData> {
    next: Option<&'a Node<TData>>,
}

//? No lifetime here, List doesn't have any associated lifetimes
impl<TData> List<TData> {
    //? We declare a fresh lifetime here for the *exact* borrow that
    //? creates the iter. Now &self needs to be valid as long as the
    //? Iter is around.

    // * lifetime elision
    // pub fn iter<'a>(&'a self) -> Iter<'a, TData> {
    //? or
    // pub fn iter(&self) -> Iter<'_, TData> {
    pub fn iter(&self) -> Iter<TData> {
        // Iter {
        //     // next: self.head.map(|node| &node),
        //     //? We want to be storing &Node's, but we're getting &Box<Node>s.
        //     //? Ok, that's easy enough, we just need to dereference the
        //     //? Box before we take our reference:
        //     next: self
        //         .head
        //         //? We forgot as_ref, so we're moving the box into map, which means
        //         //? it would be dropped, which means our references would be dangling:
        //         .as_ref()
        //         .map(|node| &*node),
        // }
        //? as_ref added another layer of indirection we need to remove:
        Iter {
            next: self.head.as_deref(),
        }
    }
}

//? We *do* have a lifetime here, because Iter has one that we need to define
impl<'a, TData> Iterator for Iter<'a, TData> {
    type Item = &'a TData;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            // todo: deep as_deref | Deref trait❗
            //? self.next = node.next.as_ref().map(|node| &**node);
            //? refactored to:
            /*
             * pub fn map<U, F>(self, f: F) -> Option<U>
             *
             * The turbofish, ::<>, lets us tell the compiler what we think the
             * types of those generics should be. In this case ::<&Node<T>, _>
             * says "it should return a &Node<T>, and I don't know/care about that other type".
             *
             * This in turn lets the compiler know that &node should have deref coercion applied to it,
             * so we don't need to manually apply all those *'s!
             */
            // self.next = node.next.as_ref().map::<&Node<TData>, _>(|node| &node);
            //? refactored to:
            self.next = node.next.as_deref();
            &node.elem
        })
    }
}
