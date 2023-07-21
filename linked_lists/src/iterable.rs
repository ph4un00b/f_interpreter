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

/*
 * That's kind of a big deal, if you ask me. There are a couple reasons why this works:
 * We take the Option<&mut> so we have exclusive access to the mutable reference.
 *
 * No need to worry about someone looking at it again.
 *
 * Rust understands that it's ok to shard a mutable reference into the subfields
 * of the pointed-to struct, because there's no way to "go back up", and they're definitely disjoint.
 */
pub struct IterMut<'a, T> {
    next: Option<&'a mut Node<T>>,
}

impl<T> List<T> {
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut {
            next: self.head.as_deref_mut(),
        }
    }
}

impl<'a, T> Iterator for IterMut<'a, T> {
    //? It turns out that you can apply this basic logic to get a safe IterMut for an array or a tree as well!
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next
            /*
             * However for other types this is garbage.
             * Integers have no ownership semantics; they're just meaningless numbers!
             * This is why integers are marked as Copy.
             * Copy types are known to be perfectly copyable by a bitwise copy.
             * As such, they have a super power: when moved, the old value is still usable.
             * As a consequence, you can even move a Copy type out of a reference without replacement!
             *
             * Critically to why this code was working, shared references are also Copy!
             * Because & is copy, Option<&> is also Copy.
             * So when we did self.next.map it was fine because the Option was just copied.
             * Now we can't do that, because &mut isn't Copy (if you copied an &mut,
             *  you'd have two &mut's to the same location in memory, which is forbidden).
             * Instead, we should properly take the Option to get it.
             */
            .take()
            .map(|node| {
                self.next = node.next.as_deref_mut();
                &mut node.elem
            })
    }
}
