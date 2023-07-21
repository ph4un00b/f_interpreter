pub struct List<TData> {
    head: Env<TData>,
}

//? yay type aliases!
//? prefer option over manually created option❗
type Env<TData> = Option<Box<Node<TData>>>;

struct Node<TData> {
    elem: TData,
    next: Env<TData>,
}

impl<TData> List<TData> {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn push(&mut self, elem: TData) {
        let new_node = Box::new(Node {
            elem,
            next: self.head.take(),
        });
        self.head = Some(new_node);
    }

    pub fn pop(&mut self) -> Option<TData> {
        //? match option { None => None, Some(x) => Some(y) }
        //? is such an incredibly common idiom that it was called map.
        self.head.take().map(|node| {
            self.head = node.next;
            node.elem
        })
    }

    pub fn read_peek(&mut self) -> Option<&TData> {
        self.head
            /*
             * impl<T> Option<T> {
             *      pub fn as_ref(&self) -> Option<&T>;
             * }
             */
            .as_ref()
            //? Map takes self by value, which would move the Option out of the thing it's in.
            //? Previously this was fine because we had just taken it out,
            //? but now we actually want to leave it where it was.
            //? The correct way to handle this is with the as_ref method on Option,
            .map(|ref_node| &ref_node.elem)
    }

    pub fn mut_peek(&mut self) -> Option<&mut TData> {
        self.head.as_mut().map(|ref_node| &mut ref_node.elem)
    }
}

impl<TData> Drop for List<TData> {
    fn drop(&mut self) {
        let mut cur_link = self.head.take();
        while let Some(mut boxed_node) = cur_link {
            cur_link = boxed_node.next.take();
        }
    }
}

#[cfg(test)]
mod test {
    use super::List;

    #[test]
    fn peek() {
        let mut list = List::new();
        assert_eq!(list.read_peek(), None);
        assert_eq!(list.mut_peek(), None);
        list.push(1);
        list.push(2);
        list.push(3);

        assert_eq!(list.read_peek(), Some(&3));
        assert_eq!(list.mut_peek(), Some(&mut 3));
        //? means "the argument is a mutable reference,
        //? but just copy the value it points to into value, please."
        // list.mut_peek().map(|&mut value| *value = 42); <- ❗error
        // list.mut_peek().map(|value| *value = 42); <- ✅ good
        //? refactored to:
        if let Some(value) = list.mut_peek() {
            *value = 42
        }

        assert_eq!(list.read_peek(), Some(&42));
        assert_eq!(list.pop(), Some(42));
    }

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
