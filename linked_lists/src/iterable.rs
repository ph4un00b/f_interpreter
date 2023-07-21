use crate::linked_list::List;

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
