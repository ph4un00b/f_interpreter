use std::borrow::Cow;

//? Replaces all the ` ` characters with `_`
fn replace_spaces_with_allocations(input: &str) -> String {
    input.replace(' ', "_")
}

fn replace_spaces(input: &str) -> Cow<str> {
    if input.contains(' ') {
        Cow::Owned(input.replace(' ', "_"))
    } else {
        Cow::Borrowed(input)
    }
}

fn main() {
    println!("{}", replace_spaces_with_allocations("Hello, world!"));
    //? Did the second call replace anything? Did you have
    //? to allocate a String and copy all the data anyway,
    //? even though nothing changed?
    println!("{}", replace_spaces_with_allocations("Hello!"));
    //? with cow
    println!("{}", replace_spaces("Hello, world!"));
    println!("{}", replace_spaces("Hello!"));
}
