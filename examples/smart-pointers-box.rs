use bytes::Bytes;
use std::collections::HashMap;
fn main() {
    let mut kv = HashMap::new();
    kv.insert("jamon".to_string(), Bytes::from(vec![0, 2, 4]));
    let boxed = Box::new(kv);

    println!("BOX GOT {:?}", *boxed);
}
