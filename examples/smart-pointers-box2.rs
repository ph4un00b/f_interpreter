use bytes::Bytes;
use std::collections::HashMap;

struct MiCaja<TData>(TData);

impl<TData> std::ops::Deref for MiCaja<TData> {
    type Target = TData;

    fn deref(&self) -> &Self::Target {
        //? Cuando ingresamos *y en el Listado 15-9,
        //? en realidad Rust ejecuta este código:
        //? *(y.deref())
        &self.0
    }
}

impl<TData> MiCaja<TData> {
    fn new(x: TData) -> MiCaja<TData> {
        MiCaja(x)
    }
}

fn hello(name: &str) {
    println!("Hello, {name}!");
}

fn main() {
    let mut kv = HashMap::new();
    kv.insert("jamon".to_string(), Bytes::from(vec![0, 2, 4]));
    let boxed = MiCaja::new(kv);

    println!("CAJA GOT {:?}", *boxed);

    let m = &MiCaja::new(String::from("Rust"));
    hello(m);
    //? La biblioteca estándar proporciona una implementación de Deref
    //? en String que devuelve una cadena de texto,
    //? y esto está en la documentación de la API de Deref.

    //? Rust llama a deref nuevamente para convertir
    //? el &String en &str, que coincide con la definición de la
    //?función hello
    let m = MiCaja::new(String::from("Rust"));
    hello(&m);
    //? Si Rust no implementara la coerción Deref,
    //?tendríamos que escribir el
    //? para llamar a hello con un valor de tipo &MyBox<String>.
    let m = MiCaja::new(String::from("Rust"));
    hello(&(*m)[..]);
}
