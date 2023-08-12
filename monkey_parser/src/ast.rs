#[derive(Debug, Clone)]
pub struct InstanceCallable {}
#[derive(Debug, Clone)]
pub struct RowCallable {}
#[derive(Debug, Clone)]
pub struct FuncCallable {}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum V {
    Done,
    Return(Box<V>),
    Instance(Box<InstanceCallable>),
    Row(Box<RowCallable>),
    Func(Box<FuncCallable>),
    NativeFunc(String),
    I32(i32),
    F64(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum FnType {
    // None,
    // Constructor,
    // Function,
    // Column,
}
