use alloc::{string::String, vec, vec::Vec};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Debug, Deserialize, PartialEq, Clone)]
#[repr(C, u8)]
pub enum Value {
    Void,
    Error(Error),
    Bool(bool),
    String(String),
    List(Vec<Self>),
    Function(Function),
    StackFunction {
        //TODO terrible name
        addr: usize,
        arity: usize,
        upvalues_refs_idx: usize,
    },
    NativeFunction(usize),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    I128(i128),
    U128(u128),
    Isize(isize),
    Usize(usize),
    Char(char),
    F32(f32),
    F64(f64),
}

impl From<()> for Value {
    fn from(_val: ()) -> Self {
        Value::Void
    }
}

impl From<Value> for usize {
    fn from(val: Value) -> Self {
        match val {
            Value::Usize(i) => i,
            _ => unreachable!(),
        }
    }
}

impl From<Value> for String {
    fn from(val: Value) -> Self {
        match val {
            Value::String(i) => i,
            _ => unreachable!(),
        }
    }
}

#[derive(Serialize, Debug, Deserialize, PartialEq, Clone)]
pub enum Error {
    InvalidOp(u8),
}

#[derive(Serialize, Debug, Deserialize, PartialEq, Clone)]
pub struct Function {
    pub is_variadic: bool,
    pub arity: usize,
    pub upvalues_bytecode: Vec<u8>,
}
impl Function {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            is_variadic: false,
            arity: 0,
            upvalues_bytecode: vec![],
        }
    }
}
