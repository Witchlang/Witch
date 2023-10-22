use alloc::{string::String, vec, vec::Vec};
use serde::{Deserialize, Serialize};

use crate::{vm::Op, stack::Pointer};

#[derive(Serialize, Debug, Deserialize, PartialEq, Clone)]
#[repr(C, u8)]
pub enum Value {
    Void,
    Error(Error),
    Bool(bool),
    String(String),
    List(Vec<Self>),
    Function(Function),
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

#[derive(Serialize, Debug, Deserialize, PartialEq, Clone)]
pub enum Error {
    InvalidOp(Op),
}

#[derive(Serialize, Debug, Deserialize, PartialEq, Clone)]
pub struct Function {
    pub is_variadic: bool,
    pub is_method: bool,
    pub arity: usize,
    pub bytecode: Vec<u8>,
    pub upvalues: Vec<usize>,
    pub upvalue_count: u8,
    pub upvalues_bytecode: Vec<u8>,
}
impl Function {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            is_variadic: false,
            is_method: false,
            arity: 0,
            bytecode: vec![],
            upvalues: vec![],
            upvalue_count: 0,
            upvalues_bytecode: vec![],
        }
    }
}
