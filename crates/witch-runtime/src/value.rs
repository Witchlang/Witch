use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
#[repr(C, u8)]
pub enum Value {
    Void,
    Bool(bool),
    String(String),
    List(Vec<Self>),
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
