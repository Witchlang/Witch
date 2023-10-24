use alloc::vec;
use alloc::vec::Vec;

use crate::value::Value;

/// Pointer is a usize referring to a Value on the heap
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Pointer(pub usize);

#[derive(Copy, Clone, Debug)]
pub enum Entry {
    Void,
    Isize(isize),
    Usize(usize),
    Bool(bool),
    Pointer(Pointer),
}

impl From<Entry> for Value {
    fn from(entry: Entry) -> Self {
        match entry {
            Entry::Bool(b) => Value::Bool(b),
            Entry::Usize(i) => Value::Usize(i),
            Entry::Void => Value::Void,
            x => todo!("{:?}", x),
        }
    }
}

impl Into<Entry> for Value {
    fn into(self) -> Entry {
        match self {
            Value::Usize(i) => Entry::Usize(i),
            x => todo!("{:?}", x),
        }
    }
}

/// The stack is your normal stack-based abstraction, which of course cheats
/// where applicable - elements may be modified in place or accessed at their indices
/// rather than necessarily requiring pushing or poping.
#[derive(Debug)]
pub struct Stack {
    data: Vec<Entry>,
}

impl Stack {
    pub fn new() -> Self {
        Self { data: vec![] }
    }
    pub fn push(&mut self, entry: Entry) {
        self.data.push(entry);
    }

    pub fn pop(&mut self) -> Option<Entry> {
        self.data.pop()
    }

    pub fn last_mut(&mut self) -> Option<&mut Entry> {
        self.data.last_mut()
    }

    pub fn get(&mut self, idx: usize) -> Entry {
        self.data[idx]
    }

    pub fn set(&mut self, idx: usize, entry: Entry) {
        self.data[idx] = entry;
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn truncate(&mut self, len: usize) {
        self.data.truncate(len);
    }
}
