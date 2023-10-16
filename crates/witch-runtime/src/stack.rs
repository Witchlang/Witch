use alloc::vec;
use alloc::vec::Vec;

use crate::value::Value;

/// Pointer is a usize referring to a Value or an Entry located somewhere else
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Pointer {
    /// Refers to an entry within the stack
    Stack(usize),

    /// Refers to a value handle within our GC'd heap
    Heap(usize),

    /// Refers to a stack entry within the value cache
    Cache(usize),

    /// Refers to a function within our list of compiled functions
    Function(usize),
}

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
            Entry::Pointer(p) => Value::Pointer(p),
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
}
