use alloc::vec;
use alloc::vec::Vec;

use crate::heap::Handle;
use crate::value::Value;

/// Pointer is a usize referring to a Value or an Entry located somewhere else
#[derive(Copy, Clone)]
pub enum Pointer {
    /// Refers to an entry within the stack
    Stack(usize),

    /// Refers to a value handle within our GC'd heap
    Heap(Handle),

    /// Refers to a stack entry within the value cache
    Cache(usize),

    /// Refers to a function within our list of compiled functions
    Function(usize),
}

#[derive(Copy, Clone)]
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
            _x => todo!(),
        }
    }
}

/// The stack is your normal stack-based abstraction, which of course cheats
/// where applicable - elements may be modified in place or accessed at their indices
/// rather than necessarily requiring pushing or poping.
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
}
