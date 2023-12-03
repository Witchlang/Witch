use alloc::vec;
use alloc::vec::Vec;

use crate::value::Value;

/// Pointer is a usize referring to a Value on the heap
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Pointer {
    Heap(usize),
    Vtable(usize),
    Builtin(usize),
}

/// Pointer is a usize referring to a Value on the heap
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Function {
    pub addr: usize,
    pub arity: usize,
    pub upvalues_refs_idx: usize,
}

#[derive(Copy, Clone, Debug)]
pub enum Entry {
    Void,
    Isize(isize),
    Usize(usize),
    Bool(bool),
    Pointer(Pointer),
    Function(Function),
}

impl Entry {
    pub fn as_heap_pointer(self) -> Pointer {
        match self {
            Entry::Pointer(p) => p,
            _ => unreachable!(),
        }
    }
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

/// The stack is your normal stack-based abstraction, which of course cheats
/// where applicable - elements may be modified in place or accessed at their indices
/// rather than necessarily requiring pushing or poping.
#[derive(Debug)]
pub struct Stack {
    data: Vec<Entry>,
}

impl Default for Stack {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn take(&mut self, n: usize) -> Vec<Entry> {
        let mut taken = vec![];
        for _ in 0..n {
            taken.push(self.data.pop().unwrap());
        }
        taken.reverse();
        taken
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn truncate(&mut self, len: usize) {
        self.data.truncate(len);
    }
}
