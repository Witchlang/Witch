use alloc::rc::Rc;
use core::cell::RefCell;
use slab::Slab;

use crate::value::Value;

#[derive(Debug, Clone)]
pub enum Object {
    /// A Value object is any value that is not a List
    Value(Rc<RefCell<Value>>),

    /// A List object is of type Value::List, but contains heap pointers to all items in the list
    /// in order to allow access into items by index
    List(Vec<usize>),
}

#[derive(Default)]
pub struct Heap {
    mem: Slab<Object>,
}

impl Heap {
    /// Inserts a value into heap memory, returning and index key to retrieve it.
    /// Value::List gets chopped up into multiple heap entries and returns a key to a special Object::List type,
    /// allowing access to individual list items.
    pub fn insert(&mut self, value: Value) -> usize {
        match value {
            Value::List(list) => {
                let mut keys = vec![];
                for v in list.into_iter() {
                    keys.push(self.insert(v));
                }
                self.mem.insert(Object::List(keys))
            }
            _ => self.mem.insert(Object::Value(Rc::new(RefCell::new(value)))),
        }
    }

    /// Takes a list of heap pointers and "collects" them in a new heap object,
    /// that points to each one
    pub fn create_list(&mut self, keys: Vec<usize>) -> usize {
        self.mem.insert(Object::List(keys))
    }

    pub fn get(&mut self, key: usize) -> Rc<RefCell<Value>> {
        let obj = self.mem.get_mut(key).unwrap().clone();
        match obj {
            Object::Value(v) => v.clone(),
            Object::List(ref vec) => {
                let mut l = vec![];
                for v in vec.iter() {
                    let val = self.get(*v).borrow().clone();
                    l.push(val);
                }
                Rc::new(RefCell::new(Value::List(l)))
            }
        }
    }

    pub fn get_list_item_ptr(&mut self, key: usize, idx: usize) -> usize {
        let obj = self.mem.get_mut(key).unwrap();
        match obj {
            Object::List(list) => list[idx],
            _ => unreachable!(),
        }
    }
}

// #[derive(Debug)]
// pub struct Handle {
//     ptr: *mut Value,
// }

// impl Handle {
//     pub unsafe fn get(&self) -> &Value {
//         &*self.ptr
//     }

//     #[allow(clippy::mut_from_ref)]
//     pub unsafe fn get_mut(&self) -> &mut Value {
//         &mut *self.ptr
//     }
// }

// impl Copy for Handle {}
// impl Clone for Handle {
//     fn clone(&self) -> Self {
//         *self
//     }
// }

// impl PartialEq<Self> for Handle {
//     fn eq(&self, other: &Self) -> bool {
//         self.ptr == other.ptr
//     }
// }
// impl Eq for Handle {}

// impl Hash for Handle {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.ptr.hash(state);
//     }
// }
