use alloc::rc::Rc;
use core::{cell::RefCell};
use slab::Slab;

use crate::value::Value;

#[derive(Default)]
pub struct Heap {
    mem: Slab<Rc<RefCell<Value>>>,
}

impl Heap {
    pub fn insert(&mut self, value: Value) -> usize {
        self.mem.insert(Rc::new(value.into()))
    }

    pub fn get(&mut self, key: usize) -> Rc<RefCell<Value>> {
        self.mem.get_mut(key).unwrap().clone()
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
