use crate::value::Value;
use alloc::boxed::Box;

#[derive(Default)]
pub struct Heap {
    objects: Vec<Box<Value>>,
}

impl Heap {
    pub fn push(&mut self, value: Value) -> usize {
        let ptr = Box::new(value);
        self.objects.push(ptr);
        self.objects.len() - 1
    }

    pub fn get(&mut self, idx: usize) -> Value {
        *core::mem::replace(&mut self.objects[idx], Box::new(Value::Void))
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
