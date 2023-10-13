use crate::value::Value;
use alloc::boxed::Box;
use core::hash::{Hash, Hasher};
use hashbrown::HashSet;

#[derive(Default)]
pub struct Heap {
    objects: HashSet<Handle>,
}

impl Heap {
    pub fn insert(&mut self, value: Value) -> Handle {
        let ptr = Box::into_raw(Box::new(value));
        let handle = Handle { ptr };
        self.objects.insert(handle);

        handle
    }
}

#[cfg_attr(feature = "parser", derive(Debug))]
pub struct Handle {
    ptr: *mut Value,
}

impl Handle {
    pub unsafe fn get(&self) -> &Value {
        &*self.ptr
    }

    #[allow(clippy::mut_from_ref)]
    pub unsafe fn get_mut(&self) -> &mut Value {
        &mut *self.ptr
    }
}

impl Copy for Handle {}
impl Clone for Handle {
    fn clone(&self) -> Self {
        *self
    }
}

impl PartialEq<Self> for Handle {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}
impl Eq for Handle {}

impl Hash for Handle {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}
