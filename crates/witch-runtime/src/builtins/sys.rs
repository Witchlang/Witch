use witch_macro::builtin;
use crate::vm::Vm;
use super::BuiltinInfo;
use alloc::string::String;

use alloc::ffi::CString;

#[builtin]
pub fn witch__print(_vm: &mut Vm, msg: String) {
    unsafe {
        let s = CString::new(msg).unwrap();
        libc::printf(s.as_ptr() as *const i8);
    }
}