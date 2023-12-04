use core::ffi::c_int;

use super::BuiltinInfo;
use crate::vm::Vm;
use alloc::string::String;
use witch_macro::builtin;

use alloc::ffi::CString;

#[builtin]
pub fn witch__libc_puts(_vm: &mut Vm, msg: CString) -> c_int {
    unsafe { libc::puts(msg.as_ptr()) }
}
