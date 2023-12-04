use super::BuiltinInfo;
use crate::vm::Vm;
use witch_macro::builtin;

use alloc::ffi::CString;

#[builtin]
pub fn witch_conv_cstring_to_string(_vm: &mut Vm, string: String) -> CString {
    CString::new(string).unwrap()
}
