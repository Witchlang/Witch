use super::BuiltinInfo;
use crate::vm::Vm;
use witch_macro::builtin;

use alloc::ffi::CString;
use alloc::string::String;

#[builtin]
pub fn witch_conv_cstring_to_string(_vm: &mut Vm, string: CString) -> String {
    string.into_string().unwrap()
}

#[builtin]
pub fn witch_conv_string_to_cstring(_vm: &mut Vm, string: String) -> CString {
    CString::new(string).unwrap()
}
