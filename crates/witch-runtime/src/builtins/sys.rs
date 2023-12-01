use witch_macro::builtin;
use crate::vm::Vm;
use crate::builtins::BuiltinInfo;

#[builtin]
pub fn witch_sys_print(vm: &mut Vm, msg: String) {
    // libc println
}