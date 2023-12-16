#![feature(lang_items, start, libc, core_intrinsics, rustc_private)]
#![no_std]
extern crate libc;
use alloc::{vec, ffi::CString};
use witch_runtime::vm::Vm;
extern crate wee_alloc;
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
extern crate alloc;

#[cfg(not(test))]
#[lang = "eh_personality"]
extern "C" fn rust_eh_personality() {}

#[cfg(not(test))]
#[panic_handler]
fn my_panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[no_mangle]
pub extern "C" fn run() -> i32 {
    let mut vm = Vm::new();
    vm.run(vec![21]);


    unsafe {
        libc::puts(CString::new("Hello world").unwrap().as_ptr());
    }
    0
}
