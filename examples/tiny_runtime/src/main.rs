#![feature(lang_items, start, libc, core_intrinsics, rustc_private)]
#![no_std]
#![no_main]
extern crate libc;
use alloc::vec;
use witch_runtime::vm::Vm;
extern crate wee_alloc;
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
extern crate alloc;

#[cfg(not(test))]
#[lang = "eh_personality"]
extern "C" fn rust_eh_personality() {}

#[no_mangle]
pub extern "C" fn main(_argc: i32, _argv: *const *const u8) -> i32 {
    let mut vm = Vm::new();
    let resz = vm.run(vec![21]);

    let res = match resz {
        Ok(r) => r,
        Err(r) => r,
    };

    unsafe {
        libc::printf(&res as *const _ as *const i8);
    }
    0
}

#[cfg(not(test))]
#[panic_handler]
fn my_panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[allow(non_snake_case)]
#[no_mangle]
pub extern "C" fn _Unwind_Resume() -> ! {
    loop {}
}
