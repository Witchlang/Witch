#![feature(arc_unwrap_or_clone)]
#![feature(ptr_from_ref)]
#![feature(error_in_core)]
#![feature(concat_idents)]
#![cfg_attr(not(feature = "debug"), no_std)]
extern crate alloc;

/// This allows us to use the dbg! macro without having to care about no_std.
/// It's simply a no-op unless running in debug mode
#[macro_export]
macro_rules! dbg {
    ($($expr:tt)*) => {
        // If debug_assertions is true, use dbg! macr
        #[cfg(feature = "debug")]
        std::dbg!($($expr)*);
    };
}

mod heap;
mod stack;
pub mod builtins;
pub mod value;
pub mod vm;
