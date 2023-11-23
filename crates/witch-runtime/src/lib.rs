#![feature(arc_unwrap_or_clone)]
#![feature(ptr_from_ref)]
#![feature(error_in_core)]
#![cfg_attr(not(feature = "debug"), no_std)]
extern crate alloc;

mod heap;
mod native_function;
mod stack;
pub mod value;
pub mod vm;
