#![feature(arc_unwrap_or_clone)]
#![feature(ptr_from_ref)]
#![cfg_attr(not(feature = "debug"), no_std)]
extern crate alloc;

mod heap;
mod stack;
pub mod vm;
pub mod value;
