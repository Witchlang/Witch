#![feature(arc_unwrap_or_clone)]
#![feature(ptr_from_ref)]
#![cfg_attr(not(feature = "debug"), no_std)]
extern crate alloc;

mod stack;
pub mod value;
pub mod vm;
