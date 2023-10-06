//! The Witch Programming Language
//!
//! This create is the only one you want to use.
//! It exposes the `witch` runtime, as well as the compiler (behind the "compiler" feature flag).
//! NOTE: Unlike the runtime, the compiler lets the Rust standard library and a couple of other
//! dependencies to the heavy lifting for a lot of things. This is why it's behind a feature flag.
#![cfg_attr(not(feature = "compiler"), no_std)]

#[cfg(feature = "compiler")]
pub use witch_compiler::compile;

pub use witch_runtime::vm::Vm;
