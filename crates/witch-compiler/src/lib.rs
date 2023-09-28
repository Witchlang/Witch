//! The `witch-compiler` crate takes a Witch source file (or several) and emits
//! Witch Bytecode, a binary representation of the program which the `witch-runtime`
//! can execute.
//!
//! It exports the `compile` function, which either returns the final bytecode or
//! an error which can be written to stderr.

use std::path::PathBuf;

mod ast;
mod error;

/// Takes a Witch source file and compiles it to bytecode, or returns `error::Error`.
pub fn compile<'a>(file_path: PathBuf) -> Result<Vec<u8>, error::Error<'a>> {
    let _ast = ast::parse(file_path);
    Ok(vec![])
}


