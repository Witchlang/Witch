//! The `witch-compiler` crate takes a Witch source file (or several) and emits
//! Witch Bytecode, a binary representation of the program which the `witch-runtime`
//! can execute.
//!
//! It exports the `compile` function, which either returns the final bytecode or
//! an error which can be written to stderr.
#![feature(type_alias_impl_trait)]

use std::path::PathBuf;

use compiler::Context;

mod ast;
mod compiler;
mod error;
mod types;

/// Takes a Witch source file and compiles it to bytecode, or returns `error::Error`.
pub fn compile<'a>(
    file_path: PathBuf,
    maybe_ctx: Option<Context>,
) -> Result<(Vec<u8>, Context), error::Error<'a>> {
    let ast = ast::parse(file_path)?;
    let mut ctx = maybe_ctx.unwrap_or_default();
    let (bc, _) = compiler::compile(&mut ctx, &ast)?;
    let prelude = ctx.flush();
    Ok(([prelude, bc].concat(), ctx))
}
