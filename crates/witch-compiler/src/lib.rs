//! The `witch-compiler` crate takes a Witch source file (or several) and emits
//! Witch Bytecode, a binary representation of the program which the `witch-runtime`
//! can execute.
//!
//! It exports the `compile` function, which either returns the final bytecode or
//! an error which can be written to stderr.
#![feature(type_alias_impl_trait)]
#![feature(iter_advance_by)]
#![feature(assert_matches)]

use anyhow::{Context as AnyhowContext, Result};

use std::{
    env::current_dir,
    path::{Path, PathBuf},
};

type Context = i32; //dummy

mod error;
mod parser;
mod types;

/// Takes a Witch source file and compiles it to bytecode, or returns `error::Error`.
pub fn compile(file_path: PathBuf, _maybe_ctx: Option<Context>) -> Result<(Vec<u8>, Context)> {
    let (_root_path, source) = resolve_file(None, file_path)?;
    let mut parser = parser::Parser::new(&source);
    let _ast = parser.file();

    // let ast = ast::parse(file_path)?;
    // let mut ctx = maybe_ctx.unwrap_or_default();
    // let (bc, _) = compiler::compile(&mut ctx, &ast)?;
    // let prelude = ctx.flush();
    // Ok(([prelude, bc].concat(), ctx))
    Ok((vec![], 0))
}

/// Canonicalizes a file path from our `start_path`, returning the new path as well as the file contents.
fn resolve_file(start_path: Option<PathBuf>, file_path: PathBuf) -> Result<(PathBuf, String)> {
    let file_path = if let Some(start_path) = start_path {
        let cwd = current_dir().with_context(|| format!("Failed to get current directory"))?;
        let p = cwd
            .as_path()
            .join(Path::new(&start_path).parent().unwrap())
            .join(file_path.clone());
        std::fs::canonicalize(p).with_context(|| {
            format!(
                "Failed to canonicalize path {}",
                file_path.clone().to_string_lossy()
            )
        })?
    } else {
        let cwd = current_dir().with_context(|| format!("Failed to get current directory"))?;
        let p = cwd.as_path().join(file_path.clone());
        std::fs::canonicalize(p).with_context(|| {
            format!(
                "Failed to canonicalize path {}",
                file_path.clone().to_string_lossy()
            )
        })?
    };
    let source = std::fs::read_to_string(file_path.clone()).with_context(|| {
        format!(
            "Failed to read file {}",
            file_path.clone().to_string_lossy()
        )
    })?;
    Ok((file_path, source))
}
