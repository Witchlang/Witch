//! The `witch-compiler` crate takes a Witch source file (or several) and emits
//! Witch Bytecode, a binary representation of the program which the `witch-runtime`
//! can execute.
//!
//! It exports the `compile` function, which either returns the final bytecode or
//! an error which can be written to stderr.
#![feature(type_alias_impl_trait)]
#![feature(iter_advance_by)]
#![feature(assert_matches)]

use anyhow::Context as AContext;
use anyhow::Result;
use compiler::Cached;
use compiler::Context;

use std::{
    env::current_dir,
    path::{Path, PathBuf},
};

use witch_parser::Parser;

mod compiler;
mod error;

/// Takes a Witch source file and compiles it to bytecode, or returns `error::Error`.
pub fn compile(file_path: PathBuf, maybe_ctx: Option<Context>) -> Result<(Vec<u8>, Context)> {
    let (_root_path, source) = resolve_file(None, file_path)?;
    let mut parser = Parser::new(&source);
    let ast = parser.file().unwrap();

    // If no context is provided, create a new one and compile the prelude for it
    let mut ctx = maybe_ctx.unwrap_or_else(|| {
        let mut ctx = Context::default();
        let (prelude, _) = compiler::compile(&mut ctx, &witch_std::prelude()).unwrap();
        ctx.prelude = Some(prelude);
        ctx
    });

    let (bc, _) = compiler::compile(&mut ctx, &ast).unwrap();
    Ok(([ctx.flush(), bc].concat(), ctx))
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
