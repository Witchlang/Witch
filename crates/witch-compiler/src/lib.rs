//! The `witch-compiler` crate takes a Witch source file (or several) and emits
//! Witch Bytecode, a binary representation of the program which the `witch-runtime`
//! can execute.
//!
//! It exports the `compile` function, which either returns the final bytecode or
//! an error which can be written to stderr.
#![feature(type_alias_impl_trait)]
#![feature(iter_advance_by)]
#![feature(assert_matches)]
#![feature(let_chains)]
#![feature(map_try_insert)]

use anyhow::Context as AContext;
use anyhow::Result;

use compiler::context::Context;
use witch_parser::types::Type;
use witch_std::prelude;

use std::collections::HashMap;
use std::{
    env::current_dir,
    path::{Path, PathBuf},
};

use witch_parser::Parser;

use crate::compiler::context::Module;
use crate::compiler::LocalVariable;
use crate::module::resolve_dependencies;

mod compiler;
mod error;
mod module;

/// Takes a Witch source file and compiles it to bytecode, or returns `error::Error`.
pub fn compile(file_path: PathBuf) -> Result<Vec<u8>> {
    let (root_path, source) = resolve_file(None, file_path)?;
    let mut parser = Parser::new(&source);
    let module = parser.module(root_path.clone()).unwrap();

    let mut modules = vec![prelude()];
    resolve_dependencies(module, &mut modules);

    // For each module, compile them. Calculate stack offset for each before compiling the next one
    // for each module compilation, look up its imports and add them to the locals of the main scope
    // when running module.symbol() etc, look up the imported module in the reference vec and apply its stack_offset to its local variable
    // for all other stack interactions, apply the current modules stack offset

    let mut bc = vec![];
    let mut module_library = vec![];
    for module in modules.iter() {
        println!("{:?}", &module.path);
        let mut ctx = Context::new(module.path.clone(), &module_library);

        for (mod_path, _) in module.imports.iter() {
            ctx.scope()?.locals.push(LocalVariable {
                name: mod_path.file_stem().unwrap().to_string_lossy().to_string(),
                is_captured: false,
                r#type: Type::Module {
                    path: mod_path.clone(),
                },
            });
        }

        let (mut bytecode, _) = compiler::compile(&mut ctx, &module.ast)?;
        dbg!(&ctx.scopes[0]);
        bytecode = [ctx.flush(), bytecode].concat();
        bc.append(&mut bytecode);
        module_library.push((
            module.path.clone(),
            Module {
                locals: ctx.scope()?.locals.clone(),
                vtable_count: ctx.functions_cache.len(),
                path: module.path.clone(),
                stack_offset: module_library.iter().fold(0, |mut acc, (_, module)| {
                    acc += module.locals.len();
                    acc
                }),
                vtable_offset: module_library.iter().fold(0, |mut acc, (_, module)| {
                    acc += module.vtable_count;
                    acc
                }),
            },
        ));
    }

    let calculated = module_library
        .iter()
        .fold(0, |acc, cur| acc + cur.1.stack_offset)
        + module_library.last().unwrap().1.locals.len();
    dbg!(&calculated);

    Ok(bc)

    // // If no context is provided, create a new one and compile the prelude for it
    // let mut ctx = maybe_ctx.unwrap_or_else(|| {
    //     let mut ctx = Context::new(root_path);
    //     let (prelude, _) = compiler::compile(&mut ctx, &witch_std::prelude()).unwrap();
    //     ctx.prelude = Some(prelude);
    //     ctxs
    // });

    // let (bc, _) = compiler::compile(&mut ctx, &ast).unwrap();

    // Ok(([ctx.flush(), bc].concat(), ctx))
}

/// Canonicalizes a file path from our `start_path`, returning the new path as well as the file contents.
fn resolve_file(start_path: Option<PathBuf>, file_path: PathBuf) -> Result<(PathBuf, String)> {
    let file_path = if let Some(start_path) = start_path {
        let cwd = current_dir().with_context(|| "Failed to get current directory".to_string())?;
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
        let cwd = current_dir().with_context(|| "Failed to get current directory".to_string())?;
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
