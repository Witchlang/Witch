//! The Witch standard library and prelude functions
//!
//! This crate supplies the prelude AST, which is a Witch-built set of interfaces and functions that are always available,
//! as well as the Witch standard library, supplied both as an AST for for the compiler crate, and a vec of native function closures for the runtime.
use std::path::PathBuf;

use witch_parser::{Module, Parser};
pub fn prelude() -> Module {
    let source = include_str!("../prelude/main.witch");
    let mut parser = Parser::new(source);
    parser.module(PathBuf::from("<prelude>")).unwrap()
}
