use std::{collections::HashMap, ops::Range, path::PathBuf};

use witch_runtime::{value::Value, vm::BinaryOp};

use crate::types::{Type, TypeDecl};

use super::Spanned;

/// Ast describes the abstract syntax tree used for Witch.
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, PartialEq)]
pub enum Ast {
    Nop,

    // A compiler directive
    Annotation {
        name: String,
        statement: Box<Self>,
        span: Range<usize>,
    },

    // Assigns an expression to a variable.
    Assignment {
        ident: String,
        expr: Box<Spanned<Self>>,
    },

    // Imports a module by path
    Import {
        path: Box<PathBuf>,
        span: Range<usize>,
    },

    // A function expression defines an anonymous function (arg1, arg2) -> {expr}
    Function {
        is_variadic: bool,
        args: Vec<(String, Type)>,
        returns: Type,
        body: Box<Self>,
    },

    // A value expression.
    Value(Value),

    // Let declares a new variable. It is always followed by
    // an Assignment expression.
    Let {
        ident: String,
        expr: Box<Spanned<Self>>,
    },

    // A named or anonymous struct.
    // During actual struct expressions, like Foo { field: 1 },
    // no methods are available. These are declared in the type declaration.
    Struct {
        name: Option<String>,
        fields: HashMap<String, Spanned<Self>>,
    },

    // Resolves a variable by name.
    Var(String),

    // An access expression allows us to access entries within objects,
    // such as items in lists, functions in modules, variants in enums or fields in structs.
    Access {
        container: Box<Spanned<Self>>,
        key: Box<Spanned<Self>>,
    },

    // A return value to be put on the stack and return from the current scope.
    Return {
        expr: Box<Self>,
        span: Range<usize>,
    },

    // Resolves a list of expressions, e.g.
    // [1, 2, (1+2), get4()]
    List(Vec<Spanned<Self>>),

    // Expresses a binary operation, such as 1 <op> 1.
    BinaryOperation {
        a: Box<Spanned<Self>>,
        op: BinaryOp,
        b: Box<Spanned<Self>>,
    },

    /// Calls a function expresion with the provided values as arguments.
    Call {
        expr: Box<Self>,
        args: Vec<Self>,
        span: Range<usize>,
    },

    // A statement is an expression, and then the "rest" of the AST.
    Statement {
        stmt: Box<Self>,
        rest: Box<Self>,
        span: Range<usize>,
    },

    // A block is a wrapper around a number of expressions with their own scope.
    Block(Box<Spanned<Self>>),

    // An if expression conditionally runs the `then_` or `else_` branch depending on the predicate expression.
    If {
        predicate: Box<Spanned<Self>>,
        then_: Box<Spanned<Self>>,
        else_: Box<Spanned<Self>>,
    },

    // Expresses a `while` loop. (Condition, Block)
    While {
        predicate: Box<Spanned<Self>>,
        expr: Box<Spanned<Self>>,
    },

    // Breaks the current loop
    Break,

    // Jumps to the next iteration of the current loop
    Continue,

    // A type declaration. Registers a custom type with the compiler.
    Type {
        name: String,
        decl: TypeDecl,
        span: Range<usize>,
    },

    // A named module
    Mod {
        name: String,
        expr: Box<Spanned<Self>>,
    },
}
