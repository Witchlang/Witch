use std::{collections::HashMap, ops::Range, path::PathBuf};

use witch_runtime::value::Value;

use crate::pattern::{Case, Pattern};
use crate::types::{Type, TypeDecl};

use super::Spanned;

#[derive(Clone, Debug, PartialEq)]
pub enum Key {
    Usize(usize),
    String(String),
    Expression(Box<Ast>),
}

#[repr(u8)]
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
    Bang,
    Pow,
}

impl Operator {
    pub fn resulting_type(&self, lhs: Type) -> Type {
        match self {
            Operator::Add
            | Operator::Sub
            | Operator::Mul
            | Operator::Div
            | Operator::Mod
            | Operator::Pow => lhs,
            Operator::Lt
            | Operator::Gt
            | Operator::Lte
            | Operator::Gte
            | Operator::NotEq
            | Operator::Eq
            | Operator::And
            | Operator::Or
            | Operator::Bang => Type::Bool,
        }
    }

    pub fn prefix_binding(&self) -> ((), u8) {
        match self {
            Operator::Add | Operator::Sub | Operator::Bang => ((), 51),
            _ => unreachable!(),
        }
    }

    pub fn infix_binding(&self) -> Option<(u8, u8)> {
        let result = match self {
            Operator::Or => (1, 2),
            Operator::And => (3, 4),
            Operator::Eq | Operator::NotEq => (5, 6),
            Operator::Lt | Operator::Lte | Operator::Gt | Operator::Gte => (7, 8),
            Operator::Add | Operator::Sub => (9, 10),
            Operator::Mul | Operator::Div | Operator::Mod => (11, 12),
            Operator::Pow => (22, 21),
            Operator::Bang => {
                return None;
            }
        };
        Some(result)
    }

    pub fn postfix_binding(&self) -> Option<(u8, ())> {
        if let Operator::Bang = self {
            return Some((101, ()));
        }
        None
    }
}

/// Ast describes the abstract syntax tree used for Witch.
#[derive(Clone, Debug, PartialEq)]
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
        lhs: Box<Self>,
        rhs: Box<Self>,
        span: Range<usize>,
    },

    /// A case expression, used for pattern matching
    Case {
        expr: Box<Self>,
        cases: Vec<Case>,
        span: Range<usize>,
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
        generics: Vec<(String, Type)>,
    },

    // A value expression.
    Value(Value),

    // Let declares a new variable. It is always followed by
    // an Assignment expression.
    Let {
        ident: String,
        annotated_type: Option<Type>,
        expr: Box<Self>,
        span: Range<usize>,
    },

    // A named or anonymous struct.
    // During actual struct expressions, like Foo { field: 1 },
    // no methods are available. These are declared in the type declaration.
    Struct {
        ident: Option<String>,
        fields: HashMap<String, Self>,
        span: Range<usize>,
    },

    // Resolves a variable by name.
    Var(String),

    // A member access expression allows us to access entries within objects,
    // such as items in lists, functions in modules, variants in enums or fields in structs.
    Member {
        container: Box<Self>,
        key: Key,
        span: Range<usize>,
    },

    // A return value to be put on the stack and return from the current scope.
    Return {
        expr: Box<Self>,
        span: Range<usize>,
    },

    // Resolves a list of expressions, e.g.
    // [1, 2, (1+2), get4()]
    List {
        items: Vec<Self>,
        span: Range<usize>,
    },

    // Expresses a binary operation, such as 1 <op> 1.
    Infix {
        lhs: Box<Self>,
        op: Operator,
        rhs: Box<Self>,
        span: Range<usize>,
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
        predicate: Box<Self>,
        then_: Box<Self>,
        else_: Box<Self>,
        span: Range<usize>,
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
