use std::{
    env::current_dir,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result as AnyhowResult};
use ariadne::{Report, ReportKind, Label, Color, Source};
use chumsky::{span::SimpleSpan, Parser};

use crate::types::{Type, TypeDecl};

mod lexer;
use lexer::lexer;

// Dummy value for now, replace with runtime-compatible value
#[derive(Clone, Debug, PartialEq)]
pub enum Value {}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum BinaryOp {
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
    Gte
}


#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
   
    // Imports a module by path
    Import(Box<PathBuf>),

    // A function expression defines an anonymous function (arg1, arg2) -> {expr}
    Function {
        is_variadic: bool,
        is_method: bool,
        args: Vec<String>,
        body: Box<Self>,
        r#type: Type
    },

    // A value expression.
    Value {
        value: Value,
    },

    // Let declares a new variable. It is always followed by
    // an Assignment expression.
    Let {
        ident: String, 
        expr: Box<Spanned<Self>>
    },

    // Assigns an expression to a variable.
    Assignment {
        ident: String, 
        expr: Box<Spanned<Self>>
    },

    // Resolves a variable by name.
    Var(String),

    // An Entry expression allows us to access entries within objects, 
    // such as items in lists, functions in modules or fields in structs. 
    // `is_method_call` is used to deduce whether we should keep the object on the stack
    // in order to implicitly pass it as `self` into the called entry. 
    Entry {
        object: Box<Spanned<Self>>,
        key: Box<Spanned<Self>>,
        is_method_call: bool
    },
    
    // A return value to be put on the stack and return from the current scope.
    Return(Box<Spanned<Self>>),

    // Resolves a list of expressions, e.g.
    // [1, 2, (1+2), get4()]
    List(Vec<Spanned<Self>>),

    // Expresses a binary operation, such as 1 <op> 1.
    BinaryOperation {
        a: Box<Spanned<Self>>,
        op: BinaryOp,
        b: Box<Spanned<Self>>
    },

    /// Calls a function expresion with the provided values as arguments.
    Call {
        expr: Box<Spanned<Self>>,
        args: Vec<Spanned<Self>>
    },

    // A statement is an expression, and then the "rest" of the AST.
    Statement {
        expr: Box<Spanned<Self>>,
        rest: Option<Box<Spanned<Self>>>
    },

    // A block is a wrapper around a number of expressions with their own scope.
    Block(Box<Spanned<Self>>),

    // An if expression conditionally runs the `then_` or `else_` branch depending on the predicate expression.
    If {
        predicate: Box<Spanned<Self>>,
        then_: Box<Spanned<Self>>,
        else_: Box<Spanned<Self>>
    },

    // Expresses a `while` loop. (Condition, Block)
    While {
        predicate: Box<Spanned<Self>>,
        expr: Box<Spanned<Self>>
    },

    // Breaks the current loop
    Break,

    // Jumps to the next iteration of the current loop
    Continue,

    // A type declaration. Registers a custom type with the compiler.
    Type {
        name: String,
        decl: TypeDecl
    },

}

// Convenience types for dealing with Spans, 
// i.e. location offsets in the incoming source code. 
pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

/// Takes a filename and its source contents and parses it, 
/// hopefully returning an abstract syntax tree.
pub fn parse<'a>(
    root_path: PathBuf
) -> Result<Ast, crate::error::Error<'a>> {

    let (file_path, source) = resolve_file(None, root_path)?;

    let (_tokens, errs) = lexer().parse(&source).into_output_errors();

    // let (expr, parse_errs) = if let Some(tokens) = &tokens {
    //     let (ast, parse_errs) = parser()
    //         .map_with_span(|ast, span| (ast, span))
    //         .parse(
    //             tokens
    //                 .as_slice()
    //                 .spanned((string.len()..string.len()).into()),
    //         )
    //         .into_output_errors();
    //     if let Some((expr, _file_span)) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
    //         (Some(expr.0), parse_errs)
    //     } else {
    //         (None, parse_errs)
    //     }
    // } else {
    //     (None, Vec::new())
    // };

    // if let Some(expr) = expr {
    //     return Ok(expr);
    // }

    let file_path_string = file_path.clone().to_string_lossy().to_string();

    let reports: Vec<Report<'a, (String, std::ops::Range<usize>)>> = errs
        .into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        // .chain(
        //     parse_errs
        //         .into_iter()
        //         .map(|e| e.map_token(|tok| tok.to_string())),
        // )
        .map(|e| {
            Report::build(ReportKind::Error, file_path_string.clone(), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((file_path_string.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .finish()
        })
        .collect();

    Err((Source::from(&source), reports).into())
}


/// Canonicalizes a file path from our `start_path`, returning the new path as well as the file contents.
fn resolve_file(start_path: Option<PathBuf>, file_path: PathBuf) -> AnyhowResult<(PathBuf, String)> {
    let file_path = if let Some(start_path) = start_path {
        let cwd = current_dir().with_context(|| format!("Failed to get current directory"))?;
        let p = cwd
            .as_path()
            .join(Path::new(&start_path).parent().unwrap())
            .join(file_path.clone());
        std::fs::canonicalize(p)
            .with_context(|| {
                format!(
                    "Failed to canonicalize path {}",
                    file_path.clone().to_string_lossy()
                )
            })?
    } else {
        let cwd = current_dir().with_context(|| format!("Failed to get current directory"))?;
        let p = cwd.as_path().join(file_path.clone());
        std::fs::canonicalize(p)
            .with_context(|| {
                format!(
                    "Failed to canonicalize path {}",
                    file_path.clone().to_string_lossy()
                )
            })?
    };
    let source = std::fs::read_to_string(file_path.clone())
        .with_context(|| format!("Failed to read file {}", file_path.clone().to_string_lossy()))?;
    Ok((file_path, source))
}