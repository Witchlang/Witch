use std::collections::HashMap;
use std::ffi::OsString;
use std::path::{Component, PathBuf};

use crate::error::Result;
use crate::lexer::{Kind, Lexer};
use crate::Module;

use crate::ast::Ast;
use crate::r#type::{enum_declaration, interface_declaration, struct_declaration};
use crate::types::Type;

use super::expression::{expression, function_expression};
use super::r#type::type_literal;
use super::Parser;

/// TODO currently this parses a module multiple time if imported from different modules.
/// Let the import map be a map between path and indices to a cache vec of modules instead
pub fn imports<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    root_path: PathBuf,
    import_map: &mut HashMap<PathBuf, Module>,
) -> Result<()> {
    match p.peek() {
        Some(Kind::KwImport) => {
            p.consume(&Kind::KwImport)?;

            let mut path = build_path(p, vec![])?.iter().collect::<PathBuf>();

            // Ensure its a .witch file
            if !path.ends_with(".witch") {
                path.set_extension("witch");
            }

            // Deduce whether path is relative to the entry module or abstract
            // - If it starts with `witch`, it's the std library
            // - If it starts with ./, its local
            // - If it just starts with an identifier, its a Grimoire package (TODO)
            let module_file_path = match path.components().next() {
                Some(Component::Normal(x)) => todo!("{:?}", x),
                Some(Component::CurDir) => root_path.parent().unwrap().join(path).canonicalize(),
                _ => panic!("very bad module path"),
            }
            .expect("oh no");

            let source =
                std::fs::read_to_string(module_file_path.clone()).expect("file read error");

            let mut parser = Parser::new(&source);
            let module = parser
                .module(module_file_path.clone())
                .expect("module parser error");

            import_map.insert(module_file_path, module);

            imports(p, root_path, import_map)
        }
        _ => Ok(()),
    }
}

pub fn statement<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<Ast> {
    let start = p.cursor;
    let stmt = match p.peek() {
        Some(Kind::RBrace) => Ast::Nop,
        Some(Kind::KwImport) => {
            p.consume(&Kind::KwImport)?;

            let path = build_path(p, vec![])?.iter().collect::<PathBuf>();

            let stmt = Ast::Import {
                path: Box::new(path),
                span: start..p.cursor,
            };
            let end = p.cursor;
            Ast::Statement {
                stmt: Box::new(stmt),
                rest: Box::new(statement(p)?),
                span: start..end,
            }
        }
        Some(Kind::KwEnum) => {
            let enum_decl = enum_declaration(p)?;
            let end = p.cursor;
            Ast::Statement {
                stmt: Box::new(enum_decl),
                rest: Box::new(statement(p)?),
                span: start..end,
            }
        }
        Some(Kind::KwInterface) => {
            let interface_decl = interface_declaration(p)?;
            let end = p.cursor;
            Ast::Statement {
                stmt: Box::new(interface_decl),
                rest: Box::new(statement(p)?),
                span: start..end,
            }
        }
        Some(Kind::KwStruct) => {
            let struct_decl = struct_declaration(p)?;
            let end = p.cursor;
            Ast::Statement {
                stmt: Box::new(struct_decl),
                rest: Box::new(statement(p)?),
                span: start..end,
            }
        }
        Some(Kind::Ident) => {
            // Statements starting with an identifier are assignments
            // or function declarations (which are actually assignments)
            // or expressions:
            // xyz = 1
            // foo.bar = bar.foo
            // id () -> { 5-1 }
            // id()
            let expr = expression(p)?;

            if p.at(Kind::Semicolon) {
                p.consume(&Kind::Semicolon)?;
            }
            Ast::Statement {
                stmt: Box::new(expr),
                rest: Box::new(statement(p)?),
                span: start..p.cursor,
            }
        }
        Some(Kind::KwLet) => {
            p.consume(&Kind::KwLet)?;
            let (annotated_type, assignment) = assignment(p)?;
            let end = p.cursor;

            let ident = match assignment.clone() {
                Ast::Assignment { lhs, .. } => match *lhs {
                    Ast::Var(ident) => ident,
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            };

            let assignment = Ast::Let {
                ident,
                annotated_type,
                expr: Box::new(assignment),
                span: start..end,
            };
            if p.at(Kind::Semicolon) {
                p.consume(&Kind::Semicolon)?;
            }
            Ast::Statement {
                stmt: Box::new(assignment),
                rest: Box::new(statement(p)?),
                span: start..end,
            }
        }
        Some(Kind::KwReturn) => {
            p.consume(&Kind::KwReturn)?;
            let expr = Box::new(expression(p)?);
            Ast::Return {
                expr,
                span: start..p.cursor,
            }
        }
        Some(Kind::KwFn) => {
            let (ident, expr) = function_declaration(p)?;
            if p.at(Kind::Semicolon) {
                p.consume(&Kind::Semicolon)?;
            }
            let end = p.cursor;
            let assignment = Ast::Let {
                ident,
                annotated_type: None,
                expr: Box::new(expr),
                span: start..end,
            };
            Ast::Statement {
                stmt: Box::new(assignment),
                rest: Box::new(statement(p)?),
                span: start..end,
            }
        }
        Some(Kind::KwIf) => {
            let if_else = if_else(p)?;
            let end = p.cursor;
            Ast::Statement {
                stmt: Box::new(if_else),
                rest: Box::new(statement(p)?),
                span: start..end,
            }
        }
        Some(Kind::At) => annotation(p)?,
        Some(_) => {
            let expr = expression(p)?;
            if p.at(Kind::Semicolon) {
                p.consume(&Kind::Semicolon)?;
            }
            let end = p.cursor;
            Ast::Statement {
                stmt: Box::new(expr),
                rest: Box::new(statement(p)?),
                span: start..end,
            }
        }
        None => {
            return Ok(Ast::Nop);
        }
    };
    if p.at(Kind::Semicolon) {
        p.consume(&Kind::Semicolon)?;
    }
    Ok(stmt)
}

fn build_path<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    mut components: Vec<OsString>,
) -> Result<Vec<OsString>> {
    match p.peek() {
        Some(Kind::Dot) => {
            p.consume(&Kind::Dot)?;
            components.push(OsString::from("."));
            build_path(p, components)
        }

        Some(Kind::Slash) => {
            p.consume(&Kind::Slash)?;
            build_path(p, components)
        }

        Some(Kind::Ident) => {
            let token = p.consume(&Kind::Ident)?;
            let component = OsString::from(p.text(&token));
            components.push(component);
            build_path(p, components)
        }

        Some(Kind::Semicolon) => {
            p.consume(&Kind::Semicolon)?;
            Ok(components)
        }

        _ => panic!("oops"),
    }
}

fn assignment<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<(Option<Type>, Ast)> {
    let start = p.cursor;
    let token = p.consume(&Kind::Ident)?;
    let name = p.text(&token).to_string();

    let annotated_type = if p.at(Kind::Colon) {
        let _ = p.consume(&Kind::Colon)?;
        Some(type_literal(p)?)
    } else {
        None
    };

    p.consume(&Kind::Eq)?;

    let rhs = Box::new(expression(p)?);

    Ok((
        annotated_type,
        Ast::Assignment {
            lhs: Box::new(Ast::Var(name)),
            rhs,
            span: start..p.cursor,
        },
    ))
}

/// Parses an if statement with an optional else statement afterwards.
/// # Example
/// ```no
/// if predicate == true {
///     then_this()
/// } else {
///     else_this()
/// }
///
/// # or
/// if !predicate {
///     only_then()
/// }
/// #...
/// ```
fn if_else<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<Ast> {
    let start = p.cursor;
    let _ = p.consume(&Kind::KwIf)?;
    let predicate = Box::new(expression(p)?);
    let _ = p.consume(&Kind::LBrace)?;
    let then_ = Box::new(statement(p)?);
    let _ = p.consume(&Kind::RBrace)?;
    let mut else_ = Box::new(Ast::Nop);

    if p.at(Kind::KwElse) {
        let _ = p.consume(&Kind::KwElse)?;
        let _ = p.consume(&Kind::LBrace)?;
        else_ = Box::new(statement(p)?);
        let _ = p.consume(&Kind::RBrace)?;
    }

    Ok(Ast::If {
        predicate,
        then_,
        else_,
        span: start..p.cursor,
    })
}

fn annotation<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<Ast> {
    let start = p.cursor;
    p.consume(&Kind::At)?;
    let token = p.consume(&Kind::Ident)?;
    let name = p.text(&token).to_string();

    // Todo args
    // like @get "/"

    let end = p.cursor;
    p.consume(&Kind::Semicolon)?;

    Ok(Ast::Annotation {
        name,
        span: start..end,
        statement: Box::new(statement(p)?),
    })
}

/// A named function declaration is just
/// an identifier before the function expression
/// my_function [T](a: T) -> usize { ... }
pub fn function_declaration<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
) -> Result<(String, Ast)> {
    p.consume(&Kind::KwFn)?;

    let token = p.consume(&Kind::Ident)?;
    let name = p.text(&token).to_string();

    Ok((name, function_expression(p)?))
}
