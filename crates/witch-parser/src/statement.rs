use std::path::PathBuf;

use crate::error::Result;
use crate::lexer::{Kind, Lexer};

use crate::ast::Ast;
use crate::r#type::{enum_declaration, interface_declaration, struct_declaration};
use crate::types::Type;

use super::expression::{expression, function_expression};
use super::r#type::type_literal;
use super::Parser;

pub fn statement<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<Ast> {
    let start = p.cursor;
    let stmt = match p.peek() {
        Some(Kind::RBrace) => Ast::Nop,
        Some(Kind::KwImport) => {
            let token = p.consume(&Kind::KwImport)?;
            let stmt = Ast::Import {
                path: Box::new(PathBuf::from(p.text(&token))),
                span: token.span,
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
