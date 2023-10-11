use std::path::PathBuf;

use crate::error::Result;
use crate::parser::lexer::{Kind, Lexer};

use crate::parser::ast::Ast;
use crate::parser::r#type::{enum_declaration, interface_declaration, struct_declaration};

use super::expression::{expression, function_expression};
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
        Some(Kind::KwLet) => {
            p.consume(&Kind::KwLet);
            let (ident, expr) = assignment(p)?;
            p.consume(&Kind::Semicolon);
            let end = p.cursor;
            let assignment = Ast::Let {
                ident,
                expr: Box::new(expr),
                span: start..end,
            };
            Ast::Statement {
                stmt: Box::new(assignment),
                rest: Box::new(statement(p)?),
                span: start..end,
            }
        }
        Some(Kind::KwReturn) => {
            p.consume(&Kind::KwReturn);
            let expr = Box::new(expression(p)?);
            Ast::Return {
                expr,
                span: start..p.cursor,
            }
        }
        Some(Kind::KwFn) => {
            let (ident, expr) = function_declaration(p)?;
            let end = p.cursor;
            let assignment = Ast::Let {
                ident,
                expr: Box::new(expr),
                span: start..end,
            };
            Ast::Statement {
                stmt: Box::new(assignment),
                rest: Box::new(statement(p)?),
                span: start..end,
            }
        }
        Some(Kind::At) => annotation(p)?,
        Some(_) => {
            let expr = expression(p)?;
            let end = p.cursor;
            Ast::Statement {
                stmt: Box::new(expr),
                rest: Box::new(statement(p)?),
                span: start..end,
            }
        }
        kind => {
            let token = p.tokens.next().unwrap();
            todo!(
                "{:?} not yet implemented: {} at {}",
                kind,
                p.text(&token),
                p.cursor
            )
        }
    };
    if p.at(Kind::Semicolon) {
        p.consume(&Kind::Semicolon)?;
    }
    Ok(stmt)
}

fn assignment<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<(String, Ast)> {
    let start = p.cursor;
    let token = p.consume(&Kind::Ident)?;
    let ident = p.text(&token).to_string();

    p.consume(&Kind::Eq)?;

    let expr = Box::new(expression(p)?);

    Ok((
        ident.clone(),
        Ast::Assignment {
            ident,
            expr,
            span: start..p.cursor,
        },
    ))
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

/// A named function declaration
pub fn function_declaration<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
) -> Result<(String, Ast)> {
    p.consume(&Kind::KwFn);

    let token = p.consume(&Kind::Ident)?;
    let name = p.text(&token).to_string();

    Ok((name, function_expression(p)?))
}
