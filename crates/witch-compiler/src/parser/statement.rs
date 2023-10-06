use std::path::PathBuf;

use crate::parser::lexer::{Kind, Lexer};

use crate::parser::ast::Ast;
use crate::parser::r#type::{enum_declaration, interface_declaration, struct_declaration};

use super::expression::{function_expression, inline_expression};
use super::Parser;

pub fn statement<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Ast {
    let start = p.cursor;
    match p.peek() {
        Kind::KwImport => {
            let token = p.tokens.next().unwrap();
            let stmt = Ast::Import {
                path: Box::new(PathBuf::from(p.text(&token))),
                span: token.span,
            };
            let end = p.cursor;
            Ast::Statement {
                stmt: Box::new(stmt),
                rest: Box::new(statement(p)),
                span: start..end,
            }
        }
        Kind::KwEnum => {
            let enum_decl = enum_declaration(p);
            let end = p.cursor;
            Ast::Statement {
                stmt: Box::new(enum_decl),
                rest: Box::new(statement(p)),
                span: start..end,
            }
        }
        Kind::KwInterface => {
            let interface_decl = interface_declaration(p);
            let end = p.cursor;
            Ast::Statement {
                stmt: Box::new(interface_decl),
                rest: Box::new(statement(p)),
                span: start..end,
            }
        }
        Kind::KwStruct => {
            let struct_decl = struct_declaration(p);
            let end = p.cursor;
            Ast::Statement {
                stmt: Box::new(struct_decl),
                rest: Box::new(statement(p)),
                span: start..end,
            }
        }
        Kind::KwReturn => {
            p.consume(&Kind::KwReturn);
            let expr = Box::new(inline_expression(p));
            Ast::Return {
                expr,
                span: start..p.cursor,
            }
        }
        Kind::At => annotation(p),
        kind => {
            let token = p.tokens.next().unwrap();
            todo!(
                "{:?} not yet implemented: {} at {}",
                kind,
                p.text(&token),
                p.cursor
            )
        }
    }
}

fn annotation<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Ast {
    let start = p.cursor;
    p.consume(&Kind::At);
    let token = p.consume(&Kind::Ident);
    let name = p.text(&token).to_string();

    // Todo args
    // like @get "/"

    let end = p.cursor;
    p.consume(&Kind::Semicolon);

    Ast::Annotation {
        name,
        span: start..end,
        statement: Box::new(statement(p)),
    }
}

/// A named function declaration
pub fn function_declaration<'input>(p: &mut Parser<'input, Lexer<'input>>) -> (String, Ast) {
    p.consume(&Kind::KwFn);

    let token = p.consume(&Kind::Ident);
    let name = p.text(&token).to_string();

    (name, function_expression(p))
}
