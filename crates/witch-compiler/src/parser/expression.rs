use crate::types::Type;
use std::collections::HashMap;
use witch_runtime::value::Value;

use super::{
    ast::Ast,
    lexer::{Kind, Lexer},
    r#type::type_literal,
    statement::statement,
    Parser,
};

pub fn inline_expression<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Ast {
    match p.peek() {
        lit @ Kind::Int | lit @ Kind::String | lit @ Kind::Float => {
            let token = p.consume(&lit);
            let txt = p.text(&token);
            return match lit {
                Kind::Int => Ast::Value(Value::Usize(txt.parse().expect("invalid integer"))),
                Kind::Float => Ast::Value(Value::F32(txt.parse().expect("invalid 32bit float"))),
                Kind::String => Ast::Value(Value::String(txt.to_string())),
                _ => unreachable!(),
            };
        }
        Kind::Ident => {
            let token = p.consume(&Kind::Ident);
            let varname = p.text(&token);
            let var = Ast::Var(varname.to_string());

            // Variables can be called as functions
            if p.at(Kind::LParen) {
                return function_call(p, Box::new(var));
            }
            return var;
        }
        Kind::LParen => {
            // A parenthesized expression
            p.consume(&Kind::LParen);
            let expr = inline_expression(p);
            p.consume(&Kind::RParen);

            // Parenthesized expressions can be called as functions
            // (IIFEs)
            if p.at(Kind::LParen) {
                return function_call(p, Box::new(expr));
            }
            return expr;
        }
        x => todo!("todo expression {:?} at {}", x, p.cursor),
    }
    Ast::Nop
}

fn function_call<'input>(p: &mut Parser<'input, Lexer<'input>>, expr: Box<Ast>) -> Ast {
    let mut args = vec![];
    let start = p.cursor;
    p.consume(&Kind::LParen);
    while !p.at(Kind::RParen) {
        let arg = inline_expression(p);
        args.push(arg);
        if p.at(Kind::Comma) {
            p.consume(&Kind::Comma);
        }
    }
    p.consume(&Kind::RParen);
    Ast::Call {
        expr,
        args,
        span: start..p.cursor,
    }
}

pub fn function_expression<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Ast {
    // Possibly type variables
    // <T, U>
    let type_vars = if let Kind::LAngle = p.peek() {
        p.consume(&Kind::LAngle);
        let vars = p.repeating(vec![], Kind::Ident, Some(Kind::Comma));
        p.consume(&Kind::RAngle);
        vars.iter()
            .map(|t| p.text(t).to_string())
            .collect::<Vec<String>>()
    } else {
        vec![]
    };

    p.consume(&Kind::LParen);
    let args = list_args(p, vec![]);
    let mut is_variadic = false;
    if p.at(Kind::DotDotDot) {
        p.consume(&Kind::DotDotDot);
        is_variadic = true;
    }
    p.consume(&Kind::RParen);

    p.consume(&Kind::Arrow);

    let mut returns = Type::Any;
    let body = if p.at_type_literal() {
        returns = type_literal(p);
        p.consume(&Kind::LBrace);
        let stmt = statement(p);
        p.consume(&Kind::RBrace);
        stmt
    } else {
        inline_expression(p)
    };

    Ast::Function {
        args,
        returns,
        body: Box::new(body),
        is_variadic,
    }
}

fn list_args<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    mut args: Vec<(String, Type)>,
) -> Vec<(String, Type)> {
    if p.at(Kind::RParen) {
        return args;
    }

    let token = p.consume(&Kind::Ident);
    let name = p.text(&token).to_string();

    match p.peek() {
        Kind::Colon => {
            p.consume(&Kind::Colon);
            let ty = type_literal(p);
            args.push((name, ty));
            return list_args(p, args);
        }
        Kind::Comma => {
            p.consume(&Kind::Comma);
            return list_args(p, args);
        }
        _ => args,
    }
}
