use crate::types::Type;
use std::collections::HashMap;
use witch_runtime::value::Value;

use super::{
    ast::Ast,
    either,
    lexer::{Kind, Lexer},
    r#type::{properties, type_literal},
    statement::statement,
    Parser,
};

pub fn inline_expression<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Ast {
    let start = p.cursor;
    let expr = match p.peek() {
        Some(lit @ Kind::Int) | Some(lit @ Kind::String) | Some(lit @ Kind::Float) => {
            let token = p.consume(&lit);
            let txt = p.text(&token);
            match lit {
                Kind::Int => Ast::Value(Value::Usize(txt.parse().expect("invalid integer"))),
                Kind::Float => Ast::Value(Value::F32(txt.parse().expect("invalid 32bit float"))),
                Kind::String => Ast::Value(Value::String(txt.to_string())),
                _ => unreachable!(),
            }
        }
        Some(Kind::Ident) => {
            // An expression starting with an identifier can be
            // - A variable: my_var
            // - A struct: Foo { }

            let token = p.consume(&Kind::Ident);
            let ident = p.text(&token).to_string();

            if p.at(Kind::LBrace) {
                // Struct expression
                p.consume(&Kind::LBrace);
                let fields = map_values(p, HashMap::default());
                p.consume(&Kind::RBrace);

                Ast::Struct {
                    ident: Some(ident),
                    fields,
                    span: start..p.cursor,
                }
            } else {
                let var = Ast::Var(ident);

                // Variables can be called as functions
                if p.at(Kind::LParen) {
                    function_call(p, Box::new(var))
                } else {
                    var
                }
            }
        }
        Some(Kind::LParen) => {
            // An expression starting with a left paren can be
            // - A function expression: () -> {}
            // - A nested expression: (varname + 6 * (2 - 1))
            either(p, vec![try_function_expression, try_nested_expression])
        }
        x => panic!("invalid expression start: {:?} at {}", x, p.cursor),
    };

    member_or_func_call(p, expr)
}

/// Recursively resolve function calls or member access for the expression
pub fn member_or_func_call<'input>(p: &mut Parser<'input, Lexer<'input>>, expr: Ast) -> Ast {
    let start = p.cursor;

    match p.peek() {
        Some(Kind::Dot) => {
            p.consume(&Kind::Dot);
            let token = p.consume(&Kind::Ident);
            let key = p.text(&token).to_string();
            member_or_func_call(
                p,
                Ast::Member {
                    container: Box::new(expr),
                    key,
                    span: start..p.cursor,
                },
            )
        }

        // Any expression can be called
        Some(Kind::LParen) => {
            let fn_call = function_call(p, Box::new(expr));
            member_or_func_call(p, fn_call)
        }
        _ => expr,
    }
}

/// A map of Ident -> Expression bindings
/// ## Example
/// ```no
/// field: "Hello",
/// field_2: some_var
pub fn map_values<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    mut properties_: HashMap<String, Ast>,
) -> HashMap<String, Ast> {
    let token = p.consume(&Kind::Ident);
    let name = p.text(&token).to_string();

    let expr = if p.at(Kind::Colon) {
        p.consume(&Kind::Colon);
        inline_expression(p)
    } else {
        Ast::Var(name.clone())
    };

    properties_.insert(name, expr);

    let res = match p.peek() {
        Some(Kind::Comma) => {
            p.consume(&Kind::Comma);
            if p.at(Kind::Ident) {
                map_values(p, properties_)
            } else {
                properties_
            }
        }
        _ => properties_,
    };
    // May have an automatic semicolon. Disregard it.
    if p.at(Kind::Semicolon) {
        p.consume(&Kind::Semicolon);
    }

    res
}

fn try_nested_expression<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Option<Ast> {
    p.try_consume(&Kind::LParen)?;
    let expr = if p.at(Kind::RParen) {
        Ast::Nop
    } else {
        inline_expression(p)
    };
    p.consume(&Kind::RParen);
    Some(expr)
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

pub fn try_function_expression<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Option<Ast> {
    // Possibly type variables
    // <T, U>
    let type_vars = if let Some(Kind::LAngle) = p.peek() {
        p.consume(&Kind::LAngle);
        let vars = p.repeating(vec![], Kind::Ident, Some(Kind::Comma));
        p.consume(&Kind::RAngle);
        vars.iter()
            .map(|t| p.text(t).to_string())
            .collect::<Vec<String>>()
    } else {
        vec![]
    };

    p.try_consume(&Kind::LParen)?;
    let args = try_list_args(p, vec![])?;
    let mut is_variadic = false;
    if p.at(Kind::DotDotDot) {
        p.consume(&Kind::DotDotDot);
        is_variadic = true;
    }
    p.try_consume(&Kind::RParen)?;

    let constraints = where_constraints(p);
    let mut generics = HashMap::default();
    for v in type_vars.into_iter() {
        generics.insert(v, Type::Any);
    }
    for (k, v) in constraints.into_iter() {
        generics.entry(k).and_modify(|e| *e = v);
    }

    p.try_consume(&Kind::Arrow)?;

    let mut returns = Type::Unknown;
    let body = if p.at_type_literal() {
        returns = type_literal(p);
        p.try_consume(&Kind::LBrace)?;

        let stmt = statement(p);
        p.try_consume(&Kind::RBrace)?;
        stmt
    } else {
        inline_expression(p)
    };

    Some(Ast::Function {
        args,
        returns,
        body: Box::new(body),
        is_variadic,
        generics,
    })
}

pub fn where_constraints<'input>(p: &mut Parser<'input, Lexer<'input>>) -> HashMap<String, Type> {
    let mut constraints = HashMap::default();
    if p.at(Kind::KwWhere) {
        p.consume(&Kind::KwWhere);
        constraints = properties(p, Kind::Comma, constraints);
    }

    constraints
}

#[test]
fn it_parses_function_expressions() {
    let mut p = Parser::new("() -> 1");
    let result = try_function_expression(&mut p);
    assert!(result.is_some());

    let mut p = Parser::new("(a, b, c) -> 1");
    let result = try_function_expression(&mut p);
    assert!(result.is_some());

    let mut p = Parser::new("(a: string, b: i32) -> i32 { return 1 }");
    let result = try_function_expression(&mut p);
    assert!(result.is_some());
}

fn try_list_args<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    mut args: Vec<(String, Type)>,
) -> Option<Vec<(String, Type)>> {
    if p.at(Kind::RParen) {
        return Some(args);
    }

    let token = p.try_consume(&Kind::Ident)?;
    let name = p.text(&token).to_string();

    let ty = if matches!(p.peek(), Some(Kind::Colon)) {
        p.consume(&Kind::Colon);
        type_literal(p)
    } else {
        Type::Unknown
    };

    args.push((name, ty));

    if matches!(p.peek(), Some(Kind::Comma)) {
        p.consume(&Kind::Comma);
        return try_list_args(p, args);
    }

    Some(args)
}
