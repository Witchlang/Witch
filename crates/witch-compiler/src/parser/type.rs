use std::collections::HashMap;

use crate::error::{Error, Result};
use crate::parser::lexer::{Kind, Lexer};

use crate::parser::ast::Ast;
use crate::types::TypeDecl;
use crate::types::{EnumVariant, Type};

use super::expression::where_constraints;
use super::statement::function_declaration;
use super::Parser;
/// A struct declaration:
/// ## Example
/// ```no
/// struct Foo<I> where I: Iterator<String> {
///   field: Bar<Baz>
///   field2: string
///
///   function method(a: I, b: string) -> i32 {
///     return a.map((s) -> s + b)
///   }
/// }
/// ```
pub fn struct_declaration<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<Ast> {
    let start = p.cursor;
    p.consume(&Kind::KwStruct)?;

    // Struct name
    let token = p.consume(&Kind::Ident)?;
    let name = p.text(&token).to_string();

    // Possibly type variables
    // <T, U>
    let type_vars = if let Some(Kind::LAngle) = p.peek() {
        p.consume(&Kind::LAngle)?;
        let vars = p.repeating(vec![], Kind::Ident, Some(Kind::Comma))?;
        p.consume(&Kind::RAngle)?;
        vars.iter()
            .map(|t| p.text(t).to_string())
            .collect::<Vec<String>>()
    } else {
        vec![]
    };

    // Possibly constraints for the type variables
    let constraints = vec![];
    if p.at(Kind::KwWhere) {
        todo!("interface generic constraints not yet implemented");
    }

    let mut generics = HashMap::default();
    for v in type_vars.into_iter() {
        generics.insert(v, Type::Any);
    }
    for (k, v) in constraints.into_iter() {
        generics.entry(k).and_modify(|e| *e = v);
    }

    // Start the block
    p.consume(&Kind::LBrace)?;

    let fields = properties(p, Kind::Semicolon, HashMap::default())?;

    let mut methods = vec![];
    while p.at(Kind::KwFn) {
        methods.push(function_declaration(p)?);
    }

    // End block
    p.consume(&Kind::RBrace)?;

    Ok(Ast::Type {
        name,
        decl: TypeDecl::Struct {
            generics,
            fields,
            methods,
        },
        span: (start..p.cursor),
    })
}

/// Parses an interface declaration:
/// ## Example
/// ```no
/// interface MyInterface<T,U> where T: Iterator {
///   field: Foo<T>,
///   method: (String) -> i32;
/// }
/// ```
pub fn interface_declaration<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<Ast> {
    let start = p.cursor;
    p.consume(&Kind::KwInterface)?;

    // Interface name
    let token = p.consume(&Kind::Ident)?;
    let name = p.text(&token).to_string();

    // Possibly type variables
    // <T, U>
    let type_vars = if let Some(Kind::LAngle) = p.peek() {
        p.consume(&Kind::LAngle)?;
        let vars = p.repeating(vec![], Kind::Ident, Some(Kind::Comma))?;
        p.consume(&Kind::RAngle)?;
        vars.iter()
            .map(|t| p.text(t).to_string())
            .collect::<Vec<String>>()
    } else {
        vec![]
    };

    // Possibly constraints for the type variables
    let constraints = vec![];
    if p.at(Kind::KwWhere) {
        todo!("interface generic constraints not yet implemented");
    }

    let mut generics = HashMap::default();
    for v in type_vars.into_iter() {
        generics.insert(v, Type::Any);
    }
    for (k, v) in constraints.into_iter() {
        generics.entry(k).and_modify(|e| *e = v);
    }

    // Start the block
    p.consume(&Kind::LBrace)?;

    let properties = properties(p, Kind::Semicolon, HashMap::default())?;

    // End block
    p.consume(&Kind::RBrace)?;

    Ok(Ast::Type {
        name,
        decl: TypeDecl::Interface {
            generics,
            properties,
        },
        span: (start..p.cursor),
    })
}

/// A list of Ident -> Type bindings.
/// ## Example
/// ```no
/// field: String
/// method: (i32, i32) -> i32
pub fn properties<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    separator: Kind,
    mut properties_: HashMap<String, Type>,
) -> Result<HashMap<String, Type>> {
    let token = p.consume(&Kind::Ident)?;
    let name = p.text(&token).to_string();
    p.consume(&Kind::Colon)?;
    let ty = type_literal(p)?;
    properties_.insert(name, ty);
    let res = match p.peek() {
        Some(next) if next == separator => {
            p.consume(&Kind::Semicolon)?;
            if p.at(Kind::Ident) {
                properties(p, separator, properties_)?
            } else {
                properties_
            }
        }
        _ => properties_,
    };
    Ok(res)
}

/// Parses an enum declaration:
/// enum MyEnum<T,U> where T: Iterator {
///   One(T),
///   Two,
///   Three(U)
/// }
///
pub fn enum_declaration<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<Ast> {
    let start = p.cursor;
    p.consume(&Kind::KwEnum)?;

    // Enum name
    let token = p.consume(&Kind::Ident)?;
    let name = p.text(&token).to_string();

    // Possibly type variables
    // <T, U>
    let type_vars = if let Some(Kind::LAngle) = p.peek() {
        p.consume(&Kind::LAngle)?;
        let vars = p.repeating(vec![], Kind::Ident, Some(Kind::Comma))?;
        p.consume(&Kind::RAngle)?;
        vars.iter()
            .map(|t| p.text(t).to_string())
            .collect::<Vec<String>>()
    } else {
        vec![]
    };

    // Possibly constraints for the type variables
    let constraints = where_constraints(p)?;

    let mut generics = HashMap::default();
    for v in type_vars.into_iter() {
        generics.insert(v, Type::Any);
    }
    for (k, v) in constraints.into_iter() {
        generics.entry(k).and_modify(|e| *e = v);
    }

    // Start the block
    p.consume(&Kind::LBrace)?;

    // List variants
    let variants = enum_variants(p, vec![])?;

    // Last variant may have an automatic semicolon. Disregard it.
    if p.at(Kind::Semicolon) {
        p.consume(&Kind::Semicolon)?;
    }

    // End block
    p.consume(&Kind::RBrace)?;

    Ok(Ast::Type {
        name,
        decl: TypeDecl::Enum { generics, variants },
        span: (start..p.cursor),
    })
}

/// A comma-separated list of types.
/// ## Example
/// ```no
/// T, Foo<Bar<Baz>>, i32, List<String>
/// ```
fn list_types<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    mut types: Vec<Type>,
) -> Result<Vec<Type>> {
    let types = match p.peek() {
        Some(Kind::Comma) => {
            p.consume(&Kind::Comma)?;
            list_types(p, types)?
        }
        Some(Kind::Ident | Kind::LParen) => {
            types.push(type_literal(p)?);
            types
        }
        _ => types,
    };
    Ok(types)
}

/// A single type literal.
/// ## Example
/// ```no
/// i32
/// <T>(T) -> Void
/// (String, i32, Foo<Bar>) -> []String
/// String
/// []i32
/// Foo<Bar<Baz>, i32>
/// Iterator + SomeInterface
/// ```
pub fn type_literal<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<Type> {
    let ty = match p.peek() {
        Some(Kind::Ident) => {
            let token = p.consume(&Kind::Ident)?;
            let ident = p.text(&token);
            let mut inner = vec![];

            if p.at(Kind::LAngle) {
                p.consume(&Kind::LAngle)?;
                inner = list_types(p, inner)?;
                p.consume(&Kind::RAngle)?;
            }

            Type::from_str(ident, inner)
        }
        Some(Kind::LSquare) => {
            p.consume(&Kind::LSquare)?;
            p.consume(&Kind::RSquare)?;
            Type::List(Box::new(type_literal(p)?))
        }
        Some(Kind::LParen | Kind::LAngle) => function_signature(p)?,
        kind => {
            return Err(Error::new(
                &format!("Unknown type error. Expected type literal, got: {:?}", kind),
                p.cursor..p.cursor,
                p.input,
            ))
        }
    };

    // If we encounter a plus sign, its an Intersection type.
    // We recursively flatten it to a topmost Intersection.
    if p.at(Kind::Plus) {
        p.consume(&Kind::Plus)?;
        let mut types = vec![ty];
        match type_literal(p)? {
            Type::Intersection(ref mut t) => {
                types.append(t);
            }
            t => types.push(t),
        }
        return Ok(Type::Intersection(types));
    }
    Ok(ty)
}

/// A function signature.
/// ## Example
/// ```no
/// <I>(string, I, i32...) where I: Iterator -> void
/// () -> String
/// ```
fn function_signature<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<Type> {
    let type_vars = if let Some(Kind::LAngle) = p.peek() {
        p.consume(&Kind::LAngle)?;
        let vars = p.repeating(vec![], Kind::Ident, Some(Kind::Comma))?;
        p.consume(&Kind::RAngle)?;
        vars.iter()
            .map(|t| p.text(t).to_string())
            .collect::<Vec<String>>()
    } else {
        vec![]
    };

    p.consume(&Kind::LParen)?;
    let args = list_types(p, vec![])?;
    let mut is_variadic = false;
    if p.at(Kind::DotDotDot) {
        p.consume(&Kind::DotDotDot)?;
        is_variadic = true;
    }

    p.consume(&Kind::RParen)?;
    p.consume(&Kind::Arrow)?;
    let returns = Box::new(type_literal(p)?);

    // Possibly constraints for the type variables
    let constraints = vec![];
    if p.at(Kind::KwWhere) {
        todo!("function signatures generic constraints not yet implemented");
    }

    let mut generics = HashMap::default();
    for v in type_vars.into_iter() {
        generics.insert(v, Type::Any);
    }
    for (k, v) in constraints.into_iter() {
        generics.entry(k).and_modify(|e| *e = v);
    }

    Ok(Type::Function {
        args,
        returns,
        is_variadic,
        generics,
    })
}

/// Parses a comma separated list of enum variants. Allows trailing comma.
/// ## Example
/// ```no
/// None,
/// Some(T),
/// Variant(String, i32, Foo<Bar<Baz>>)
/// ```
pub fn enum_variants<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    mut variants: Vec<EnumVariant>,
) -> Result<Vec<EnumVariant>> {
    match p.peek() {
        Some(Kind::Ident) => {
            let token = p.consume(&Kind::Ident)?;
            let name = p.text(&token).to_string();
            match p.peek() {
                Some(Kind::Comma) => {
                    p.consume(&Kind::Comma)?;
                    enum_variants(p, variants)
                }
                Some(Kind::LParen) => {
                    p.consume(&Kind::LParen)?;
                    let types = list_types(p, vec![])?;
                    variants.push(EnumVariant {
                        name,
                        discriminant: variants.len(),
                        types: Some(types),
                    });
                    p.consume(&Kind::RParen)?;
                    enum_variants(p, variants)
                }
                _ => Ok(variants),
            }
        }
        Some(Kind::Comma) => {
            p.consume(&Kind::Comma)?;
            enum_variants(p, variants)
        }
        _ => Ok(variants),
    }
}
