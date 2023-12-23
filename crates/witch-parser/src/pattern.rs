use crate::{
    error::{Error, Result},
    expression::expression,
    lexer::{Kind, Lexer},
    statement::statement,
    Ast, Parser,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    // [x, y, z, rest...] -> {}
    List(Vec<Pattern>),

    Bool(bool),

    String(String),

    Number(usize),

    // FooBar(42, x, 11)
    Variant(String, Vec<Pattern>),

    // A variable pattern, e.g. var -> {}
    Variable(String),

    // _ -> {}
    Empty,
}

/// A single case (or row) in a match expression/table.
#[derive(Clone, PartialEq, Debug)]
pub struct Case {
    pub pattern: Pattern,
    pub guard: Option<Ast>,
    pub body: Ast,
    pub bindings: Vec<String>,
}

impl Case {
    fn new(pattern: Pattern, guard: Option<Ast>, body: Ast) -> Self {
        Self {
            pattern,
            guard,
            body,
            bindings: vec![],
        }
    }
}

pub fn cases<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    mut list: Vec<Case>,
) -> Result<Vec<Case>> {
    let pattern = pattern(p)?;
    p.consume(&Kind::Arrow)?;

    let ast = if p.at(Kind::LBrace) {
        p.consume(&Kind::LBrace)?;
        let stmt = statement(p)?;
        p.consume(&Kind::RBrace)?;
        stmt
    } else {
        expression(p)?
    };

    list.push(Case::new(pattern, None, ast));

    if !p.at(Kind::RBrace) {
        return cases(p, list.clone());
    }

    Ok(list)
}

pub fn pattern<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<Pattern> {
    match p.peek() {
        // Literal strings and numbers
        Some(lit @ Kind::Int | lit @ Kind::String) => {
            let token = p.consume(&lit)?;
            let txt = p.text(&token);
            match lit {
                Kind::Int => Ok(Pattern::Number(txt.parse().unwrap())),
                Kind::String => Ok(Pattern::String(txt.to_string())),
                _ => unreachable!(),
            }
        }

        // Discarded pattern
        Some(Kind::Under) => {
            p.consume(&Kind::Under)?;
            Ok(Pattern::Empty)
        }

        // Idents can mean a number of different things:
        // True/False, variables, Variant
        Some(Kind::Ident) => {
            let token = p.consume(&Kind::Ident)?;
            let txt = p.text(&token);
            match txt.chars().next().unwrap().is_lowercase() {
                true => Ok(Pattern::Variable(p.text(&token).to_string())),
                false => match txt {
                    "True" => Ok(Pattern::Bool(true)),
                    "False" => Ok(Pattern::Bool(false)),
                    name => {
                        let mut fields = vec![];
                        if p.at(Kind::LParen) {
                            p.consume(&Kind::LParen)?;
                            fields = patterns(p, vec![])?;
                            p.consume(&Kind::RParen)?;
                        }
                        Ok(Pattern::Variant(name.to_string(), fields))
                    }
                },
            }
        }

        // An invalid pattern
        _ => Err(Error::fatal()),
    }
}

pub fn patterns<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    mut list: Vec<Pattern>,
) -> Result<Vec<Pattern>> {
    list.push(pattern(p)?);

    if p.at(Kind::Comma) {
        p.consume(&Kind::Comma)?;
        return patterns(p, list);
    }

    Ok(list)
}
