use crate::{
    error::Result,
    expression::expression,
    lexer::{Kind, Lexer},
    statement::statement,
    Ast, Parser,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    // [x, y, z, rest...]
    List(Vec<Pattern>),

    // Constructor(Constructor, Vec<Pattern>),

    // _
    Ignored,
}

pub fn cases<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    mut list: Vec<(Pattern, Ast)>,
) -> Result<Vec<(Pattern, Ast)>> {
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

    list.push((pattern, ast));

    if p.at(Kind::Comma) {
        list.append(&mut cases(p, list.clone())?);
    }

    Ok(list)
}

pub fn pattern<'input>(p: &mut Parser<'input, Lexer<'input>>) -> Result<Pattern> {
    p.consume(&Kind::Under)?;

    Ok(Pattern::Ignored)
}
