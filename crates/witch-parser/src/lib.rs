//! The parser module is responsible for turning our Token iterator into an Abstract Syntax Tree.
//! It does so using recursive descent, by combining multiple small parsing functions into larger chains.
//! Expression precendence is built using the Pratt parsing algorithm.
#![feature(type_alias_impl_trait)]
#![feature(iter_advance_by)]
#![feature(assert_matches)]
use std::iter::Peekable;

use logos::Span;

mod lexer;
use lexer::Token;

use crate::error::{Error, Result};
use crate::types::Type;

use self::lexer::{Kind, Lexer};
pub mod ast;
pub use ast::Ast;
mod expression;
mod statement;
mod r#type;
pub mod types;
mod error;

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T>((T, Span));

pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input: &'input str,
    tokens: Peekable<I>,
    read_count: usize,
    cursor: usize,
}

impl<'input> Parser<'input, Lexer<'input>> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            tokens: Lexer::new(input).peekable(),
            read_count: 0,
            cursor: 0,
        }
    }

    pub fn fork(&mut self) -> Self {
        let mut fork = Self {
            input: self.input,
            tokens: Lexer::new(self.input).peekable(),
            read_count: self.read_count,
            cursor: self.cursor,
        };
        let _ = fork.tokens.advance_by(self.read_count);
        assert_eq!(&self.peek(), &fork.peek());
        fork
    }

    /// Get the source text of a token.
    pub fn text(&self, token: &Token) -> &'input str {
        &self.input[token.span.start..token.span.end]
    }

    /// Look-ahead one token and see what kind of token it is.
    pub fn peek(&mut self) -> Option<Kind> {
        self.tokens.peek().map(|t| t.kind.clone())
    }

    /// Checks whether the next token is of a given kind
    pub fn at(&mut self, kind: Kind) -> bool {
        self.peek() == Some(kind)
    }

    // pub fn at_type_literal(&mut self) -> bool {
    //     let forward_input = &self.input[self.cursor..];
    //     let mut tmp = Self::new(forward_input);
    //     !matches!(type_literal(&mut tmp)?, Type::Unknown)
    // }

    /// Move forward one token in the input and check
    /// that we pass the kind of token we expect.
    pub fn consume(&mut self, expected: &Kind) -> Result<Token> {
        let token = self.tokens.next().ok_or(Error::new(
            &format!("Unexpected end of input. Expected: {:?}", expected),
            self.cursor..self.cursor,
            self.input,
        ))?;

        if &token.kind != expected {
            return Err(Error::new(
                &format!(
                    "Unexpected token. Expected {:?}, got {:?}",
                    expected, token.kind
                ),
                self.cursor..self.cursor,
                self.input,
            ));
        }

        self.cursor = token.span.end;
        self.read_count += 1;
        Ok(token)
    }

    // /// Move forward one token in the input and check
    // /// that we pass the kind of token we expect, but only if we know its there.
    // pub fn try_consume(&mut self, expected: &Kind) -> Option<Token> {
    //     if self.at(expected.to_owned()) {
    //         return Some(self.consume(expected));
    //     }

    //     None
    // }

    pub fn file(&mut self) -> Result<ast::Ast> {
        statement::statement(self)
    }

    /// Repeats a token until we peek something else. Optionally takes a separator between each repetition.
    fn repeating(
        &mut self,
        mut tokens: Vec<Token>,
        expected: Kind,
        separator: Option<Kind>,
    ) -> Result<Vec<Token>> {
        match (self.peek(), &separator) {
            (Some(kind), _) if kind == expected => {
                tokens.push(self.consume(&kind)?);
                self.repeating(tokens, expected, separator)
            }
            (Some(kind), Some(sep)) if &kind == sep => {
                self.consume(&kind)?;
                self.repeating(tokens, expected, separator)
            }
            _ => Ok(tokens),
        }
    }
}

pub fn either<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    choices: Vec<impl Fn(&mut Parser<'input, Lexer<'input>>) -> Result<Ast>>,
) -> Result<Ast> {
    for f in choices.into_iter() {
        let mut fork = p.fork();
        if let Ok(ast) = f(&mut fork) {
            *p = fork;
            return Ok(ast);
        }
    }
    Err(Error::new(
        "expected one of the patterns to match",
        p.cursor..p.cursor,
        p.input,
    ))
}

pub fn maybe_type<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    f: impl Fn(&mut Parser<'input, Lexer<'input>>) -> Result<Type>,
) -> Result<Type> {
    let mut fork = p.fork();
    let res = f(&mut fork);
    if let Ok(ast) = res {
        *p = fork;
        return Ok(ast);
    }
    res
}

pub fn maybe<'input, T>(
    p: &mut Parser<'input, Lexer<'input>>,
    f: impl Fn(&mut Parser<'input, Lexer<'input>>) -> Result<T>,
) -> Result<T> {
    let mut fork = p.fork();
    let res = f(&mut fork);
    if let Ok(ast) = res {
        *p = fork;
        return Ok(ast);
    }
    res
}

#[test]
fn forked_parser() {
    let mut p = Parser::new("some input, () = <> *");
    let _ = p.consume(&Kind::Ident);
    let _ = p.consume(&Kind::Ident);
    let _ = p.consume(&Kind::Comma);
    let mut fork = p.fork();

    assert_eq!(p.peek(), fork.peek());
}
