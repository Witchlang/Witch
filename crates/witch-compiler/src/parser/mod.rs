//! The parser module is responsible for turning our Token iterator into an Abstract Syntax Tree.
//! It does so using recursive descent, by combining multiple small parsing functions into larger chains.
//! Expression precendence is built using the Pratt parsing algorithm.

use std::{iter::Peekable, path::PathBuf};

use logos::Span;

mod lexer;
use lexer::Token;


use crate::types::Type;

use self::{
    ast::Ast,
    lexer::{Kind, Lexer},
    r#type::type_literal,
};
pub mod ast;
mod expression;
mod statement;
mod r#type;

pub enum Error {
    UnexpectedEOF,
    UnexpectedToken(Kind, Kind, usize),
}

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

    pub fn at_type_literal(&mut self) -> bool {
        let forward_input = &self.input[self.cursor..];
        let mut tmp = Self::new(forward_input);
        !matches!(type_literal(&mut tmp), Type::Unknown)
    }

    /// Move forward one token in the input and check
    /// that we pass the kind of token we expect.
    pub fn consume(&mut self, expected: &Kind) -> Token {
        let token = self.tokens.next().unwrap_or_else(|| {
            panic!(
                "Expected to consume `{:?}`, but there was no next token",
                expected
            )
        });
        assert_eq!(
            &token.kind,
            expected,
            "Expected to consume `{:?}`, but found `{:?}` at {}. around: {}",
            expected,
            token.kind,
            self.cursor,
            &self.input[self.cursor - 10..self.cursor + 10]
        );
        self.cursor = token.span.end;
        self.read_count += 1;
        token
    }

    /// Move forward one token in the input and check
    /// that we pass the kind of token we expect, but only if we know its there.
    pub fn try_consume(&mut self, expected: &Kind) -> Option<Token> {
        if self.at(expected.to_owned()) {
            return Some(self.consume(expected));
        }

        None
    }

    pub fn file(&mut self) -> ast::Ast {
        statement::statement(self)
    }

    /// Parses idents and forward slashes into a path
    fn path(&mut self, mut path: PathBuf) -> PathBuf {
        match self.peek() {
            Some(Kind::Ident) => {
                let token = self.consume(&Kind::Ident);
                let ident = self.text(&token);
                path.push(ident);
                self.path(path)
            }
            Some(Kind::Slash) => {
                self.consume(&Kind::Slash);
                self.path(path)
            }
            _ => path,
        }
    }

    /// Repeats a token until we peek something else. Optionally takes a separator between each repetition.
    fn repeating(
        &mut self,
        mut tokens: Vec<Token>,
        expected: Kind,
        separator: Option<Kind>,
    ) -> Vec<Token> {
        match (self.peek(), &separator) {
            (Some(kind), _) if kind == expected => {
                tokens.push(self.consume(&kind));
                self.repeating(tokens, expected, separator)
            }
            (Some(kind), Some(sep)) if &kind == sep => {
                self.consume(&kind);
                self.repeating(tokens, expected, separator)
            }
            _ => tokens,
        }
    }
}

pub fn either<'input>(
    p: &mut Parser<'input, Lexer<'input>>,
    choices: Vec<impl Fn(&mut Parser<'input, Lexer<'input>>) -> Option<Ast>>,
) -> Ast {
    for f in choices.into_iter() {
        let mut fork = p.fork();
        if let Some(ast) = f(&mut fork) {
            *p = fork;
            return ast;
        }
    }
    panic!("expected one of the supplied choices to parse");
}

#[test]
fn forked_parser() {
    let mut p = Parser::new("some input, () = <> *");
    p.consume(&Kind::Ident);
    p.consume(&Kind::Ident);
    p.consume(&Kind::Comma);
    let mut fork = p.fork();

    assert_eq!(p.peek(), fork.peek());
}
