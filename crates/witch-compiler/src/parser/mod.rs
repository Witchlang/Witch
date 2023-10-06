//! The parser module is responsible for turning our Token iterator into an Abstract Syntax Tree.
//! It does so using recursive descent, by combining multiple small parsing functions into larger chains.
//! Expression precendence is built using the Pratt parsing algorithm.

use std::{iter::Peekable, path::PathBuf};

use logos::Span;

mod lexer;
use lexer::Token;
use witch_runtime::value::Value;

use crate::types::Type;

use self::{
    lexer::{Kind, Lexer},
    r#type::type_literal,
};
pub mod ast;
mod expression;
mod statement;
mod r#type;

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T>((T, Span));

pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input: &'input str,
    tokens: Peekable<I>,
    cursor: usize,
}

impl<'input> Parser<'input, Lexer<'input>> {
    pub fn new(input: &'input str) -> Parser<'input, Lexer<'input>> {
        Parser {
            input,
            tokens: Lexer::new(input).peekable(),
            cursor: 0,
        }
    }

    /// Get the source text of a token.
    pub fn text(&self, token: &Token) -> &'input str {
        &self.input[token.span.start..token.span.end]
    }

    /// Look-ahead one token and see what kind of token it is.
    pub fn peek(&mut self) -> Kind {
        self.tokens
            .peek()
            .unwrap_or_else(|| panic!("no new token found at {}", self.cursor))
            .kind
            .clone()
    }

    /// Checks whether the next token is of a given kind
    pub fn at(&mut self, kind: Kind) -> bool {
        self.peek() == kind
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
            &token.kind, expected,
            "Expected to consume `{:?}`, but found `{:?}` at {}",
            expected, token.kind, self.cursor
        );
        self.cursor = token.span.end;
        token
    }

    pub fn file(&mut self) -> ast::Ast {
        statement::statement(self)
    }

    /// Parses idents and forward slashes into a path
    fn path(&mut self, mut path: PathBuf) -> PathBuf {
        match self.peek() {
            Kind::Ident => {
                let token = self.tokens.next().unwrap();
                let ident = self.text(&token);
                path.push(ident);
                self.path(path)
            }
            Kind::Slash => {
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
            (kind, _) if kind == expected => {
                tokens.push(self.tokens.next().unwrap());
                self.repeating(tokens, expected, separator)
            }
            (kind, Some(sep)) if &kind == sep => {
                self.consume(&kind);
                self.repeating(tokens, expected, separator)
            }
            _ => tokens,
        }
    }

    fn parse_expression(&mut self) -> ast::Ast {
        match self.peek() {
            lit @ Kind::String | lit @ Kind::Int | lit @ Kind::Float => {
                let txt = {
                    let token = self.tokens.next().unwrap();
                    self.text(&token)
                };

                match lit {
                    Kind::Int => ast::Ast::Value(Value::I32(txt.parse().expect("invalid int"))),
                    Kind::Float => ast::Ast::Value(Value::F32(txt.parse().expect("invalid float"))),
                    Kind::String => {
                        ast::Ast::Value(Value::String(txt[1..(txt.len() - 1)].to_string()))
                    }
                    _ => unreachable!(),
                }
            }
            _ => todo!(),
        }
    }
}
