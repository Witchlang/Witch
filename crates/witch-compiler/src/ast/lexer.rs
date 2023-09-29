//! The lexer module handles turning our source code into
//! a stream (vec) of Tokens.

use crate::ast::Span;
use chumsky::prelude::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Import,
    Null,
    Bool(bool),
    Int(i32),
    Float(f32),
    Bytes(Vec<u8>),
    Str(&'src str),
    Op(&'src str),
    Ctrl(&'src str),
    Ident(&'src str),
    Mod,
    Fn,
    Type,
    Struct,
    Enum,
    Interface,
    Let,
    If,
    Else,
    Return,
    Loop,
    While,
    Break,
    Continue,
    Where,
}

// impl<'src> fmt::Display for Token<'src> {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             Token::Import => write!(f, "import"),
//             Token::Null => write!(f, "null"),
//             Token::Bytes(_x) => write!(f, "[byte(s)]"),
//             Token::Bool(x) => write!(f, "{}", x),
//             Token::Int(n) => write!(f, "{}", n),
//             Token::Float(n) => write!(f, "{}", n),
//             Token::Str(s) => write!(f, "{}", s),
//             Token::Op(s) => write!(f, "{}", s),
//             Token::Ctrl(c) => write!(f, "{}", c),
//             Token::Ident(s) => write!(f, "{}", s),
//             Token::Mod => write!(f, "mod"),
//             Token::Fn => write!(f, "fn"),
//             Token::Type => write!(f, "type"),
//             Token::Struct => write!(f, "struct"),
//             Token::Enum => write!(f, "enum"),
//             Token::Interface => write!(f, "interface"),
//             Token::Let => write!(f, "let"),
//             Token::If => write!(f, "if"),
//             Token::Else => write!(f, "else"),
//             Token::Return => write!(f, "return"),
//             Token::Loop => write!(f, "loop"),
//             Token::While => write!(f, "while"),
//             Token::Break => write!(f, "break"),
//             Token::Continue => write!(f, "continue"),
//             Token::Where => write!(f, "where"),
//         }
//     }
// }

/// Returns a `Parser` which takes our source input and turns it into a vec of `Token`s.
pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    // A parser for numbers.
    // Normal numbers turn into Int, while numbers with fractions turn into Floats.
    // Example: 10, 10.5.
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .slice()
        .from_str()
        .unwrapped()
        .map(|float: f32| {
            if float.fract() == 0.0 {
                Token::Int(float as i32)
            } else {
                Token::Float(float)
            }
        });

    // A parser for hex encoded bytes.
    // Example: \x00fa80 or 0xd34db33f.
    let bytes = (just('0').or(just('\\')).ignore_then(just('x')).ignore_then(
        any::<_, extra::Err<Rich<'src, char, Span>>>()
            .filter(|c: &char| c.is_ascii_hexdigit())
            .repeated()
            .exactly(2)
            .collect::<String>()
            .map(|s| match hex::decode(s).unwrap_or_default() {
                byte if byte.len() == 1 => byte[0],
                _ => 0,
            }),
    ))
    .repeated()
    .at_least(1)
    .collect::<Vec<u8>>()
    .map(Token::Bytes);

    // A parser for strings.
    // Example: "I am a string".
    let str_ = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .map_slice(Token::Str);

    // A parser for operators
    let op = one_of("+*-/!=<>")
        .repeated()
        .at_least(1)
        .map_slice(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{};,:.|").map_slice(Token::Ctrl);

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident| match ident {
        "import" => Token::Import,
        "fn" => Token::Fn,
        "type" => Token::Type,
        "struct" => Token::Struct,
        "enum" => Token::Enum,
        "interface" => Token::Interface,
        "let" => Token::Let,
        "if" => Token::If,
        "else" => Token::Else,
        "mod" => Token::Mod,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "null" => Token::Null,
        "return" => Token::Return,
        "loop" => Token::Loop,
        "while" => Token::While,
        "break" => Token::Break,
        "continue" => Token::Continue,
        "where" => Token::Where,
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = bytes.or(num).or(str_).or(op).or(ctrl).or(ident);

    // Comments start with // and end in a newline.
    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
