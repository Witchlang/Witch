use std::iter::Peekable;

use logos::{Logos, Span, SpannedIter};

#[derive(Logos, Debug, Clone, PartialEq, Eq)]
#[logos(subpattern ident = r"[A-Za-z]([A-Za-z]|_|\d)*")]
pub enum Kind {
    #[token(".")]
    Dot,
    #[token("...")]
    DotDotDot,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("@")]
    At,
    #[token("%")]
    Percent,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Times,
    #[token("\n")]
    Newline,
    #[token("/")]
    Slash,
    #[token("\\")]
    BackSlash,
    #[token("|")]
    Pipe,
    #[token("^")]
    Pow,
    #[token("=")]
    Eq,
    #[token("!")]
    Bang,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("==")]
    Eqq,
    #[token("!=")]
    Neq,
    #[token("<=")]
    Lte,
    #[token(">=")]
    Gte,
    #[token("_")]
    Under,
    #[token("->")]
    Arrow,

    // Brackets
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    String,
    #[regex(r#"c"((\\"|\\\\)|[^\\"])*""#)]
    CString,
    #[regex(r"/\*([^*]|\**[^*/])*\*+/")]
    Comment,
    #[regex(r#"#[^\n]*\n"#)]
    LineComment,
    #[regex(r#"\d+"#, priority = 2)]
    Int,
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#)]
    Float,
    #[regex(r#"(?&ident)"#)]
    Ident,

    // Keywords
    #[token("let")]
    KwLet,
    #[token("new")]
    KwNew,
    #[regex(r#"import"#)]
    KwImport,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("function")]
    KwFn,
    #[token("struct")]
    KwStruct,
    #[token("enum")]
    KwEnum,
    #[token("where")]
    KwWhere,
    #[token("interface")]
    KwInterface,
    #[token("loop")]
    KwLoop,
    #[token("while")]
    KwWhile,
    #[token("for")]
    KwFor,
    #[token("in")]
    KwIn,
    #[token("break")]
    KwBreak,
    #[token("continue")]
    KwContinue,
    #[token("return")]
    KwReturn,
    #[token("yield")]
    KwYield,
    #[token("case")]
    KwCase,
    #[regex(r"[ \t\f]+", logos::skip)]
    #[regex(r"/\*([^*]|\*+[^*/])*\*?")] // unclosed comments == end
    #[end]
    End,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: Kind,
    pub span: Span,
}

pub struct Lexer<'input> {
    lexer: Peekable<SpannedIter<'input, Kind>>,
    prev_kind: Option<Kind>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            lexer: Kind::lexer(input).spanned().peekable(),
            prev_kind: None,
        }
    }

    /// Deduces whether we should do automatic semicolon insertion.
    fn should_asi(&mut self) -> bool {
        matches!(
            self.prev_kind,
            Some(
                Kind::Ident
                    | Kind::KwBreak
                    | Kind::KwContinue
                    | Kind::KwReturn
                    | Kind::Int
                    | Kind::Float
                    | Kind::String
                    | Kind::RParen
            )
        ) && !matches!(&mut self.lexer.peek(), Some((Ok(Kind::Dot), _))) // Dont ASI between chained method calls
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((Ok(kind), span)) = self.lexer.next() {
            // Filter away commments by skipping ahead
            if matches!(kind, Kind::Comment | Kind::LineComment) {
                return self.next();
            }

            // For newlines, check if we should do automatic semicolon insertion.
            // If we shouldn't, discard the newline by skipping ahead.
            if matches!(kind, Kind::Newline) {
                let _should = self.should_asi();
                if self.should_asi() {
                    self.prev_kind = Some(Kind::Semicolon);
                    return Some(Token {
                        kind: Kind::Semicolon,
                        span,
                    });
                } else {
                    return self.next();
                }
            }

            self.prev_kind = Some(kind.clone());
            return Some(Token { kind, span });
        }
        None
    }
}
