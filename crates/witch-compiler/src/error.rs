use std::{fmt::Display, ops::Range};

pub type Result<T> = anyhow::Result<T>;

#[derive(Debug)]
pub struct Error {
    msg: String,
    span: Range<usize>,
    source: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl std::error::Error for Error {}

impl Error {
    pub fn new(msg: &str, span: Range<usize>, source: &str) -> Self {
        Self {
            msg: msg.to_string(),
            // span.start..span.end is copy, but span is only clone. whyyy
            span: span.start..span.end,
            source: source[span.start..span.end].to_string(),
        }
    }

    pub fn fatal() -> Self {
        Self {
            msg: "Something terrible happened. Code: 4545345".to_string(),
            span: 0..0,
            source: "".to_string(),
        }
    }
}
