use std::ops::Range;

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    msg: String,
    span: Range<usize>,
    source: String,
}

impl Error {
    pub fn new(msg: &str, span: Range<usize>, source: &str) -> Self {
        Self {
            msg: msg.to_string(),
            // span.start..span.end is copy, but span is only clone. whyyy
            span: span.start..span.end,
            source: source[span.start..span.end].to_string(),
        }
    }
}
