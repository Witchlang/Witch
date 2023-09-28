//! This Error module allows us to wrap any std::err::Errors as well as the
//! Ariadne reports we use for the AST parser and Compiler steps, and return
//! them as a unified type to the outside world.
use ariadne::{Report, Source};

#[derive(Debug)]
pub enum Error<'a> {
    Ariadne(Source, Vec<Report<'a, (String, std::ops::Range<usize>)>>),
    Generic(anyhow::Error),
}

impl<'a> Error<'a> {
    pub fn eprint(self) {
        match self {
            Error::Ariadne(source, reports) => {
                for r in reports.into_iter() {
                    let _ = r.eprint(("".to_string(), source.clone()));
                }
            }
            Error::Generic(error) => {
                eprint!("\n\nERROR: {}\n\n", error);
            }
        };
    }
}

impl<'a> From<anyhow::Error> for Error<'a> {
    fn from(err: anyhow::Error) -> Error<'a> {
        Error::Generic(err)
    }
}

impl<'a> From<(Source, Report<'a, (String, std::ops::Range<usize>)>)> for Error<'a> {
    fn from((s, r): (Source, Report<'a, (String, std::ops::Range<usize>)>)) -> Error<'a> {
        Error::Ariadne(s, vec![r])
    }
}

impl<'a> From<(Source, Vec<Report<'a, (String, std::ops::Range<usize>)>>)> for Error<'a> {
    fn from((s, r): (Source, Vec<Report<'a, (String, std::ops::Range<usize>)>>)) -> Error<'a> {
        Error::Ariadne(s, r)
    }
}

