//! The `witch-compiler` crate takes a Witch source file (or several) and emits
//! Witch Bytecode, a binary representation of the program which the `witch-runtime` 
//! can execute. 
//! 
//! It exports the `compile` function, which either returns the final bytecode or
//! an error report which is Write (e.g. to pipe into stderr).

use std::{path::{PathBuf, Path}, env::current_dir};
use ariadne::{Report as AriadneReport, ReportKind};

mod ast;

/// Report wraps the Ariande Report type in order to make things a bit more readable. 
#[derive(Debug)]
pub struct Report<'a> {
    pub report: AriadneReport<'a, (String, std::ops::Range<usize>)>
}

impl<'a> Report<'a> {
    fn quick_error(msg: &str) -> Self {
        let report = AriadneReport::build(ReportKind::Error, "", 0)
        .with_message(msg)
        .finish();
        Self { report }
    }
}

// TODO implement From std::io::Error for Report


/// Takes a Witch source file and compiles it to bytecode.
pub fn compile<'a>(
    file_path: PathBuf
) -> Result<Vec<u8>, Report<'a>> {
    let (file_path, source) = resolve_file(None, file_path)?;
    Ok(vec![])
}

/// Canonicalizes a file path from our `start_path`, returning the new path as well as the file contents. 
fn resolve_file<'a>(start_path: Option<PathBuf>, file_path: PathBuf) -> Result<(String, String), Report<'a>> {
    let file_path = if let Some(start_path) = start_path {
        let cwd = current_dir().map_err(|_| Report::quick_error("unable to get current dir"))?;
        let p = cwd.as_path().join(Path::new(&start_path).parent().unwrap()).join(file_path);
        std::fs::canonicalize(p).map_err(|_| Report::quick_error("unable to canonicalize"))?.as_path().to_string_lossy().into_owned()
    } else {
        let cwd = current_dir().map_err(|_| Report::quick_error("unable to get current dir"))?;
        let p = cwd.as_path().join(file_path);
        std::fs::canonicalize(p).map_err(|e| {dbg!(e); Report::quick_error("unable to canonicalize")})?.as_path().to_string_lossy().into_owned()
    };
    let source = std::fs::read_to_string(file_path.clone()).unwrap();
    Ok((file_path, source))
}