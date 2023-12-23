//! The runtime implementation of pattern matching is based on a decision tree built by the compiler

#[derive(Debug)]
pub enum Decision {
    Failure,
}
