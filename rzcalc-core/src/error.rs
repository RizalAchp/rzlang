use std::{fmt::Display, io};

use crate::{EvalError, ParseError};

#[derive(Debug)]
pub enum RzError {
    ParseError(ParseError),
    EvalError(EvalError),
    IoError(io::Error),
}

impl Display for RzError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RzError::ParseError(err) => write!(f, "{err}"),
            RzError::EvalError(err) => write!(f, "{err}"),
            RzError::IoError(err) => write!(f, "{err}"),
        }
    }
}

macro_rules! impl_error {
    ($($errtype:ty => $error:ident),* $(,)?) => {$(
        impl From<$errtype> for RzError {
            fn from(value: $errtype) -> Self {
                Self::$error(value)
            }
        }
    )*};
}
impl_error! {
    ParseError => ParseError,
    EvalError => EvalError,
    io::Error => IoError
}
