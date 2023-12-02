use core::str::Utf8Error;
use std::{fmt::Display, io};

use crate::{EvalError, ParseError};

#[derive(Debug)]
pub enum RzError {
    ParseError(ParseError),
    EvalError(EvalError),
    IoError(io::Error),
    Utf8(Utf8Error),
    Any(String),
}

impl Display for RzError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RzError::ParseError(err) => write!(f, "{err}"),
            RzError::EvalError(err) => write!(f, "ERROR: {err}"),
            RzError::IoError(err) => write!(f, "ERROR: {err}"),
            RzError::Any(any) => write!(f, "ERROR: {any}"),
            RzError::Utf8(err) => write!(f, "ERROR: {err}"),
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
    Utf8Error => Utf8,
    io::Error => IoError,
    String => Any,
}

impl From<Box<dyn std::error::Error>> for RzError {
    fn from(value: Box<dyn std::error::Error>) -> Self {
        Self::Any(value.to_string())
    }
}
