use core::{fmt, str::Utf8Error};
use std::{fmt::Display, io};

use crate::{types::ValueError, EvalError, ParseError};

#[derive(Debug)]
pub enum RzError {
    Io(io::Error),
    Parse(ParseError),
    Eval(EvalError),
    Value(ValueError),
    Utf8(Utf8Error),
    Fmt(fmt::Error),
    Any(String),
}

impl Display for RzError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RzError::Parse(err) => write!(f, "ERROR (ParseError): {err}"),
            RzError::Eval(err) => write!(f, "ERROR (EvalError): {err}"),
            RzError::Io(err) => write!(f, "ERROR (IoError): {err}"),
            RzError::Utf8(err) => write!(f, "ERROR (Utf8Error): {err}"),
            RzError::Value(err) => write!(f, "ERROR (ValueError): {err}"),
            RzError::Any(err) => write!(f, "ERROR (AnyError): {err}"),
            RzError::Fmt(err) => write!(f, "ERROR (FmtError): {err}"),
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
    ParseError => Parse,
    EvalError => Eval,
    ValueError => Value,
    Utf8Error => Utf8,
    io::Error => Io,
    fmt::Error => Fmt,
    String => Any,
}

impl From<Box<dyn std::error::Error>> for RzError {
    fn from(value: Box<dyn std::error::Error>) -> Self {
        Self::Any(value.to_string())
    }
}
