use std::fmt::Display;

use crate::{types::ParseNumberError, Loc};

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnExpectedToken(String, String),
    RequireIdentButGot(String),
    ParseNumber(ParseNumberError),
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub loc: Loc,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub const fn new(loc: Loc, kind: ParseErrorKind) -> Self {
        Self { loc, kind }
    }
    pub fn unexpected_token(loc: Loc, tok: impl ToString, desc: impl ToString) -> Self {
        Self::new(
            loc,
            ParseErrorKind::UnExpectedToken(tok.to_string(), desc.to_string()),
        )
    }
    pub fn require_ident_but_got(loc: Loc, tok: impl ToString) -> Self {
        Self::new(loc, ParseErrorKind::RequireIdentButGot(tok.to_string()))
    }
    pub fn parse_number(err: ParseNumberError, loc: &Loc) -> Self {
        Self::new(*loc, ParseErrorKind::ParseNumber(err))
    }
}

pub type ParseResult<T> = ::std::result::Result<T, ParseError>;

impl std::error::Error for ParseError {}
impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorKind::UnExpectedToken(tok, desc) => {
                write!(f, "unexpected token {tok} - {desc}")
            }
            ParseErrorKind::RequireIdentButGot(tok) => {
                write!(f, "require ident but got '{tok}'")
            }
            ParseErrorKind::ParseNumber(err) => write!(f, "Failed to parse number - {err}"),
        }
    }
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Loc { row, span_col } = self.loc;
        write!(f, "{row}:{span_col}: {}", self.kind)
    }
}
