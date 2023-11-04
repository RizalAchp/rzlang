use std::fmt::Display;

use crate::{types::ParseNumberError, Span, TokenType as Tok};

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnExpectedToken(Tok),
    ParseNumber(ParseNumberError),
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub span: Span,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub const fn new(span: Span, kind: ParseErrorKind) -> Self {
        Self { span, kind }
    }
    pub const fn unexpected_token(tok: Tok, span: Span) -> Self {
        Self::new(span, ParseErrorKind::UnExpectedToken(tok))
    }
    pub const fn parse_number(err: ParseNumberError, span: Span) -> Self {
        Self::new(span, ParseErrorKind::ParseNumber(err))
    }
}

pub type ParseResult<T> = ::std::result::Result<T, ParseError>;

impl std::error::Error for ParseError {}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ParseErrorKind::UnExpectedToken(tok) => {
                let Span(before, after) = self.span;
                write!(f, "unexpected token {tok} at pos:{before}..{after}")
            }
            ParseErrorKind::ParseNumber(err) => write!(f, "{err}"),
        }
    }
}
