use std::fmt::Display;

use crate::{types::ParseNumberError, Loc, Span, Token, TokenType};

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    UnExpectedToken(TokenType, String),
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
    pub fn unexpected_token(tok: impl AsRef<Token>, desc: impl ToString) -> Self {
        let tok = tok.as_ref();
        Self::new(
            tok.loc.clone(),
            ParseErrorKind::UnExpectedToken(tok.tok.clone(), desc.to_string()),
        )
    }
    pub fn require_ident_but_got(loc: Loc, tok: impl ToString) -> Self {
        Self::new(loc, ParseErrorKind::RequireIdentButGot(tok.to_string()))
    }
    pub fn parse_number(err: ParseNumberError, loc: impl AsRef<Loc>) -> Self {
        Self::new(loc.as_ref().clone(), ParseErrorKind::ParseNumber(err))
    }
}

pub type ParseResult<T> = ::std::result::Result<T, ParseError>;

impl std::error::Error for ParseError {}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let error_msg = match &self.kind {
            ParseErrorKind::UnExpectedToken(tok, desc) => {
                format!("ERROR: unexpected token {tok} - {desc}")
            }
            ParseErrorKind::RequireIdentButGot(tok) => {
                format!("ERROR: require ident but got '{tok}'")
            }
            ParseErrorKind::ParseNumber(err) => format!("ERROR: Failed to parse number - {err}"),
        };
        match &self.loc {
            Loc::File {
                path,
                row,
                span_col,
            } => {
                write!(f, "{}:{row}:{span_col}: {error_msg}", path.display())
            }
            Loc::Repl {
                span_col: Span(start, end),
                line,
            } => {
                let width = start + 2;
                writeln!(f, "> {line}")?;
                writeln!(f, "> {:>width$}{t}", ' ', t = "^".repeat(end - start))?;
                f.write_str(&error_msg)
            }
        }
    }
}
