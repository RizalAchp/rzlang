use std::{path::Path, rc::Rc};

use crate::{Span, TokenType as Tok};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub span: Span,
    pub token: Tok,
    pub source: Option<Rc<Path>>,
}

pub type ParseResult<T> = ::std::result::Result<T, ParseError>;
