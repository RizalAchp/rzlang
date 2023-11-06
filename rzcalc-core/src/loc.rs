use std::{path::Path, rc::Rc};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span(pub usize, pub usize);
impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Loc {
    File {
        path: Rc<Path>,
        row: usize,
        span_col: Span,
    },
    Repl {
        span_col: Span,
        line: Rc<str>,
    },
}

impl Loc {
    pub fn loc_file(path: impl Into<Rc<Path>>, row: usize, span_col: Span) -> Self {
        Self::File {
            path: path.into(),
            row,
            span_col,
        }
    }
    pub fn loc_repl(line: impl Into<Rc<str>>, span_col: Span) -> Self {
        Self::Repl {
            span_col,
            line: line.into(),
        }
    }
}
impl AsRef<Loc> for Loc {
    fn as_ref(&self) -> &Loc {
        self
    }
}
