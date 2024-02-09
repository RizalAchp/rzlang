#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span(pub usize, pub usize);
impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct Loc {
    pub row: usize,
    pub span_col: Span,
}

impl Loc {
    pub const fn new(row: usize, span_col: Span) -> Self {
        Self { row, span_col }
    }
}
impl AsRef<Loc> for Loc {
    fn as_ref(&self) -> &Loc {
        self
    }
}
