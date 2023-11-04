use std::{borrow::Cow, fmt::Display, rc::Rc};

#[repr(i8)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Op {
    Not,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitOr,
    BitXor,
    BitAnd,
    Shr,
    Shl,
}
impl Op {
    #[inline]
    pub const fn prec(self) -> i8 {
        match self {
            Op::Or => 1,
            Op::And => 2,
            Op::BitOr => 3,
            Op::BitXor => 4,
            Op::BitAnd => 5,
            Op::Lt | Op::Gt | Op::Lte | Op::Gte => 6,
            Op::Eq | Op::Neq => 7,
            Op::Shr | Op::Shl => 8,
            Op::Add | Op::Sub => 9,
            Op::Mul | Op::Div | Op::Rem => 10,
            Op::Not => 11,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    End,
    If,
    Then,
    Else,
    True,
    False,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    Assign,
    Arrow,
    Colon,
    Operator(Op),
    Unknown(u8),
    Number(Rc<str>),
    Comment(Rc<str>),
    LitStr(Rc<str>),
    Ident(Rc<str>),
}

impl Op {
    pub const fn name(self) -> &'static str {
        match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Eq => "==",
            Op::Neq => "!=",
            Op::Lt => "<",
            Op::Gt => ">",
            Op::Lte => "<=",
            Op::Gte => ">=",
            Op::Rem => "%",
            Op::BitOr => "|",
            Op::BitXor => "^",
            Op::BitAnd => "&",
            Op::Shr => ">>",
            Op::Shl => "<<",
            Op::Not => "not",
            Op::And => "and",
            Op::Or => "or",
        }
    }
}
impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Span(pub usize, pub usize);

impl TokenType {
    pub fn name(&self) -> Cow<'_, str> {
        match self {
            TokenType::LeftBracket => "[".into(),
            TokenType::RightBracket => "]".into(),
            TokenType::LeftParen => "(".into(),
            TokenType::RightParen => ")".into(),
            TokenType::Comma => ",".into(),
            TokenType::Assign => "=".into(),
            TokenType::Arrow => "=>".into(),
            TokenType::Number(x) => Cow::Owned(x.to_string()),
            TokenType::Ident(x) => Cow::Owned(x.to_string()),
            TokenType::Operator(x) => x.name().into(),
            TokenType::True => "true".into(),
            TokenType::False => "false".into(),
            TokenType::If => "if".into(),
            TokenType::Then => "then".into(),
            TokenType::Else => "else".into(),
            TokenType::End => "<end>".into(),
            TokenType::Colon => ":".into(),
            TokenType::Unknown(n) => format!("<unknown token: '{n}'>").into(),
            TokenType::Comment(s) => format!(r#"<comment: "{s}""#).into(),
            TokenType::LitStr(s) => format!(r#"<literal: "{s}">"#).into(),
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

pub struct Token {
    pub tok: TokenType,
    pub span: Span,
}
#[inline]
pub const fn token(tok: TokenType, span: Span) -> Token {
    Token { tok, span }
}
