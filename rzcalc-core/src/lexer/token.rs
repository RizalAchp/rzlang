use std::{borrow::Cow, fmt::Display, rc::Rc};

use crate::Loc;

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
    Unknown(char),
    Number(Rc<str>),
    Str(Rc<str>),
    Ident(Rc<str>),
    Comment(Rc<str>),
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

impl TokenType {
    pub fn name(&self) -> Cow<'static, str> {
        match self {
            TokenType::LeftBracket => "[".into(),
            TokenType::RightBracket => "]".into(),
            TokenType::LeftParen => "(".into(),
            TokenType::RightParen => ")".into(),
            TokenType::Comma => ",".into(),
            TokenType::Assign => "=".into(),
            TokenType::Arrow => "=>".into(),
            TokenType::True => "true".into(),
            TokenType::False => "false".into(),
            TokenType::If => "if".into(),
            TokenType::Then => "then".into(),
            TokenType::Else => "else".into(),
            TokenType::End => "<end>".into(),
            TokenType::Colon => ":".into(),
            TokenType::Number(x) => format!("<number: '{x}'>").into(),
            TokenType::Ident(x) => format!("<ident: '{x}'>").into(),
            TokenType::Operator(x) => format!("<operator: '{x}'>").into(),
            TokenType::Unknown(c) => format!("<unknown token: '{c}'>").into(),
            TokenType::Str(s) => format!("<literal string: '{s}'>").into(),
            TokenType::Comment(s) => format!("<comment: '{s}'>").into(),
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub loc: Loc,
    pub tok: TokenType,
}
#[inline]
pub const fn token(tok: TokenType, loc: Loc) -> Token {
    Token { tok, loc }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            loc: Loc::Repl {
                span_col: crate::Span(0, 0),
                line: "".into(),
            },
            tok: TokenType::End,
        }
    }
}

impl AsRef<Token> for Token {
    fn as_ref(&self) -> &Token {
        self
    }
}
