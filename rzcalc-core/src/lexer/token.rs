use std::fmt::Display;

use crate::Loc;

#[repr(i8)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum TokenType<'s> {
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
    Op(Op),
    Unknown(char),
    Number(&'s str),
    Str(&'s str),
    Ident(&'s str),
    Comment(&'s str),
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

impl<'s> TokenType<'s> {
    pub fn name(&self) -> String {
        match self {
            TokenType::LeftBracket => "[".to_owned(),
            TokenType::RightBracket => "]".to_owned(),
            TokenType::LeftParen => "(".to_owned(),
            TokenType::RightParen => ")".to_owned(),
            TokenType::Comma => ",".to_owned(),
            TokenType::Assign => "=".to_owned(),
            TokenType::Arrow => "=>".to_owned(),
            TokenType::True => "true".to_owned(),
            TokenType::False => "false".to_owned(),
            TokenType::If => "if".to_owned(),
            TokenType::Then => "then".to_owned(),
            TokenType::Else => "else".to_owned(),
            TokenType::End => "<end>".to_owned(),
            TokenType::Colon => ":".to_owned(),
            TokenType::Number(x) => format!("<number: '{x}'>"),
            TokenType::Ident(x) => format!("<ident: '{x}'>"),
            TokenType::Op(x) => format!("<operator: '{x}'>"),
            TokenType::Unknown(c) => format!("<unknown token: '{c}'>"),
            TokenType::Str(s) => format!("<literal string: '{s}'>"),
            TokenType::Comment(s) => format!("<comment: '{s}'>"),
        }
    }
}

impl Display for TokenType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'s> {
    pub loc: Loc,
    pub tok: TokenType<'s>,
}

#[inline]
pub const fn token(tok: TokenType<'_>, loc: Loc) -> Token<'_> {
    Token { tok, loc }
}

impl Default for Token<'_> {
    fn default() -> Self {
        Self {
            loc: Loc::default(),
            tok: TokenType::End,
        }
    }
}
impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Token")
            .field("location", &self.loc)
            .field("kind", &self.tok)
            .finish()
    }
}
