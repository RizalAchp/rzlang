mod stream;
mod token;

use std::{fs::File, io::Read, path::Path};

pub use stream::{ReaderStream, StrStream, Stream};
pub use token::{token, Op, Span, Token, TokenType};

pub struct Lexer {
    index: usize,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new<S: Stream>(mut stream: S) -> Self {
        let mut tokens = vec![];
        while let Some(c) = stream.peek() {
            if c.is_ascii_whitespace() {
                stream.next();
                continue;
            }
            let begin = stream.index();
            let tok = Self::parse_token(&mut stream);
            let end = stream.index();
            tokens.push(token(tok, Span(begin, end)));
        }

        let index = stream.index();
        tokens.push(token(TokenType::End, Span(index, index + 1)));

        Lexer { index: 0, tokens }
    }

    pub fn from_string<S: AsRef<str>>(string: S) -> Self {
        Self::new(StrStream::new(string.as_ref()))
    }
    pub fn from_reader<R: Read>(reader: R) -> Self {
        Self::new(ReaderStream::new(reader))
    }
    pub fn from_file<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        let stream = ReaderStream::<File>::from_file(path)?;
        Ok(Self::new(stream))
    }
}
const fn is_numeric(c: u8) -> bool {
    c.is_ascii_digit()
        || c.is_ascii_hexdigit()
        || c == b'.'
        || c == b'x'
        || c == b'o'
        || c == b'b'
        || c == b'e'
}

impl Lexer {
    #[inline]
    fn parse_str(s: String) -> TokenType {
        match s.as_ref() {
            "and" => TokenType::Operator(Op::And),
            "or" => TokenType::Operator(Op::Or),
            "not" => TokenType::Operator(Op::Not),
            "true" => TokenType::True,
            "false" => TokenType::False,
            "if" => TokenType::If,
            "then" => TokenType::Then,
            "else" => TokenType::Else,
            _ => TokenType::Ident(s.into()),
        }
    }

    fn parse_token<S: Stream>(stream: &mut S) -> TokenType {
        let c = stream.peek_char();
        if c.is_ascii_alphabetic() {
            let mut buffer = String::new();
            while let Some(ch) = stream.next_if(|u| u.is_ascii_alphanumeric() || u == b'_') {
                buffer.push(ch as char);
            }
            return Self::parse_str(buffer);
        }

        if c.is_ascii_digit() || c == b'.' {
            let mut buffer = String::new();
            while let Some(ch) = stream.next_if(is_numeric) {
                buffer.push(ch as char);
            }
            return TokenType::Number(buffer.into());
        }

        stream.next();
        macro_rules! consume_ret {
            ($s:ident, $tok:expr) => {{
                $s.next();
                return $tok;
            }};
        }

        match (c, stream.peek_char()) {
            (b'&', b'&') => consume_ret!(stream, TokenType::Operator(Op::And)),
            (b'|', b'|') => consume_ret!(stream, TokenType::Operator(Op::Or)),
            (b'=', b'=') => consume_ret!(stream, TokenType::Operator(Op::Eq)),
            (b'!', b'=') => consume_ret!(stream, TokenType::Operator(Op::Neq)),
            (b'<', b'=') => consume_ret!(stream, TokenType::Operator(Op::Lte)),
            (b'<', b'<') => consume_ret!(stream, TokenType::Operator(Op::Shl)),
            (b'>', b'=') => consume_ret!(stream, TokenType::Operator(Op::Gte)),
            (b'>', b'>') => consume_ret!(stream, TokenType::Operator(Op::Shr)),
            (b'=', b'>') => consume_ret!(stream, TokenType::Arrow),
            (b'/', b'/') => {
                stream.next();
                let mut buffer = String::new();
                while let Some(u) = stream.next_if(|x| x == b'\n' || x == b'\r') {
                    buffer.push(u as char);
                }
                return TokenType::Comment(buffer.into());
            }
            _ => {}
        }

        match c {
            b'[' => TokenType::LeftBracket,
            b']' => TokenType::RightBracket,
            b'(' => TokenType::LeftParen,
            b')' => TokenType::RightParen,
            b',' => TokenType::Comma,
            b'=' => TokenType::Assign,
            b':' => TokenType::Colon,
            b'+' => TokenType::Operator(Op::Add),
            b'-' => TokenType::Operator(Op::Sub),
            b'*' => TokenType::Operator(Op::Mul),
            b'/' => TokenType::Operator(Op::Div),
            b'%' => TokenType::Operator(Op::Rem),
            b'|' => TokenType::Operator(Op::BitOr),
            b'&' => TokenType::Operator(Op::BitAnd),
            b'^' => TokenType::Operator(Op::BitXor),
            b'>' => TokenType::Operator(Op::Gt),
            b'<' => TokenType::Operator(Op::Lt),
            c => TokenType::Unknown(c),
        }
    }

    pub fn peek_tok(&self) -> TokenType {
        self.tokens
            .get(self.index)
            .map(|x| x.tok.clone())
            .unwrap_or(TokenType::End)
    }

    pub fn next_tok(&mut self) -> TokenType {
        let tok = self.peek_tok();
        self.index += 1;
        tok
    }

    pub fn prev_tok(&mut self) {
        if self.index > 0 {
            self.index -= 1;
        }
    }

    pub fn span_tok(&self) -> Span {
        if self.index < self.tokens.len() {
            self.tokens[self.index].span
        } else {
            self.tokens.last().unwrap().span
        }
    }
}
