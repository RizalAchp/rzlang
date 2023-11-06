mod stream;
mod token;

use std::{path::Path, rc::Rc};

pub use stream::CharStream;
pub use token::{token, Op, Token, TokenType};

use crate::{Loc, Span};

pub struct Lexer {
    index: usize,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(mut stream: CharStream, file_path: Option<Rc<Path>>) -> Self {
        let mut tokens = Vec::with_capacity(1024);
        let mut buffer = String::with_capacity(1024);
        while let Some(c) = stream.gpeek() {
            if c.is_ascii_whitespace() {
                stream.gnext();
                continue;
            }
            let begin = stream.col();
            let tok = Self::parse_token(&mut stream, &mut buffer);
            let end = stream.col();
            let span = Span(begin, end);
            let loc = if let Some(p) = &file_path {
                Loc::loc_file(p.clone(), stream.row(), span)
            } else {
                Loc::loc_repl(stream.current_line(), span)
            };
            tokens.push(token(tok, loc));
        }

        let index = stream.col();
        let span = Span(index, index + 1);
        let loc = if let Some(p) = &file_path {
            Loc::loc_file(p.clone(), stream.row(), span)
        } else {
            Loc::loc_repl(stream.current_line(), span)
        };
        tokens.push(token(TokenType::End, loc));

        Lexer { index: 0, tokens }
    }

    pub fn from_string<S: AsRef<str>>(string: S) -> Self {
        Self::new(CharStream::new(string.as_ref()), None)
    }
    pub fn from_file<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        let path = path.as_ref();
        let content = std::fs::read_to_string(path)?;
        let stream = CharStream::new(&content);
        Ok(Self::new(stream, Some(path.into())))
    }
}
const fn is_numeric(c: char) -> bool {
    c.is_ascii_digit()
        || c.is_ascii_hexdigit()
        || c == '.'
        || c == 'x'
        || c == 'o'
        || c == 'b'
        || c == 'e'
}

impl Lexer {
    #[inline]
    fn parse_str(s: impl AsRef<str>) -> TokenType {
        match s.as_ref() {
            "and" => TokenType::Operator(Op::And),
            "or" => TokenType::Operator(Op::Or),
            "not" => TokenType::Operator(Op::Not),
            "true" => TokenType::True,
            "false" => TokenType::False,
            "if" => TokenType::If,
            "then" => TokenType::Then,
            "else" => TokenType::Else,
            s => TokenType::Ident(s.into()),
        }
    }

    fn parse_token(stream: &mut CharStream, buffer: &mut String) -> TokenType {
        let c = stream.peek_char();
        buffer.clear();
        if c.is_ascii_alphabetic() {
            while let Some(ch) = stream.next_if(|u| u.is_ascii_alphanumeric() || u == '_') {
                buffer.push(ch);
            }
            return Self::parse_str(buffer);
        }

        buffer.clear();
        if c.is_ascii_digit() || c == '.' {
            while let Some(ch) = stream.next_if(is_numeric) {
                buffer.push(ch);
            }
            return TokenType::Number(buffer.clone().into());
        }

        stream.gnext();
        macro_rules! consume_ret {
            ($s:ident, $tok:expr) => {{
                $s.gnext();
                return $tok;
            }};
        }

        buffer.clear();
        match (c, stream.peek_char()) {
            ('&', '&') => consume_ret!(stream, TokenType::Operator(Op::And)),
            ('|', '|') => consume_ret!(stream, TokenType::Operator(Op::Or)),
            ('=', '=') => consume_ret!(stream, TokenType::Operator(Op::Eq)),
            ('!', '=') => consume_ret!(stream, TokenType::Operator(Op::Neq)),
            ('<', '=') => consume_ret!(stream, TokenType::Operator(Op::Lte)),
            ('<', '<') => consume_ret!(stream, TokenType::Operator(Op::Shl)),
            ('>', '=') => consume_ret!(stream, TokenType::Operator(Op::Gte)),
            ('>', '>') => consume_ret!(stream, TokenType::Operator(Op::Shr)),
            ('=', '>') => consume_ret!(stream, TokenType::Arrow),
            ('/', '/') => {
                stream.gnext();
                while let Some(u) = stream.next_if(|x| matches!(x, '\n' | '\r')) {
                    buffer.push(u);
                }
                return TokenType::Comment(buffer.clone().into());
            }
            _ => {}
        }

        buffer.clear();
        match c {
            '[' => TokenType::LeftBracket,
            ']' => TokenType::RightBracket,
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            ',' => TokenType::Comma,
            '=' => TokenType::Assign,
            ':' => TokenType::Colon,
            '+' => TokenType::Operator(Op::Add),
            '-' => TokenType::Operator(Op::Sub),
            '*' => TokenType::Operator(Op::Mul),
            '/' => TokenType::Operator(Op::Div),
            '%' => TokenType::Operator(Op::Rem),
            '|' => TokenType::Operator(Op::BitOr),
            '&' => TokenType::Operator(Op::BitAnd),
            '^' => TokenType::Operator(Op::BitXor),
            '>' => TokenType::Operator(Op::Gt),
            '<' => TokenType::Operator(Op::Lt),
            '!' => TokenType::Operator(Op::Not),
            '"' => {
                while let Some(c) = stream.next_if(|x| x != '\"') {
                    buffer.push(c);
                }
                stream.gnext();
                TokenType::Str(buffer.clone().into())
            }
            c => TokenType::Unknown(c),
        }
    }
    #[inline]
    pub fn get_peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    #[inline]
    pub fn get_next(&mut self) -> Option<&Token> {
        self.index += 1;
        let tok = self.get_peek();
        tok
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

    pub fn loc_tok(&self) -> &Loc {
        if self.index < self.tokens.len() {
            &self.tokens[self.index].loc
        } else {
            &self.tokens.last().unwrap().loc
        }
    }
}
