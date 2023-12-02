mod stream;
mod token;

use std::{path::Path, rc::Rc};

pub use stream::CharStream;
pub use token::{token, Op, Token, TokenType};

use crate::{Loc, Span};

pub struct Lexer<'s> {
    index: usize,
    stream: CharStream<'s>,
    file_path: Option<Rc<Path>>,
    buffer: String,
    tokens: Vec<Token>,
}

impl<'s> Lexer<'s> {
    pub fn new(stream: CharStream<'s>, file_path: Option<Rc<Path>>) -> Lexer<'s> {
        let tokens = Vec::with_capacity(1024);
        let buffer = String::with_capacity(1024);

        Lexer {
            index: 0,
            buffer,
            stream,
            file_path,
            tokens,
        }
    }
    pub fn from_string(string: &'s str) -> Lexer<'s> {
        Lexer::<'s>::new(CharStream::new(string), None)
    }
    pub fn from_bytes(bytes: &'s [u8]) -> Result<Lexer<'s>, crate::RzError> {
        let bytes = CharStream::<'s>::from_bytes(bytes)?;
        Ok(Lexer::<'s>::new(bytes, None))
    }
}
const fn is_numeric(c: char) -> bool {
    c.is_ascii_digit()
        || c.is_ascii_hexdigit()
        || c == '.'
        || c == '_'
        || c == 'x'
        || c == 'o'
        || c == 'b'
        || c == 'e'
}

impl<'s> Lexer<'s> {
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

    fn parse_token(&mut self) -> TokenType {
        let c = self.stream.peek_char();
        self.buffer.clear();
        if c.is_alphabetic() {
            while let Some(ch) = self.stream.next_if(|u| u.is_alphanumeric() || u == '_') {
                self.buffer.push(ch);
            }
            return Self::parse_str(&self.buffer);
        }

        self.buffer.clear();
        if c.is_ascii_digit() || c == '.' {
            while let Some(ch) = self.stream.next_if(is_numeric) {
                if ch != '_' {
                    self.buffer.push(ch);
                }
            }
            return TokenType::Number(self.buffer.clone().into());
        }

        self.stream.gnext();
        macro_rules! consume_ret {
            ($s:expr, $tok:expr) => {{
                $s.gnext();
                return $tok;
            }};
        }

        self.buffer.clear();
        match (c, self.stream.peek_char()) {
            ('&', '&') => consume_ret!(self.stream, TokenType::Operator(Op::And)),
            ('|', '|') => consume_ret!(self.stream, TokenType::Operator(Op::Or)),
            ('=', '=') => consume_ret!(self.stream, TokenType::Operator(Op::Eq)),
            ('!', '=') => consume_ret!(self.stream, TokenType::Operator(Op::Neq)),
            ('<', '=') => consume_ret!(self.stream, TokenType::Operator(Op::Lte)),
            ('<', '<') => consume_ret!(self.stream, TokenType::Operator(Op::Shl)),
            ('>', '=') => consume_ret!(self.stream, TokenType::Operator(Op::Gte)),
            ('>', '>') => consume_ret!(self.stream, TokenType::Operator(Op::Shr)),
            ('=', '>') => consume_ret!(self.stream, TokenType::Arrow),
            ('/', '/') => {
                self.stream.gnext();
                while let Some(u) = self.stream.next_if(|x| matches!(x, '\n' | '\r')) {
                    self.buffer.push(u);
                }
                return TokenType::Comment(self.buffer.clone().into());
            }
            _ => {}
        }

        self.buffer.clear();
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
                while let Some(c) = self.stream.next_if(|x| x != '\"') {
                    self.buffer.push(c);
                }
                self.stream.gnext();
                TokenType::Str(self.buffer.clone().into())
            }
            c => TokenType::Unknown(c),
        }
    }

    pub fn next_impl(&mut self) {
        while let Some(c) = self.stream.gpeek() {
            if c.is_whitespace() {
                self.stream.gnext();
                continue;
            }

            let begin = self.stream.col();
            let tok = self.parse_token();
            let end = self.stream.col();
            let span = Span(begin, end);

            let loc = if let Some(p) = &self.file_path {
                Loc::loc_file(p.clone(), self.stream.row(), span)
            } else {
                Loc::loc_repl(self.stream.current_line(), span)
            };
            self.tokens.push(token(tok, loc));
            return;
        }
    }

    #[inline]
    pub fn get_peek(&mut self) -> Option<&Token> {
        self.next_impl();
        self.tokens.get(self.index)
    }

    #[inline]
    pub fn get_next(&mut self) -> Option<Token> {
        self.next_impl();
        let tok = self.tokens.get(self.index).cloned();
        self.index += 1;
        tok
    }

    pub fn peek_tok(&mut self) -> TokenType {
        self.next_impl();
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
