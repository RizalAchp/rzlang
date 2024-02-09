mod stream;
mod token;

pub use stream::CharStream;
pub use token::{token, Op, Token, TokenType};

use crate::{Loc, Span};

pub struct Lexer<'s> {
    _input: &'s str,
    tokens: Vec<Token<'s>>,
    index: usize,
}

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str) -> Self {
        let mut tokens = Vec::new();
        let mut stream = CharStream::new(input);

        while let Some(c) = stream.peek() {
            if c.is_whitespace() {
                stream.next();
                continue;
            }
            let begin = stream.col();
            let row = stream.row();
            let tok = Self::parse_token(&mut stream);
            tokens.push(token(tok, Loc::new(row, Span(begin, stream.col()))));
        }

        Self {
            index: 0,
            _input: input,
            tokens,
        }
    }

    pub fn from_string(string: &'s str) -> Self {
        Lexer::<'s>::new(string)
    }
    pub fn from_bytes(bytes: &'s [u8]) -> Result<Self, crate::RzError> {
        let content = std::str::from_utf8(bytes)?;
        Ok(Lexer::<'s>::new(content))
    }
}
const fn is_numeric(c: &char) -> bool {
    c.is_ascii_digit()
        || c.is_ascii_hexdigit()
        || *c == '.'
        || *c == '_'
        || *c == 'x'
        || *c == 'o'
        || *c == 'b'
        || *c == 'e'
}

impl<'s> Lexer<'s> {
    #[inline]
    fn parse_str(s: &'s str) -> TokenType<'s> {
        match s {
            "and" => TokenType::Op(Op::And),
            "or" => TokenType::Op(Op::Or),
            "not" => TokenType::Op(Op::Not),
            "true" => TokenType::True,
            "false" => TokenType::False,
            "if" => TokenType::If,
            "then" => TokenType::Then,
            "else" => TokenType::Else,
            s => TokenType::Ident(s),
        }
    }

    fn parse_token(stream: &mut CharStream<'s>) -> TokenType<'s> {
        let c = stream.next().unwrap_or(CharStream::EOF);
        if c.is_alphabetic() {
            let s = stream.get_string_when(|x| x.is_alphanumeric() || *x == '_');
            return Self::parse_str(s);
        }

        if c.is_ascii_digit() || c == '.' {
            let s = stream.get_string_when(is_numeric);
            return TokenType::Number(s);
        }

        macro_rules! consume_ret {
            ($s:expr, $tok:expr) => {{
                $s.next();
                return $tok;
            }};
        }

        match (c, stream.peek()) {
            ('&', Some('&')) => consume_ret!(stream, TokenType::Op(Op::And)),
            ('|', Some('|')) => consume_ret!(stream, TokenType::Op(Op::Or)),
            ('=', Some('=')) => consume_ret!(stream, TokenType::Op(Op::Eq)),
            ('!', Some('=')) => consume_ret!(stream, TokenType::Op(Op::Neq)),
            ('<', Some('=')) => consume_ret!(stream, TokenType::Op(Op::Lte)),
            ('<', Some('<')) => consume_ret!(stream, TokenType::Op(Op::Shl)),
            ('>', Some('=')) => consume_ret!(stream, TokenType::Op(Op::Gte)),
            ('>', Some('>')) => consume_ret!(stream, TokenType::Op(Op::Shr)),
            ('=', Some('>')) => consume_ret!(stream, TokenType::Arrow),
            ('/', Some('/')) => {
                stream.next();
                let s = stream.get_string_when(|x| matches!(x, '\n' | '\r'));
                return TokenType::Comment(s);
            }
            _ => {}
        }

        match c {
            '[' => TokenType::LeftBracket,
            ']' => TokenType::RightBracket,
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            ',' => TokenType::Comma,
            '=' => TokenType::Assign,
            ':' => TokenType::Colon,
            '+' => TokenType::Op(Op::Add),
            '-' => TokenType::Op(Op::Sub),
            '*' => TokenType::Op(Op::Mul),
            '/' => TokenType::Op(Op::Div),
            '%' => TokenType::Op(Op::Rem),
            '|' => TokenType::Op(Op::BitOr),
            '&' => TokenType::Op(Op::BitAnd),
            '^' => TokenType::Op(Op::BitXor),
            '>' => TokenType::Op(Op::Gt),
            '<' => TokenType::Op(Op::Lt),
            '!' => TokenType::Op(Op::Not),
            '\"' => {
                stream.next();
                let s = stream.get_string_when(|x| *x != '\"');
                stream.next();
                TokenType::Str(s)
            }
            '\'' => {
                stream.next();
                let s = stream.get_string_when(|x| *x != '\'');
                stream.next();
                TokenType::Str(s)
            }
            c => TokenType::Unknown(c),
        }
    }

    #[inline(always)]
    pub fn get_peek(&mut self) -> Option<&Token<'s>> {
        self.tokens.get(self.index)
    }

    #[inline(always)]
    pub fn get_next(&mut self) -> Option<&Token<'s>> {
        let tok = self.tokens.get(self.index);
        self.index += 1;
        tok
    }

    #[inline(always)]
    pub fn peek_tok(&mut self) -> TokenType<'s> {
        self.get_peek().map(|x| x.tok).unwrap_or(TokenType::End)
    }

    #[inline(always)]
    pub fn next_tok(&mut self) -> TokenType<'s> {
        self.get_next().map(|x| x.tok).unwrap_or(TokenType::End)
    }

    #[inline(always)]
    pub fn prev_tok(&mut self) {
        if self.index > 0 {
            self.index -= 1;
        }
    }
    #[inline(always)]
    pub fn loc_tok(&self) -> &Loc {
        &self.tokens[self.index - 1].loc
    }
}
