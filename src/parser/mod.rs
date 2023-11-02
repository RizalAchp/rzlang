mod error;
mod node;

use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::{raise, Value};
use crate::{Lexer, Op, TokenType as Tok};

pub use error::{ParseError, ParseResult};
pub use node::Node;

pub struct Parser {
    pub lexer: Lexer,
    pub src: Option<PathBuf>,
}
impl Parser {
    pub fn parse_file<P: AsRef<Path>>(p: P) -> std::io::Result<Self> {
        let p = p.as_ref();
        let lexer = Lexer::from_file(p)?;
        Ok(Self {
            lexer,
            src: Some(p.to_path_buf()),
        })
    }
    pub fn parse_string<S: AsRef<str>>(s: S) -> Self {
        Self {
            lexer: Lexer::from_string(s),
            src: None,
        }
    }

    pub fn parse(mut self) -> ParseResult<Node> {
        match self.parse_statement() {
            Ok(ok) => Ok(ok),
            Err(mut err) => {
                if let Some(source) = self.src {
                    err.source = Some(source.into());
                    Err(err)
                } else {
                    Err(err)
                }
            }
        }
    }
}

macro_rules! unexpected_prev_token {
    ($self:ident) => {{
        $self.lexer.prev_tok();
        raise!(ParseError {
            token: $self.lexer.peek_tok(),
            span: $self.lexer.span_tok(),
            source: None,
        })
    }};
}

impl Parser {
    #[inline]
    fn expect_token<D>(&mut self, token: &Tok, default: D) -> ParseResult<D> {
        if self.lexer.next_tok() != *token {
            unexpected_prev_token!(self);
        }
        Ok(default)
    }

    fn parse_list(&mut self, closing: &Tok) -> ParseResult<Vec<Node>> {
        let mut args = vec![];

        while self.lexer.peek_tok() != *closing {
            args.push(self.parse_expr()?);
            if self.lexer.peek_tok() == Tok::Comma {
                self.lexer.next_tok();
            } else {
                break;
            }
        }
        self.expect_token(closing, args)
    }

    fn parse_primitive(&mut self) -> ParseResult<Node> {
        let out = match self.lexer.next_tok() {
            Tok::True => Node::Immediate(Value::Boolean(true)),
            Tok::False => Node::Immediate(Value::Boolean(false)),
            Tok::Ident(name) => Node::Var(name),
            Tok::Number(num) => {
                if let Ok(x) = num.parse() {
                    Node::Immediate(Value::Number(x))
                } else {
                    unexpected_prev_token!(self);
                }
            }
            Tok::LeftParen => {
                let expr = self.parse_expr()?;
                self.expect_token(&Tok::RightParen, expr)?
            }
            Tok::LeftBracket => {
                let args = self.parse_list(&Tok::RightBracket)?;
                Node::List(args)
            }
            _ => unexpected_prev_token!(self),
        };

        Ok(out)
    }

    fn parse_apply(&mut self) -> ParseResult<Node> {
        let mut out = self.parse_primitive()?;

        loop {
            out = match self.lexer.next_tok() {
                Tok::LeftParen => {
                    let args = self.parse_list(&Tok::RightParen)?;
                    Node::Apply(Rc::new(out), args)
                }
                Tok::LeftBracket => {
                    let index = self.parse_expr()?;
                    self.expect_token(
                        &Tok::RightBracket,
                        Node::Index(Rc::new(out), Rc::new(index)),
                    )?
                }
                _ => {
                    self.lexer.prev_tok();
                    break;
                }
            }
        }

        Ok(out)
    }

    fn parse_monop(&mut self) -> ParseResult<Node> {
        if let Tok::Operator(op) = self.lexer.peek_tok() {
            if op == Op::Add || op == Op::Sub || op == Op::Not {
                self.lexer.next_tok();
                let arg = self.parse_monop()?;
                return Ok(Node::MonOp(op, Rc::new(arg)));
            }
        }
        self.parse_apply()
    }

    fn parse_binop(&mut self, prec: i8) -> ParseResult<Node> {
        let mut lhs = self.parse_monop()?;

        loop {
            match self.lexer.peek_tok() {
                Tok::Operator(op) if prec <= op.prec() => {
                    self.lexer.next_tok();
                    let rhs = self.parse_binop(op.prec() + 1)?;
                    lhs = Node::BinOp(op, Rc::new(lhs), Rc::new(rhs));
                }
                _ => break Ok(lhs),
            }
        }
    }

    fn parse_cond(&mut self) -> ParseResult<Node> {
        let mut lhs = self.parse_binop(0)?;

        loop {
            if self.lexer.next_tok() != Tok::If {
                self.lexer.prev_tok();
                break Ok(lhs);
            }

            let cond = self.parse_expr()?;
            let expr = self.parse_expr()?;
            let rhs = self.expect_token(&Tok::Else, expr)?;
            lhs = Node::Cond(Rc::new(cond), Rc::new(lhs), Rc::new(rhs));
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Node> {
        let out = match (
            self.lexer.next_tok(),
            self.lexer.next_tok(),
            self.lexer.next_tok(),
            self.lexer.next_tok(),
        ) {
            // Case 1: x => body
            (Tok::Ident(x), Tok::Arrow, _, _) => {
                self.lexer.prev_tok();
                self.lexer.prev_tok();
                let args = vec![x];
                let body = self.parse_expr()?;
                Node::Lambda(args, Rc::new(body))
            }

            // Case 2: () => body
            (Tok::LeftParen, Tok::RightParen, Tok::Arrow, _) => {
                self.lexer.prev_tok();
                let args = vec![];
                let body = self.parse_expr()?;
                Node::Lambda(args, Rc::new(body))
            }

            // Case 3: (x) => body
            (Tok::LeftParen, Tok::Ident(x), Tok::RightParen, Tok::Arrow) => {
                let args = vec![x];
                let body = self.parse_expr()?;
                Node::Lambda(args, Rc::new(body))
            }

            // Case 4: (x, y, z) => body
            (Tok::LeftParen, Tok::Ident(x), Tok::Comma, Tok::Ident(y)) => {
                let mut args = vec![x, y];

                loop {
                    match self.lexer.next_tok() {
                        Tok::Comma => (),
                        Tok::RightParen => break,
                        _ => unexpected_prev_token!(self),
                    };

                    match self.lexer.next_tok() {
                        Tok::Ident(x) => args.push(x),
                        _ => unexpected_prev_token!(self),
                    }
                }

                if self.lexer.next_tok() != Tok::Arrow {
                    unexpected_prev_token!(self);
                }

                let body = self.parse_expr()?;
                Node::Lambda(args, Rc::new(body))
            }

            _ => {
                self.lexer.prev_tok();
                self.lexer.prev_tok();
                self.lexer.prev_tok();
                self.lexer.prev_tok();
                self.parse_cond()?
            }
        };

        Ok(out)
    }

    fn parse_statement(&mut self) -> ParseResult<Node> {
        let lhs = self.parse_expr()?;

        Ok(match (lhs, self.lexer.next_tok()) {
            (out, Tok::End) => out,
            (Node::Var(var), Tok::Assign) => {
                let body = self.parse_statement()?;
                match body {
                    Node::Lambda(args, body) => Node::FunDef(var, args, body),
                    body => Node::VarDef(var, Rc::new(body)),
                }
            }
            (Node::Apply(lhs, args), Tok::Assign) => {
                let var = match lhs.get_var() {
                    Some(var) => var,
                    None => unexpected_prev_token!(self),
                };

                let mut params = vec![];

                for arg in args {
                    if let Node::Var(name) = arg {
                        params.push(name);
                    } else {
                        unexpected_prev_token!(self);
                    }
                }

                let body = self.parse_statement()?;
                Node::FunDef(var, params, Rc::new(body))
            }
            _ => unexpected_prev_token!(self),
        })
    }
}
