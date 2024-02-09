mod error;
mod node;

use std::rc::Rc;

use crate::types::Number;
use crate::{bail, RzError, Value};
use crate::{Lexer, Op, TokenType as Tok};

pub use error::ParseError;
pub use node::Node;

use error::ParseResult;

pub struct Parser;
impl Parser {
    pub fn parse_string(s: &str) -> Result<Node, RzError> {
        let mut lexer = Lexer::from_string(s);
        Self::parse_statement(&mut lexer).map_err(From::from)
    }
    pub fn parse_bytes(s: &[u8]) -> Result<Node, RzError> {
        let mut lexer = Lexer::from_bytes(s)?;
        Self::parse_statement(&mut lexer).map_err(From::from)
    }
}

macro_rules! unexpected_prev_token {
    ($l:expr, $($desc:tt)*) => {{
        $l.prev_tok();
        let p = $l.get_peek().cloned().unwrap_or_default();
        bail!(ParseError::unexpected_token(p.loc, p.tok, format!($($desc)*)))
    }};
}
macro_rules! required_ident {
    ($l:expr, $got:expr) => {{
        $l.prev_tok();
        bail!(ParseError::require_ident_but_got(
            $l.loc_tok().clone(),
            $got,
        ))
    }};
}

impl Parser {
    #[inline]
    fn expect_token<D>(lex: &mut Lexer, token: &Tok, default: D) -> ParseResult<D> {
        if lex.next_tok() != *token {
            unexpected_prev_token!(lex, "expected token '{token}'");
        }
        Ok(default)
    }

    fn parse_list(lex: &mut Lexer, closing: &Tok) -> ParseResult<Vec<Node>> {
        let mut args = vec![];

        while lex.peek_tok() != *closing {
            args.push(Self::parse_expr(lex)?);
            if lex.peek_tok() == Tok::Comma {
                lex.next_tok();
            } else {
                break;
            }
        }
        Self::expect_token(lex, closing, args)
    }

    fn parse_primitive(lex: &mut Lexer) -> ParseResult<Node> {
        let out = match lex.next_tok() {
            Tok::True => Node::Immediate(Value::Bool(true)),
            Tok::False => Node::Immediate(Value::Bool(false)),
            Tok::Ident(name) => Node::Var(name.into()),
            Tok::Number(num) => match num.parse::<Number>() {
                Ok(num) => Node::Immediate(Value::Num(num)),
                Err(err) => return Err(ParseError::parse_number(err, lex.loc_tok())),
            },
            Tok::Str(s) => Node::Immediate(Value::Str(s.into())),
            Tok::LeftParen => {
                let expr = Self::parse_expr(lex)?;
                Self::expect_token(lex, &Tok::RightParen, expr)?
            }
            Tok::LeftBracket => {
                let args = Self::parse_list(lex, &Tok::RightBracket)?;
                Node::List(args)
            }
            _ => unexpected_prev_token!(
                lex,
                "expected primitive type like [boolean, number, string, or list]"
            ),
        };

        Ok(out)
    }

    fn parse_apply(lex: &mut Lexer) -> ParseResult<Node> {
        let mut out = Self::parse_primitive(lex)?;

        loop {
            out = match lex.next_tok() {
                Tok::LeftParen => {
                    let args = Self::parse_list(lex, &Tok::RightParen)?;
                    Node::Apply(Rc::new(out), args)
                }
                Tok::LeftBracket => {
                    let index = Self::parse_expr(lex)?;
                    Self::expect_token(
                        lex,
                        &Tok::RightBracket,
                        Node::Index(Rc::new(out), Rc::new(index)),
                    )?
                }
                _ => {
                    lex.prev_tok();
                    break;
                }
            }
        }

        Ok(out)
    }

    fn parse_monop(lex: &mut Lexer) -> ParseResult<Node> {
        if let Tok::Op(op) = lex.peek_tok() {
            if op == Op::Add || op == Op::Sub || op == Op::Not {
                lex.next_tok();
                let arg = Self::parse_monop(lex)?;
                return Ok(Node::MonOp(op, Rc::new(arg)));
            }
        }
        Self::parse_apply(lex)
    }

    fn parse_binop(lex: &mut Lexer, prec: i8) -> ParseResult<Node> {
        let mut lhs = Self::parse_monop(lex)?;
        loop {
            match lex.peek_tok() {
                Tok::Op(op) if prec <= op.prec() => {
                    lex.next_tok();
                    let rhs = Self::parse_binop(lex, op.prec() + 1)?;
                    lhs = Node::BinOp(op, Rc::new(lhs), Rc::new(rhs));
                }
                _ => break Ok(lhs),
            }
        }
    }

    fn parse_cond(lex: &mut Lexer) -> ParseResult<Node> {
        let mut lhs = Self::parse_binop(lex, 0)?;

        loop {
            if lex.next_tok() != Tok::If {
                lex.prev_tok();
                break Ok(lhs);
            }

            let cond = Self::parse_expr(lex)?;
            Self::expect_token(lex, &Tok::Else, ())?;
            let rhs = Self::parse_expr(lex)?;
            lhs = Node::Cond(Rc::new(cond), Rc::new(lhs), Rc::new(rhs));
        }
    }

    fn parse_expr(lex: &mut Lexer<'_>) -> ParseResult<Node> {
        let a = lex.next_tok();
        let b = lex.next_tok();
        let c = lex.next_tok();
        let d = lex.next_tok();
        match (a, b, c, d) {
            // Case 1: x => body
            (Tok::Ident(x), Tok::Arrow, _, _) => {
                let args = vec![x.into()];
                lex.prev_tok();
                lex.prev_tok();
                let body = Self::parse_expr(lex)?;
                Ok(Node::Lambda(args, Rc::new(body)))
            }
            // Case 2: () => body
            (Tok::LeftParen, Tok::RightParen, Tok::Arrow, _) => {
                let args = vec![];
                lex.prev_tok();
                let body = Self::parse_expr(lex)?;
                Ok(Node::Lambda(args, Rc::new(body)))
            }
            // Case 3: (x) => body
            (Tok::LeftParen, Tok::Ident(x), Tok::RightParen, Tok::Arrow) => {
                let args = vec![x.into()];
                let body = Self::parse_expr(lex)?;
                Ok(Node::Lambda(args, Rc::new(body)))
            }
            // Case 4: (x, y, z) => body
            (Tok::LeftParen, Tok::Ident(x), Tok::Comma, Tok::Ident(y)) => {
                let mut args = vec![x.into(), y.into()];
                loop {
                    match lex.next_tok() {
                        Tok::Comma => (),
                        Tok::RightParen => break,
                        _ => unexpected_prev_token!(lex, "expected comma or closing paren params"),
                    };
                    match lex.next_tok() {
                        Tok::Ident(x) => args.push(x.into()),
                        _ => unexpected_prev_token!(lex, "expected ident for params"),
                    }
                }
                if lex.next_tok() != Tok::Arrow {
                    unexpected_prev_token!(lex, "expected an '=>' (arrow) after definition");
                }
                let body = Self::parse_expr(lex)?;
                Ok(Node::Lambda(args, Rc::new(body)))
            }
            _ => {
                lex.prev_tok();
                lex.prev_tok();
                lex.prev_tok();
                lex.prev_tok();
                Self::parse_cond(lex)
            }
        }
    }

    fn parse_statement(lex: &mut Lexer<'_>) -> ParseResult<Node> {
        let lhs = Self::parse_expr(lex)?;

        Ok(match (lhs, lex.next_tok()) {
            (out, Tok::End) => out,
            (Node::Var(var), Tok::Assign) => {
                let body = Self::parse_statement(lex)?;
                match body {
                    Node::Lambda(args, body) => Node::FunDef(var, args, body),
                    body => Node::VarDef(var, Rc::new(body)),
                }
            }
            (Node::Apply(lhs, args), Tok::Assign) => {
                let var = match lhs.get_var() {
                    Some(var) => var,
                    None => required_ident!(lex, lhs),
                };
                let mut params = vec![];
                for arg in args {
                    if let Node::Var(name) = arg {
                        params.push(name);
                    } else {
                        required_ident!(lex, lhs)
                    }
                }
                let body = Self::parse_statement(lex)?;
                Node::FunDef(var, params, Rc::new(body))
            }
            _ => unexpected_prev_token!(lex, "expected statements"),
        })
    }
}
