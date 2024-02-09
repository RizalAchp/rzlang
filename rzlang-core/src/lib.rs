mod builtin;
mod context;
mod error;
mod eval;
mod lexer;
mod loc;
mod parser;
mod types;

pub use builtin::context as builtin_context;
pub use context::Context;
pub use error::RzError;
pub use eval::{Eval, EvalError};
pub use lexer::{CharStream, Lexer, Op, Token, TokenType};
pub use loc::{Loc, Span};
pub use parser::{Node, ParseError, Parser};
pub use types::{
    BuiltinFunction, BuiltinFunctionType, BuiltinFunctionWithParam, Callable, Function, TypeId,
    Value,
};

macro_rules! bail {
    ($err:expr) => {
        return Err($err)
    };
}
pub(crate) use bail;
