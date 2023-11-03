mod context;
mod error;
mod eval;
mod lexer;
mod parser;
mod types;

pub use context::Context;
pub use error::RzError;
pub use eval::{Eval, EvalError};
pub use lexer::{Lexer, Op, ReaderStream, Span, StrStream, Stream, Token, TokenType};
pub use parser::{Node, ParseError, Parser};
pub use types::{
    builtin_context, BuiltinFunction, BuiltinFunctionWithParam, Callable, Function, TypeId, Value,
};

macro_rules! bail {
    ($err:expr) => {
        return Err($err)
    };
}
pub(crate) use bail;
