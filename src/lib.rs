mod context;
mod eval;
mod lexer;
mod parser;
mod types;

pub use context::Context;
pub use eval::{Eval, EvalError};
pub use lexer::{Lexer, Op, ReaderStream, Span, StrStream, Stream, Token, TokenType};
pub use parser::{Node, ParseError, ParseResult, Parser};
pub use types::{builtin_context, Callable, Function, Value};

macro_rules! raise {
    ($x:expr) => { return ::std::result::Result::Err($x) };
    ($x:expr, $msg:expr) => { $crate::raise!($x($msg.into())) };
    ($x:expr, $($fmt:tt)*) => {
        $crate::raise!($x, format!($($fmt)*))
    };
}
pub(crate) use raise;
