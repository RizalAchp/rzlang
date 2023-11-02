mod builtin;
mod callable;
mod number;

pub use builtin::context as builtin_context;
pub use callable::{BuiltinFunction, Callable, Function};
use num_traits::{Float, Zero};

use std::fmt::Display;
use std::rc::Rc;
use std::{cmp, fmt};

#[derive(Clone)]
pub enum Value {
    Number(number::Number),
    Str(String),
    Boolean(bool),
    List(Rc<[Value]>),
    Function(Rc<dyn callable::Callable>),
}

impl Value {
    #[inline]
    pub fn number(num: impl Into<number::Number>) -> Self {
        Self::Number(num.into())
    }
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Boolean(x) => *x,
            Value::Number(x) => !x.is_zero(),
            Value::Function(_) => true,
            Value::List(lst) => !lst.is_empty(),
            Value::Str(s) => !s.is_empty(),
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            Value::Boolean(_) => "boolean",
            Value::Number(_) => "number",
            Value::Function(_) => "function",
            Value::List(_) => "list",
            Value::Str(_) => "string",
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(x) => write!(f, "Number({:?})", x),
            Value::Boolean(x) => write!(f, "Boolean({:?})", x),
            Value::Function(_) => write!(f, "Function(...)"),
            Value::List(x) => write!(f, "List({:?})", x),
            Value::Str(s) => write!(f, r#"String("{s}")"#),
        }
    }
}

impl cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<cmp::Ordering> {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => {
                if let Some(x) = x.partial_cmp(y) {
                    Some(x)
                } else {
                    y.is_nan().partial_cmp(&x.is_nan())
                }
            }
            (Value::Boolean(x), Value::Boolean(y)) => x.partial_cmp(y),
            (Value::List(x), Value::List(y)) => x.partial_cmp(y),
            (Value::Function(x), Value::Function(y)) => {
                if Rc::ptr_eq(x, y) {
                    Some(cmp::Ordering::Equal)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::List(l) => write!(f, "<{l:?} (list)>"),
            Value::Function(func) => {
                write!(f, "<{}(..) (function)>", func.name().unwrap_or("anonymous"))
            }
            Value::Str(s) => write!(f, r#""{s}""#),
        }
    }
}

impl cmp::PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        self.partial_cmp(other) == Some(cmp::Ordering::Equal)
    }
}

impl AsRef<Value> for Value {
    fn as_ref(&self) -> &Value {
        self
    }
}
