mod builtin;
mod callable;
mod error;
mod number;
mod typeid;

pub use builtin::context as builtin_context;
pub use callable::{
    BuiltinFunction, BuiltinFunctionParams, BuiltinFunctionWithParam, Callable, Function,
};
pub use typeid::ValueTypeId;

use itertools::Itertools;
pub use number::{Number, ParseNumberError};

use num_traits::{Float, Zero};
use std::fmt::Display;
use std::rc::Rc;
use std::{cmp, fmt};

pub use error::ValueError;

pub type Str = Rc<str>;
pub type List = Rc<[Value]>;
pub type Func = Rc<dyn callable::Callable>;

#[derive(Clone)]
pub enum Value {
    None,
    Bool(bool),
    Num(Number),
    List(List),
    Str(Str),
    Func(Func),
}

impl Value {
    #[inline]
    pub const fn type_id(&self) -> ValueTypeId {
        use ValueTypeId as VT;
        match self {
            Value::None => VT::None,
            Value::Bool(_) => VT::Bool,
            Value::Num(_) => VT::Num,
            Value::List(_) => VT::List,
            Value::Str(_) => VT::Str,
            Value::Func(_) => VT::Func,
        }
    }
    #[inline]
    pub const fn type_name(&self) -> &'static str {
        self.type_id().type_name()
    }

    #[inline]
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(x) => *x,
            Value::Num(x) => !x.is_zero(),
            Value::Func(_) => true,
            Value::List(lst) => !lst.is_empty(),
            Value::Str(s) => !s.is_empty(),
            Value::None => false,
        }
    }
}

#[allow(unused)]
impl Value {
    #[inline]
    pub fn num(num: impl Into<Number>) -> Self {
        Self::Num(num.into())
    }
    #[inline]
    pub fn str(string: impl Into<Str>) -> Self {
        Self::Str(string.into())
    }
    #[inline]
    pub fn func(func: impl Into<Func>) -> Self {
        Self::Func(func.into())
    }
    #[inline]
    pub fn list(list: impl Into<List>) -> Self {
        Self::List(list.into())
    }
    pub const fn none() -> Self {
        Self::None
    }
}

#[allow(unused)]
impl Value {
    pub const fn is_number(&self) -> bool {
        matches!(self, Self::Num(_) | Self::Bool(_))
    }
    pub const fn is_string(&self) -> bool {
        matches!(self, Self::Str(_))
    }
    // string is an list, because string is just an list of char
    pub const fn is_list(&self) -> bool {
        matches!(self, Self::Str(_) | Self::List(_))
    }
    pub const fn is_function(&self) -> bool {
        matches!(self, Self::Func(_))
    }

    pub fn is_same_type(&self, other: &Self) -> bool {
        self.type_id() == other.type_id()
    }
}

#[allow(unused)]
impl Value {
    pub fn get_str(&self) -> Result<Str, ValueError> {
        match self {
            Self::Str(s) => Ok(s.clone()),
            s => Err(ValueError::mismatch_type(s.type_id(), ValueTypeId::Str)),
        }
    }
    pub fn get_list(&self) -> Result<List, ValueError> {
        match self {
            Self::List(l) => Ok(l.clone()),
            s => Err(ValueError::mismatch_type(s.type_id(), ValueTypeId::List)),
        }
    }
    pub fn get_func(&self) -> Result<Func, ValueError> {
        match self {
            Self::Func(f) => Ok(f.clone()),
            s => Err(ValueError::mismatch_type(s.type_id(), ValueTypeId::Func)),
        }
    }
    pub fn get_num(&self) -> Result<Number, ValueError> {
        match self {
            Self::Num(n) => Ok(*n),
            s => Err(ValueError::mismatch_type(s.type_id(), ValueTypeId::Num)),
        }
    }
}

impl<N: Into<Number>> From<N> for Value {
    fn from(value: N) -> Self {
        Self::num(value)
    }
}

impl From<Str> for Value {
    fn from(value: Str) -> Self {
        Self::str(value)
    }
}
impl From<&Str> for Value {
    fn from(value: &Str) -> Self {
        Self::str(value.clone())
    }
}
impl From<List> for Value {
    fn from(value: List) -> Self {
        Self::List(value)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Str(s) => write!(f, "{s}"),
            Value::List(l) => write!(f, "{l:?}"),
            Value::Func(func) => {
                write!(f, "<{}(...)>", func.name().unwrap_or("anonymous function"))
            }
            Value::None => f.write_str("None"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Num(x) => write!(f, "{x:?}"),
            Value::Bool(x) => write!(f, "{x:?}"),
            Value::Str(x) => write!(f, "{x:?}"),
            Value::List(x) => write!(f, "{x:#?}"),
            Value::Func(x) => {
                let name = x.name().unwrap_or("anonymous");
                let args = x
                    .param()
                    .map(|vals| vals.iter().join(","))
                    .unwrap_or_else(|| "...".to_owned());
                write!(f, "function <{name} ({args})>")
            }
            Value::None => f.write_str("None"),
        }
    }
}

impl cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<cmp::Ordering> {
        match (self, other) {
            (Value::None, Value::None) => Some(cmp::Ordering::Equal),
            (Value::Num(x), Value::Num(y)) => match x.partial_cmp(y) {
                Some(x) => Some(x),
                None => y.is_nan().partial_cmp(&x.is_nan()),
            },
            (Value::Func(x), Value::Func(y)) => match Rc::ptr_eq(x, y) {
                true => Some(cmp::Ordering::Equal),
                false => None,
            },
            (Value::Bool(x), Value::Bool(y)) => x.partial_cmp(y),
            (Value::Str(x), Value::Str(y)) => x.partial_cmp(y),
            (Value::List(x), Value::List(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}
impl cmp::PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        self.partial_cmp(other) == Some(cmp::Ordering::Equal)
    }
}

impl AsRef<Value> for Value {
    fn as_ref(&self) -> &Self {
        self
    }
}
