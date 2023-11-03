use std::{fmt::Display, rc::Rc};

use crate::{
    types::{TypeId as TpId, ValueError},
    Op, Value,
};

#[derive(Debug, PartialEq, Eq)]
pub enum EvalErrorKind {
    UndefinedVariable(Rc<str>),
    NotCallable(TpId),

    IndexOutOfBound {
        idx: usize,
        size: usize,
    },
    InvalidUnaryOp {
        operator: Op,
        tp: TpId,
    },
    InvalidBinaryOp {
        operator: Op,
        tp_lhs: TpId,
        tp_rhs: TpId,
    },
    InvalidIndexer {
        typename: &'static str,
    },
    InvalidFunctionCallArguments {
        name: Rc<str>,
        nargs: usize,
        len_args: usize,
    },
    InvalidFunctionCallArgumentsType {
        name: &'static str,
        arg: &'static str,
        required: TpId,
        got: TpId,
    },
    CannotBeIndex {
        typename: &'static str,
    },

    StackOverFlow(usize),
    ValueError(ValueError),

    NotAllowed(String),
    Any(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct EvalError(EvalErrorKind);

impl EvalError {
    pub const fn new(kind: EvalErrorKind) -> Self {
        Self(kind)
    }
    pub const fn undefined_variable(var: Rc<str>) -> Self {
        Self::new(EvalErrorKind::UndefinedVariable(var))
    }
    pub const fn not_callable(val: &Value) -> Self {
        Self::new(EvalErrorKind::NotCallable(val.type_id()))
    }
    pub const fn index_out_of_bound(idx: usize, size: usize) -> Self {
        Self::new(EvalErrorKind::IndexOutOfBound { idx, size })
    }
    pub const fn invalid_unary(operator: Op, val: &Value) -> Self {
        Self::new(EvalErrorKind::InvalidUnaryOp {
            operator,
            tp: val.type_id(),
        })
    }
    pub const fn invalid_binary(operator: Op, lhs: &Value, rhs: &Value) -> Self {
        Self::new(EvalErrorKind::InvalidBinaryOp {
            operator,
            tp_lhs: lhs.type_id(),
            tp_rhs: rhs.type_id(),
        })
    }

    pub const fn invalid_indexer(typename: &'static str) -> Self {
        Self::new(EvalErrorKind::InvalidIndexer { typename })
    }

    pub const fn cannot_be_index(val: &Value) -> Self {
        Self::new(EvalErrorKind::CannotBeIndex {
            typename: val.type_name(),
        })
    }
    pub const fn stack_overflow(n: usize) -> Self {
        Self::new(EvalErrorKind::StackOverFlow(n))
    }

    pub fn invalid_function_call_args(
        name: impl Into<Rc<str>>,
        nargs: usize,
        len_args: usize,
    ) -> Self {
        Self::new(EvalErrorKind::InvalidFunctionCallArguments {
            name: name.into(),
            nargs,
            len_args,
        })
    }

    pub const fn invalid_function_arguments_type(
        name: &'static str,
        arg: &'static str,
        required: TpId,
        got: TpId,
    ) -> Self {
        Self::new(EvalErrorKind::InvalidFunctionCallArgumentsType {
            name,
            arg,
            required,
            got,
        })
    }

    pub fn not_allowed(s: impl ToString) -> Self {
        Self::new(EvalErrorKind::NotAllowed(s.to_string()))
    }
    pub fn any(s: impl ToString) -> Self {
        Self::new(EvalErrorKind::Any(s.to_string()))
    }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            EvalErrorKind::UndefinedVariable(v) => write!(f, "undefined variable of '{v}'."),

            EvalErrorKind::NotCallable(tp) => write!(f, "value type of {tp} is not callable."),
            EvalErrorKind::IndexOutOfBound { idx, size } => write!(
                f,
                "index {idx} is out of bounds for list type of size {size}."
            ),
            EvalErrorKind::InvalidUnaryOp {
                operator,
                tp: typename,
            } => {
                write!(
                    f,
                    "invalid unary operator '{operator}' for type {typename}."
                )
            }
            EvalErrorKind::InvalidBinaryOp {
                operator,
                tp_lhs: typename_lhs,
                tp_rhs: typename_rhs,
            } => write!(
                f,
                "invalid binary operator '{operator}' for types {typename_lhs} and {typename_rhs}.",
            ),

            EvalErrorKind::InvalidIndexer { typename } => {
                write!(f, "number of type {typename} cannot be used as index.")
            }
            EvalErrorKind::CannotBeIndex { typename } => {
                write!(f, "value of type {typename} cannot be used as index.")
            }
            EvalErrorKind::StackOverFlow(n) => {
                write!(f, "stack overflow, call depth cannot exceed {n}")
            }
            EvalErrorKind::InvalidFunctionCallArguments {
                name,
                nargs,
                len_args,
            } => {
                write!(f, "function call {name} expected {nargs} argument[s], {len_args} argument[s] given",)
            }
            EvalErrorKind::InvalidFunctionCallArgumentsType {
                name,
                arg,
                required,
                got,
            } => {
                write!(f, "argument '{arg}' of function call '{name}' required to be type of <{required}>, but got type of <{got}>")
            }
            EvalErrorKind::ValueError(err) => write!(f, "{err}"),
            EvalErrorKind::NotAllowed(s) => write!(f, "{s}, is not allowed"),
            EvalErrorKind::Any(s) => write!(f, "{s}"),
        }
    }
}

impl From<ValueError> for EvalError {
    fn from(value: ValueError) -> Self {
        Self::new(EvalErrorKind::ValueError(value))
    }
}
