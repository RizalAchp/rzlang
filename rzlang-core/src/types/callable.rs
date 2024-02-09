use core::fmt;
use std::{cell::RefCell, rc::Rc};

use crate::{bail, eval::Eval, Context, EvalError, Node, RzError};

use super::Value;

pub trait Callable: std::fmt::Debug {
    fn call(&self, args: &[Value], ctx: &mut Context) -> Result<Value, RzError>;

    #[inline]
    fn name(&self) -> Option<&str> {
        None
    }
    #[inline]
    fn param(&self) -> Option<&[Rc<str>]> {
        None
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: Option<Rc<str>>,
    pub params: Vec<Rc<str>>,
    pub body: RefCell<Node>,
}

impl Callable for Function {
    #[inline]
    fn name(&self) -> Option<&str> {
        self.name.as_ref().map(|x| x as _)
    }
    #[inline]
    fn param(&self) -> Option<&[Rc<str>]> {
        Some(&self.params)
    }
    fn call(&self, args: &[Value], parent: &mut Context) -> Result<Value, RzError> {
        if self.params.len() != args.len() {
            bail!(RzError::Eval(EvalError::invalid_function_call_args(
                self.name.clone().unwrap_or("anonymous".into()),
                self.params.len(),
                args.len()
            )))
        }

        let mut ctx = Context::with_parent(parent);
        for (param, arg) in self.params.iter().zip(args) {
            ctx.set(param.clone(), arg.clone());
        }

        Eval::eval(&*self.body.borrow(), &mut ctx)
    }
}

pub type BuiltinFunctionType = fn(&[Value], &mut Context) -> Result<Value, RzError>;

pub struct BuiltinFunction(pub &'static str, pub BuiltinFunctionType);
impl Callable for BuiltinFunction {
    #[inline]
    fn name(&self) -> Option<&str> {
        Some(self.0)
    }

    fn call(&self, args: &[Value], ctx: &mut Context) -> Result<Value, RzError> {
        (self.1)(args, ctx)
    }
}

impl fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("BuiltinFunction")
            .field(&self.0)
            .field(&self.1)
            .finish()
    }
}

pub type BuiltinFunctionParams = &'static [(&'static str, super::TypeId)];

macro_rules! params {
    ($($arg:ident: $tparg:ident),* $(,)?) => {[$((stringify!($arg), $crate::TypeId::$tparg)),*]};
}
pub(crate) use params;

pub struct BuiltinFunctionWithParam {
    pub name: &'static str,
    pub params: BuiltinFunctionParams,
    pub inner: BuiltinFunctionType,
}
impl BuiltinFunctionWithParam {
    pub const fn new(
        name: &'static str,
        params: BuiltinFunctionParams,
        inner: BuiltinFunctionType,
    ) -> Self {
        Self {
            name,
            params,
            inner,
        }
    }
}

impl Callable for BuiltinFunctionWithParam {
    #[inline]
    fn name(&self) -> Option<&str> {
        Some(self.name)
    }

    fn call(&self, args: &[Value], ctx: &mut Context) -> Result<Value, RzError> {
        // validate the len of set params with the len of args passed by caller
        let param_c = self.params.iter().filter(|(_, i)| !i.is_none()).count();
        if param_c != args.len() {
            bail!(RzError::Eval(EvalError::invalid_function_call_args(
                self.name,
                self.params.len(),
                args.len()
            )))
        }
        // validate the type by type of param set and the args type id passed by caller
        for ((param_name, param_type_id), arg) in self.params.iter().zip(args) {
            if !arg.type_id().eq(param_type_id) {
                return Err(RzError::Eval(EvalError::invalid_function_arguments_type(
                    self.name,
                    param_name,
                    *param_type_id,
                    arg.type_id(),
                )));
            }
        }

        (self.inner)(args, ctx)
    }
}

impl fmt::Debug for BuiltinFunctionWithParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BuiltinFunctionWithParam")
            .field("name", &self.name)
            .field("params", &self.params)
            .field("inner", &self.inner)
            .finish()
    }
}
