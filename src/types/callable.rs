use std::{cell::RefCell, rc::Rc};

use crate::{bail, eval::Eval, Context, EvalError, Node};

use super::Value;

pub trait Callable {
    fn call(&self, args: &[Value], ctx: &mut Context) -> Result<Value, EvalError>;

    #[inline]
    fn name(&self) -> Option<&str> {
        None
    }
    #[inline]
    fn param(&self) -> Option<&[Rc<str>]> {
        None
    }
}

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
    fn call(&self, args: &[Value], parent: &mut Context) -> Result<Value, EvalError> {
        if self.params.len() != args.len() {
            bail!(EvalError::invalid_function_call_args(
                self.name.clone().unwrap_or("anonymous".into()),
                self.params.len(),
                args.len()
            ))
        }

        let mut ctx = Context::with_parent(parent);
        for (param, arg) in self.params.iter().zip(args) {
            ctx.set(param, arg.clone());
        }

        Eval::eval(&*self.body.borrow(), &mut ctx)
    }
}

pub struct BuiltinFunction<F>(pub &'static str, pub F);
impl<F> Callable for BuiltinFunction<F>
where
    F: 'static,
    F: Fn(&[Value], &mut Context) -> Result<Value, EvalError>,
{
    #[inline]
    fn name(&self) -> Option<&str> {
        Some(self.0)
    }

    fn call(&self, args: &[Value], ctx: &mut Context) -> Result<Value, EvalError> {
        (self.1)(args, ctx)
    }
}

pub type BuiltinFunctionParams = &'static [(&'static str, super::TypeId)];

macro_rules! params {
    ($($arg:ident: $tparg:ident),* $(,)?) => {[$((stringify!($arg), $crate::TypeId::$tparg)),*]};
}
pub(crate) use params;

pub struct BuiltinFunctionWithParam<F> {
    pub name: &'static str,
    pub params: BuiltinFunctionParams,
    pub inner: F,
}
impl<T> BuiltinFunctionWithParam<T> {
    pub const fn new<F>(
        name: &'static str,
        params: BuiltinFunctionParams,
        func: F,
    ) -> BuiltinFunctionWithParam<F>
    where
        F: Fn(&[Value], &mut Context) -> Result<Value, EvalError>,
    {
        BuiltinFunctionWithParam::<F> {
            name,
            params,
            inner: func,
        }
    }
}

impl<F> Callable for BuiltinFunctionWithParam<F>
where
    F: Fn(&[Value], &mut Context) -> Result<Value, EvalError>,
{
    #[inline]
    fn name(&self) -> Option<&str> {
        Some(self.name)
    }

    fn call(&self, args: &[Value], ctx: &mut Context) -> Result<Value, EvalError> {
        // validate the len of set params with the len of args passed by caller
        if self.params.len() != args.len() {
            bail!(EvalError::invalid_function_call_args(
                self.name,
                self.params.len(),
                args.len()
            ))
        }
        // validate the type by type of param set and the args type id passed by caller
        for ((param_name, param_type_id), arg) in self.params.iter().zip(args) {
            if arg.type_id().ne(param_type_id) {
                return Err(EvalError::invalid_function_arguments_type(
                    self.name,
                    param_name,
                    *param_type_id,
                    arg.type_id(),
                ));
            }
        }

        (self.inner)(args, ctx)
    }
}
