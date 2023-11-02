use std::{cell::RefCell, rc::Rc};

use crate::{eval::Eval, raise, Context, EvalError, Node};

use super::Value;

pub trait Callable {
    fn name(&self) -> Option<&str>;
    fn call(&self, args: &[Value], ctx: &mut Context) -> Result<Value, EvalError>;
}

pub struct Function {
    pub name: Option<Rc<str>>,
    pub params: Vec<Rc<str>>,
    pub body: RefCell<Node>,
}

impl Callable for Function {
    fn name(&self) -> Option<&str> {
        self.name.as_ref().map(|x| x as _)
    }

    fn call(&self, args: &[Value], parent: &mut Context) -> Result<Value, EvalError> {
        if self.params.len() != args.len() {
            raise!(EvalError, "wrong number of arguments")
        }

        let mut ctx = Context::with_parent(parent);
        for (param, arg) in self.params.iter().zip(args) {
            ctx.set(param, arg.clone());
        }

        Eval::eval(&*self.body.borrow(), &mut ctx)
    }
}

pub struct BuiltinFunction<F>(pub String, pub F);
impl<F> Callable for BuiltinFunction<F>
where
    F: Fn(&[Value], &mut Context) -> Result<Value, EvalError>,
{
    fn name(&self) -> Option<&str> {
        Some(&self.0)
    }

    fn call(&self, args: &[Value], ctx: &mut Context) -> Result<Value, EvalError> {
        self.1(args, ctx)
    }
}
