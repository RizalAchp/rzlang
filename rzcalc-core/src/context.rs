use std::{collections::HashMap, rc::Rc};

use nanorand::WyRand;

use crate::{
    bail,
    types::{BuiltinFunctionParams, TypeId},
    BuiltinFunction, BuiltinFunctionWithParam, Callable, EvalError, Value,
};

#[derive(Debug, Clone)]
pub struct Context<'a> {
    rng: WyRand,
    stack: Vec<Rc<str>>,
    stack_size: usize,
    parent: Option<&'a Context<'a>>,
    scope: HashMap<Rc<str>, Value>,
}

impl<'a> Default for Context<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Context<'a> {
    const MAX_STACK_SIZE: Option<&'static str> = option_env!("RZ_STACK_SIZE");

    pub fn new() -> Self {
        let stack_size = Self::MAX_STACK_SIZE
            .unwrap_or("512")
            .parse::<usize>()
            .unwrap_or(512);
        Context {
            rng: WyRand::new(),
            stack: Vec::with_capacity(stack_size),
            stack_size,
            parent: None,
            scope: HashMap::new(),
        }
    }

    #[allow(unused)]
    pub fn new_with_builtin() -> Context<'static> {
        crate::builtin_context()
    }

    pub fn with_parent(ctx: &'a Context) -> Self {
        Context {
            rng: ctx.rng.clone(),
            stack: ctx.stack.clone(),
            stack_size: ctx.stack_size,
            parent: Some(ctx),
            scope: HashMap::new(),
        }
    }

    pub fn get(&self, key: impl AsRef<str>) -> Option<Value> {
        let key = key.as_ref();
        if let Some(x) = self.scope.get(key) {
            Some(x.clone())
        } else if let Some(p) = self.parent {
            p.get(key)
        } else {
            None
        }
    }

    pub fn set(&mut self, key: impl AsRef<str>, val: Value) {
        self.scope.insert(key.as_ref().into(), val);
    }

    pub fn call(&mut self, fun: &dyn Callable, args: &[Value]) -> Result<Value, EvalError> {
        if self.stack.len() >= self.stack_size {
            bail!(EvalError::stack_overflow(self.stack_size));
        }

        let name = fun.name().unwrap_or("anonymous function").into();

        self.stack.push(name);
        let result = fun.call(args, self);
        self.stack.pop();
        result
    }

    #[inline]
    pub fn rng(&mut self) -> &mut WyRand {
        &mut self.rng
    }
}

#[allow(unused)]
impl<'a> Context<'a> {
    #[inline]
    pub fn set_const(&mut self, key: &str, val: impl Into<Value>) {
        self.set(key, val.into())
    }

    pub fn set_func_with_args<F>(&mut self, key: &'static str, args: BuiltinFunctionParams, fun: F)
    where
        F: 'static,
        F: Fn(&[Value], &mut Context) -> Result<Value, EvalError>,
    {
        self.set(
            key,
            Value::Func(Rc::new(BuiltinFunctionWithParam::<F>::new(key, args, fun))),
        );
    }
    pub fn set_func<F>(&mut self, key: &'static str, fun: F)
    where
        F: 'static,
        F: Fn(&[Value], &mut Context) -> Result<Value, EvalError>,
    {
        self.set(key, Value::Func(Rc::new(BuiltinFunction(key, fun))))
    }

    pub fn set_unary<F>(
        &mut self,
        key: &'static str,
        args: &'static [(&'static str, TypeId); 1],
        func: F,
    ) where
        F: 'static,
        F: Fn(&Value) -> Result<Value, EvalError>,
    {
        self.set_func_with_args(key, args, move |args, ctx| func(&args[0]))
    }

    pub fn set_binary<F>(
        &mut self,
        key: &'static str,
        args: &'static [(&'static str, TypeId); 2],
        func: F,
    ) where
        F: 'static,
        F: Fn(&Value, &Value) -> Result<Value, EvalError>,
    {
        self.set_func_with_args(key, args, move |args, ctx| func(&args[0], &args[1]))
    }
}

#[macro_export]
macro_rules! add_binary_function {
    ($ctx:ident, $name:ident ($($arg:ident: $tparg:ident),*) $fnbody:expr) => {
        $ctx.set_binary(stringify!($name), [$((stringify!($ident), $crate::ValueTypeId::$tparg)),*], $fnbody)
    };
}
