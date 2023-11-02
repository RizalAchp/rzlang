use std::{collections::HashMap, rc::Rc};

use crate::{raise, Callable, EvalError, Value};

#[derive(Debug, Clone)]
pub struct Context<'a> {
    stack: Vec<Rc<str>>,
    parent: Option<&'a Context<'a>>,
    scope: HashMap<Rc<str>, Value>,
}

impl<'a> Default for Context<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Context {
            stack: vec![],
            parent: None,
            scope: HashMap::new(),
        }
    }

    pub fn with_parent(ctx: &'a Context) -> Self {
        Context {
            stack: ctx.stack.clone(),
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
        if self.stack.len() >= 512 {
            raise!(
                EvalError,
                "stack overflow, call depth cannot exceed {}",
                self.stack.len()
            )
        }

        let name = fun.name().unwrap_or("anonymous function");

        self.stack.push(name.into());
        let result = fun.call(args, self);
        self.stack.pop();
        result
    }
}
