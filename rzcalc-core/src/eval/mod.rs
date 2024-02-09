mod error;
mod evaluate;

use std::rc::Rc;

use crate::{bail, Context, Function, Node, Op, RzError, Value};

pub use error::EvalError;

pub trait Eval {
    fn eval(&self, ctx: &mut Context) -> Result<Value, RzError>;
}

impl Eval for Node {
    fn eval(&self, ctx: &mut Context) -> Result<Value, RzError> {
        match self {
            Node::Immediate(val) => Ok(val.clone()),
            Node::VarDef(key, arg) => {
                let val = arg.eval(ctx)?;
                ctx.set(key.clone(), val.clone());
                Ok(val)
            }
            Node::FunDef(var, params, body) => {
                let dummy = Function {
                    name: Some(var.clone()),
                    params: params.to_owned(),
                    body: Node::Var("?".into()).into(),
                };
                let cell = Rc::new(dummy);
                let fun = Value::Func(cell.clone());

                *cell.body.borrow_mut() = {
                    let mut child_ctx = Context::with_parent(ctx);
                    child_ctx.set(var.clone(), fun.clone());
                    evaluate::bind_vars(body, params, &child_ctx)?
                };

                ctx.set(var.clone(), fun.clone());
                Ok(fun)
            }
            Node::Var(var) => match ctx.get(var) {
                Some(val) => Ok(val),
                None => bail!(RzError::Eval(EvalError::undefined_variable(var.clone()))),
            },
            Node::BinOp(Op::And, lhs, rhs) => {
                let x = lhs.eval(ctx)?;
                if x.as_bool() {
                    rhs.eval(ctx)
                } else {
                    Ok(x)
                }
            }
            Node::BinOp(Op::Or, lhs, rhs) => {
                let x = lhs.eval(ctx)?;
                if !x.as_bool() {
                    rhs.eval(ctx)
                } else {
                    Ok(x)
                }
            }
            Node::Cond(cond, lhs, rhs) => {
                let x = cond.eval(ctx)?;

                if x.as_bool() {
                    lhs.eval(ctx)
                } else {
                    rhs.eval(ctx)
                }
            }
            Node::BinOp(op, lhs, rhs) => {
                let x = lhs.eval(ctx)?;
                let y = rhs.eval(ctx)?;
                evaluate::evaluate_binop(*op, &x, &y)
            }
            Node::MonOp(op, arg) => {
                let x = arg.eval(ctx)?;
                evaluate::evaluate_monop(*op, &x)
            }
            Node::Apply(fun, args) => {
                let f = fun.eval(ctx)?;
                let mut vals = vec![];
                for arg in args {
                    vals.push(arg.eval(ctx)?);
                }
                evaluate::evaluate_apply(f, vals, ctx)
            }
            Node::Index(lhs, rhs) => {
                let lhs = lhs.eval(ctx)?;
                let rhs = rhs.eval(ctx)?;
                evaluate::evaluate_index(&lhs, &rhs)
            }
            Node::Range(..) => bail!(RzError::Eval(EvalError::any(
                "Range syntax is only supported as list index"
            ))),

            Node::List(args) => {
                let mut vals = vec![];
                for arg in args {
                    vals.push(arg.eval(ctx)?);
                }
                Ok(Value::List(vals.into()))
            }
            Node::Lambda(args, body) => evaluate::evaluate_lambda(None, args, body, ctx),
        }
    }
}
