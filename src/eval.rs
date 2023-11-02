use std::{cmp, f64::EPSILON, ops::Deref, rc::Rc};

use crate::{raise, Context, Function, Node, Op, Value};
use itertools::chain;
use num_traits::Float;

#[derive(Debug, PartialEq, Eq)]
pub struct EvalError(pub String);

pub trait Eval {
    fn eval(&self, ctx: &mut Context) -> Result<Value, EvalError>;
}

impl Eval for Node {
    fn eval(&self, ctx: &mut Context) -> Result<Value, EvalError> {
        match self {
            Node::Immediate(val) => Ok(val.clone()),
            Node::VarDef(key, arg) => {
                let val = arg.eval(ctx)?;
                ctx.set(key, val.clone());
                Ok(val)
            }
            Node::FunDef(var, params, body) => {
                let dummy = Function {
                    name: Some(var.clone()),
                    params: params.to_owned(),
                    body: Node::Var("?".into()).into(),
                };
                let cell = Rc::new(dummy);
                let fun = Value::Function(cell.clone());

                *cell.body.borrow_mut() = {
                    let mut child_ctx = Context::with_parent(ctx);
                    child_ctx.set(var, fun.clone());
                    bind_vars(body, params, &child_ctx)?
                };

                ctx.set(var, fun.clone());
                Ok(fun)
            }
            Node::Var(var) => {
                if let Some(val) = ctx.get(var) {
                    Ok(val)
                } else {
                    raise!(EvalError, "undefined variable '{}'", var)
                }
            }
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
                evaluate_binop(*op, &x, &y)
            }
            Node::MonOp(op, arg) => {
                let x = arg.eval(ctx)?;
                evaluate_monop(*op, &x)
            }
            Node::Apply(fun, args) => {
                let f = fun.eval(ctx)?;
                let mut vals = vec![];
                for arg in args {
                    vals.push(arg.eval(ctx)?);
                }
                evaluate_apply(f, vals, ctx)
            }
            Node::Index(lhs, rhs) => {
                let lhs = lhs.eval(ctx)?;
                let rhs = rhs.eval(ctx)?;
                evaluate_index(&lhs, &rhs)
            }
            Node::Range(..) => {
                raise!(EvalError, "Range syntax is only supported as list index")
            }
            Node::List(args) => {
                let mut vals = vec![];
                for arg in args {
                    vals.push(arg.eval(ctx)?);
                }
                Ok(Value::List(vals.into()))
            }
            Node::Lambda(args, body) => evaluate_lambda(None, args, body, ctx),
        }
    }
}

fn evaluate_binop(op: Op, lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    use Value::Boolean as B;
    use Value::List as L;
    use Value::Number as N;

    if let Some(b) = match op {
        Op::Eq => Some(lhs == rhs),
        Op::Neq => Some(lhs != rhs),
        Op::Lt => lhs.partial_cmp(rhs).map(|x| x == cmp::Ordering::Less),
        Op::Gt => lhs.partial_cmp(rhs).map(|x| x == cmp::Ordering::Greater),
        Op::Lte => lhs.partial_cmp(rhs).map(|x| x != cmp::Ordering::Greater),
        Op::Gte => lhs.partial_cmp(rhs).map(|x| x != cmp::Ordering::Less),
        _ => None,
    } {
        return Ok(B(b));
    }

    #[rustfmt::skip]
    let out = match (op, lhs, rhs) {
        (Op::Add, L(x), L(y)) => {
            let mut tmp = vec![];
            tmp.extend_from_slice(x);
            tmp.extend_from_slice(y);
            L(tmp.into())
        }

        (Op::Add,     N(x), N(y)) => N(*x + *y),
        (Op::Sub,     N(x), N(y)) => N(*x - *y),
        (Op::Mul,     N(x), N(y)) => N(*x * *y),
        (Op::Div,     N(x), N(y)) => N(*x / *y),

        (Op::Rem,     N(x), N(y)) => N(*x % *y),
        (Op::Shr,     N(x), N(y)) => N(*x >> *y),
        (Op::Shl,     N(x), N(y)) => N(*x << *y),

        (Op::BitOr,   N(x), N(y)) => N(*x | *y),
        (Op::BitAnd,  N(x), N(y)) => N(*x & *y),
        (Op::BitXor,  N(x), N(y)) => N(*x ^ *y),

        (Op::Mul,     B(x), B(y)) => B(*x && *y),
        (Op::Add,     B(x), B(y)) => B(*x || *y),

        (Op::BitOr,   B(x), B(y)) => B(*x | *y),
        (Op::BitAnd,  B(x), B(y)) => B(*x & *y),
        (Op::BitXor,  B(x), B(y)) => B(*x ^ *y),

        _ => {
            raise!(
                EvalError,
                "invalid binary operator '{}' for types {} and {}",
                op.name(),
                lhs.type_name(),
                rhs.type_name()
            )
        }
    };

    Ok(out)
}

fn evaluate_monop(op: Op, arg: &Value) -> Result<Value, EvalError> {
    use Value::Boolean as B;
    use Value::Number as N;

    let out = match (op, arg) {
        (Op::Add, N(x)) => N(*x),
        (Op::Sub, N(x)) => N(-*x),
        (Op::Not, x) => B(!x.as_bool()),
        _ => {
            raise!(
                EvalError,
                "invalid unary operator '{}' for type {}",
                op.name(),
                arg.type_name()
            );
        }
    };

    Ok(out)
}

fn evaluate_apply(
    fun: impl AsRef<Value>,
    args: impl AsRef<[Value]>,
    ctx: &mut Context,
) -> Result<Value, EvalError> {
    let fun = fun.as_ref();
    if let Value::Function(f) = fun {
        ctx.call(f.deref(), args.as_ref())
    } else {
        raise!(
            EvalError,
            "value of type {} is not callable",
            fun.type_name()
        )
    }
}

fn evaluate_index(list: &Value, index: &Value) -> Result<Value, EvalError> {
    match (list, index) {
        (Value::List(list), Value::Number(f)) => {
            let i = f.round().int();
            let n = list.len();

            if !f.is_finite() || (f.real() - i as f64).abs() > EPSILON {
                raise!(EvalError, "{} cannot be used as index", f);
            }

            match list.get(i as usize) {
                Some(v) => Ok(v.clone()),
                _ => raise!(
                    EvalError,
                    "index {} is out of bounds for list of size {}",
                    i,
                    n
                ),
            }
        }
        (list, Value::List(indices)) => {
            let mut result = vec![];

            for index in indices.iter() {
                result.push(evaluate_index(list, index)?);
            }

            Ok(Value::List(result.into()))
        }
        (Value::List(_), x) => raise!(
            EvalError,
            "value of type {} cannot be used as index",
            x.type_name()
        ),
        (x, _) => raise!(
            EvalError,
            "value of type {} cannot be indexed",
            x.type_name()
        ),
    }
}

fn bind_vars(
    node: impl AsRef<Node>,
    bound: impl AsRef<[Rc<str>]>,
    ctx: &Context,
) -> Result<Node, EvalError> {
    let node = node.as_ref();
    let bound = bound.as_ref();
    let out = match node {
        Node::Var(var) => {
            if bound.contains(var) {
                Node::Var(var.clone())
            } else if let Some(val) = ctx.get(var) {
                Node::Immediate(val.clone())
            } else {
                raise!(EvalError, "undefined variable '{}'", var);
            }
        }
        Node::Lambda(args, body) => {
            let new_bound: Vec<_> = chain(args, bound).cloned().collect();
            Node::Lambda(args.clone(), Rc::new(bind_vars(body, new_bound, ctx)?))
        }
        Node::BinOp(op, x, y) => Node::BinOp(
            *op,
            Rc::new(bind_vars(x, bound, ctx)?),
            Rc::new(bind_vars(y, bound, ctx)?),
        ),
        Node::MonOp(op, x) => Node::MonOp(*op, Rc::new(bind_vars(x, bound, ctx)?)),
        Node::Apply(fun, args) => {
            let fun = Rc::new(bind_vars(fun, bound, ctx)?);
            let mut vals = vec![];
            for arg in args {
                vals.push(bind_vars(arg, bound, ctx)?);
            }
            Node::Apply(fun, vals)
        }
        Node::Index(lhs, rhs) => Node::Index(
            Rc::new(bind_vars(lhs, bound, ctx)?),
            Rc::new(bind_vars(rhs, bound, ctx)?),
        ),
        Node::Cond(cond, lhs, rhs) => Node::Cond(
            Rc::new(bind_vars(cond, bound, ctx)?),
            Rc::new(bind_vars(lhs, bound, ctx)?),
            Rc::new(bind_vars(rhs, bound, ctx)?),
        ),
        Node::List(args) => {
            let mut vals = vec![];
            for arg in args {
                vals.push(bind_vars(arg, bound, ctx)?);
            }
            Node::List(vals)
        }
        Node::Range(lbnd, ubnd, step) => Node::Range(
            lbnd.as_ref()
                .map(|x| bind_vars(x, bound, ctx))
                .transpose()?
                .map(Rc::new),
            ubnd.as_ref()
                .map(|x| bind_vars(x, bound, ctx))
                .transpose()?
                .map(Rc::new),
            step.as_ref()
                .map(|x| bind_vars(x, bound, ctx))
                .transpose()?
                .map(Rc::new),
        ),
        Node::Immediate(_) => node.clone(),
        Node::VarDef(_, _) | Node::FunDef(_, _, _) => {
            raise!(EvalError, "assignment within lambda is not allowed")
        }
    };

    Ok(out)
}

fn evaluate_lambda(
    name: Option<&str>,
    params: &[Rc<str>],
    body: &Node,
    ctx: &Context,
) -> Result<Value, EvalError> {
    let fun = Function {
        name: name.map(|x| format!("user-defined {}", x).into()),
        params: params.to_owned(),
        body: bind_vars(body, params, ctx)?.into(),
    };

    Ok(Value::Function(Rc::new(fun)))
}
