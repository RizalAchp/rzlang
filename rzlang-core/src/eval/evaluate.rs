use std::{cmp, f64::EPSILON, ops::Deref, rc::Rc};

use itertools::chain;

use crate::{bail, Context, EvalError, Function, Node, Op, RzError, Value};

pub(super) fn evaluate_binop(op: Op, lhs: &Value, rhs: &Value) -> Result<Value, RzError> {
    use Value::Bool as B;
    use Value::List as L;
    use Value::Num as N;
    use Value::Str as S;

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
            let mut tmp = x.to_vec();
            tmp.extend_from_slice(y);
            L(tmp.into())
        }
        (Op::Add, S(x), S(y)) => S((x.to_string() + y).into()),

        (Op::Add,     N(x), N(y)) => N(*x + *y),
        (Op::Sub,     N(x), N(y)) => N(*x - *y),
        (Op::Mul,     N(x), N(y)) => N(*x * *y),
        (Op::Div,     N(x), N(y)) => N(x.safe_div(*y)),

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

        _ => bail!(From::from(EvalError::invalid_binary(op, lhs, rhs))),
    };

    Ok(out)
}

pub(super) fn evaluate_monop(op: Op, arg: &Value) -> Result<Value, RzError> {
    use Value::Bool as B;
    use Value::Num as N;

    let out = match (op, arg) {
        (Op::Add, N(x)) => N(*x),
        (Op::Sub, N(x)) => N(-*x),
        (Op::Not, x) => B(!x.as_bool()),
        _ => bail!(From::from(EvalError::invalid_unary(op, arg))),
    };

    Ok(out)
}

pub(super) fn evaluate_apply(
    fun: impl AsRef<Value>,
    args: impl AsRef<[Value]>,
    ctx: &mut Context,
) -> Result<Value, RzError> {
    let fun = fun.as_ref();
    if let Value::Func(f) = fun {
        ctx.call(f.deref(), args.as_ref())
    } else {
        bail!(From::from(EvalError::not_callable(fun)))
    }
}

pub(super) fn evaluate_index(list: &Value, index: &Value) -> Result<Value, RzError> {
    match (list, index) {
        (Value::List(list), Value::Num(f)) => {
            let i = f.round().int() as usize;
            let n = list.len();

            if !f.is_finite() || (f.real() - i as f64).abs() > EPSILON {
                bail!(From::from(EvalError::invalid_indexer(f.type_name())));
            }

            match list.get(i) {
                Some(v) => Ok(v.clone()),
                _ => bail!(From::from(EvalError::index_out_of_bound(i, n))),
            }
        }
        (list, Value::List(indices)) => {
            let mut result = vec![];

            for index in indices.iter() {
                result.push(evaluate_index(list, index)?);
            }

            Ok(Value::List(result.into()))
        }
        (Value::List(_), x) => bail!(From::from(EvalError::invalid_indexer(x.type_name()))),
        (x, _) => bail!(From::from(EvalError::cannot_be_index(x))),
    }
}

pub(super) fn bind_vars(
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
                bail!(EvalError::undefined_variable(var.clone()))
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
            bail!(EvalError::not_allowed("assignment within lambda"))
        }
    };

    Ok(out)
}

pub(super) fn evaluate_lambda(
    name: Option<&str>,
    params: &[Rc<str>],
    body: &Node,
    ctx: &Context,
) -> Result<Value, RzError> {
    let fun = Function {
        name: name.map(|x| format!("user-defined {}", x).into()),
        params: params.to_owned(),
        body: bind_vars(body, params, ctx)?.into(),
    };

    Ok(Value::Func(Rc::new(fun)))
}
