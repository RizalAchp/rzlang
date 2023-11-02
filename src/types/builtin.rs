use std::cmp;
use std::f64::{self, consts};
use std::rc::Rc;

use num_traits::Float;
use rand::random;

use crate::{raise, Callable, Context, EvalError, Value};

use super::number::Number;
use super::BuiltinFunction;

#[inline]
fn set_const(ctx: &mut Context, key: &str, val: impl Into<Number>) {
    ctx.set(key, Value::number(val))
}

#[inline]
fn check_args_num<'a, const N: usize>(
    name: &str,
    args: &'a [Value],
) -> Result<&'a [Value; N], EvalError> {
    match args.get(..N).and_then(|x| x.try_into().ok()) {
        Some(k) => Ok(k),
        None => raise!(
            EvalError,
            "{name} expected {N} argument[s], {len} argument[s] given",
            len = args.len(),
        ),
    }
}

#[inline]
fn cast_function(arg: &Value) -> Result<&dyn Callable, EvalError> {
    match arg {
        Value::Function(fun) => Ok(&**fun),
        _ => raise!(EvalError, "invalid cast of {} to function", arg.type_name()),
    }
}

#[inline]
fn cast_list(arg: &Value) -> Result<&[Value], EvalError> {
    match arg {
        Value::List(list) => Ok(list),
        _ => raise!(EvalError, "invalid cast of {} to list", arg.type_name()),
    }
}

#[inline]
fn cast_string(arg: &Value) -> Result<String, EvalError> {
    match arg {
        Value::Str(s) => Ok(s.clone()),
        _ => raise!(EvalError, "invalid cast of {} to string", arg.type_name()),
    }
}

#[inline]
fn cast_number(arg: &Value) -> Result<Number, EvalError> {
    match arg {
        Value::Number(x) => Ok(*x),
        Value::Boolean(true) => Ok(Number::Real(1.0)),
        Value::Boolean(false) => Ok(Number::Real(0.0)),
        _ => raise!(EvalError, "invalid cast of {} to number", arg.type_name()),
    }
}

#[inline]
fn set_closure<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(&[Value], &mut Context) -> Result<Value, EvalError>,
{
    ctx.set(
        key,
        Value::Function(Rc::new(BuiltinFunction(key.to_string(), fun))),
    );
}

#[inline]
fn set_unary<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(Number) -> Number,
{
    let name = key.to_owned();
    set_closure(ctx, key, move |args, _| {
        let [x] = check_args_num::<1>(&name, args)?;
        let x = cast_number(x)?;

        Ok(Value::Number(fun(x)))
    });
}

#[inline]
fn set_binary<F: 'static>(ctx: &mut Context, key: &str, fun: F)
where
    F: Fn(Number, Number) -> Number,
{
    let name = key.to_owned();
    set_closure(ctx, key, move |args, _| {
        let [x, y] = check_args_num::<2>(&name, args)?;
        let x = cast_number(x)?;
        let y = cast_number(y)?;

        Ok(Value::Number(fun(x, y)))
    });
}

fn set_util(ctx: &mut Context) {
    for (key, ord) in &[
        ("max", cmp::Ordering::Less),
        ("min", cmp::Ordering::Greater),
    ] {
        let ord = *ord;

        set_closure(ctx, key, move |args, _| {
            if args.is_empty() {
                raise!(EvalError, "{} of empty sequence", key);
            }

            let args = match args {
                [Value::List(list)] => list,
                x => x,
            };

            let mut best = args[0].clone();

            for arg in args {
                if let Some(x) = best.partial_cmp(arg) {
                    if x == ord {
                        best = arg.clone();
                    }
                } else {
                    raise!(
                        EvalError,
                        "types '{}' and '{}' cannot be compared",
                        best.type_name(),
                        arg.type_name()
                    );
                }
            }

            Ok(best)
        });
    }

    set_closure(ctx, "rand", |args, _| {
        let a = args.get(0).map(cast_number).transpose()?;
        let b = args.get(1).map(cast_number).transpose()?;

        let (a, b) = match (a, b) {
            (Some(x), Some(y)) => (x, y),
            (Some(x), None) => (Number::Real(0.0), x),
            _ => (Number::Real(0.0), Number::Real(1.0)),
        };

        let out = (b - a) * Number::Real(random::<f64>()) + a;
        Ok(Value::Number(out))
    });

    set_closure(ctx, "range", |args, _| {
        let a = args.get(0).map(cast_number).transpose()?;
        let b = args.get(1).map(cast_number).transpose()?;
        use Number::Real as R;

        let (a, b) = match (a, b) {
            (Some(x), Some(y)) => (x, y),
            (Some(x), None) => (R(0.0), x),
            _ => (R(0.0), R(0.0)),
        };

        let mut i = 0;
        let mut list = vec![];

        while a + R(i as f64) < b {
            list.push(Value::Number(a + R(i as f64)));
            i += 1;
        }

        Ok(Value::List(list.into()))
    });

    set_closure(ctx, "map", |args, ctx| {
        let [fun, list] = check_args_num::<2>("map", args)?;
        let fun = cast_function(fun)?;
        let list = cast_list(list)?;

        let out = list
            .iter()
            .map(|x| ctx.call(fun, &[x.clone()]))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Value::List(out.into()))
    });

    set_closure(ctx, "linspace", |args, _| {
        let [lbnd, ubnd, steps] = check_args_num::<3>("linspace", args)?;
        let lbnd = cast_number(lbnd)?;
        let ubnd = cast_number(ubnd)?;
        let steps = cast_number(steps)?;
        let n = steps.floor().int();

        if n <= 2 {
            raise!(
                EvalError,
                "number of steps cannot be less than 2, got {}",
                steps
            );
        }

        use Number::Real as R;
        let list = (0..n)
            .map(|i| R((i as f64) / ((n - 1) as f64)))
            .map(|v| (R(1.0) - v) * lbnd + v * ubnd)
            .map(Value::Number)
            .collect::<Vec<_>>();

        Ok(Value::List(list.into()))
    });

    set_closure(ctx, "sort", |args, _| {
        let [list] = check_args_num::<1>("sort", args)?;
        let mut vec = cast_list(list)?.to_vec();
        vec.sort_by(|a, b| a.partial_cmp(b).unwrap_or(cmp::Ordering::Equal));

        Ok(Value::List(vec.into()))
    });

    set_closure(ctx, "length", |args, _| {
        let [arg] = check_args_num::<1>("length", args)?;
        let n = cast_list(arg)
            .map(|x| x.len())
            .or_else(|_| cast_string(arg).map(|x| x.chars().count()))?;
        Ok(Value::Number(Number::Real(n as f64)))
    });

    macro_rules! impl_closure_print {
        ($($print:ident),*) => {$(
            set_closure(ctx, stringify!($print), |args, _| {
                let val = args.get(0).ok_or_else(|| EvalError("no arguments provided".to_owned()))?;
                $print!("{val}");
                Ok(val.clone())
            });
        )*}
    }
    impl_closure_print!(print, println, eprint, eprintln);
    set_closure(ctx, "debug", |args, _| {
        let arg = args
            .get(0)
            .ok_or_else(|| EvalError("no arguments provided".to_owned()))?;
        Ok(dbg!(arg).clone())
    });

    set_closure(ctx, "binary", |args, _| {
        let arg = args
            .get(0)
            .ok_or_else(|| EvalError("no arguments provided".to_owned()))?;
        let num = cast_number(arg)?;
        let fmt = if args.get(1).is_some() {
            format!("{:#b}", num.int())
        } else {
            format!("{:b}", num.int())
        };
        Ok(Value::Str(fmt))
    });

    set_closure(ctx, "octal", |args, _| {
        let arg = args
            .get(0)
            .ok_or_else(|| EvalError("no arguments provided".to_owned()))?;
        let num = cast_number(arg)?;
        let fmt = if args.get(1).is_some() {
            format!("{:#o}", num.int())
        } else {
            format!("{:o}", num.int())
        };
        Ok(Value::Str(fmt))
    });

    set_closure(ctx, "hex", |args, _| {
        let arg = args
            .get(0)
            .ok_or_else(|| EvalError("no arguments provided".to_owned()))?;
        let num = cast_number(arg)?;
        let fmt = if args.get(1).is_some() {
            format!("{:X}", num.int())
        } else {
            format!("{:x}", num.int())
        };
        Ok(Value::Str(fmt))
    });

    //set_closure(ctx, "sum", |args| {
    //    check_args_n("sum", args, 1)?;
    //    let mut list = cast_list(&args[0])?.to_vec();
    //    Ok(Value::List(list.into()))
    //});
}

#[allow(unused)]
pub fn context() -> Context<'static> {
    let mut ctx = Context::new();

    {
        let c = &mut ctx;
        set_const(c, "pi", consts::PI);
        set_const(c, "tau", consts::TAU);
        set_const(c, "e", consts::E);
        set_const(c, "nan", f64::NAN);
        set_const(c, "inf", f64::INFINITY);
        set_const(c, "neginf", f64::NEG_INFINITY);

        set_unary(c, "asin", |x| x.asin());
        set_unary(c, "acos", |x| x.acos());
        set_unary(c, "atan", |x| x.atan());
        set_unary(c, "sin", |x| x.sin());
        set_unary(c, "cos", |x| x.cos());
        set_unary(c, "tan", |x| x.tan());
        set_unary(c, "ln", |x| x.ln());
        set_unary(c, "log10", |x| x.log10());
        set_unary(c, "log2", |x| x.log2());
        set_unary(c, "abs", |x| x.abs());
        set_unary(c, "ceil", |x| x.ceil());
        set_unary(c, "floor", |x| x.floor());
        set_unary(c, "round", |x| x.round());
        set_unary(c, "sqrt", |x| x.sqrt());
        set_unary(c, "exp", |x| x.exp());
        set_unary(c, "float", |x| x);
        set_unary(c, "sign", |x| x.signum());

        set_binary(c, "min_num", |x, y| x.min(y));
        set_binary(c, "max_num", |x, y| x.max(y));

        set_binary(c, "powf", |x, y| x.powf(y));
        set_binary(c, "pow", |x, y| x.powi(y.int() as i32));
        set_binary(c, "log", |x, y| x.log(y));
        set_binary(c, "hypot", |x, y| x.hypot(y));
        set_binary(c, "atan2", |x, y| x.atan2(y));

        set_util(c);
    }

    ctx
}
