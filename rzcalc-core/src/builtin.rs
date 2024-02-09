use std::cmp;
use std::f64::{self, consts};

use crate::types::{params, Number, Value, ValueError};
use crate::{Context, EvalError, TypeId};

#[rustfmt::skip]
#[allow(unused)]
pub fn context() -> Context<'static> {
    let mut ctx = Context::new();

    ctx.set_const("PI", consts::PI);
    ctx.set_const("TAU", consts::TAU);
    ctx.set_const("E", consts::E);
    ctx.set_const("NAN", f64::NAN);
    ctx.set_const("INF", f64::INFINITY);
    ctx.set_const("NEGINF", f64::NEG_INFINITY);

    ctx.set_func_with_args("asin",  &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.asin().into()   ));
    ctx.set_func_with_args("acos",  &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.acos().into()   ));
    ctx.set_func_with_args("atan",  &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.atan().into()   ));
    ctx.set_func_with_args("sin",   &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.sin().into()    ));
    ctx.set_func_with_args("cos",   &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.cos().into()    ));
    ctx.set_func_with_args("tan",   &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.tan().into()    ));
    ctx.set_func_with_args("ln",    &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.ln().into()     ));
    ctx.set_func_with_args("log10", &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.log10().into()  ));
    ctx.set_func_with_args("log2",  &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.log2().into()   ));
    ctx.set_func_with_args("abs",   &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.abs().into()    ));
    ctx.set_func_with_args("ceil",  &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.ceil().into()   ));
    ctx.set_func_with_args("floor", &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.floor().into()  ));
    ctx.set_func_with_args("round", &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.round().into()  ));
    ctx.set_func_with_args("sqrt",  &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.sqrt().into()   ));
    ctx.set_func_with_args("exp",   &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.exp().into()    ));
    ctx.set_func_with_args("float", &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.real().into()   ));
    ctx.set_func_with_args("sign",  &params!(num: Num), move |args, _| Ok(args.first().unwrap().get_num()?.signum().into() ));

    ctx.set_func_with_args("min_num", &params!(lhs: Num, rhs: Num), move |args, _| Ok(args[0].get_num()?.min(args[1].get_num()?).into()               ));
    ctx.set_func_with_args("max_num", &params!(lhs: Num, rhs: Num), move |args, _| Ok(args[0].get_num()?.max(args[1].get_num()?).into()               ));
    ctx.set_func_with_args("powf",    &params!(lhs: Num, rhs: Num), move |args, _| Ok(args[0].get_num()?.powf(args[1].get_num()?).into()              ));
    ctx.set_func_with_args("pow",     &params!(lhs: Num, rhs: Num), move |args, _| Ok(args[0].get_num()?.powi(args[1].get_num()?.int() as i32).into() ));
    ctx.set_func_with_args("log",     &params!(lhs: Num, rhs: Num), move |args, _| Ok(args[0].get_num()?.log(args[1].get_num()?).into()               ));
    ctx.set_func_with_args("hypot",   &params!(lhs: Num, rhs: Num), move |args, _| Ok(args[0].get_num()?.hypot(args[1].get_num()?).into()             ));
    ctx.set_func_with_args("atan2",   &params!(lhs: Num, rhs: Num), move |args, _| Ok(args[0].get_num()?.atan2(args[1].get_num()?).into()             ));

    set_util(&mut ctx);
    ctx
}

fn set_util(ctx: &mut Context) {
    ctx.set_func("max", move |args, _| {
        if args.is_empty() {
            return Err(From::from(EvalError::empty_arguments("max", TypeId::Any)));
        }
        let args = match args {
            [Value::List(list)] => list,
            x => x,
        };
        Ok(args
            .iter()
            .max_by(|x, y| x.partial_cmp(y).unwrap_or(cmp::Ordering::Equal))
            .unwrap()
            .clone())
    });

    ctx.set_func("min", move |args, _| {
        if args.is_empty() {
            return Err(From::from(EvalError::empty_arguments("min", TypeId::Any)));
        }
        let args = match args {
            [Value::List(list)] => list,
            x => x,
        };
        Ok(args
            .iter()
            .min_by(|x, y| x.partial_cmp(y).unwrap_or(cmp::Ordering::Equal))
            .unwrap()
            .clone())
    });

    ctx.set_func("rand", move |args, ctx| {
        let a = args.first().map(Value::get_num).transpose()?;
        let b = args.get(1).map(Value::get_num).transpose()?;
        let (start, end) = match (a, b) {
            (Some(x), Some(y)) => (x.int(), y.int()),
            (Some(x), None) => (0, x.int()),
            _ => (0, 1),
        };

        Ok(Value::num(nanorand::Rng::generate_range(
            ctx.rng(),
            start..=end,
        )))
    });

    ctx.set_func("range", |args, _| {
        use Number::Int as I;
        let a = args.first().map(Value::get_num).transpose()?;
        let b = args.get(1).map(Value::get_num).transpose()?;
        let step = args
            .get(2)
            .map(|x| Value::get_num(x).unwrap_or(I(1)).int())
            .unwrap_or(1);

        let (start, end) = match (a, b) {
            (Some(x), Some(y)) => (x.int(), y.int()),
            (Some(x), None) => (0, x.int()),
            _ => (0, 0),
        };

        Ok(Value::List(
            (start..end)
                .step_by(step as usize)
                .map(|x| Value::Num(Number::Int(x)))
                .collect(),
        ))
    });

    ctx.set_func_with_args(
        "map",
        &params!(map_function: Func, listtype: List),
        |args, ctx| {
            let fun = args[0].get_func()?;
            let list = args[1].get_list()?;

            let out = list
                .iter()
                .map(|x| ctx.call(&*fun, &[x.clone()]))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Value::List(out.into()))
        },
    );

    ctx.set_func_with_args(
        "linspace",
        &params!(lbnd: Num, ubnd: Num, steps: Num),
        |args, _| {
            let lbnd = args[0].get_num()?;
            let ubnd = args[1].get_num()?;
            let steps = args[2].get_num()?.int();
            if steps <= 2 {
                return Err(From::from(EvalError::any(format!(
                    "number of steps cannot be less than 2, got {}",
                    steps
                ))));
            }

            use Number::Real as R;

            Ok(Value::List(
                (0..steps)
                    .map(|i| R((i as f64) / ((steps - 1) as f64)))
                    .map(|v| (R(1.0) - v) * lbnd + v * ubnd)
                    .map(Value::Num)
                    .collect(),
            ))
        },
    );

    ctx.set_func_with_args("sort", &params!(listtype: List), |args, _| {
        let mut vec = args[0].get_list()?.to_vec();
        vec.sort_by(|a, b| a.partial_cmp(b).unwrap_or(cmp::Ordering::Equal));
        Ok(Value::List(vec.into()))
    });

    ctx.set_func_with_args("length", &params!(list_like_type: List), |args, _| {
        let val = &args[0];
        match val
            .get_list()
            .map(|x| x.len())
            .or_else(|_| val.get_str().map(|x| x.chars().count()))
        {
            Ok(n) => Ok(Value::num(n as i64)),
            Err(_) => Err(From::from(ValueError::not_list_alike(val.type_id()))),
        }
    });

    macro_rules! impl_closure_print {
        ($($print:ident),*) => {$(
            ctx.set_func(stringify!($print), |args, _| {
                if args.is_empty() {
                    return Err(From::from(EvalError::any(
                        concat!("no arguments provided, function '", stringify!($print), "' requires an argument")
                    )));
                }
                let len = args.len();
                for (i, arg) in args.iter().enumerate() {
                    $print!("{arg}{}", if i != (len - 1) {" "} else {""});
                }
                Ok(Value::None)
            });
        )*}
    }
    impl_closure_print!(print, println, eprint, eprintln);

    ctx.set_func("dbg", |args, _| {
        if args.is_empty() {
            return Err(From::from(EvalError::any(concat!(
                "no arguments provided, function '",
                stringify!($print),
                "' requires an argument"
            ))));
        }
        let len = args.len();
        for (i, arg) in args.iter().enumerate() {
            eprint!("{arg:?}{}", if i != (len - 1) { " " } else { "" });
        }
        eprintln!();
        Ok(args.first().cloned().unwrap())
    });

    ctx.set_func_with_args("bin", &params!(num: Num, with_prefix: Any), |args, _| {
        let num = args[0].get_num()?;
        let with_prefix = args[1].as_bool();
        Ok(Value::Str(
            if with_prefix {
                format!("{:#b}", num.int())
            } else {
                format!("{:b}", num.int())
            }
            .into(),
        ))
    });

    ctx.set_func_with_args("oct", &params!(num: Num, with_prefix: Any), |args, _| {
        let num = args[0].get_num()?;
        let with_prefix = args[1].as_bool();
        Ok(Value::Str(
            if with_prefix {
                format!("{:#o}", num.int())
            } else {
                format!("{:o}", num.int())
            }
            .into(),
        ))
    });

    ctx.set_func_with_args(
        "hex",
        &params!(num: Num, upper_case: Any, with_prefix: Any),
        |args, _| {
            let num = args[0].get_num()?.int();
            let upper_case = args[1].as_bool();
            let with_prefix = args[2].as_bool();
            Ok(Value::Str(
                match (with_prefix, upper_case) {
                    (true, true) => format!("{:#X}", num),
                    (true, false) => format!("{:#x}", num),
                    (false, true) => format!("{:X}", num),
                    (false, false) => format!("{:x}", num),
                }
                .into(),
            ))
        },
    );

    ctx.set_func("help", |args, _ctx| {
        use std::fmt::Write;

        let mut out = String::new();
        for arg in args {
            write!(out, "{} {}", arg.type_name(), arg)?;
        }
        Ok(Value::Str(out.into()))
    });

    //set_closure(ctx, "sum", |args| {
    //    check_args_n("sum", args, 1)?;
    //    let mut list = cast_list(&args[0])?.to_vec();
    //    Ok(Value::List(list.into()))
    //});
}
