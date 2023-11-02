mod eval {
    use crate::{builtin_context, eval::Eval, Context, Value};

    thread_local!(static GLOBAL_CONTEXT: Context<'static> = builtin_context());

    fn check(line: &str, expected: Value) {
        let root = crate::Parser::parse_string(line)
            .parse()
            .expect("failed to parse string");
        let mut ctx = GLOBAL_CONTEXT.with(|x| x.clone());
        assert_eq!(
            root.eval(&mut ctx),
            Ok(expected.clone()),
            "<expr: '{line}'> is not equal to {expected}"
        );
    }

    #[test]
    fn test_operator() {
        check("1 + 2 * 3", Value::number(7));
        check("1 * 2 + 3", Value::number(5));

        check("2 * 3", Value::number(6));
        check("4 / 2", Value::number(2));
        check("1 + 2", Value::number(3));
        check("2 - 1", Value::number(1));
        check("5 % 2", Value::number(1));
        check("1 << 10", Value::number(1 << 10));
        check("10 >> 2", Value::number(10 >> 2));
        check("10 | 2", Value::number(10 | 2));
        check("10 & 2", Value::number(10 & 2));
        check("10 ^ 2", Value::number(10 ^ 2));

        check("true * true", Value::Boolean(true));
        check("true * false", Value::Boolean(false));
        check("true + false", Value::Boolean(true));
        check("false + false", Value::Boolean(false));

        check("1 and 0", Value::number(0));
        check("0 and 2", Value::number(0));
        check("3 or 0", Value::number(3));
        check("0 or 4", Value::number(4));

        check("1 and 2 or 3 and 4", Value::number(2));
        check("0 and 2 or 3 and 4", Value::number(4));
        check("0 and 0 or 3 and 4", Value::number(4));
        check("0 and 0 or 0 and 4", Value::number(0));
        check("0 and 0 or 0 and 0", Value::number(0));

        check("1 or 2 and 3 or 4", Value::number(1));
        check("0 or 2 and 3 or 4", Value::number(3));
        check("0 or 0 and 3 or 4", Value::number(4));
        check("0 or 0 and 0 or 4", Value::number(4));
        check("0 or 0 and 0 or 0", Value::number(0));
    }

    #[test]
    fn test_operator_extra() {
        check("(5 << 2) % (2 * 6)", Value::number(8));
        check("2 * 3.5", Value::number(7.0));
        check("4.8 / 2.4", Value::number(2.0));
        check("0x10 + 0x20", Value::number(48));
        check("0b1010 - 0b1101", Value::number(-3));
        check("(0x1A * 0b11) % 7", Value::number(1));
        check("1.5 + 0x1A - 0b1010", Value::number(17.5));

        const EXPECT: f64 = ((0x20 + 0b1101) as f64 * 2.5) - 1.25;
        check("(0x20 + 0b1101) * 2.5 - 1.25", Value::number(EXPECT));
        check("pi * 2", Value::number(std::f64::consts::TAU));
    }

    #[test]
    fn test_cmp() {
        check("1 == 1", Value::Boolean(true));
        check("1 != 1", Value::Boolean(false));
        check("1 <= 1", Value::Boolean(true));
        check("1 >= 1", Value::Boolean(true));
        check("1 <= 1", Value::Boolean(true));
        check("1 >= 1", Value::Boolean(true));

        check("1 == 2", Value::Boolean(false));
        check("1 != 2", Value::Boolean(true));
        check("1 <= 2", Value::Boolean(true));
        check("1 >= 2", Value::Boolean(false));
        check("1 <= 2", Value::Boolean(true));
        check("1 >= 2", Value::Boolean(false));
        check("1 == 1 && 2 == 2", Value::Boolean(true));
    }

    #[test]
    fn test_lambda() {
        check("(x => x)(1)", Value::number(1));
        check("((x) => x)(1)", Value::number(1));
        check("(x => y => x + y)(1)(2)", Value::number(3));
        check("(x => y => z => x+y+z)(1)(2)(3)", Value::number(6));
        check("((x, y) => x + y)(1, 2)", Value::number(3));

        check("((x, y) => x * y)(3, 3)", Value::number(9));
    }
}

mod builtin {
    use crate::{builtin_context, Context, Eval, Value};
    use core::f64::consts;

    thread_local!(static GLOBAL_CONTEXT: Context<'static> = builtin_context());
    fn check(line: &str, expected: Value) {
        let root = crate::Parser::parse_string(line)
            .parse()
            .expect("failed to parse string");
        let mut ctx = GLOBAL_CONTEXT.with(|x| x.clone());
        assert_eq!(
            root.eval(&mut ctx),
            Ok(expected.clone()),
            "<expr: '{line}'> is not equal to {expected}"
        );
    }

    #[test]
    fn builtin_const() {
        check("pi", Value::number(consts::PI));
        check("tau", Value::number(consts::TAU));
        check("e", Value::number(consts::E));
        check("nan", Value::number(f64::NAN));
        check("inf", Value::number(f64::INFINITY));
        check("neginf", Value::number(f64::NEG_INFINITY));
    }

    #[test]
    fn builtin_unary() {
        check("asin(69)", Value::number(69f64.asin()));
        check("acos(69)", Value::number(69f64.acos()));
        check("atan(69)", Value::number(69f64.atan()));
        check("sin(69)", Value::number(69f64.sin()));
        check("cos(69)", Value::number(69f64.cos()));
        check("tan(69)", Value::number(69f64.tan()));
        check("ln(69)", Value::number(69f64.ln()));
        check("log10(69)", Value::number(69f64.log10()));
        check("log2(69)", Value::number(69f64.log2()));
        check("abs(69)", Value::number(69f64.abs()));
        check("ceil(69)", Value::number(69f64.ceil()));
        check("floor(69)", Value::number(69f64.floor()));
        check("round(69)", Value::number(69f64.round()));
        check("sqrt(69)", Value::number(69f64.sqrt()));
        check("exp(69)", Value::number(69f64.exp()));
        check("float(69)", Value::number(69f64));
        check("sign(69)", Value::number(69f64.signum()));
        check("sign(-69)", Value::number(-69f64.signum()));
    }

    #[test]
    fn builtin_binary() {
        check("min_num(69, 420)", Value::number(69f64.min(420f64)));
        check("max_num(69, 420)", Value::number(69f64.max(420f64)));
        check("powf(69, 2)", Value::number(69f64.powf(2f64)));
        check("pow(69, 2)", Value::number(69f64.powi(2)));
        check("log(69, 420)", Value::number(69f64.log(420f64)));
        check("hypot(69, 420)", Value::number(69f64.hypot(420f64)));
        check("atan2(69, 420)", Value::number(69f64.atan2(420f64)));
    }
}
