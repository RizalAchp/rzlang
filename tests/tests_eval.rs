use rzcalc::{builtin_context, Eval, Context, Value, Parser};

thread_local!(static GLOBAL_CONTEXT: Context<'static> = builtin_context());

fn check(line: &str, expected: Value) {
    let root = Parser::parse_string(line)
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
