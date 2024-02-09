use rzcalc_core::{builtin_context, Eval, Parser, Value};

macro_rules! check_eval {
    ($line:literal, $expected:expr $(,)?) => {
        let root = Parser::parse_string($line);
        let mut ctx = builtin_context();
        match root.and_then(|x| x.eval(&mut ctx)) {
            Ok(ok) => assert_eq!(
                ok, $expected,
                "<expr: '{}'> is not equal to {}",
                $line, $expected
            ),
            Err(err) => panic!("{err}"),
        }
    };
}

#[test]
fn test_operator() {
    check_eval!("1 + 2 * 3", Value::num(7));
    check_eval!("1 * 2 + 3", Value::num(5));

    check_eval!("2 * 3", Value::num(6));
    check_eval!("4 / 2", Value::num(2));
    check_eval!("1 + 2", Value::num(3));
    check_eval!("2 - 1", Value::num(1));
    check_eval!("5 % 2", Value::num(1));
    check_eval!("1 << 10", Value::num(1 << 10));
    check_eval!("10 >> 2", Value::num(10 >> 2));
    check_eval!("10 | 2", Value::num(10 | 2));
    check_eval!("10 & 2", Value::num(10 & 2));
    check_eval!("10 ^ 2", Value::num(10 ^ 2));

    check_eval!("true * true", Value::Bool(true));
    check_eval!("true * false", Value::Bool(false));
    check_eval!("true + false", Value::Bool(true));
    check_eval!("false + false", Value::Bool(false));

    check_eval!("1 and 0", Value::num(0));
    check_eval!("0 and 2", Value::num(0));
    check_eval!("3 or 0", Value::num(3));
    check_eval!("0 or 4", Value::num(4));

    check_eval!("1 and 2 or 3 and 4", Value::num(2));
    check_eval!("0 and 2 or 3 and 4", Value::num(4));
    check_eval!("0 and 0 or 3 and 4", Value::num(4));
    check_eval!("0 and 0 or 0 and 4", Value::num(0));
    check_eval!("0 and 0 or 0 and 0", Value::num(0));

    check_eval!("1 or 2 and 3 or 4", Value::num(1));
    check_eval!("0 or 2 and 3 or 4", Value::num(3));
    check_eval!("0 or 0 and 3 or 4", Value::num(4));
    check_eval!("0 or 0 and 0 or 4", Value::num(4));
    check_eval!("0 or 0 and 0 or 0", Value::num(0));
}

#[test]
fn test_operator_extra() {
    check_eval!("(5 << 2) % (2 * 6)", Value::num(8));
    check_eval!("2 * 3.5", Value::num(7.0));
    check_eval!("4.8 / 2.4", Value::num(2.0));
    check_eval!("0x10 + 0x20", Value::num(48));
    check_eval!("0b1010 - 0b1101", Value::num(-3));
    check_eval!("(0x1A * 0b11) % 7", Value::num(1));
    check_eval!("1.5 + 0x1A - 0b1010", Value::num(17.5));

    const EXPECT: f64 = ((0x20 + 0b1101) as f64 * 2.5) - 1.25;
    check_eval!("(0x20 + 0b1101) * 2.5 - 1.25", Value::num(EXPECT));
    check_eval!("PI * 2", Value::num(std::f64::consts::TAU));
}

#[test]
fn test_cmp() {
    check_eval!("1 == 1", Value::Bool(true));
    check_eval!("1 != 1", Value::Bool(false));
    check_eval!("1 <= 1", Value::Bool(true));
    check_eval!("1 >= 1", Value::Bool(true));
    check_eval!("1 <= 1", Value::Bool(true));
    check_eval!("1 >= 1", Value::Bool(true));

    check_eval!("1 == 2", Value::Bool(false));
    check_eval!("1 != 2", Value::Bool(true));
    check_eval!("1 <= 2", Value::Bool(true));
    check_eval!("1 >= 2", Value::Bool(false));
    check_eval!("1 <= 2", Value::Bool(true));
    check_eval!("1 >= 2", Value::Bool(false));
    check_eval!("1 == 1 && 2 == 2", Value::Bool(true));
}

#[test]
fn test_lambda() {
    check_eval!("(x => x)(1)", Value::num(1));
    check_eval!("((x) => x)(1)", Value::num(1));
    check_eval!("(x => y => x + y)(1)(2)", Value::num(3));
    check_eval!("(x => y => z => x+y+z)(1)(2)(3)", Value::num(6));
    check_eval!("((x, y) => x + y)(1, 2)", Value::num(3));

    check_eval!("((x, y) => x * y)(3, 3)", Value::num(9));
}

#[test]
fn test_str() {
    check_eval!("\"hello world\"", Value::str("hello world"));
    check_eval!(
        "\"hello world\" + \" hehe\"",
        Value::str("hello world hehe")
    );
}
