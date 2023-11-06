use rzcalc_core::{builtin_context, Context, Eval, Parser, Value};

use std::rc::Rc;
use std::sync::Mutex;
thread_local!(static GLOBAL_CONTEXT: Rc<Mutex<Context<'static>>> = Rc::new(Mutex::new(builtin_context())));
fn check(line: &str, expected: Value) {
    let root = Parser::parse_string(line)
        .parse()
        .expect("failed to parse string");
    let ctx_lock = GLOBAL_CONTEXT.with(|m| m.clone());
    let mut ctx = ctx_lock.lock().unwrap();
    match root.eval(&mut ctx) {
        Ok(ok) => assert_eq!(ok, expected, "<expr: '{line}'> is not equal to {expected}"),
        Err(err) => panic!("{err}"),
    }
}

#[test]
fn test_operator() {
    check("1 + 2 * 3", Value::num(7));
    check("1 * 2 + 3", Value::num(5));

    check("2 * 3", Value::num(6));
    check("4 / 2", Value::num(2));
    check("1 + 2", Value::num(3));
    check("2 - 1", Value::num(1));
    check("5 % 2", Value::num(1));
    check("1 << 10", Value::num(1 << 10));
    check("10 >> 2", Value::num(10 >> 2));
    check("10 | 2", Value::num(10 | 2));
    check("10 & 2", Value::num(10 & 2));
    check("10 ^ 2", Value::num(10 ^ 2));

    check("true * true", Value::Bool(true));
    check("true * false", Value::Bool(false));
    check("true + false", Value::Bool(true));
    check("false + false", Value::Bool(false));

    check("1 and 0", Value::num(0));
    check("0 and 2", Value::num(0));
    check("3 or 0", Value::num(3));
    check("0 or 4", Value::num(4));

    check("1 and 2 or 3 and 4", Value::num(2));
    check("0 and 2 or 3 and 4", Value::num(4));
    check("0 and 0 or 3 and 4", Value::num(4));
    check("0 and 0 or 0 and 4", Value::num(0));
    check("0 and 0 or 0 and 0", Value::num(0));

    check("1 or 2 and 3 or 4", Value::num(1));
    check("0 or 2 and 3 or 4", Value::num(3));
    check("0 or 0 and 3 or 4", Value::num(4));
    check("0 or 0 and 0 or 4", Value::num(4));
    check("0 or 0 and 0 or 0", Value::num(0));
}

#[test]
fn test_operator_extra() {
    check("(5 << 2) % (2 * 6)", Value::num(8));
    check("2 * 3.5", Value::num(7.0));
    check("4.8 / 2.4", Value::num(2.0));
    check("0x10 + 0x20", Value::num(48));
    check("0b1010 - 0b1101", Value::num(-3));
    check("(0x1A * 0b11) % 7", Value::num(1));
    check("1.5 + 0x1A - 0b1010", Value::num(17.5));

    const EXPECT: f64 = ((0x20 + 0b1101) as f64 * 2.5) - 1.25;
    check("(0x20 + 0b1101) * 2.5 - 1.25", Value::num(EXPECT));
    check("pi * 2", Value::num(std::f64::consts::TAU));
}

#[test]
fn test_cmp() {
    check("1 == 1", Value::Bool(true));
    check("1 != 1", Value::Bool(false));
    check("1 <= 1", Value::Bool(true));
    check("1 >= 1", Value::Bool(true));
    check("1 <= 1", Value::Bool(true));
    check("1 >= 1", Value::Bool(true));

    check("1 == 2", Value::Bool(false));
    check("1 != 2", Value::Bool(true));
    check("1 <= 2", Value::Bool(true));
    check("1 >= 2", Value::Bool(false));
    check("1 <= 2", Value::Bool(true));
    check("1 >= 2", Value::Bool(false));
    check("1 == 1 && 2 == 2", Value::Bool(true));
}

#[test]
fn test_lambda() {
    check("(x => x)(1)", Value::num(1));
    check("((x) => x)(1)", Value::num(1));
    check("(x => y => x + y)(1)(2)", Value::num(3));
    check("(x => y => z => x+y+z)(1)(2)(3)", Value::num(6));
    check("((x, y) => x + y)(1, 2)", Value::num(3));

    check("((x, y) => x * y)(3, 3)", Value::num(9));
}

#[test]
fn test_str() {
    check(r#""hello world""#, Value::str("hello world"));
    check(r#""hello world" + " hehe""#, Value::str("hello world hehe"));
}
