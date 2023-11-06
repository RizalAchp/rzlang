use core::f64::consts;
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
fn builtin_const() {
    check("pi", Value::num(consts::PI));
    check("tau", Value::num(consts::TAU));
    check("e", Value::num(consts::E));
    check("nan", Value::num(f64::NAN));
    check("inf", Value::num(f64::INFINITY));
    check("neginf", Value::num(f64::NEG_INFINITY));
}

#[test]
fn builtin_unary() {
    check("asin(69)", Value::num(69f64.asin()));
    check("acos(69)", Value::num(69f64.acos()));
    check("atan(69)", Value::num(69f64.atan()));
    check("sin(69)", Value::num(69f64.sin()));
    check("cos(69)", Value::num(69f64.cos()));
    check("tan(69)", Value::num(69f64.tan()));
    check("ln(69)", Value::num(69f64.ln()));
    check("log10(69)", Value::num(69f64.log10()));
    check("log2(69)", Value::num(69f64.log2()));
    check("abs(69)", Value::num(69f64.abs()));
    check("ceil(69)", Value::num(69f64.ceil()));
    check("floor(69)", Value::num(69f64.floor()));
    check("round(69)", Value::num(69f64.round()));
    check("sqrt(69)", Value::num(69f64.sqrt()));
    check("exp(69)", Value::num(69f64.exp()));
    check("float(69)", Value::num(69f64));
    check("sign(69)", Value::num(69f64.signum()));
    check("sign(-69)", Value::num(-69f64.signum()));
}

#[test]
fn builtin_binary() {
    check("min_num(69, 420)", Value::num(69f64.min(420f64)));
    check("max_num(69, 420)", Value::num(69f64.max(420f64)));
    check("powf(69, 2)", Value::num(69f64.powf(2f64)));
    check("pow(69, 2)", Value::num(69f64.powi(2)));
    check("log(69, 420)", Value::num(69f64.log(420f64)));
    check("hypot(69, 420)", Value::num(69f64.hypot(420f64)));
    check("atan2(69, 420)", Value::num(69f64.atan2(420f64)));
}

#[test]
fn builtin_function_range() {
    check(
        "range(5)",
        Value::list((0..5).map(Value::num).collect::<Vec<_>>()),
    );
    check(
        "range(2, 5)",
        Value::list((2..5).map(Value::num).collect::<Vec<_>>()),
    );
    check(
        "range(1, 10, 2)",
        Value::list((1..10).step_by(2).map(Value::num).collect::<Vec<_>>()),
    );
}

#[test]
fn builtin_function_hex() {
    // hex function params => hex(number: number, upper_case: bool, with_prefix: bool)
    check("hex(69, false, false)", Value::str("45"));
    check("hex(69, false, true)", Value::str("0x45"));
    check("hex(69, true, true)", Value::str("0x45"));
    check("hex(420, false, false)", Value::str("1a4"));
    check("hex(420, false, true)", Value::str("0x1a4"));
    check("hex(420, true, true)", Value::str("0x1A4"));
}

#[test]
fn builtin_function_bin() {
    // bin function params => bin(number: number, with_prefix: bool)
    check("bin(69, false)", Value::str("1000101"));
    check("bin(69, true)", Value::str("0b1000101"));
    check("bin(420, false)", Value::str("110100100"));
    check("bin(420, true)", Value::str("0b110100100"));
}

#[test]
fn builtin_function_oct() {
    // oct function params => oct(number: number, with_prefix: bool)
    check("oct(69, false)", Value::str("105"));
    check("oct(69, true)", Value::str("0o105"));
    check("oct(420, false)", Value::str("644"));
    check("oct(420, true)", Value::str("0o644"));
}
