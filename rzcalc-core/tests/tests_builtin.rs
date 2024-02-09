use core::f64::consts;
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
fn builtin_const() {
    check_eval!("PI", Value::num(consts::PI));
    check_eval!("TAU", Value::num(consts::TAU));
    check_eval!("E", Value::num(consts::E));
    check_eval!("NAN", Value::num(f64::NAN));
    check_eval!("INF", Value::num(f64::INFINITY));
    check_eval!("NEGINF", Value::num(f64::NEG_INFINITY));
}

#[test]
fn builtin_unary() {
    check_eval!("asin(69)", Value::num(69f64.asin()));
    check_eval!("acos(69)", Value::num(69f64.acos()));
    check_eval!("atan(69)", Value::num(69f64.atan()));
    check_eval!("sin(69)", Value::num(69f64.sin()));
    check_eval!("cos(69)", Value::num(69f64.cos()));
    check_eval!("tan(69)", Value::num(69f64.tan()));
    check_eval!("ln(69)", Value::num(69f64.ln()));
    check_eval!("log10(69)", Value::num(69f64.log10()));
    check_eval!("log2(69)", Value::num(69f64.log2()));
    check_eval!("abs(69)", Value::num(69f64.abs()));
    check_eval!("ceil(69)", Value::num(69f64.ceil()));
    check_eval!("floor(69)", Value::num(69f64.floor()));
    check_eval!("round(69)", Value::num(69f64.round()));
    check_eval!("sqrt(69)", Value::num(69f64.sqrt()));
    check_eval!("exp(69)", Value::num(69f64.exp()));
    check_eval!("float(69)", Value::num(69f64));
    check_eval!("sign(69)", Value::num(69f64.signum()));
    check_eval!("sign(-69)", Value::num(-69f64.signum()));
}

#[test]
fn builtin_binary() {
    check_eval!("min_num(69, 420)", Value::num(69f64.min(420f64)));
    check_eval!("max_num(69, 420)", Value::num(69f64.max(420f64)));
    check_eval!("powf(69, 2)", Value::num(69f64.powf(2f64)));
    check_eval!("pow(69, 2)", Value::num(69f64.powi(2)));
    check_eval!("log(69, 420)", Value::num(69f64.log(420f64)));
    check_eval!("hypot(69, 420)", Value::num(69f64.hypot(420f64)));
    check_eval!("atan2(69, 420)", Value::num(69f64.atan2(420f64)));
}

#[test]
fn builtin_function_range() {
    check_eval!(
        "range(5)",
        Value::list((0..5).map(Value::num).collect::<Vec<_>>()),
    );
    check_eval!(
        "range(2, 5)",
        Value::list((2..5).map(Value::num).collect::<Vec<_>>()),
    );
    check_eval!(
        "range(1, 10, 2)",
        Value::list((1..10).step_by(2).map(Value::num).collect::<Vec<_>>()),
    );
}

#[test]
fn builtin_function_hex() {
    // hex function params => hex(number: number, upper_case: bool, with_prefix: bool)
    check_eval!("hex(69, false, false)", Value::str("45"));
    check_eval!("hex(69, false, true)", Value::str("0x45"));
    check_eval!("hex(69, true, true)", Value::str("0x45"));
    check_eval!("hex(420, false, false)", Value::str("1a4"));
    check_eval!("hex(420, false, true)", Value::str("0x1a4"));
    check_eval!("hex(420, true, true)", Value::str("0x1A4"));
}

#[test]
fn builtin_function_bin() {
    // bin function params => bin(number: number, with_prefix: bool)
    check_eval!("bin(69, false)", Value::str("1000101"));
    check_eval!("bin(69, true)", Value::str("0b1000101"));
    check_eval!("bin(420, false)", Value::str("110100100"));
    check_eval!("bin(420, true)", Value::str("0b110100100"));
}

#[test]
fn builtin_function_oct() {
    // oct function params => oct(number: number, with_prefix: bool)
    check_eval!("oct(69, false)", Value::str("105"));
    check_eval!("oct(69, true)", Value::str("0o105"));
    check_eval!("oct(420, false)", Value::str("644"));
    check_eval!("oct(420, true)", Value::str("0o644"));
}
