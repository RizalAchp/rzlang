use rustyline::error::ReadlineError;
use rzcalc_core::{self, builtin_context, Context, Eval, Node, Parser, RzError, Span, Value};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut ctx = builtin_context();
    let mut rl = rustyline::DefaultEditor::new()?;

    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).ok();
                parse_line(line, &mut ctx);
            }
            Err(ReadlineError::Interrupted) => {
                eprintln!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                eprintln!("CTRL-D");
                break;
            }
            Err(err) => {
                eprintln!("ERROR: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").map_err(From::from)
}

fn print_error(line: impl AsRef<str>, error: impl Into<RzError>) {
    let line = line.as_ref();
    eprintln!("ERROR: {line}");
    match std::convert::Into::<RzError>::into(error) {
        RzError::ParseError(err) => {
            let Span(l, r) = err.span;
            let w = 7 + l;
            let repeat = r - l;
            eprintln!("{:>w$}{}", ' ', "^".repeat(repeat));
            eprintln!(" - {err}");
        }
        RzError::EvalError(err) => eprintln!("ERROR: EvalError - {err}"),
        RzError::IoError(err) => eprintln!("ERROR: IoError - {err}"),
    }
}

fn print_value(expr: Node, value: Value) {
    println!("<expr> => {expr}");
    println!("       => {value}");
}

fn parse_line(line: impl AsRef<str>, ctx: &mut Context<'_>) {
    let line = line.as_ref();
    match Parser::parse_string(line).parse() {
        Ok(node) => match node.eval(ctx) {
            Ok(value) => print_value(node, value),
            Err(err) => print_error(line, err),
        },
        Err(err) => print_error(line, err),
    }
}
