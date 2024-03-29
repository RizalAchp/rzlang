mod highlight;
mod repl;

use std::{path::PathBuf, process::exit};

use repl::{print_error, print_value, Repl};
use rzcalc_core::{self, builtin_context, Context, Eval, Parser, RzError};
pub type Result<T> = ::std::result::Result<T, RzError>;

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let mut args = std::env::args();
    let program = args
        .next()
        .expect("args should have at lease one arguments");
    let mut ctx = builtin_context();
    if let Some(next) = args.next() {
        match next.as_str() {
            "-v" | "--version" => {
                eprintln!("{NAME}-{VERSION}");
                return;
            }
            "-h" | "--help" => {
                eprintln!("USAGE: {program} [OPT:?]");
                eprintln!("OPT:");
                eprintln!("   -h,--help             show this text");
                eprintln!("   -e,--eval <ARGS..>    evaluate the expresion given in <ARGS...>");
                eprintln!("   <FILE>                evaluate content in file");
                eprintln!();
                eprintln!("if not any of OPT is given, program is running in repl mode");
                return;
            }
            "-e" => {
                let content = args.collect::<Vec<_>>().join(" ");
                if !exec_from_string(content, &mut ctx) {
                    exit(1);
                }
                return;
            }
            s => {
                if !exec_from_file(s, &mut ctx) {
                    exit(1);
                }
                return;
            }
        }
    }

    let mut repl = Repl::new();
    repl.start(&mut ctx)
}

fn exec_from_file(path: impl Into<PathBuf>, ctx: &mut Context<'_>) -> bool {
    let path = path.into();
    if !path.exists() {
        eprintln!("path {} is not exists!", path.display());
        return false;
    }
    let content = match std::fs::read(&path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!(
                "Failed to read content if file '{}' - {err}",
                path.display()
            );
            return false;
        }
    };
    match Parser::parse_bytes(&content).and_then(|x| x.eval(ctx).map(|v| (x, v))) {
        Ok((expr, value)) => print_value(&expr, value),
        Err(err) => {
            print_error(err);
            return false;
        }
    }
    true
}

fn exec_from_string(s: String, ctx: &mut Context<'_>) -> bool {
    let parser = Parser::parse_string(&s);
    match parser.and_then(|x| x.eval(ctx).map(|v| (x, v))) {
        Ok((expr, v)) => print_value(&expr, v),
        Err(err) => {
            print_error(err);
            return false;
        }
    }
    true
}
