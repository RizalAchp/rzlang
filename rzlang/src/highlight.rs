use rzcalc_core::{Node, Value};
use termion::color::*;

use std::fmt;

macro_rules! fg {
    ($clr:ident, $f:ident, $dpy:expr) => {
        write!($f, "{fg}{}{rst}", $dpy, fg = Fg($clr), rst = Fg(Reset))
    };
}

pub struct HighlightedArray<'a, T>(&'a [T]);
impl<'a, T> fmt::Display for HighlightedArray<'a, T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, val) in self.0.iter().enumerate() {
            write!(f, "{val}")?;
            if (i + 1) < self.0.len() {
                write!(f, ",")?;
            }
        }
        Ok(())
    }
}

pub struct HighlightedNode<'a>(pub &'a Node);
impl<'a> fmt::Display for HighlightedNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Node::*;
        match self.0 {
            Immediate(val) => write!(f, "{}", HighlightedValue(val)),
            Var(v) => fg!(Cyan, f, v),
            List(vals) => {
                write!(f, "[")?;
                highlight_arr_node(f, vals)?;
                write!(f, "]")
            }
            MonOp(op, subnode) => {
                write!(
                    f,
                    "(unary: {}{op}{}({}))",
                    Fg(LightCyan),
                    Fg(Reset),
                    Self(subnode)
                )
            }
            BinOp(op, rhs, lhs) => write!(
                f,
                "(binary: {} {}{op}{} {})",
                Self(lhs),
                Fg(LightCyan),
                Fg(Reset),
                Self(rhs)
            ),
            Apply(n, args) => {
                write!(f, "(apply: {}(", Self(n))?;
                highlight_arr_node(f, args)?;
                write!(f, "))")
            }
            Index(expr, index) => write!(f, "(index: {}[{}]", Self(expr), Self(index)),
            Lambda(args, subexpr) => {
                write!(f, "(lambda: (")?;
                for (i, val) in args.iter().enumerate() {
                    write!(f, "{val}")?;
                    if (i + 1) < args.len() {
                        write!(f, ",")?;
                    }
                }
                write!(f, ") => {})", Self(subexpr))
            }
            Cond(cond, lhs, rhs) => write!(
                f,
                "(condition: if {} then {} else {})",
                Self(cond),
                Self(lhs),
                Self(rhs)
            ),
            VarDef(name, subexpr) => write!(
                f,
                "(variable definition: {}{name}{} = {sub})",
                Fg(Cyan),
                Fg(Reset),
                sub = Self(subexpr)
            ),
            FunDef(name, args, subexpr) => write!(
                f,
                "(function definition: {}{name}{} = ({}) => {})",
                Fg(Cyan),
                Fg(Reset),
                HighlightedArray(args),
                Self(subexpr)
            ),
            Range(_, _, _) => write!(f, "(range: (...)"),
        }
    }
}
pub struct HighlightedValue<'a>(pub &'a Value);
impl fmt::Display for HighlightedValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::None => fg!(LightRed, f, "None"),
            Value::Bool(b) => fg!(LightYellow, f, b),
            Value::Num(n) => fg!(LightYellow, f, n),
            Value::Str(s) => fg!(LightGreen, f, s),
            Value::List(list) => {
                write!(f, "[")?;
                for (i, val) in list.iter().enumerate() {
                    write!(f, "{}", Self(val))?;
                    if (i + 1) < list.len() {
                        write!(f, ",")?;
                    }
                }
                write!(f, "]")
            }
            Value::Func(func) => match (func.name(), func.param()) {
                (None, None) => fg!(Cyan, f, "(anon func)()"),
                (None, Some(args)) => write!(f, "{}(anon func)({args:?}){}", Fg(Cyan), Fg(Reset)),
                (Some(n), None) => write!(f, "{}({n})(){}", Fg(Cyan), Fg(Reset)),
                (Some(n), Some(args)) => write!(f, "{}({n})({args:?}){}", Fg(Cyan), Fg(Reset)),
            },
        }
    }
}

fn highlight_arr_node(f: &mut fmt::Formatter, arr: &[Node]) -> fmt::Result {
    for (i, val) in arr.iter().enumerate() {
        write!(f, "{}", HighlightedNode(val))?;
        if (i + 1) < arr.len() {
            write!(f, ",")?;
        }
    }
    Ok(())
}
