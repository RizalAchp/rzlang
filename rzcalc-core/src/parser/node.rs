use std::rc::Rc;

use crate::{Op, Value};

#[derive(Clone, Debug)]
pub enum Node {
    Immediate(Value),
    MonOp(Op, Rc<Node>),
    BinOp(Op, Rc<Node>, Rc<Node>),
    Apply(Rc<Node>, Vec<Node>),
    Index(Rc<Node>, Rc<Node>),
    Lambda(Vec<Rc<str>>, Rc<Node>),
    Cond(Rc<Node>, Rc<Node>, Rc<Node>),
    List(Vec<Node>),
    Var(Rc<str>),
    VarDef(Rc<str>, Rc<Node>),
    FunDef(Rc<str>, Vec<Rc<str>>, Rc<Node>),
    Range(Option<Rc<Node>>, Option<Rc<Node>>, Option<Rc<Node>>),
}
impl Node {
    pub fn get_var(&self) -> Option<Rc<str>> {
        match self {
            Self::Var(var) => Some(var.clone()),
            _ => None,
        }
    }
}

impl AsRef<Node> for Node {
    fn as_ref(&self) -> &Node {
        self
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Immediate(value) => write!(f, "{value}"),
            Node::List(list) => write!(f, "{list:?}"),
            Node::Var(var) => write!(f, "{var}"),
            Node::MonOp(op, node) => write!(f, "<unary: {op}({node})>"),
            Node::BinOp(op, lhs, rhs) => write!(f, "<binary: {lhs} '{op}' {rhs}>"),
            Node::Apply(name, args) => write!(f, "<apply: {name}({args:?})>"),
            Node::Index(name, index) => write!(f, "<indexing: {name}[{index}]>"),
            Node::Lambda(args, node) => write!(f, "<lamda: ({args:?}) => [{node}]>"),
            Node::Cond(x, y, z) => write!(f, "<condition: if {x} then {y} else {z}>"),
            Node::VarDef(name, item) => write!(f, "<vardef: {name} = {item}>"),
            Node::FunDef(name, args, expr) => write!(f, "<funcdef: {name} = ({args:?}) => {expr}>"),
            Node::Range(start, stop, step) => match (start, stop, step) {
                (None, None, None) => write!(f, "<range: (...)>"),
                (None, None, Some(s)) => write!(f, "<range: (..{s})>"),
                (None, Some(s), None) => write!(f, "<range: (..{s})>"),
                (None, Some(a), Some(b)) => write!(f, "<range: (..{a}{b})>"),
                (Some(s), None, None) => write!(f, "<range: ({s}..)>"),
                (Some(a), None, Some(b)) => write!(f, "<range: ({a}..{b})>"),
                (Some(a), Some(b), None) => write!(f, "<range: ({a}..{b})>"),
                (Some(a), Some(b), Some(c)) => write!(f, "<range: ({a}..{b}{c})>"),
            },
        }
    }
}
