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
