use std::rc::Rc;
use std::fmt::{Debug, Formatter};
use crate::expr::Expr;
use crate::env::Env;

type Apply = dyn Fn(Vec<Val>) -> anyhow::Result<Val>;

#[derive(Clone)]
pub enum Val {
    Null,
    Bool(bool),
    Num(i32),
    Pair(Box<Val>, Box<Val>),
    Proc {
        args: Vec<String>,
        body: Expr,
        env: Rc<Env>,
    },
    Builtin(String, Rc<Apply>),
}

impl Debug for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Null => write!(f, "Null"),
            Val::Bool(val) => write!(f, "Bool({})", val),
            Val::Num(n) => write!(f, "Num({})", n),
            Val::Pair(a, b) => write!(f, "Pair({:?}, {:?})", a, b),
            Val::Proc { args, body, env } => write!(f, "Proc {{ args: {:?}, body: {:?}, env: {:?} }}", args, body, env),
            Val::Builtin(name, _) => write!(f, "Builtin({})", name),
        }
    }
}
