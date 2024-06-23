use std::rc::Rc;
use std::fmt::{Debug, Formatter};
use anyhow::anyhow;
use std::iter::zip;

#[derive(Debug, Clone)]
pub enum Env {
    Empty,
    Extended {
        name: String,
        value: Val,
        inner: Rc<Self>,
    },
}

impl Env {
    pub fn new() -> Rc<Self> {
        Rc::new(Env::Empty)
    }

    fn set(self: &Rc<Self>, name: String, value: Val) -> Rc<Self> {
        Rc::new(Env::Extended {
            name,
            value,
            inner: self.clone(),
        })
    }

    fn get(&self, key: &str) -> Option<Val> {
        match self {
            Env::Empty => None,
            Env::Extended {
                name,
                value,
                inner: _inner,
            } if key == name => Some(value.clone()),
            Env::Extended { inner, .. } => inner.get(key),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Exp {
    Var(String),
    Bool(String),
    Num(String),
    // Str(String),
    Or(Box<Exp>, Box<Exp>),
    And(Box<Exp>, Box<Exp>),
    App(Box<Exp>, Vec<Exp>),
    If {
        pred: Box<Exp>,
        then: Box<Exp>,
        otherwise: Box<Exp>,
    },
    Let {
        vars: Vec<String>,
        vals: Vec<Exp>,
        body: Box<Exp>,
    },
    Proc {
        args: Vec<String>,
        body: Box<Exp>,
    },
}


type Apply = dyn Fn(Vec<Val>) -> anyhow::Result<Val>;

#[derive(Clone)]
pub enum Val {
    Null,
    Bool(bool),
    Num(i32),
    Pair(Box<Val>, Box<Val>),
    Str(String),
    Proc {
        args: Vec<String>,
        body: Exp,
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
            Val::Str(s) => write!(f, "Str(\"{}\")", s),
            Val::Proc { args, body, env } => write!(f, "Proc {{ args: {:?}, body: {:?}, env: {:?} }}", args, body, env),
            Val::Builtin(name, _) => write!(f, "Builtin({})", name),
        }
    }
}

impl Exp {
    pub fn evaluate(self, env: &Rc<Env>) -> anyhow::Result<Val> {
        match self {
            // Exp::Symbol(val) => Self::eval_symbol(val),
            Exp::Bool(val) => Self::eval_bool(val),
            Exp::Num(val) => Self::eval_num(val),
            Exp::Var(var) => Self::eval_var(&var, env),
            Exp::Or(lhs, rhs) => Self::eval_or(*lhs, *rhs, env),
            Exp::And(lhs, rhs) => Self::eval_and(*lhs, *rhs, env),
            Exp::If {
                pred,
                then,
                otherwise,
            } => Self::eval_if(*pred, *then, *otherwise, env),
            Exp::Let { vars, vals, body } => Self::eval_let(vars, vals, *body, env),
            Exp::Proc { args, body } => Self::eval_proc_exp(args, *body, env),
            Exp::App(operator, operands) => Self::eval_app(*operator, operands, env),
        }
    }

    fn eval_let(vars: Vec<String>, vals: Vec<Exp>, body: Exp, env: &Rc<Env>) -> anyhow::Result<Val> {
        let vals = Self::eval_many(env, vals)?;
        let new_env = Self::extend_many(vars, vals, env);
        body.evaluate(&new_env)
    }

    fn eval_proc_exp(args: Vec<String>, body: Exp, env: &Rc<Env>) -> anyhow::Result<Val> {
        Ok(Val::Proc {
            args,
            body,
            env: env.clone(),
        })
    }

    fn eval_app(operator: Exp, operands: Vec<Exp>, env: &Rc<Env>) -> anyhow::Result<Val> {
        let operator = operator.evaluate(env)?;
        let operands: Vec<Val> = Self::eval_many(env, operands)?;

        match operator {
            Val::Builtin(_, func) => func(operands),
            Val::Proc { args, body, env } => {
                let new_env = Self::extend_many(args, operands, &env);
                body.evaluate(&new_env)
            }
            _ => Err(anyhow!("can't apply ")),
        }
    }

    fn eval_many(env: &Rc<Env>, exps: Vec<Exp>) -> anyhow::Result<Vec<Val>> {
        exps.into_iter().map(|val| val.evaluate(env)).collect()
    }

    fn extend_many(vars: Vec<String>, vals: Vec<Val>, env: &Rc<Env>) -> Rc<Env> {
        zip(vars, vals).fold(env.clone(), |env, (var, val)| env.set(var, val))
    }

    fn eval_or(lhs: Exp, rhs: Exp, env: &Rc<Env>) -> anyhow::Result<Val> {
        match lhs.evaluate(env)? {
            Val::Bool(true) => Ok(Val::Bool(true)),
            Val::Bool(false) => match rhs.evaluate(env)? {
                Val::Bool(val) => Ok(Val::Bool(val)),
                _ => Err(anyhow!("Or expect booleans")),
            },
            _ => Err(anyhow!("Or expect booleans")),
        }
    }
    fn eval_and(lhs: Exp, rhs: Exp, env: &Rc<Env>) -> anyhow::Result<Val> {
        match lhs.evaluate(env)? {
            Val::Bool(false) => Ok(Val::Bool(false)),
            Val::Bool(true) => match rhs.evaluate(env)? {
                Val::Bool(val) => Ok(Val::Bool(val)),
                _ => Err(anyhow!("And expect booleans")),
            },
            _ => Err(anyhow!("And expect booleans")),
        }
    }

    fn eval_if(pred: Exp, then: Exp, otherwise: Exp, env: &Rc<Env>) -> anyhow::Result<Val> {
        match pred.evaluate(env) {
            Ok(Val::Bool(true)) => then.evaluate(env),
            Ok(Val::Bool(false)) => otherwise.evaluate(env),
            _ => Err(anyhow!("if expect boolean")),
        }
    }

    fn eval_var(var: &str, env: &Rc<Env>) -> anyhow::Result<Val> {
        match var {
            "-" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_sub))),
            "+" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_add))),
            "*" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_mul))),
            "/" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_div))),
            ">" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_gt))),
            ">=" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_ge))),
            "<" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_lt))),
            "<=" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_le))),
            "not" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_not))),
            "cons" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_cons))),
            "car" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_car))),
            "cdr" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_cdr))),
            "list" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_list))),
            "pair?" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_pair_pred))),
            "number?" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_number_pred))),
            "boolean?" => Ok(Val::Builtin(var.into(), Rc::new(apply::apply_boolean_pred))),
            _ => env.get(var)
                .ok_or_else(|| anyhow!("Couldn't find '{var}' in env"))
        }
    }

    fn eval_num(val: String) -> anyhow::Result<Val> {
        val.parse::<i32>()
            .map(Val::Num)
            .map_err(|_| anyhow!("'{val}' is not a valid number"))
    }

    fn eval_bool(val: String) -> anyhow::Result<Val> {
        match val.as_str() {
            "#f" | "#false" => Ok(Val::Bool(false)),
            "#t" | "#true" => Ok(Val::Bool(true)),
            _ => Err(anyhow!("'{val}' is not a valid boolean")),
        }
    }
}

pub mod apply {
    use crate::ast::Val;
    use anyhow::{anyhow, Result};

    fn make_list(mut vals: impl Iterator<Item=Val>) -> Val {
        match vals.next() {
            None => Val::Null,
            Some(curr) => Val::Pair(Box::new(curr), Box::new(make_list(vals))),
        }
    }

    pub(crate) fn apply_list(operands: Vec<Val>) -> Result<Val> {
        Ok(make_list(operands.into_iter()))
    }

    pub(crate) fn apply_boolean_pred(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Bool(..)] => Ok(Val::Bool(true)),
            [_] => Ok(Val::Bool(false)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_number_pred(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(..)] => Ok(Val::Bool(true)),
            [_] => Ok(Val::Bool(false)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_pair_pred(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Pair(..)] => Ok(Val::Bool(true)),
            [_] => Ok(Val::Bool(false)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_cdr(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Pair(_, right)] => Ok(right.as_ref().clone()),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_car(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Pair(left, _)] => Ok(left.as_ref().clone()),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_cons(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [lhs, rhs] => Ok(Val::Pair(Box::new(lhs.clone()), Box::new(rhs.clone()))),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_not(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Bool(val)] => Ok(Val::Bool(!val)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_le(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Bool(lhs <= rhs)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_lt(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Bool(lhs < rhs)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_ge(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Bool(lhs >= rhs)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_gt(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Bool(lhs > rhs)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_div(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Num(lhs / rhs)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_mul(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Num(lhs * rhs)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_add(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Num(lhs + rhs)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }

    pub(crate) fn apply_sub(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Num(lhs - rhs)),
            [Val::Num(val)] => Ok(Val::Num(-val)),
            _ => Err(anyhow!("Incorrect params")),
        }
    }
}
