use std::rc::Rc;
use std::fmt::Debug;
use anyhow::{anyhow, Result};
use std::iter::zip;
use crate::apply;
use crate::env::Env;
use crate::val::Val;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    Bool(String),
    Num(String),
    Or(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    If {
        pred: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>,
    },
    Let {
        vars: Vec<String>,
        vals: Vec<Expr>,
        body: Box<Expr>,
    },
    Proc {
        args: Vec<String>,
        body: Box<Expr>,
    },
}

impl Expr {
    pub fn evaluate(self, env: &Rc<Env>) -> Result<Val> {
        match self {
            Expr::Bool(val) => Self::eval_bool(val),
            Expr::Num(val) => Self::eval_num(val),
            Expr::Var(var) => Self::eval_var(&var, env),
            Expr::Or(lhs, rhs) => Self::eval_or(*lhs, *rhs, env),
            Expr::And(lhs, rhs) => Self::eval_and(*lhs, *rhs, env),
            Expr::If {
                pred,
                then,
                otherwise,
            } => Self::eval_if(*pred, *then, *otherwise, env),
            Expr::Let { vars, vals, body } => Self::eval_let(vars, vals, *body, env),
            Expr::Proc { args, body } => Self::eval_proc_exp(args, *body, env),
            Expr::App(operator, operands) => Self::eval_app(*operator, operands, env),
        }
    }

    fn eval_let(vars: Vec<String>, vals: Vec<Expr>, body: Expr, env: &Rc<Env>) -> Result<Val> {
        let vals = Self::eval_many(env, vals)?;
        let new_env = Self::extend_many(vars, vals, env);
        body.evaluate(&new_env)
    }

    fn eval_proc_exp(args: Vec<String>, body: Expr, env: &Rc<Env>) -> Result<Val> {
        Ok(Val::Proc {
            args,
            body,
            env: env.clone(),
        })
    }

    fn eval_app(operator: Expr, operands: Vec<Expr>, env: &Rc<Env>) -> Result<Val> {
        let operator = operator.evaluate(env)?;
        let operands: Vec<Val> = Self::eval_many(env, operands)?;

        match operator {
            Val::Builtin(_, func) => func(operands),
            Val::Proc { args, body, env } => {
                let new_env = Self::extend_many(args, operands, &env);
                body.evaluate(&new_env)
            }
            _ => Err(anyhow!("can't apply operator '{operator:?}' is not a proc")),
        }
    }

    fn eval_many(env: &Rc<Env>, exps: Vec<Expr>) -> Result<Vec<Val>> {
        exps.into_iter().map(|val| val.evaluate(env)).collect()
    }

    fn extend_many(vars: Vec<String>, vals: Vec<Val>, env: &Rc<Env>) -> Rc<Env> {
        zip(vars, vals).fold(env.clone(), |env, (var, val)| env.set(var, val))
    }

    fn eval_or(lhs: Expr, rhs: Expr, env: &Rc<Env>) -> Result<Val> {
        match lhs.evaluate(env)? {
            Val::Bool(true) => Ok(Val::Bool(true)),
            Val::Bool(false) => match rhs.evaluate(env)? {
                Val::Bool(val) => Ok(Val::Bool(val)),
                _ => Err(anyhow!("Or expect booleans")),
            },
            _ => Err(anyhow!("Or expect booleans")),
        }
    }
    fn eval_and(lhs: Expr, rhs: Expr, env: &Rc<Env>) -> Result<Val> {
        match lhs.evaluate(env)? {
            Val::Bool(false) => Ok(Val::Bool(false)),
            Val::Bool(true) => match rhs.evaluate(env)? {
                Val::Bool(val) => Ok(Val::Bool(val)),
                _ => Err(anyhow!("And expect booleans")),
            },
            _ => Err(anyhow!("And expect booleans")),
        }
    }

    fn eval_if(pred: Expr, then: Expr, otherwise: Expr, env: &Rc<Env>) -> Result<Val> {
        match pred.evaluate(env) {
            Ok(Val::Bool(true)) => then.evaluate(env),
            Ok(Val::Bool(false)) => otherwise.evaluate(env),
            _ => Err(anyhow!("if expect boolean")),
        }
    }

    fn eval_var(var: &str, env: &Rc<Env>) -> Result<Val> {
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

    fn eval_num(val: String) -> Result<Val> {
        val.parse::<i32>()
            .map(Val::Num)
            .map_err(|_| anyhow!("'{val}' is not a valid number"))
    }

    fn eval_bool(val: String) -> Result<Val> {
        match val.as_str() {
            "#f" | "#false" => Ok(Val::Bool(false)),
            "#t" | "#true" => Ok(Val::Bool(true)),
            _ => Err(anyhow!("'{val}' is not a valid boolean")),
        }
    }
}
