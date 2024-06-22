use std::iter::zip;
use std::rc::Rc;

use anyhow::{anyhow, Context, Result};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "lang.pest"]
struct MyParser;

#[derive(Debug, Clone)]
enum Env {
    Empty,
    Extended {
        name: String,
        value: Val,
        inner: Rc<Self>,
    },
}

impl Env {
    fn new() -> Rc<Self> {
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
enum Exp {
    Var(String),
    Bool(String),
    Num(String),
    // Symbol(String),
    // Str(String),
    Or(Box<Exp>, Box<Exp>),
    And(Box<Exp>, Box<Exp>),
    App(String, Vec<Exp>),
    // Cond {
    //     clauses: Vec<(Vec<Exp>, Exp)>,
    //     otherwise: Option<Box<Exp>>,
    // },
    // Case {
    //     key: Box<Exp>,
    //     clauses: Vec<(Vec<Exp>, Exp)>,
    //     otherwise: Option<Box<Exp>>,
    // },
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
    // Sequencing(Vec<Exp>),
}

#[derive(Debug, Clone)]
enum Val {
    Null,
    Bool(bool),
    Num(i32),
    Pair(Box<Val>, Box<Val>),
    // Symbol(String),
    // Str(String),
    Proc {
        args: Vec<String>,
        body: Exp,
        env: Rc<Env>,
    },
}


impl Exp {
    pub fn evaluate(self, env: &Rc<Env>) -> Result<Val> {
        match self {
            // Exp::Symbol(val) => Self::eval_symbol(val),
            // Exp::Str(val) => Self::eval_string(val),
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
            Exp::App(operator, operands) => Self::eval_app(&operator, operands, env),
            // Exp::Cond { .. } => todo!(),
            // Exp::Case { .. } => todo!(),
            // Exp::Sequencing(..) => todo!(),
        }
    }

    fn eval_let(vars: Vec<String>, vals: Vec<Exp>, body: Exp, env: &Rc<Env>) -> Result<Val> {
        let vals = Self::eval_many(env, vals)?;
        let new_env = Self::extend_many(vars, vals, env);
        body.evaluate(&new_env)
    }

    fn eval_proc_exp(args: Vec<String>, body: Exp, env: &Rc<Env>) -> Result<Val> {
        Ok(Val::Proc {
            args,
            body,
            env: env.clone(),
        })
    }

    fn eval_app(
        operator: &str,
        operands: Vec<Exp>,
        env: &Rc<Env>,
    ) -> Result<Val> {
        let operands: Vec<Val> = Self::eval_many(env, operands)?;

        match operator {
            // numbers
            "-" => apply::apply_sub(operands),
            "+" => apply::apply_add(operands),
            "*" => apply::apply_mul(operands),
            "/" => apply::apply_div(operands),
            ">" => apply::apply_gt(operands),
            ">=" => apply::apply_ge(operands),
            "<" => apply::apply_lt(operands),
            "<=" => apply::apply_le(operands),
            // booleans
            "not" => apply::apply_not(operands),
            // pairs
            "cons" => apply::apply_cons(operands),
            "car" => apply::apply_car(operands),
            "cdr" => apply::apply_cdr(operands),
            "list" => apply::apply_list(operands),
            // pred?
            "pair?" => apply::apply_pair_pred(operands),
            "number?" => apply::apply_number_pred(operands),
            "boolean?" => apply::apply_boolean_pred(operands),
            // "string?" => apply::apply_string_pred(operands),
            // "symbol?" => apply::apply_symbol_pred(operands),
            _ => match env.get(operator) {
                None => Err(anyhow!("")),
                Some(Val::Proc { args, body, env }) => {
                    let new_env = Self::extend_many(args, operands, &env);
                    body.evaluate(&new_env)
                }
                _ => Err(anyhow!("")),
            },
        }
    }

    // fn eval_string(val: String) -> Result<Val> {
    //     Ok(Val::Str(val.into()))
    // }

    // fn eval_symbol(val: String) -> Result<Val> {
    //     Ok(Val::Symbol(val.into()))
    // }

    fn eval_many(env: &Rc<Env>, exps: Vec<Exp>) -> Result<Vec<Val>> {
        exps.into_iter().map(|val| val.evaluate(env)).collect()
    }

    fn extend_many(vars: Vec<String>, vals: Vec<Val>, env: &Rc<Env>) -> Rc<Env> {
        zip(vars, vals).fold(env.clone(), |env, (var, val)| env.set(var, val))
    }

    fn eval_or(lhs: Exp, rhs: Exp, env: &Rc<Env>) -> Result<Val> {
        match lhs.evaluate(env)? {
            Val::Bool(true) => Ok(Val::Bool(true)),
            Val::Bool(false) => match rhs.evaluate(env)? {
                Val::Bool(val) => Ok(Val::Bool(val)),
                _ => Err(anyhow!("")),
            },
            _ => Err(anyhow!("")),
        }
    }
    fn eval_and(lhs: Exp, rhs: Exp, env: &Rc<Env>) -> Result<Val> {
        match lhs.evaluate(env)? {
            Val::Bool(false) => Ok(Val::Bool(false)),
            Val::Bool(true) => match rhs.evaluate(env)? {
                Val::Bool(val) => Ok(Val::Bool(val)),
                _ => Err(anyhow!("")),
            },
            _ => Err(anyhow!("")),
        }
    }

    fn eval_if(pred: Exp, then: Exp, otherwise: Exp, env: &Rc<Env>) -> Result<Val> {
        match pred.evaluate(env) {
            Ok(Val::Bool(true)) => then.evaluate(env),
            Ok(Val::Bool(false)) => otherwise.evaluate(env),
            _ => Err(anyhow!("")),
        }
    }

    fn eval_var(var: &str, env: &Rc<Env>) -> Result<Val> {
        match env.get(var) {
            None => Err(anyhow!("")),
            Some(val) => Ok(val),
        }
    }

    fn eval_num(val: String) -> Result<Val> {
        match val.parse::<i32>() {
            Ok(val) => Ok(Val::Num(val)),
            Err(_) => Err(anyhow!("")),
        }
    }

    fn eval_bool(val: String) -> Result<Val> {
        match val.as_str() {
            "#f" => Ok(Val::Bool(false)),
            "#t" => Ok(Val::Bool(true)),
            _ => Err(anyhow!("")),
        }
    }
}

pub mod apply {
    use anyhow::{anyhow, Result};
    use crate::{Val};

    fn make_list(mut vals: impl Iterator<Item=Val>) -> Val {
        match vals.next() {
            None => Val::Null,
            Some(curr) => Val::Pair(Box::new(curr), Box::new(make_list(vals))),
        }
    }

    pub(crate) fn apply_list(operands: Vec<Val>) -> Result<Val> {
        Ok(make_list(operands.into_iter()))
    }

    // pub(crate) fn apply_symbol_pred(operands: Vec<Val>) -> Result<Val> {
    //     match operands.as_slice() {
    //         [Val::Symbol(..)] => Ok(Val::Bool(true)),
    //         [_] => Ok(Val::Bool(false)),
    //         _ => Err(anyhow!("")),
    //     }
    // }

    // pub(crate) fn apply_string_pred(operands: Vec<Val>) -> Result<Val> {
    //     match operands.as_slice() {
    //         [Val::Str(..)] => Ok(Val::Bool(true)),
    //         [_] => Ok(Val::Bool(false)),
    //         _ => Err(anyhow!("")),
    //     }
    // }

    pub(crate) fn apply_boolean_pred(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Bool(..)] => Ok(Val::Bool(true)),
            [_] => Ok(Val::Bool(false)),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_number_pred(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(..)] => Ok(Val::Bool(true)),
            [_] => Ok(Val::Bool(false)),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_pair_pred(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Pair(..)] => Ok(Val::Bool(true)),
            [_] => Ok(Val::Bool(false)),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_cdr(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Pair(_, right)] => Ok(right.as_ref().clone()),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_car(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Pair(left, _)] => Ok(left.as_ref().clone()),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_cons(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [lhs, rhs] => Ok(Val::Pair(Box::new(lhs.clone()), Box::new(rhs.clone()))),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_not(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Bool(val)] => Ok(Val::Bool(!val)),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_le(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Bool(lhs <= rhs)),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_lt(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Bool(lhs < rhs)),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_ge(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Bool(lhs >= rhs)),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_gt(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Bool(lhs > rhs)),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_div(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Num(lhs / rhs)),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_mul(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Num(lhs * rhs)),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_add(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Num(lhs + rhs)),
            _ => Err(anyhow!("")),
        }
    }

    pub(crate) fn apply_sub(operands: Vec<Val>) -> Result<Val> {
        match operands.as_slice() {
            [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Num(lhs - rhs)),
            [Val::Num(val)] => Ok(Val::Num(-val)),
            _ => Err(anyhow!("")),
        }
    }
}

fn parse(pair: Pair<Rule>) -> Result<Exp> {
    match pair.as_rule() {
        Rule::exp => parse(
            pair.into_inner().next()
                .context("Can't fail - expression always has internal")?
        ),
        Rule::number => { Ok(Exp::Num(pair.as_str().into())) }
        Rule::bool => { Ok(Exp::Bool(pair.as_str().into())) }
        Rule::var => { Ok(Exp::Var(pair.as_str().into())) }
        Rule::or => {
            let mut inner = pair.into_inner();
            let lhs = parse(inner.next().expect("Or must have two inner expressions"))?;
            let rhs = parse(inner.next().expect("Or must have two inner expressions"))?;
            Ok(Exp::Or(Box::new(lhs), Box::new(rhs)))
        }
        Rule::and => {
            let mut inner = pair.into_inner();
            let lhs = parse(inner.next().expect("And must have two inner expressions"))?;
            let rhs = parse(inner.next().expect("And must have two inner expressions"))?;
            Ok(Exp::And(Box::new(lhs), Box::new(rhs)))
        }
        Rule::r#if => {
            let mut inner_rule = pair.into_inner();
            let pred = parse(inner_rule.next().expect("If must have two inner expressions"))?;
            let then = parse(inner_rule.next().expect("If must have two inner expressions"))?;
            let otherwise = parse(inner_rule.next().expect("If must have two inner expressions"))?;
            Ok(Exp::If {
                pred: Box::new(pred),
                then: Box::new(then),
                otherwise: Box::new(otherwise),
            })
        }
        Rule::r#let => {
            let mut inner_rule = pair.into_inner().peekable();

            let mut vars = vec![];
            let mut vals = vec![];

            while let Some(first) = inner_rule.next() {
                if inner_rule.peek().is_some() {
                    vars.push(first.as_str().into());
                    vals.push(parse(inner_rule.next().unwrap())?)
                } else {
                    return Ok(Exp::Let {
                        vars,
                        vals,
                        body: Box::new(parse(first)?),
                    });
                }
            }
            Err(anyhow!("error with let"))
        }
        Rule::proc => {
            let mut inner_rule = pair.into_inner();
            let args: Vec<_> = inner_rule.clone()
                .take_while(|p| p.as_rule() == Rule::ident)
                .map(|p| p.as_str().into())
                .collect();

            let body = parse(inner_rule.find(|p| p.as_rule() == Rule::exp).unwrap())?;
            // println!("proc with body {body:?}");
            Ok(Exp::Proc {
                args,
                body: Box::new(body),
            })
        }
        Rule::app => {
            let mut inner_rule = pair.into_inner();
            let operator = inner_rule.next().unwrap().as_str().into();
            let operands = inner_rule.map(parse).collect::<Result<_>>()?;
            Ok(Exp::App(operator, operands))
        }
        Rule::op | Rule::alpha | Rule::digit | Rule::ident => unreachable!()
    }
}

fn main() {
    let a = MyParser::parse(
        Rule::exp,
        // "(- (+ 4 -21) 69)",
        "(let ((x 1)(add (lambda ( a b) ((+ a b))))) ((add -1 2)))",
    )
        .expect("parsing failed")
        .next()
        .unwrap();
    // println!("{:?}", &a);

    let b = parse(a).unwrap();
    println!("parse {:?}", &b);

    let env = Env::new();
    let v = b.evaluate(&env);
    println!("eval {v:?}");
    //
    // let a = Exp::Let {
    //     vars: vec!["x".into()],
    //     vals: vec![Exp::App(
    //         "list".into(),
    //         vec![Exp::Bool("t".into()), Exp::Num("-6".into())],
    //     )],
    //     body: Box::new(Exp::App("cdr".into(), vec![Exp::Var("x".into())])),
    // }
    //     .evaluate(&env);
    //
    // println!("{:?}", a)
}
