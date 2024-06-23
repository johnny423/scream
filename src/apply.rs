use crate::val::Val;
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
