use crate::val::Val;
use anyhow::{anyhow, Result};
use std::ops::{Add, Div, Mul};

// Predicates
fn apply_pred<F>(operands: &[Val], pred: F) -> Result<Val>
where
    F: Fn(&Val) -> bool,
{
    match operands {
        [val] => Ok(Val::Bool(pred(val))),
        _ => Err(anyhow!("Incorrect params")),
    }
}

pub(crate) fn apply_boolean_pred(operands: Vec<Val>) -> Result<Val> {
    apply_pred(&operands, |val| matches!(val, Val::Bool(..)))
}

pub(crate) fn apply_number_pred(operands: Vec<Val>) -> Result<Val> {
    apply_pred(&operands, |val| matches!(val, Val::Num(..)))
}

pub(crate) fn apply_pair_pred(operands: Vec<Val>) -> Result<Val> {
    apply_pred(&operands, |val| matches!(val, Val::Pair(..)))
}


// Lists and pairs
fn make_list(mut vals: impl Iterator<Item=Val>) -> Val {
    vals.next().map_or(Val::Null, |curr| {
        Val::Pair(Box::new(curr), Box::new(make_list(vals)))
    })
}

pub(crate) fn apply_list(operands: Vec<Val>) -> Result<Val> {
    Ok(make_list(operands.into_iter()))
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

// Booleans
fn apply_comparison<F>(operands: &[Val], op: F) -> Result<Val>
where
    F: Fn(i32, i32) -> bool,
{
    match operands {
        [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Bool(op(*lhs, *rhs))),
        _ => Err(anyhow!("Incorrect params")),
    }
}


pub(crate) fn apply_le(operands: Vec<Val>) -> Result<Val> {
    apply_comparison(&operands, |a, b| a <= b)
}

pub(crate) fn apply_lt(operands: Vec<Val>) -> Result<Val> {
    apply_comparison(&operands, |a, b| a < b)
}

pub(crate) fn apply_ge(operands: Vec<Val>) -> Result<Val> {
    apply_comparison(&operands, |a, b| a >= b)
}

pub(crate) fn apply_gt(operands: Vec<Val>) -> Result<Val> {
    apply_comparison(&operands, |a, b| a > b)
}

pub(crate) fn apply_not(operands: Vec<Val>) -> Result<Val> {
    match operands.as_slice() {
        [Val::Bool(val)] => Ok(Val::Bool(!val)),
        _ => Err(anyhow!("Incorrect params")),
    }
}

// Arithmetic
fn apply_arithmetic<F>(operands: &[Val], op: F) -> Result<Val>
where
    F: Fn(i32, i32) -> i32,
{
    match operands {
        [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Num(op(*lhs, *rhs))),
        _ => Err(anyhow!("Incorrect params")),
    }
}

pub(crate) fn apply_div(operands: Vec<Val>) -> Result<Val> {
    apply_arithmetic(&operands, i32::div)
}

pub(crate) fn apply_mul(operands: Vec<Val>) -> Result<Val> {
    apply_arithmetic(&operands, i32::mul)
}

pub(crate) fn apply_add(operands: Vec<Val>) -> Result<Val> {
    apply_arithmetic(&operands, i32::add)
}

pub(crate) fn apply_sub(operands: Vec<Val>) -> Result<Val> {
    match operands.as_slice() {
        [Val::Num(lhs), Val::Num(rhs)] => Ok(Val::Num(lhs - rhs)),
        [Val::Num(val)] => Ok(Val::Num(-val)),
        _ => Err(anyhow!("Incorrect params")),
    }
}
