use std::rc::Rc;
use crate::val::Val;

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

    pub fn set(self: &Rc<Self>, name: String, value: Val) -> Rc<Self> {
        Rc::new(Env::Extended {
            name,
            value,
            inner: self.clone(),
        })
    }

    pub fn get(&self, key: &str) -> Option<Val> {
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
