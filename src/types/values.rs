use std::{borrow::Cow, collections::HashMap, fmt::Display};

use super::{exprs::Expr, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Fn {
        typedef: Type,
        expr: Box<Expr>,
    },
    Obj {
        typedef: Type,
        entries: HashMap<String, Value>,
    },
    Void,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Number(x) => *x != 0.0,
            Value::String(x) => !x.is_empty(),
            Value::Bool(x) => *x,
            Value::Fn {
                expr: _,
                typedef: _,
            } => true,
            Value::Obj {
                typedef: _,
                entries: _,
            } => true,
            Value::Void => false,
        }
    }
    pub fn get_type(&self) -> Cow<'_, Type> {
        match self {
            Value::Number(_) => Cow::Owned(Type::Number),
            Value::String(_) => Cow::Owned(Type::String),
            Value::Bool(_) => Cow::Owned(Type::Bool),
            Value::Fn { expr: _, typedef } => Cow::Borrowed(typedef),
            Value::Obj {
                typedef,
                entries: _,
            } => Cow::Borrowed(typedef),
            Value::Void => Cow::Owned(Type::Void),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0.partial_cmp(r0),
            (Self::String(l0), Self::String(r0)) => l0.partial_cmp(r0),
            (Self::Bool(l0), Self::Bool(r0)) => l0.partial_cmp(r0),
            _ => Some(std::cmp::Ordering::Equal),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(x) => write!(f, "{x}"),
            Value::String(x) => write!(f, "{x}"),
            Value::Bool(x) => write!(f, "{x}"),
            Value::Fn { typedef, expr } => write!(f, "{typedef} {expr}"),
            Value::Obj {
                typedef: _,
                entries,
            } => {
                let list = entries
                    .iter()
                    .map(|(name, value)| format!("{name} = {value}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{{ {list} }}")
            }
            Value::Void => write!(f, "_"),
        }
    }
}
