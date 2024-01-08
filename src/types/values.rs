use std::fmt::Display;

use super::exprs::{Expr, Type};

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Fn { typedef: Type, expr: Box<Expr> },
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
            Value::Void => false,
        }
    }
    pub fn get_type(&self) -> Type {
        match self {
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Bool(_) => Type::Bool,
            Value::Fn { expr: _, typedef } => typedef.clone(),
            Value::Void => Type::Void,
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let val = match self {
            Type::Number => "Num",
            Type::String => "Str",
            Type::Bool => "Bool",
            Type::Void => "_",
            Type::Def(val) => val,
            Type::Or(types) => {
                return write!(
                    f,
                    "({})",
                    types
                        .iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<String>>()
                        .join(" | ")
                )
            }
            Type::Fn { args, returns } => {
                let mut list = args
                    .iter()
                    .map(|a| format!("{}: {}", a.0, a.1))
                    .collect::<Vec<String>>()
                    .join(", ");

                if !list.is_empty() {
                    list += " ";
                }
                return write!(f, "fn {list}-> {}", returns);
            }
        };
        write!(f, "{val}")
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(x) => write!(f, "{x}"),
            Value::String(x) => write!(f, "{x}"),
            Value::Bool(x) => write!(f, "{x}"),
            Value::Fn { typedef, expr } => write!(f, "{typedef} {expr}"),
            Value::Void => write!(f, "_"),
        }
    }
}
