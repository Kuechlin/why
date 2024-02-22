use std::{collections::HashMap, fmt::Display, rc::Rc};

use colored::Colorize;

use super::{
    ast::Expr,
    types::{ObjType, Type},
    Span,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Fn(FnVal),
    Obj(HashMap<String, Rc<Self>>),
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnVal {
    pub ty: Rc<Type>,
    pub expr: Box<Expr>,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Number(x) => *x != 0.0,
            Value::String(x) => !x.is_empty(),
            Value::Bool(x) => x.clone(),
            Value::Fn(_) => true,
            Value::Obj(_) => true,
            Value::Void => false,
        }
    }
    pub fn get_type(&self) -> Rc<Type> {
        match self {
            Value::Number(_) => Rc::new(Type::Number(Span::default())),
            Value::String(_) => Rc::new(Type::String(Span::default())),
            Value::Bool(_) => Rc::new(Type::Bool(Span::default())),
            Value::Fn(f) => f.ty.clone(),
            Value::Obj(entries) => Rc::new(Type::Obj(ObjType {
                entries: entries
                    .iter()
                    .map(|(name, value)| ((name.clone(), Span::default()), value.get_type()))
                    .collect(),
                span: Span::default(),
            })),
            Value::Void => Rc::new(Type::Void),
        }
    }
    pub fn print(&self) {
        match self {
            Value::Void => {
                println!("{}", "void".cyan())
            }
            res => {
                print!("{}", "result: ".green());
                println!("{}", res.to_string());
            }
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0.partial_cmp(&r0),
            (Self::String(l0), Self::String(r0)) => l0.partial_cmp(&r0),
            (Self::Bool(l0), Self::Bool(r0)) => l0.partial_cmp(&r0),
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
            Value::Fn(fun) => write!(f, "{} {}", fun.ty, "expr"),
            Value::Obj(entries) => {
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
