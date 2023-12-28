use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    Literal(Value),
    Unary {
        op: Token,
        expr: Box<Expr>,
    },
    Binary {
        op: Token,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Void,
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Number(x) => x.to_string(),
            Value::String(x) => x.clone(),
            Value::Bool(x) => x.to_string(),
            Value::Void => "".to_owned(),
        }
    }
}
