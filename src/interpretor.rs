use crate::types::{Expr, Token, Value};

type EvalResult = Result<Value, &'static str>;

pub fn eval(expr: &Box<Expr>) -> EvalResult {
    match expr.as_ref() {
        Expr::Literal(value) => Ok(value.clone()),
        Expr::Unary { op, expr } => visit_unary(op, expr),
        Expr::Binary { op, left, right } => visit_binary(op, left, right),
    }
}

fn is_truthy(val: Value) -> bool {
    match val {
        Value::Number(x) => x != 0.0,
        Value::String(x) => !x.is_empty(),
        Value::Bool(x) => x,
        Value::Void => false,
    }
}

fn visit_unary(op: &Token, expr: &Box<Expr>) -> EvalResult {
    let value = eval(expr)?;

    if *op == Token::Bang {
        Ok(Value::Bool(!is_truthy(value)))
    } else if *op == Token::Minus {
        match value {
            Value::Number(x) => Ok(Value::Number(-x)),
            _ => Err("invalid value"),
        }
    } else {
        Err("invalid unary expression")
    }
}

fn math(op: &Token, left: &Value, right: &Value) -> EvalResult {
    let a = match left {
        Value::Number(x) => x,
        _ => return Err("invalid left value"),
    };
    let b = match right {
        Value::Number(x) => x,
        _ => return Err("invalid right value"),
    };

    Ok(Value::Number(match op {
        Token::Plus => a + b,
        Token::Minus => a - b,
        Token::Star => a * b,
        Token::Slash => a / b,
        _ => return Err("invalid operator"),
    }))
}

fn concat(left: &String, right: &Value) -> EvalResult {
    let mut result = left.to_string();
    result.push_str(right.to_string().as_str());
    Ok(Value::String(result))
}

fn visit_binary(op: &Token, left: &Box<Expr>, right: &Box<Expr>) -> EvalResult {
    let left_value = eval(left)?;
    let right_value = eval(right)?;

    match *op {
        Token::Plus => match left_value {
            Value::String(val) => concat(&val, &right_value),
            _ => math(op, &left_value, &right_value),
        },
        Token::Minus => math(op, &left_value, &right_value),
        Token::Star => math(op, &left_value, &right_value),
        Token::Slash => math(op, &left_value, &right_value),
        Token::BangEqual => Ok(Value::Bool(left_value != right_value)),
        Token::EqualEqual => Ok(Value::Bool(left_value == right_value)),
        Token::Greater => Ok(Value::Bool(left_value > right_value)),
        Token::GreaterEqual => Ok(Value::Bool(left_value >= right_value)),
        Token::Less => Ok(Value::Bool(left_value < right_value)),
        Token::LessEqual => Ok(Value::Bool(left_value <= right_value)),
        _ => panic!("invalid operator"),
    }
}
