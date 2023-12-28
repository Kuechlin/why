use crate::{
    expressions::{Expr, Value},
    lexer::Token,
};

pub fn eval(expr: &Box<Expr>) -> Value {
    match expr.as_ref() {
        Expr::Literal(value) => value.clone(),
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

fn visit_unary(op: &Token, expr: &Box<Expr>) -> Value {
    let value = eval(expr);

    if *op == Token::Bang {
        return Value::Bool(!is_truthy(value));
    }
    if *op == Token::Minus {
        return match value {
            Value::Number(x) => Value::Number(-x),
            _ => panic!("invalid value"),
        };
    }

    panic!("invalid unary expression")
}

fn math(op: &Token, left: &Value, right: &Value) -> Value {
    let a = match left {
        Value::Number(x) => x,
        _ => panic!("invalid left value"),
    };
    let b = match right {
        Value::Number(x) => x,
        _ => panic!("invalid right value"),
    };

    Value::Number(match op {
        Token::Plus => a + b,
        Token::Minus => a - b,
        Token::Star => a * b,
        Token::Slash => a / b,
        _ => panic!("invalid operator"),
    })
}

fn visit_binary(op: &Token, left: &Box<Expr>, right: &Box<Expr>) -> Value {
    let left_value = eval(left);
    let right_value = eval(right);

    match *op {
        Token::Plus => match left_value {
            Value::String(left) => {
                let mut result = left;
                result.push_str(right_value.to_string().as_str());
                Value::String(result)
            }
            _ => math(op, &left_value, &right_value),
        },
        Token::Minus => math(op, &left_value, &right_value),
        Token::Star => math(op, &left_value, &right_value),
        Token::Slash => math(op, &left_value, &right_value),
        Token::BangEqual => Value::Bool(left_value != right_value),
        Token::EqualEqual => Value::Bool(left_value == right_value),
        Token::Greater => Value::Bool(left_value > right_value),
        Token::GreaterEqual => Value::Bool(left_value >= right_value),
        Token::Less => Value::Bool(left_value < right_value),
        Token::LessEqual => Value::Bool(left_value <= right_value),
        _ => panic!("invalid operator"),
    }
}
