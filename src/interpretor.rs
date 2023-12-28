use std::{any::Any, collections::HashMap};

use crate::types::{Expr, Stmt, Token, Value};

type EvalResult = Result<Value, &'static str>;

pub struct Context<'a> {
    pub enclosing: Option<&'a Context<'a>>,
    pub state: HashMap<String, Value>,
}
impl Context<'_> {
    fn derive(&self) -> Context {
        Context {
            enclosing: Some(self),
            state: HashMap::new(),
        }
    }
    fn get(&self, key: &str) -> Value {
        if self.state.contains_key(key) {
            return self.state.get(key).unwrap().clone();
        }
        if self.enclosing.is_some() {
            return self.enclosing.unwrap().get(key);
        }
        return Value::Void;
    }
    fn set(&mut self, key: &str, val: Value) {
        let _ = self.state.insert(key.to_string(), val);
    }
}

pub fn eval(ctx: &mut Context, input: &Box<dyn Any>) -> EvalResult {
    // statement
    let stmt = input.downcast_ref::<Stmt>();
    if stmt.is_some() {
        eval_stmt(ctx, stmt.unwrap())
    } else {
        // expression
        let expr = input.downcast_ref::<Expr>();
        if expr.is_some() {
            eval_expr(ctx, expr.unwrap())
        } else {
            Ok(Value::Void)
        }
    }
}

// statements
fn eval_stmt(ctx: &mut Context, stmt: &Stmt) -> EvalResult {
    match stmt {
        Stmt::Let {
            identifier,
            initializer,
        } => visit_let(ctx, identifier, initializer),
    }
}

fn visit_let(ctx: &mut Context, name: &str, expr: &Box<Expr>) -> EvalResult {
    let value = eval_expr(ctx, expr.as_ref())?;
    ctx.set(name, value);
    Ok(Value::Void)
}

// expressions
fn eval_expr(ctx: &Context, expr: &Expr) -> EvalResult {
    match expr {
        Expr::Var(name) => Ok(ctx.get(name)),
        Expr::Literal(value) => Ok(value.clone()),
        Expr::Unary { op, expr } => visit_unary(ctx, op, expr),
        Expr::Binary { op, left, right } => visit_binary(ctx, op, left, right),
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

fn visit_unary(ctx: &Context, op: &Token, expr: &Box<Expr>) -> EvalResult {
    let value = eval_expr(ctx, expr.as_ref())?;

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

fn visit_binary(ctx: &Context, op: &Token, left: &Box<Expr>, right: &Box<Expr>) -> EvalResult {
    let left_value = eval_expr(ctx, left.as_ref())?;
    let right_value = eval_expr(ctx, right.as_ref())?;

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
