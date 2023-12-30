use std::collections::HashMap;

use crate::types::{Expr, RuntimeErr, Token, Value};

type EvalResult = Result<Value, RuntimeErr>;

fn error(msg: &str) -> EvalResult {
    Err(RuntimeErr {
        message: msg.to_string(),
    })
}

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
    fn set(&mut self, key: &str, val: Value) -> Result<(), &'static str> {
        if self.state.contains_key(key) {
            Err("can't reassign a value to immutable variable")
        } else {
            let _ = self.state.insert(key.to_string(), val);
            Ok(())
        }
    }

    pub fn eval(&mut self, input: &Box<Expr>) -> EvalResult {
        match input.as_ref() {
            Expr::Block { stmts } => self.eval_block(stmts),
            Expr::Let { name, expr } => self.eval_let(name, expr),
            Expr::If {
                cond,
                then,
                or,
                typedef: _,
            } => self.eval_if(cond, then, or),
            _ => self.eval_expr(input),
        }
    }

    fn eval_block(&mut self, stmts: &Vec<Box<Expr>>) -> EvalResult {
        let mut _ctx = self.derive();
        let mut result = Value::Void;
        for stmt in stmts {
            result = _ctx.eval(stmt)?;
        }
        Ok(result)
    }

    fn eval_let(&mut self, name: &str, expr: &Box<Expr>) -> EvalResult {
        let value = self.eval(expr)?;
        match self.set(name, value) {
            Ok(_) => Ok(Value::Void),
            Err(err) => error(err),
        }
    }

    fn eval_if(
        &mut self,
        cond: &Box<Expr>,
        then: &Box<Expr>,
        or: &Option<Box<Expr>>,
    ) -> EvalResult {
        let check = self.eval_expr(cond)?.is_truthy();
        if check {
            self.eval(then)
        } else {
            match or {
                Some(expr) => self.eval(expr),
                None => Ok(Value::Void),
            }
        }
    }

    // expressions
    fn eval_expr(&self, input: &Box<Expr>) -> EvalResult {
        match input.as_ref() {
            Expr::Var { name, typedef: _ } => Ok(self.get(name)),
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Unary {
                op,
                expr,
                typedef: _,
            } => self.eval_unary(op, expr),
            Expr::Binary {
                op,
                left,
                right,
                typedef: _,
            } => self.eval_binary(op, left, right),
            _ => error("invalid expression"),
        }
    }

    fn eval_unary(&self, op: &Token, expr: &Box<Expr>) -> EvalResult {
        let value = self.eval_expr(expr)?;

        if *op == Token::Bang {
            Ok(Value::Bool(!value.is_truthy()))
        } else if *op == Token::Minus {
            match value {
                Value::Number(x) => Ok(Value::Number(-x)),
                _ => error("invalid value"),
            }
        } else {
            error("invalid unary expression")
        }
    }

    fn eval_binary(&self, op: &Token, left: &Box<Expr>, right: &Box<Expr>) -> EvalResult {
        let left_value = self.eval_expr(left)?;
        let right_value = self.eval_expr(right)?;

        fn _bool(val: bool) -> EvalResult {
            Ok(Value::Bool(val))
        }

        match *op {
            Token::Plus => match left_value {
                Value::String(val) => concat(&val, &right_value),
                _ => math(op, &left_value, &right_value),
            },
            Token::Minus => math(op, &left_value, &right_value),
            Token::Star => math(op, &left_value, &right_value),
            Token::Slash => math(op, &left_value, &right_value),
            Token::BangEqual => _bool(left_value != right_value),
            Token::EqualEqual => _bool(left_value == right_value),
            Token::Greater => _bool(left_value > right_value),
            Token::GreaterEqual => _bool(left_value >= right_value),
            Token::Less => _bool(left_value < right_value),
            Token::LessEqual => _bool(left_value <= right_value),
            _ => error("invalid operator"),
        }
    }
}

fn math(op: &Token, left: &Value, right: &Value) -> EvalResult {
    let a = match left {
        Value::Number(x) => x,
        _ => return error("invalid left value"),
    };
    let b = match right {
        Value::Number(x) => x,
        _ => return error("invalid right value"),
    };

    Ok(Value::Number(match op {
        Token::Plus => a + b,
        Token::Minus => a - b,
        Token::Star => a * b,
        Token::Slash => a / b,
        _ => return error("invalid operator"),
    }))
}

fn concat(left: &String, right: &Value) -> EvalResult {
    let mut result = left.to_string();
    result.push_str(right.to_string().as_str());
    Ok(Value::String(result))
}
