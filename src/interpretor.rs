use std::collections::HashMap;

use crate::types::{BinaryOp, Expr, RuntimeErr, Type, UnaryOp, Value};

type EvalResult = Result<Value, RuntimeErr>;

fn error(msg: &str) -> EvalResult {
    Err(RuntimeErr {
        message: msg.to_string(),
    })
}

pub struct ExecCtx<'a> {
    pub enclosing: Option<&'a ExecCtx<'a>>,
    pub state: HashMap<String, Value>,
}
impl ExecCtx<'_> {
    pub fn new<'a>() -> ExecCtx<'a> {
        ExecCtx {
            enclosing: None,
            state: HashMap::new(),
        }
    }
    pub fn execute(&mut self, stmts: &Vec<Expr>) -> EvalResult {
        let mut result = Value::Void;
        for stmt in stmts {
            result = self.eval(stmt)?;
        }
        Ok(result)
    }

    fn derive(&mut self) -> ExecCtx {
        ExecCtx {
            enclosing: Some(self),
            state: HashMap::new(),
        }
    }
    fn get(&self, key: &str) -> Value {
        if self.state.contains_key(key) {
            return self.state.get(key).unwrap().clone();
        }
        match self.enclosing {
            Some(e) => e.get(key),
            None => Value::Void,
        }
    }
    fn set(&mut self, key: &str, val: Value) -> Result<(), &'static str> {
        if self.state.contains_key(key) {
            Err("can't reassign a value to immutable variable")
        } else {
            let _ = self.state.insert(key.to_string(), val);
            Ok(())
        }
    }

    pub fn eval(&mut self, input: &Expr) -> EvalResult {
        match input {
            Expr::Block { stmts } => self.eval_block(stmts),
            Expr::Let { name, expr } => self.eval_let(name, expr),
            Expr::If {
                cond,
                then,
                or,
                typedef: _,
            } => self.eval_if(cond, then, or),
            Expr::Fn { block, typedef } => self.eval_fn(block, typedef),
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
            Expr::Call {
                name,
                args,
                typedef: _,
            } => self.eval_call(name, args),
        }
    }

    fn eval_block(&mut self, stmts: &Vec<Expr>) -> EvalResult {
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
        let check = self.eval(cond)?.is_truthy();
        if check {
            self.eval(then)
        } else {
            match or {
                Some(expr) => self.eval(expr),
                None => Ok(Value::Void),
            }
        }
    }

    fn eval_fn(&mut self, expr: &Box<Expr>, typedef: &Type) -> EvalResult {
        Ok(Value::Fn {
            typedef: typedef.clone(),
            expr: expr.clone(),
        })
    }

    fn eval_call(&mut self, name: &String, args: &Vec<Expr>) -> EvalResult {
        let (typedef, fn_expr) = match self.get(name) {
            Value::Fn { typedef, expr } => (typedef, expr),
            _ => return error(format!("{name} is not a function").as_str()),
        };
        let args_def = match typedef {
            Type::Fn { args, returns: _ } => args,
            _ => return error(format!("{name} has invalid type").as_str()),
        };

        // create context and eval args
        let mut ctx = ExecCtx::new();
        for (i, (name, arg_type)) in args_def.iter().enumerate() {
            let arg = match args.get(i) {
                Some(e) => self.eval(e)?,
                None => arg_type.get_default(),
            };

            match ctx.set(name, arg) {
                Ok(_) => (),
                Err(err) => return error(err),
            }
        }

        ctx.eval(fn_expr.as_ref())
    }

    // expressions
    fn eval_unary(&mut self, op: &UnaryOp, expr: &Box<Expr>) -> EvalResult {
        let value = self.eval(expr)?;

        match op {
            UnaryOp::Bang => Ok(Value::Bool(!value.is_truthy())),
            UnaryOp::Mins => match value {
                Value::Number(x) => Ok(Value::Number(-x)),
                _ => error("invalid value"),
            },
        }
    }

    fn eval_binary(&mut self, op: &BinaryOp, left: &Box<Expr>, right: &Box<Expr>) -> EvalResult {
        let left_value = self.eval(left)?;
        let right_value = self.eval(right)?;

        match *op {
            BinaryOp::Plus => match left_value {
                Value::String(val) => concat(&val, &right_value),
                _ => math(op, &left_value, &right_value),
            },
            BinaryOp::Minus => math(op, &left_value, &right_value),
            BinaryOp::Mul => math(op, &left_value, &right_value),
            BinaryOp::Div => math(op, &left_value, &right_value),
            BinaryOp::NotEqual => Ok(Value::Bool(left_value != right_value)),
            BinaryOp::Equal => Ok(Value::Bool(left_value == right_value)),
            BinaryOp::Greater => Ok(Value::Bool(left_value > right_value)),
            BinaryOp::GreaterEqual => Ok(Value::Bool(left_value >= right_value)),
            BinaryOp::Less => Ok(Value::Bool(left_value < right_value)),
            BinaryOp::LessEqual => Ok(Value::Bool(left_value <= right_value)),
        }
    }
}

fn math(op: &BinaryOp, left: &Value, right: &Value) -> EvalResult {
    let a = match left {
        Value::Number(x) => x,
        _ => return error("invalid left value"),
    };
    let b = match right {
        Value::Number(x) => x,
        _ => return error("invalid right value"),
    };

    Ok(Value::Number(match op {
        BinaryOp::Plus => a + b,
        BinaryOp::Minus => a - b,
        BinaryOp::Mul => a * b,
        BinaryOp::Div => a / b,
        _ => return error("invalid operator"),
    }))
}

fn concat(left: &String, right: &Value) -> EvalResult {
    let mut result = left.to_string();
    result.push_str(right.to_string().as_str());
    Ok(Value::String(result))
}
