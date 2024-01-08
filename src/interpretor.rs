use std::{borrow::Cow, collections::HashMap};

use crate::types::{
    exprs::BinaryOp,
    exprs::UnaryOp,
    exprs::{Expr, Type},
    values::Value,
    RuntimeErr,
};

type EvalResult = Result<Value, RuntimeErr>;

fn error(msg: &str) -> EvalResult {
    Err(RuntimeErr {
        message: msg.to_string(),
    })
}

pub struct ExecCtx<'a> {
    pub enclosing: Option<&'a ExecCtx<'a>>,
    pub types: HashMap<String, Type>,
    pub state: HashMap<String, Value>,
}
impl ExecCtx<'_> {
    pub fn new<'a>() -> ExecCtx<'a> {
        ExecCtx {
            enclosing: None,
            types: HashMap::new(),
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
            types: HashMap::new(),
            state: HashMap::new(),
        }
    }
    fn get_value(&self, key: &str) -> Value {
        match self.state.get(key) {
            Some(val) => val.to_owned(),
            None => match self.enclosing {
                Some(parent) => parent.get_value(key),
                None => Value::Void,
            },
        }
    }
    fn set_value(&mut self, key: &str, val: Value) -> Result<(), RuntimeErr> {
        if self.state.contains_key(key) {
            Err(RuntimeErr {
                message: "can't reassign a value to immutable variable".to_owned(),
            })
        } else {
            let _ = self.state.insert(key.to_string(), val);
            Ok(())
        }
    }
    fn get_type(&self, key: &str) -> Type {
        match self.types.get(key) {
            Some(val) => val.to_owned(),
            None => match self.enclosing {
                Some(parent) => parent.get_type(key),
                None => Type::Void,
            },
        }
    }
    fn set_type(&mut self, key: &str, val: Type) -> Result<(), RuntimeErr> {
        if self.types.contains_key(key) {
            Err(RuntimeErr {
                message: "can't reassign a value to immutable variable".to_owned(),
            })
        } else {
            let _ = self.types.insert(key.to_string(), val);
            Ok(())
        }
    }

    fn type_of(&self, val: &Value) -> Cow<'_, Type> {
        match val {
            Value::Number(_) => Cow::Owned(Type::Number),
            Value::String(_) => Cow::Owned(Type::String),
            Value::Bool(_) => Cow::Owned(Type::Bool),
            Value::Fn { typedef, expr: _ } => self.resolve_type(typedef),
            Value::Void => Cow::Owned(Type::Void),
        }
    }

    fn resolve_type(&self, typedef: &Type) -> Cow<'_, Type> {
        match typedef {
            Type::Def(name) => self.resolve_type(&self.get_type(name)),
            Type::Or(types) => Cow::Owned(Type::Or(
                types
                    .iter()
                    .map(|t| self.resolve_type(t).into_owned())
                    .collect(),
            )),
            Type::Fn { args, returns } => Cow::Owned(Type::Fn {
                args: args
                    .iter()
                    .map(|t| (t.0.clone(), self.resolve_type(&t.1).into_owned()))
                    .collect(),
                returns: Box::new(self.resolve_type(&returns).into_owned()),
            }),
            _ => Cow::Owned(typedef.clone()),
        }
    }

    pub fn eval(&mut self, input: &Expr) -> EvalResult {
        match input {
            Expr::Block { stmts, span: _ } => self.eval_block(stmts),
            Expr::Let {
                name,
                expr,
                span: _,
            } => self.eval_let(&name.0, expr),
            Expr::If {
                cond,
                then,
                or,
                span: _,
            } => self.eval_if(cond, then, or),
            Expr::Fn {
                block,
                typedef,
                span: _,
            } => self.eval_fn(block, &typedef.0),
            Expr::Var(name) => Ok(self.get_value(&name.0)),
            Expr::Literal(value) => Ok(value.0.clone()),
            Expr::Unary { op, expr, span: _ } => self.eval_unary(&op.0, expr),
            Expr::Binary {
                op,
                left,
                right,
                span: _,
            } => self.eval_binary(&op.0, left, right),
            Expr::Call {
                name,
                args,
                span: _,
            } => self.eval_call(&name.0, args),
            Expr::Def {
                name,
                typedef,
                span: _,
            } => self.eval_def(&name.0, &typedef.0),
            Expr::Is {
                expr,
                cases,
                default,
                span: _,
            } => self.eval_is(expr, cases, default),
            _ => error("Invalid Expression"),
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
        self.set_value(name, value)?;
        Ok(Value::Void)
    }

    fn eval_def(&mut self, name: &str, typedef: &Type) -> EvalResult {
        self.set_type(name, typedef.clone())?;
        Ok(Value::Void)
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

    fn eval_is(&mut self, expr: &Box<Expr>, cases: &Vec<Expr>, default: &Box<Expr>) -> EvalResult {
        let value = self.eval(expr)?;

        for c in cases {
            let then = match c {
                Expr::Match {
                    op,
                    expr,
                    then,
                    span: _,
                } => {
                    let right_value = self.eval(expr)?;
                    let is_match = match op.0 {
                        BinaryOp::NotEqual => value != right_value,
                        BinaryOp::Equal => value == right_value,
                        BinaryOp::Greater => value > right_value,
                        BinaryOp::GreaterEqual => value >= right_value,
                        BinaryOp::Less => value < right_value,
                        BinaryOp::LessEqual => value <= right_value,
                        _ => false,
                    };
                    if !is_match {
                        continue;
                    }
                    then
                }
                Expr::MatchType {
                    typedef,
                    then,
                    span: _,
                } => {
                    let value_type = self.type_of(&value);
                    let match_type = self.resolve_type(&typedef.0);

                    if !match_type.includes(&value_type) {
                        continue;
                    }
                    then
                }
                _ => continue,
            };
            let mut ctx = self.derive();
            ctx.set_value("it", value)?;
            return ctx.eval(then);
        }

        let mut ctx = self.derive();
        ctx.set_value("it", value)?;
        return ctx.eval(default);
    }

    fn eval_fn(&mut self, expr: &Box<Expr>, typedef: &Type) -> EvalResult {
        Ok(Value::Fn {
            typedef: typedef.clone(),
            expr: expr.clone(),
        })
    }

    fn eval_call(&mut self, name: &String, args: &Vec<Expr>) -> EvalResult {
        let (typedef, fn_expr) = match self.get_value(name) {
            Value::Fn { typedef, expr } => (typedef, expr),
            _ => return error(format!("{name} is not a function").as_str()),
        };
        let args_def = match typedef {
            Type::Fn { args, returns: _ } => args,
            _ => return error(format!("{name} has invalid type").as_str()),
        };

        // create context and eval args
        let mut ctx = ExecCtx::new();
        for (i, (name, _)) in args_def.iter().enumerate() {
            let arg = match args.get(i) {
                Some(e) => self.eval(e)?,
                None => return error(format!("arg {name} is mission").as_str()),
            };

            ctx.set_value(name, arg)?;
        }

        ctx.eval(fn_expr.as_ref())
    }

    // expressions
    fn eval_unary(&mut self, op: &UnaryOp, expr: &Box<Expr>) -> EvalResult {
        let value = self.eval(expr)?;

        match op {
            UnaryOp::Bang => Ok(Value::Bool(!value.is_truthy())),
            UnaryOp::Minus => match value {
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
