use std::{collections::HashMap, rc::Rc};

use crate::types::{
    context::Ctx, exprs::BinaryOp, exprs::Expr, exprs::UnaryOp, types::Type, values::Value,
    RuntimeErr, Spanned,
};

type EvalResult = Result<Value, RuntimeErr>;

fn error(msg: &str) -> EvalResult {
    Err(RuntimeErr {
        message: msg.to_string(),
    })
}

impl Ctx<'_> {
    pub fn execute(&self, stmts: &Vec<Expr>) -> EvalResult {
        let mut result = Value::Void;
        for stmt in stmts {
            result = self.eval(stmt)?;
        }
        Ok(result)
    }

    pub fn eval(&self, input: &Expr) -> EvalResult {
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
            Expr::Var {
                name,
                then,
                span: _,
            } => self.eval_var(&name.0, then),
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
            Expr::New {
                entries,
                typedef,
                span: _,
            } => self.eval_new(entries, typedef),
            _ => error("Invalid Expression"),
        }
    }

    fn eval_block(&self, stmts: &Vec<Expr>) -> EvalResult {
        let mut _ctx = self.derive();
        let mut result = Value::Void;
        for stmt in stmts {
            result = _ctx.eval(stmt)?;
        }
        Ok(result)
    }

    fn eval_let(&self, name: &str, expr: &Box<Expr>) -> EvalResult {
        let value = self.eval(expr)?;
        self.set_value(name, value);
        Ok(Value::Void)
    }

    fn eval_def(&self, name: &str, typedef: &Type) -> EvalResult {
        self.set_type(name, typedef.clone());
        Ok(Value::Void)
    }

    fn eval_var(&self, name: &str, then: &Option<Box<Expr>>) -> EvalResult {
        let value = self.get_value(name);

        match then {
            Some(then) => {
                let ctx = Ctx::new(
                    None,
                    match value.as_ref() {
                        Value::Obj {
                            typedef: _,
                            entries,
                        } => Some(entries.clone()),
                        _ => None,
                    },
                );
                ctx.eval(then)
            }
            None => Ok(value.as_ref().clone()),
        }
    }

    fn eval_new(
        &self,
        entries: &HashMap<Spanned<String>, Expr>,
        typedef: &Option<Spanned<String>>,
    ) -> EvalResult {
        let mut types = HashMap::new();
        let mut values = HashMap::new();
        for ((name, _), expr) in entries {
            let value = self.eval(expr)?;
            let typedef = self.type_of(&value);
            values.insert(name.clone(), value);
            types.insert(name.clone(), typedef);
        }
        Ok(Value::Obj {
            typedef: match typedef {
                Some((name, _)) => Type::Def(name.to_owned()),
                None => Type::Obj(types),
            },
            entries: values,
        })
    }

    fn eval_if(&self, cond: &Box<Expr>, then: &Box<Expr>, or: &Option<Box<Expr>>) -> EvalResult {
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

    fn eval_is(&self, expr: &Box<Expr>, cases: &Vec<Expr>, default: &Box<Expr>) -> EvalResult {
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
                    match (
                        self.resolve_type(&typedef.0),
                        self.resolve_type(&self.type_of(&value)),
                    ) {
                        (Ok(match_type), Ok(value_type)) => {
                            if value_type.includes(&match_type) {
                                then
                            } else {
                                continue;
                            }
                        }
                        _ => continue,
                    }
                }
                _ => continue,
            };
            let ctx = self.derive();
            let _ = ctx.set_value("it", value);
            return ctx.eval(then);
        }

        let ctx = self.derive();
        ctx.set_value("it", value);
        return ctx.eval(default);
    }

    fn eval_fn(&self, expr: &Box<Expr>, typedef: &Type) -> EvalResult {
        Ok(Value::Fn {
            typedef: typedef.clone(),
            expr: expr.clone(),
        })
    }

    fn eval_call(&self, name: &String, args: &Vec<Expr>) -> EvalResult {
        let val = self.get_value(name);
        let (typedef, fn_expr) = match val.as_ref() {
            Value::Fn { typedef, expr } => (typedef, expr),
            _ => return error(format!("{name} is not a function").as_str()),
        };
        let args_def = match typedef {
            Type::Fn { args, returns: _ } => args,
            _ => return error(format!("{name} has invalid type").as_str()),
        };

        // create context and eval args
        let mut args_state = HashMap::new();
        for (i, (name, typedef)) in args_def.iter().enumerate() {
            let arg = match args.get(i) {
                Some(Expr::Fn {
                    typedef: _,
                    block,
                    span,
                }) => self.eval(&Expr::Fn {
                    typedef: (typedef.clone(), 0..0),
                    block: block.clone(),
                    span: span.clone(),
                })?,
                Some(e) => self.eval(e)?,
                None => return error(format!("arg {name} is mission").as_str()),
            };
            let _ = args_state.insert(name.to_owned(), Rc::new(arg));
        }
        let ctx = match self.get_let_ctx(name) {
            Some(ctx) => ctx.derive_with_state(args_state),
            None => return error(format!("no context found for {name}").as_str()),
        };

        ctx.eval(fn_expr.as_ref())
    }

    // expressions
    fn eval_unary(&self, op: &UnaryOp, expr: &Box<Expr>) -> EvalResult {
        let value = self.eval(expr)?;

        match op {
            UnaryOp::Bang => Ok(Value::Bool(!value.is_truthy())),
            UnaryOp::Minus => match value {
                Value::Number(x) => Ok(Value::Number(-x)),
                _ => error("invalid value"),
            },
        }
    }

    fn eval_binary(&self, op: &BinaryOp, left: &Box<Expr>, right: &Box<Expr>) -> EvalResult {
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
            BinaryOp::And => Ok(Value::Bool(
                left_value.is_truthy() && right_value.is_truthy(),
            )),
            BinaryOp::Or => Ok(Value::Bool(
                left_value.is_truthy() || right_value.is_truthy(),
            )),
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
