use std::{collections::HashMap, rc::Rc};

use crate::types::{
    ast::{
        BinaryEx, BinaryOp, BlockEx, CallEx, DefEx, Expr, FnEx, IfEx, IsEx, LetEx, MatchCase,
        ObjEx, PropEx, UnaryEx, UnaryOp,
    },
    context::Ctx,
    types::Type,
    values::{FnVal, Value},
    Spanned, SyntaxErr,
};

pub trait Eval {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr>;
}

impl Eval for Expr {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        match self {
            Expr::Val(v) => Ok(v.value.clone()),
            Expr::Let(v) => v.eval(ctx),
            Expr::Def(v) => v.eval(ctx),
            Expr::Block(v) => v.eval(ctx),
            Expr::Unary(v) => v.eval(ctx),
            Expr::Binary(v) => v.eval(ctx),
            Expr::If(v) => v.eval(ctx),
            Expr::Fn(v) => v.eval(ctx),
            Expr::Call(v) => v.eval(ctx),
            Expr::Is(v) => v.eval(ctx),
            Expr::Prop(v) => v.eval(ctx),
            Expr::Obj(v) => v.eval(ctx),
        }
    }
}

impl Eval for BlockEx {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        let mut _ctx = ctx.derive();
        let mut result = Rc::new(Value::Void);
        for stmt in &self.stmts {
            result = stmt.eval(&_ctx)?;
        }
        Ok(result)
    }
}

impl Eval for LetEx {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        let value = self.expr.eval(ctx)?;
        ctx.set_value(&self.name.0, value);
        Ok(Rc::new(Value::Void))
    }
}

impl Eval for DefEx {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        ctx.set_type(&self.name.0, self.ty.clone());
        Ok(Rc::new(Value::Void))
    }
}

impl Eval for PropEx {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        let value = ctx.get_value(&self.name.0);

        match &self.then {
            Some(then) => {
                let ctx = match value.as_ref() {
                    Value::Obj(entries) => Ctx::form_state(entries),
                    _ => Ctx::new(),
                };
                then.eval(&ctx)
            }
            None => Ok(value.clone()),
        }
    }
}

impl Eval for IfEx {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        let check = self.cond.eval(ctx)?.is_truthy();
        if check {
            self.then.eval(ctx)
        } else {
            match &self.or {
                Some(expr) => expr.eval(ctx),
                None => Ok(Rc::new(Value::Void)),
            }
        }
    }
}

impl Eval for FnEx {
    fn eval(&self, _: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        Ok(Rc::new(Value::Fn(FnVal {
            ty: self.ty.clone(),
            expr: self.block.clone(),
        })))
    }
}

impl Eval for CallEx {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        let val = ctx.get_value(&self.name.0);
        let (typedef, fn_expr) = match val.as_ref() {
            Value::Fn(f) => (&f.ty, &f.expr),
            _ => {
                return Err(SyntaxErr::new(
                    format!("{} is not a function", self.name.0).as_str(),
                    &self.name.1,
                ))
            }
        };
        let args_def = match typedef.as_ref() {
            Type::Fn(f) => &f.args,
            _ => {
                return Err(SyntaxErr::new(
                    format!("{} has invalid type", self.name.0).as_str(),
                    &self.name.1,
                ))
            }
        };

        // create context and eval args
        let mut args_state = HashMap::new();
        for (i, (name, typedef)) in args_def.iter().enumerate() {
            let arg = match self.args.get(i) {
                Some(Expr::Fn(f)) => Expr::Fn(FnEx {
                    ty: typedef.clone(),
                    block: f.block.clone(),
                    span: f.span.clone(),
                })
                .eval(ctx)?,
                Some(e) => e.eval(ctx)?,
                None => {
                    return Err(SyntaxErr::new(
                        format!("arg {} is mission", name.0).as_str(),
                        &name.1,
                    ))
                }
            };
            let _ = args_state.insert(name.0.to_owned(), arg.clone());
        }
        let ctx = match ctx.get_let_ctx(&self.name.0) {
            Some(ctx) => ctx.derive_with_state(args_state),
            None => {
                return Err(SyntaxErr::new(
                    format!("no context found for {}", self.name.0).as_str(),
                    &self.name.1,
                ))
            }
        };

        fn_expr.eval(&ctx)
    }
}

impl Eval for IsEx {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        let value = self.expr.eval(ctx)?;

        for c in &self.cases {
            let then = match c {
                MatchCase::Value(c) => {
                    let right_value = c.expr.eval(ctx)?;
                    let is_match = match c.op.0 {
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
                    &c.then
                }
                MatchCase::Type(c) => {
                    match (
                        ctx.resolve_type(c.ty.clone()),
                        ctx.resolve_type(value.get_type()),
                    ) {
                        (Ok(match_type), Ok(value_type)) => {
                            if value_type.includes(&match_type) {
                                &c.then
                            } else {
                                continue;
                            }
                        }
                        _ => continue,
                    }
                }
            };
            let ctx = ctx.derive();
            let _ = ctx.set_value("it", value);
            return then.eval(&ctx);
        }

        let ctx = ctx.derive();
        ctx.set_value("it", value);
        self.default.eval(&ctx)
    }
}

impl Eval for UnaryEx {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        let value = self.expr.eval(ctx)?;
        match self.op.0 {
            UnaryOp::Bang => Ok(Rc::new(Value::Bool(!value.is_truthy()))),
            UnaryOp::Minus => match value.as_ref() {
                Value::Number(x) => Ok(Rc::new(Value::Number(-x))),
                _ => Err(SyntaxErr::new("invalid value", &self.span)),
            },
        }
    }
}

impl Eval for BinaryEx {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        let left_value = self.left.eval(ctx)?;
        let right_value = self.right.eval(ctx)?;

        match self.op.0 {
            BinaryOp::Plus => match left_value.as_ref() {
                Value::String(val) => concat(&val, &right_value),
                _ => math(&self.op, &left_value, &right_value),
            },
            BinaryOp::Minus => math(&self.op, &left_value, &right_value),
            BinaryOp::Mul => math(&self.op, &left_value, &right_value),
            BinaryOp::Div => math(&self.op, &left_value, &right_value),
            BinaryOp::And => Ok(Rc::new(Value::Bool(
                left_value.is_truthy() && right_value.is_truthy(),
            ))),
            BinaryOp::Or => Ok(Rc::new(Value::Bool(
                left_value.is_truthy() || right_value.is_truthy(),
            ))),
            BinaryOp::NotEqual => Ok(Rc::new(Value::Bool(left_value != right_value))),
            BinaryOp::Equal => Ok(Rc::new(Value::Bool(left_value == right_value))),
            BinaryOp::Greater => Ok(Rc::new(Value::Bool(left_value > right_value))),
            BinaryOp::GreaterEqual => Ok(Rc::new(Value::Bool(left_value >= right_value))),
            BinaryOp::Less => Ok(Rc::new(Value::Bool(left_value < right_value))),
            BinaryOp::LessEqual => Ok(Rc::new(Value::Bool(left_value <= right_value))),
        }
    }
}

impl Eval for ObjEx {
    fn eval(&self, ctx: &Ctx) -> Result<Rc<Value>, SyntaxErr> {
        let mut types = HashMap::new();
        let mut values = HashMap::new();
        for ((name, _), expr) in &self.entries {
            let value = expr.eval(ctx)?;
            let typedef = value.get_type();
            values.insert(name.clone(), value);
            types.insert(name.clone(), typedef);
        }
        Ok(Rc::new(Value::Obj(values)))
    }
}

fn math(op: &Spanned<BinaryOp>, left: &Value, right: &Value) -> Result<Rc<Value>, SyntaxErr> {
    let a = match left {
        Value::Number(x) => x,
        _ => return Err(SyntaxErr::new("invalid left value", &op.1)),
    };
    let b = match right {
        Value::Number(x) => x,
        _ => return Err(SyntaxErr::new("invalid right value", &op.1)),
    };

    Ok(Rc::new(Value::Number(match op.0 {
        BinaryOp::Plus => a + b,
        BinaryOp::Minus => a - b,
        BinaryOp::Mul => a * b,
        BinaryOp::Div => a / b,
        _ => return Err(SyntaxErr::new("invalid operator", &op.1)),
    })))
}

fn concat(left: &String, right: &Value) -> Result<Rc<Value>, SyntaxErr> {
    let mut result = left.to_string();
    result.push_str(right.to_string().as_str());
    Ok(Rc::new(Value::String(result)))
}
