use crate::types::{
    ast::{
        BinaryEx, BinaryOp, BlockEx, CallEx, DefEx, Expr, FnEx, IfEx, IsEx, LetEx, MatchCase,
        ObjEx, PropEx, UnaryEx, UnaryOp,
    },
    context::Ctx,
    types::Type,
    Spannable, SyntaxErr,
};

pub trait Visit {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr>;
}

impl Visit for Expr {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        match self {
            Expr::Val(_) => vec![],
            Expr::Let(e) => e.visit(ctx),
            Expr::Def(e) => e.visit(ctx),
            Expr::Block(e) => e.visit(ctx),
            Expr::Unary(e) => e.visit(ctx),
            Expr::Binary(e) => e.visit(ctx),
            Expr::If(e) => e.visit(ctx),
            Expr::Fn(e) => e.visit(ctx),
            Expr::Call(e) => e.visit(ctx),
            Expr::Is(e) => e.visit(ctx),
            Expr::Prop(e) => e.visit(ctx),
            Expr::Obj(e) => e.visit(ctx),
        }
    }
}

impl Visit for LetEx {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        let mut errors = self.expr.visit(ctx);
        let typedef = match ctx.get_return_type(&self.expr) {
            Ok(r) => r.clone(),
            Err(err) => {
                errors.push(err);
                return errors;
            }
        };
        if let Err(err) = ctx.try_set_type(&self.name.0, typedef) {
            errors.push(err);
        }
        errors
    }
}

impl Visit for DefEx {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        let mut errors = vec![];
        if let Err(err) = ctx.try_set_type(&self.name.0, self.ty.clone()) {
            errors.push(err);
        }
        errors
    }
}

impl Visit for BlockEx {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        let mut errors = vec![];
        let ctx = ctx.derive();
        for expr in &self.stmts {
            errors.append(&mut expr.visit(&ctx));
        }
        errors
    }
}

impl Visit for UnaryEx {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        let mut errors = self.expr.visit(ctx);
        let return_type = match ctx.get_return_type(&self.expr) {
            Ok(t) => t,
            Err(err) => {
                errors.push(err);
                return errors;
            }
        };
        match return_type.as_ref() {
            Type::Number(_) => {
                if self.op.0 != UnaryOp::Minus {
                    errors.push(SyntaxErr::new(
                        "operator can only be used for numbers",
                        &self.op.1,
                    ))
                }
            }
            _ => (),
        }
        errors
    }
}

impl Visit for BinaryEx {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        let mut errors = self.left.visit(ctx);
        errors.append(&mut self.right.visit(ctx));

        let left_return = match ctx.get_return_type(&self.left) {
            Ok(t) => t,
            Err(err) => {
                errors.push(err);
                return errors;
            }
        };
        let right_return = match ctx.get_return_type(&self.right) {
            Ok(t) => t,
            Err(err) => {
                errors.push(err);
                return errors;
            }
        };

        match self.op.0 {
            BinaryOp::Plus => match (left_return.as_ref(), right_return.as_ref()) {
                // concat strings
                (Type::String(_), _) => (),
                // add numbers
                (Type::Number(_), Type::Number(_)) => (),
                _ => errors.push(SyntaxErr::new(
                    "operator can only be used for numbers or strings",
                    &self.op.1,
                )),
            },
            BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div => {
                match (left_return.as_ref(), right_return.as_ref()) {
                    (Type::Number(_), Type::Number(_)) => (),
                    _ => errors.push(SyntaxErr::new(
                        "operator can only be used for numbers",
                        &self.op.1,
                    )),
                }
            }
            BinaryOp::NotEqual
            | BinaryOp::Equal
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual
            | BinaryOp::Less
            | BinaryOp::LessEqual => {
                if left_return.as_ref().ne(right_return.as_ref()) {
                    errors.push(SyntaxErr::new("can only compare same types", &self.op.1))
                }
            }
            BinaryOp::And | BinaryOp::Or => {
                // variables get checked by truthyness
            }
        }
        errors
    }
}

impl Visit for IfEx {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        let mut errors = self.cond.visit(ctx);
        errors.append(&mut self.then.visit(ctx));

        if let Some(expr) = &self.or {
            errors.append(&mut expr.visit(ctx));
        }
        errors
    }
}

impl Visit for FnEx {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        let mut errors = vec![];
        let (args, return_type) = match ctx.resolve_type(self.ty.clone()) {
            Ok(t) => match t.as_ref().clone() {
                Type::Fn(f) => (f.args, f.returns),
                t => {
                    errors.push(SyntaxErr::new(
                        &format!("expected function type, found {}", t),
                        &t.span(),
                    ));
                    return errors;
                }
            },
            Err(msg) => {
                errors.push(msg);
                return errors;
            }
        };

        // create function scope
        let ctx = ctx.derive();
        for (name, ex) in args {
            if ctx.try_set_type(&name.0, ex.clone()).is_err() {
                errors.push(SyntaxErr::new("argument already exists", &name.1))
            }
        }
        // validate expression
        errors.append(&mut self.block.visit(&ctx));
        match ctx.get_return_type(&self.block) {
            Ok(expr_return) => {
                if !return_type.includes(&expr_return) {
                    errors.push(SyntaxErr::new(
                        format!(
                            "function block has invalid return type:\nexpected: {}\nfound {}",
                            return_type.as_ref(),
                            expr_return.as_ref()
                        )
                        .as_str(),
                        self.block.span(),
                    ));
                }
            }
            Err(err) => errors.push(err),
        }
        errors
    }
}

impl Visit for CallEx {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        let arg_types = match ctx.get_type(&self.name.0) {
            Some(t) => match t.as_ref() {
                Type::Fn(f) => f.args.clone(),
                _ => {
                    return vec![SyntaxErr::new(
                        format!("{} is not a function", &self.name.0).as_str(),
                        &self.span,
                    )]
                }
            },
            None => return vec![SyntaxErr::new("function not defined", &self.span)],
        };
        let mut errors = vec![];
        for (i, (arg_name, arg_type)) in arg_types.iter().enumerate() {
            let arg = match self.args.get(i) {
                Some(expr) => expr,
                None => {
                    errors.push(SyntaxErr::new(
                        format!("missing fn arg {}", &arg_name.0).as_str(),
                        &arg_name.1,
                    ));
                    continue;
                }
            };
            // if arg is function use defined type
            let arg_return = match arg {
                Expr::Fn(f) => {
                    let expr = Expr::Fn(FnEx {
                        ty: arg_type.clone(),
                        block: f.block.clone(),
                        span: f.span.clone(),
                    });
                    errors.append(&mut expr.visit(ctx));
                    ctx.get_return_type(&expr)
                }
                expr => {
                    errors.append(&mut expr.visit(ctx));
                    ctx.get_return_type(expr)
                }
            };
            match arg_return {
                Ok(arg_return) => {
                    if !arg_type.includes(&arg_return) {
                        errors.push(SyntaxErr::new(
                            format!("invalid arg type\nexpected: {arg_type}\nfound: {arg_return}",)
                                .as_str(),
                            arg.span(),
                        ));
                    }
                }
                Err(err) => errors.push(err),
            }
        }
        errors
    }
}

impl Visit for IsEx {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        let mut errors = self.expr.visit(ctx);
        let expr_type = match ctx.get_return_type(&self.expr) {
            Ok(t) => t.clone(),
            Err(err) => {
                errors.push(err);
                return errors;
            }
        };

        // validate cases
        for case in &self.cases {
            let (then, typedef) = match case {
                MatchCase::Value(c) => {
                    let mut err = Expr::Binary(BinaryEx {
                        op: c.op.clone(),
                        left: self.expr.clone(),
                        right: c.expr.clone(),
                        span: self.span.clone(),
                    })
                    .visit(ctx);
                    errors.append(&mut err);
                    (&c.then, expr_type.clone())
                }
                MatchCase::Type(c) => {
                    if !expr_type.includes(&c.ty) {
                        errors.push(SyntaxErr::new(
                            &format!(
                                "expression is allways false, type {} is not included in {}",
                                expr_type, c.ty,
                            ),
                            &c.ty.span(),
                        ));
                    }
                    (&c.then, c.ty.clone())
                }
            };
            // create match ctx
            let ctx = ctx.derive();
            let _ = ctx.try_set_type("it", typedef);
            errors.append(&mut then.visit(&ctx));
        }

        // validate default
        let ctx = ctx.derive();
        let _ = ctx.try_set_type("it", expr_type);
        errors.append(&mut self.default.visit(&ctx));
        errors
    }
}

impl Visit for PropEx {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        let var = match ctx.get_type(&self.name.0) {
            Some(x) => x,
            None => return vec![SyntaxErr::new("variable not defined", &self.name.1)],
        };
        if let Some(expr) = &self.then {
            let ctx = match var.as_ref() {
                Type::Obj(o) => Ctx::form_types(&o.entries),
                _ => Ctx::new(),
            };
            expr.visit(&ctx)
        } else {
            vec![]
        }
    }
}

impl Visit for ObjEx {
    fn visit(&self, ctx: &Ctx) -> Vec<SyntaxErr> {
        let mut errors = vec![];
        let obj_ctx = Ctx::new();
        for (name, expr) in &self.entries {
            errors.append(&mut expr.visit(ctx));
            let expr_type = match ctx.get_return_type(expr) {
                Ok(t) => t.clone(),
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            };
            if let Err(err) = obj_ctx.try_set_type(&name.0, expr_type) {
                errors.push(err);
            }
        }
        errors
    }
}
