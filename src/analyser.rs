use std::{borrow::Cow, collections::HashMap};

use crate::types::{
    context::Ctx,
    exprs::{BinaryOp, Expr, UnaryOp},
    types::Type,
    Span, Spanned, SyntaxErr,
};

type Errors = Vec<SyntaxErr>;

fn err(msg: &str, source: &Span) -> SyntaxErr {
    SyntaxErr {
        message: msg.to_owned(),
        source: source.clone(),
    }
}

impl Ctx<'_> {
    pub fn analyse(&mut self, stmts: &[Expr]) -> Result<(), Errors> {
        let mut errors = Vec::new();
        for stmt in stmts {
            errors.append(&mut self.visit(stmt));
        }
        let has_err = errors.len() == 0;
        if has_err {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn get_return_type(&self, expr: &Expr) -> Result<Cow<'_, Type>, SyntaxErr> {
        fn resolve<'a>(ctx: &'a Ctx<'_>, t: &Type, s: &Span) -> Result<Cow<'a, Type>, SyntaxErr> {
            match ctx.resolve_type(&t) {
                Ok(val) => Ok(val),
                Err(msg) => Err(err(&msg, s)),
            }
        }

        match expr {
            Expr::Var {
                name,
                then,
                span: _,
            } => {
                let typedef = match self.get_type(&name.0) {
                    Some(x) => x,
                    None => return Err(err(&format!("var {} is not defined", name.0), &name.1)),
                };
                if let Type::Obj(entries) = typedef.as_ref() {
                    if let Some(expr) = then {
                        // get object property
                        let ctx = Ctx::new(Some(entries.clone()), None);
                        let return_type = ctx.get_return_type(expr)?.into_owned();
                        return Ok(Cow::Owned(return_type));
                    }
                } else if let Some(_) = then {
                    return Err(err("only object properties are supported", &name.1));
                }
                resolve(self, &typedef, &name.1)
            }
            Expr::Call {
                name,
                args: _,
                span: _,
            } => match self.get_type(&name.0) {
                Some(typedef) => match typedef.as_ref() {
                    Type::Fn { args: _, returns } => resolve(self, returns.as_ref(), &name.1),
                    _ => Err(err(
                        "invalid call expression, variable is not a function",
                        &name.1,
                    )),
                },
                _ => Err(err("function is not defined", &name.1)),
            },
            Expr::Literal(val) => resolve(self, &val.0.get_type(), &val.1),
            Expr::Unary {
                op,
                expr: _,
                span: _,
            } => Ok(Cow::Owned(match op.0 {
                UnaryOp::Bang => Type::Bool,
                UnaryOp::Minus => Type::Number,
            })),
            Expr::Binary {
                op,
                left,
                right: _,
                span: _,
            } => Ok(Cow::Owned(match op.0 {
                BinaryOp::Plus => match *self.get_return_type(left)? == Type::String {
                    true => Type::String,
                    false => Type::Number,
                },
                BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div => Type::Number,
                _ => Type::Bool,
            })),
            Expr::Block { stmts, span: _ } => match stmts.last() {
                Some(expr) => self.get_return_type(expr),
                None => Ok(Cow::Owned(Type::Void)),
            },
            Expr::If {
                cond: _,
                then,
                or,
                span: _,
            } => match or {
                None => self.get_return_type(then),
                Some(or) => {
                    let then_type = self.get_return_type(then)?.into_owned();
                    let or_type = self.get_return_type(or)?.into_owned();
                    Ok(Cow::Owned(then_type.combine(&or_type)))
                }
            },
            Expr::Fn {
                block: _,
                typedef,
                span: _,
            } => resolve(self, &typedef.0, &typedef.1),
            Expr::Let {
                name: _,
                expr: _,
                span: _,
            } => Ok(Cow::Owned(Type::Void)),
            Expr::Def {
                name: _,
                typedef: _,
                span: _,
            } => Ok(Cow::Owned(Type::Void)),
            Expr::Is {
                expr,
                cases,
                default,
                span: _,
            } => {
                let expr_type = self.get_return_type(expr)?.into_owned();
                let mut return_type = self.get_return_type(default)?.into_owned();
                for case in cases {
                    let typedef = match case {
                        Expr::MatchType {
                            typedef,
                            then: _,
                            span: _,
                        } => typedef.0.clone(),
                        _ => expr_type.clone(),
                    };
                    let mut ctx = self.derive();
                    let _ = ctx.try_set_type("it", typedef);
                    let then_return = ctx.get_return_type(case)?;
                    return_type = return_type.combine(then_return.as_ref());
                }
                Ok(Cow::Owned(return_type))
            }
            Expr::Match {
                op: _,
                expr: _,
                then,
                span: _,
            } => self.get_return_type(&then),
            Expr::MatchType {
                typedef: _,
                then,
                span: _,
            } => self.get_return_type(&then),
            Expr::New {
                entries,
                typedef,
                span: _,
            } => match typedef {
                Some(t) => resolve(self, &Type::Def(t.0.clone()), &t.1),
                None => {
                    let mut types = HashMap::new();
                    for (key, expr) in entries {
                        let return_type = self.get_return_type(expr)?;
                        types.insert(key.0.clone(), return_type.into_owned());
                    }
                    Ok(Cow::Owned(Type::Obj(types)))
                }
            },
        }
    }

    // statements
    fn visit(&mut self, expr: &Expr) -> Errors {
        match expr {
            Expr::Block {
                stmts: exprs,
                span: _,
            } => self.visit_block(exprs),
            Expr::Let {
                name,
                expr,
                span: _,
            } => self.visit_let(name, expr),
            Expr::Def {
                name,
                typedef,
                span: _,
            } => self.visit_def(name, typedef),
            Expr::If {
                cond,
                then,
                or,
                span: _,
            } => self.visit_if(cond, then, or),
            Expr::Fn {
                typedef,
                block,
                span: _,
            } => self.visit_fn(typedef, block),
            Expr::Is {
                expr,
                cases,
                default,
                span: _,
            } => self.visit_is(expr, cases, default),
            Expr::New {
                entries,
                typedef,
                span: _,
            } => self.visit_new(entries, typedef),
            _ => self.visit_expr(expr),
        }
    }

    fn visit_block(&mut self, exprs: &Vec<Expr>) -> Errors {
        let mut errors = vec![];
        let mut ctx = self.derive();
        for expr in exprs {
            errors.append(&mut ctx.visit(expr));
        }
        errors
    }

    fn visit_let(&mut self, name: &Spanned<String>, expr: &Box<Expr>) -> Errors {
        let mut errors = self.visit(expr);
        let typedef = match self.get_return_type(&expr) {
            Ok(r) => r.into_owned(),
            Err(err) => {
                errors.push(err);
                return errors;
            }
        };

        if let Err(msg) = self.try_set_type(&name.0, typedef) {
            errors.push(err(msg, &name.1));
        }
        errors
    }

    fn visit_new(
        &mut self,
        entries: &HashMap<Spanned<String>, Expr>,
        typedef: &Option<Spanned<String>>,
    ) -> Errors {
        let mut errors = vec![];
        for (_, expr) in entries {
            errors.append(&mut self.visit(expr));
        }
        if let Some(t) = typedef {
            let def_type = match self.resolve_type(&Type::Def(t.0.clone())) {
                Ok(t) => t.into_owned(),
                Err(msg) => {
                    errors.push(err(&msg, &t.1));
                    return errors;
                }
            };
            match self.get_return_type(&Expr::New {
                entries: entries.clone(),
                typedef: None,
                span: 0..0,
            }) {
                Ok(expr_type) => {
                    if !def_type.includes(&expr_type) {
                        errors.push(err(
                            &format!(
                                "initializer is not assignable to {}\nexpected: {}\nfound: {}",
                                t.0, def_type, expr_type
                            ),
                            &t.1,
                        ))
                    }
                }
                Err(err) => errors.push(err),
            }
        }
        errors
    }

    fn visit_def(&mut self, name: &Spanned<String>, typedef: &Spanned<Type>) -> Errors {
        let mut errors = vec![];
        if let Err(msg) = self.try_set_type(&name.0, typedef.0.clone()) {
            errors.push(err(msg, &name.1));
        }
        errors
    }

    fn visit_if(&mut self, cond: &Box<Expr>, then: &Box<Expr>, or: &Option<Box<Expr>>) -> Errors {
        let mut errors = self.visit_expr(cond);
        errors.append(&mut self.visit(then));

        if let Some(expr) = or {
            errors.append(&mut self.visit(expr));
        }
        errors
    }

    fn visit_is(&mut self, expr: &Box<Expr>, cases: &Vec<Expr>, default: &Box<Expr>) -> Errors {
        let mut errors = self.visit(expr);
        let expr_type = match self.get_return_type(expr) {
            Ok(t) => t.into_owned(),
            Err(err) => {
                errors.push(err);
                return errors;
            }
        };

        // validate cases
        for case in cases {
            let (then, typedef) = match case {
                Expr::Match {
                    op,
                    expr: right,
                    then,
                    span: _,
                } => {
                    self.visit_binary(op, expr, right);
                    (then, expr_type.clone())
                }
                Expr::MatchType {
                    typedef,
                    then,
                    span: _,
                } => {
                    if !expr_type.includes(&typedef.0) {
                        errors.push(err(
                            &format!(
                                "expression is allways false, type {} is not included in {}",
                                expr_type, typedef.0,
                            ),
                            &typedef.1,
                        ))
                    }
                    (then, typedef.0.clone())
                }
                _ => {
                    errors.push(err("invalid match case", case.get_span()));
                    continue;
                }
            };
            // create match ctx
            let mut ctx = self.derive();
            let _ = ctx.try_set_type("it", typedef);
            errors.append(&mut ctx.visit(then));
        }

        // validate default
        let mut ctx = self.derive();
        let _ = ctx.try_set_type("it", expr_type);
        errors.append(&mut ctx.visit(default));
        errors
    }

    fn visit_fn(&mut self, typedef: &Spanned<Type>, block: &Box<Expr>) -> Errors {
        let mut errors = vec![];
        let (args, return_type) = match self.resolve_type(&typedef.0) {
            Ok(t) => match t.into_owned() {
                Type::Fn { args, returns } => (args, returns),
                t => {
                    errors.push(err(
                        &format!("expected function type, found {}", t),
                        &typedef.1,
                    ));
                    return errors;
                }
            },
            Err(msg) => {
                errors.push(err(&msg, &typedef.1));
                return errors;
            }
        };

        // create function scope
        let mut ctx = self.derive();
        for arg in args {
            if ctx.try_set_type(&arg.0, arg.1.clone()).is_err() {
                errors.push(err("argument already exists", &typedef.1))
            }
        }
        // validate expression
        errors.append(&mut ctx.visit(&block));
        match ctx.get_return_type(&block) {
            Ok(expr_return) => {
                if !return_type.includes(&expr_return) {
                    errors.push(err(
                        format!(
                            "function block has invalid return type:\nexpected: {}\nfound {}",
                            return_type.as_ref(),
                            expr_return.as_ref()
                        )
                        .as_str(),
                        block.get_span(),
                    ));
                }
            }
            Err(err) => errors.push(err),
        }
        errors
    }

    // expression
    fn visit_expr(&mut self, expr: &Expr) -> Errors {
        match expr {
            Expr::Literal(_) => vec![],
            Expr::Var {
                name,
                span: _,
                then,
            } => self.visit_variable(name, then),
            Expr::Call {
                name,
                args,
                span: _,
            } => self.visit_call(name, args),
            Expr::Unary { op, expr, span: _ } => self.visit_unary(op, expr),
            Expr::Binary {
                op,
                left,
                right,
                span: _,
            } => self.visit_binary(op, left, right),
            _ => vec![err("invalid expression", expr.get_span())],
        }
    }

    fn visit_unary(&mut self, op: &Spanned<UnaryOp>, expr: &Box<Expr>) -> Errors {
        let mut errors = self.visit_expr(expr);
        let return_type = match self.get_return_type(expr) {
            Ok(t) => t,
            Err(err) => {
                errors.push(err);
                return errors;
            }
        };
        if op.0 == UnaryOp::Minus && *return_type != Type::Number {
            errors.push(err("minus can only be applyed to numbers", &op.1));
        }
        errors
    }

    fn visit_binary(
        &mut self,
        op: &Spanned<BinaryOp>,
        left: &Box<Expr>,
        right: &Box<Expr>,
    ) -> Errors {
        let mut errors = self.visit(left);
        errors.append(&mut self.visit(right));

        let left_return = match self.get_return_type(left) {
            Ok(t) => t,
            Err(err) => {
                errors.push(err);
                return errors;
            }
        };
        let right_return = match self.get_return_type(right) {
            Ok(t) => t,
            Err(err) => {
                errors.push(err);
                return errors;
            }
        };

        match op.0 {
            BinaryOp::Plus => {
                if *left_return != Type::String
                    && (*left_return != Type::Number || *right_return != Type::Number)
                {
                    errors.push(err("operator can only be used for numbers", &op.1))
                }
            }
            BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div => {
                if *left_return != Type::Number || *right_return != Type::Number {
                    errors.push(err("operator can only be used for numbers", &op.1))
                }
            }
            BinaryOp::NotEqual
            | BinaryOp::Equal
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual
            | BinaryOp::Less
            | BinaryOp::LessEqual => {
                if *left_return != *right_return {
                    errors.push(err("can only compare same types", &op.1))
                }
            }
        }
        errors
    }

    fn visit_variable(&mut self, name: &Spanned<String>, then: &Option<Box<Expr>>) -> Errors {
        let var = match self.get_type(&name.0) {
            Some(x) => x,
            None => return vec![err("variable not defined", &name.1)],
        };
        if let Some(expr) = then {
            let mut ctx = Ctx::new(
                match var.as_ref() {
                    Type::Obj(entries) => Some(entries.clone()),
                    _ => None,
                },
                None,
            );
            ctx.visit(expr)
        } else {
            vec![]
        }
    }

    fn visit_call(&mut self, name: &Spanned<String>, args: &Vec<Expr>) -> Errors {
        let arg_types = match self.get_type(&name.0) {
            Some(t) => match t.as_ref() {
                Type::Fn { args, returns: _ } => args.clone(),
                _ => {
                    return vec![err(
                        format!("{} is not a function", &name.0).as_str(),
                        &name.1,
                    )]
                }
            },
            None => return vec![err("function not defined", &name.1)],
        };
        let mut errors = vec![];
        for (i, (arg_name, arg_type)) in arg_types.iter().enumerate() {
            let arg = match args.get(i) {
                Some(expr) => expr,
                None => {
                    errors.push(err(
                        format!("missing fn arg {}", &arg_name).as_str(),
                        &name.1,
                    ));
                    continue;
                }
            };
            // if arg is function use defined type
            let arg_return = match arg {
                Expr::Fn {
                    typedef: _,
                    block,
                    span,
                } => {
                    let expr = Expr::Fn {
                        typedef: (arg_type.clone(), span.clone()),
                        block: block.clone(),
                        span: span.clone(),
                    };
                    errors.append(&mut self.visit(&expr));
                    self.get_return_type(&expr)
                }
                expr => {
                    errors.append(&mut self.visit(expr));
                    self.get_return_type(expr)
                }
            };
            match arg_return {
                Ok(arg_return) => {
                    if !arg_type.includes(&arg_return) {
                        errors.push(err(
                            format!("invalid arg type\nexpected: {arg_type}\nfound: {arg_return}",)
                                .as_str(),
                            arg.get_span(),
                        ));
                    }
                }
                Err(err) => errors.push(err),
            }
        }
        errors
    }
}
