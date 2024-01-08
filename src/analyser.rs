use std::{borrow::Cow, collections::HashMap};

use crate::types::{
    exprs::Type,
    exprs::{BinaryOp, Expr, UnaryOp},
    Span, Spanned, SyntaxErr,
};

pub struct AnalyserCtx<'a> {
    pub enclosing: Option<&'a AnalyserCtx<'a>>,
    pub state: HashMap<String, Type>,
    pub errors: Vec<SyntaxErr>,
}

impl AnalyserCtx<'_> {
    pub fn new<'a>() -> AnalyserCtx<'a> {
        AnalyserCtx {
            enclosing: None,
            state: HashMap::new(),
            errors: Vec::new(),
        }
    }
    pub fn analyse(&mut self, stmts: &[Expr]) -> Result<(), &Vec<SyntaxErr>> {
        for stmt in stmts {
            self.visit(stmt);
        }
        let has_err = self.errors.len() == 0;
        if has_err {
            Ok(())
        } else {
            Err(&self.errors)
        }
    }

    fn derive(&self) -> AnalyserCtx {
        AnalyserCtx {
            enclosing: Some(self),
            state: HashMap::new(),
            errors: Vec::new(),
        }
    }
    fn get(&self, key: &str) -> Option<Type> {
        if self.state.contains_key(key) {
            return Some(self.state.get(key).unwrap().clone());
        }
        if self.enclosing.is_some() {
            return self.enclosing.unwrap().get(key);
        }
        return None;
    }
    fn set(&mut self, key: &str, val: Type) -> Result<(), &'static str> {
        if self.state.contains_key(key) {
            Err("can't reassign a value to immutable variable")
        } else {
            let _ = self.state.insert(key.to_string(), val);
            Ok(())
        }
    }
    fn err(&mut self, msg: &str, source: &Span) {
        self.errors.push(SyntaxErr {
            message: msg.to_owned(),
            source: source.clone(),
        })
    }

    fn is_returnt_type(&mut self, expr: &Expr, typedef: Type) -> bool {
        *self.get_return_type(expr) == typedef
    }
    fn get_return_type(&mut self, expr: &Expr) -> Cow<'_, Type> {
        match expr {
            Expr::Var(name) => match self.get(&name.0) {
                Some(val) => self.resolve_def_type(&val, &name.1),
                None => {
                    self.err("variable is not defined", &name.1);
                    Cow::Owned(Type::Void)
                }
            },
            Expr::Call {
                name,
                args: _,
                span: _,
            } => match self.get(&name.0) {
                Some(Type::Fn { args: _, returns }) => {
                    self.resolve_def_type(returns.as_ref(), &name.1)
                }
                _ => {
                    self.err("function is not defined", &name.1);
                    Cow::Owned(Type::Void)
                }
            },
            Expr::Literal(val) => self.resolve_def_type(&val.0.get_type(), &val.1),
            Expr::Unary {
                op,
                expr: _,
                span: _,
            } => Cow::Owned(match op.0 {
                UnaryOp::Bang => Type::Bool,
                UnaryOp::Minus => Type::Number,
            }),
            Expr::Binary {
                op,
                left,
                right: _,
                span: _,
            } => match op.0 {
                BinaryOp::Plus => match self.is_returnt_type(left, Type::String) {
                    true => Cow::Owned(Type::String),
                    false => Cow::Owned(Type::Number),
                },
                BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div => Cow::Owned(Type::Number),
                _ => Cow::Owned(Type::Bool),
            },
            Expr::Block { stmts, span: _ } => match stmts.last() {
                Some(expr) => self.get_return_type(expr),
                None => Cow::Owned(Type::Void),
            },
            Expr::If {
                cond: _,
                then,
                or,
                span: _,
            } => match or {
                None => self.get_return_type(then),
                Some(or) => {
                    let then_type = self.get_return_type(then).into_owned();
                    let or_type = self.get_return_type(or).into_owned();
                    Cow::Owned(then_type.combine(&or_type))
                }
            },
            Expr::Fn {
                block: _,
                typedef,
                span: _,
            } => self.resolve_def_type(&typedef.0, &typedef.1),
            Expr::Let {
                name: _,
                expr: _,
                span: _,
            } => Cow::Owned(Type::Void),
            Expr::Def {
                name: _,
                typedef: _,
                span: _,
            } => Cow::Owned(Type::Void),
            Expr::Is {
                expr,
                cases,
                span: _,
            } => {
                let expr_type = self.get_return_type(expr).into_owned();
                let mut return_type: Option<Type> = None;
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
                    let _ = ctx.set("it", typedef);
                    let then_return = ctx.get_return_type(case);
                    match return_type {
                        Some(t) => return_type = Some(t.combine(then_return.as_ref())),
                        None => return_type = Some(then_return.into_owned()),
                    }
                    self.errors.append(&mut ctx.errors);
                }

                Cow::Owned(return_type.unwrap_or(Type::Void))
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
        }
    }

    fn resolve_def_type(&mut self, typedef: &Type, span: &Span) -> Cow<'_, Type> {
        match typedef {
            Type::Def(name) => match self.get(name) {
                Some(typedef) => self.resolve_def_type(&typedef, span),
                None => {
                    self.err(format!("type {} is not defined", name).as_str(), span);
                    Cow::Owned(Type::Void)
                }
            },
            Type::Or(types) => Cow::Owned(Type::Or(
                types
                    .iter()
                    .map(|t| self.resolve_def_type(t, span).into_owned())
                    .collect(),
            )),
            Type::Fn { args, returns } => Cow::Owned(Type::Fn {
                args: args
                    .iter()
                    .map(|t| (t.0.clone(), self.resolve_def_type(&t.1, span).into_owned()))
                    .collect(),
                returns: Box::new(self.resolve_def_type(&returns, span).into_owned()),
            }),
            _ => Cow::Owned(typedef.clone()),
        }
    }

    // statements
    fn visit(&mut self, expr: &Expr) {
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
                span: _,
            } => self.visit_is(expr, cases),
            _ => self.visit_expr(expr),
        }
    }

    fn visit_block(&mut self, exprs: &Vec<Expr>) {
        let mut ctx = self.derive();
        for expr in exprs {
            ctx.visit(expr);
        }
        self.errors.append(&mut ctx.errors);
    }

    fn visit_let(&mut self, name: &Spanned<String>, expr: &Box<Expr>) {
        self.visit(expr);
        let typedef = self.get_return_type(&expr).into_owned();

        let res = self.set(&name.0, typedef);
        if res.is_err() {
            self.err(res.unwrap_err(), &name.1);
        }
    }

    fn visit_def(&mut self, name: &Spanned<String>, typedef: &Spanned<Type>) {
        let res = self.set(&name.0, typedef.0.clone());
        if res.is_err() {
            self.err(res.unwrap_err(), &name.1);
        }
    }

    fn visit_if(&mut self, cond: &Box<Expr>, then: &Box<Expr>, or: &Option<Box<Expr>>) {
        self.visit_expr(cond);
        self.visit(then);

        match or {
            Some(expr) => self.visit(expr.as_ref()),
            None => (),
        }
    }

    fn visit_is(&mut self, expr: &Box<Expr>, cases: &Vec<Expr>) {
        self.visit(expr);
        let expr_type = self.get_return_type(expr).into_owned();

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
                        self.err(
                            format!(
                                "expression is allways false, type {} is not included in {}",
                                typedef.0, expr_type
                            )
                            .as_str(),
                            &typedef.1,
                        )
                    }
                    (then, typedef.0.clone())
                }
                _ => {
                    self.err("invalid match case", case.get_span());
                    continue;
                }
            };
            let mut ctx = self.derive();
            let _ = ctx.set("it", typedef);
            ctx.visit(then);
            self.errors.append(&mut ctx.errors);
        }
    }

    fn visit_fn(&mut self, typedef: &Spanned<Type>, block: &Box<Expr>) {
        let fn_type = self.resolve_def_type(&typedef.0, &typedef.1).into_owned();
        let (args, return_type) = match fn_type {
            Type::Fn { args, returns } => (args, returns),
            _ => {
                self.err(
                    format!("expected function type, found {}", fn_type).as_str(),
                    &typedef.1,
                );
                return;
            }
        };

        // create function scope
        let mut ctx = AnalyserCtx::new();
        for arg in args {
            if ctx.set(&arg.0, arg.1.clone()).is_err() {
                self.err("argument already exists", &typedef.1)
            }
        }
        // validate expression
        ctx.visit(&block);
        let expr_return = ctx.get_return_type(&block);
        if !return_type.includes(&expr_return) {
            self.err(
                format!(
                    "function block has invalid return type:\nexpected: {}\nfound {}",
                    return_type.as_ref(),
                    expr_return.as_ref()
                )
                .as_str(),
                block.get_span(),
            );
        }
    }

    // expression
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(_) => (),
            Expr::Var(name) => self.visit_identifier(name),
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
            _ => self.err("invalid expression", expr.get_span()),
        }
    }

    fn visit_unary(&mut self, op: &Spanned<UnaryOp>, expr: &Box<Expr>) {
        self.visit_expr(expr);

        match op.0 {
            UnaryOp::Bang => (),
            UnaryOp::Minus => {
                if !self.is_returnt_type(expr, Type::Number) {
                    self.err("minus can only be applyed to numbers", &op.1)
                }
            }
        }
    }

    fn visit_binary(&mut self, op: &Spanned<BinaryOp>, left: &Box<Expr>, right: &Box<Expr>) {
        self.visit_expr(left);
        self.visit_expr(right);

        fn check_math(x: &mut AnalyserCtx, span: &Span, l: &Expr, r: &Expr) {
            if !x.is_returnt_type(l, Type::Number) || !x.is_returnt_type(r, Type::Number) {
                x.err("operator can only be used for numbers", span)
            }
        }

        match op.0 {
            BinaryOp::Plus => match self.get_return_type(left).as_ref() {
                Type::String => (),
                _ => check_math(self, &op.1, left, right),
            },
            BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div => check_math(self, &op.1, left, right),
            BinaryOp::NotEqual
            | BinaryOp::Equal
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual
            | BinaryOp::Less
            | BinaryOp::LessEqual => {
                if self.get_return_type(left).into_owned()
                    != self.get_return_type(right).into_owned()
                {
                    self.err("can only compare same types", &op.1)
                }
            }
        }
    }

    fn visit_identifier(&mut self, name: &Spanned<String>) {
        if self.get(&name.0).is_none() {
            self.err("variable not defined", &name.1);
        }
    }

    fn visit_call(&mut self, name: &Spanned<String>, args: &Vec<Expr>) {
        let arg_types = match self.get(&name.0) {
            Some(t) => match t {
                Type::Fn { args, returns: _ } => args,
                _ => {
                    self.err(format!("{} is not a function", &name.0).as_str(), &name.1);
                    return;
                }
            },
            None => {
                self.err("function not defined", &name.1);
                return;
            }
        };

        for (i, (arg_name, arg_type)) in arg_types.iter().enumerate() {
            let arg = match args.get(i) {
                Some(expr) => expr,
                None => {
                    self.err(format!("missing fn arg {}", &arg_name).as_str(), &name.1);
                    continue;
                }
            };
            // if arg is function use defined type
            match arg {
                Expr::Fn {
                    typedef: _,
                    block,
                    span,
                } => {
                    self.visit(&Expr::Fn {
                        typedef: (arg_type.clone(), span.clone()),
                        block: block.clone(),
                        span: span.clone(),
                    });
                }
                expr => self.visit(expr),
            };
            let arg_return = self.get_return_type(arg).into_owned();
            if !arg_type.includes(&arg_return) {
                self.err(
                    format!("invalid arg type\nexpected: {arg_type}\nfound: {arg_return}",)
                        .as_str(),
                    arg.get_span(),
                );
            }
        }
    }
}
