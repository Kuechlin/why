use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{analyser::Visit, interpretor::Eval};

use super::{
    ast::{BinaryOp, Expr, MatchCase, UnaryOp},
    types::{FnType, ObjType, OrType, Type},
    values::Value,
    Spannable, Spanned, SyntaxErr,
};

pub struct Ctx<'a> {
    pub enclosing: Option<&'a Self>,
    pub types: RefCell<HashMap<String, Rc<Type>>>,
    pub state: RefCell<HashMap<String, Rc<Value>>>,
}

impl Ctx<'_> {
    pub fn analyse(&self, stmts: &[Expr]) -> Result<(), Vec<SyntaxErr>> {
        let mut errors = Vec::new();
        for stmt in stmts {
            errors.append(&mut stmt.visit(self));
        }
        let has_err = errors.len() == 0;
        if has_err {
            Ok(())
        } else {
            Err(errors)
        }
    }
    pub fn eval(&self, stmts: &Vec<Expr>) -> Result<Rc<Value>, SyntaxErr> {
        let mut result = Rc::new(Value::Void);
        for stmt in stmts {
            result = stmt.eval(self)?;
        }
        Ok(result)
    }

    pub fn new<'a>() -> Ctx<'a> {
        Ctx {
            enclosing: None,
            types: RefCell::new(HashMap::new()),
            state: RefCell::new(HashMap::new()),
        }
    }
    pub fn derive<'a>(&'a self) -> Ctx<'a> {
        Ctx {
            enclosing: Some(self),
            types: RefCell::new(HashMap::new()),
            state: RefCell::new(HashMap::new()),
        }
    }
    pub fn form_types<'a>(types: &HashMap<Spanned<String>, Rc<Type>>) -> Ctx<'a> {
        let types = types
            .iter()
            .map(|(key, val)| (key.0.clone(), val.clone()))
            .collect();
        Ctx {
            enclosing: None,
            types: RefCell::new(types),
            state: RefCell::new(HashMap::new()),
        }
    }
    pub fn form_state<'a>(state: &HashMap<String, Rc<Value>>) -> Ctx<'a> {
        let state = state
            .iter()
            .map(|(key, val)| (key.clone(), val.clone()))
            .collect();
        Ctx {
            enclosing: None,
            types: RefCell::new(HashMap::new()),
            state: RefCell::new(state),
        }
    }
    pub fn derive_with_state<'a>(&'a self, state: HashMap<String, Rc<Value>>) -> Ctx<'a> {
        Ctx {
            enclosing: Some(self),
            types: RefCell::new(HashMap::new()),
            state: RefCell::new(state),
        }
    }

    // getters
    pub fn get_value(&self, key: &str) -> Rc<Value> {
        match self.state.borrow().get(key) {
            Some(val) => val.clone(),
            None => match self.enclosing {
                Some(parent) => parent.get_value(key),
                None => Rc::new(Value::Void),
            },
        }
    }
    pub fn get_type(&self, key: &str) -> Option<Rc<Type>> {
        match self.types.borrow().get(key) {
            Some(val) => Some(val.clone()),
            None => match self.enclosing {
                Some(parent) => parent.get_type(key),
                None => None,
            },
        }
    }
    pub fn get_let_ctx(&self, key: &str) -> Option<&Self> {
        if self.state.borrow().contains_key(key) {
            Some(self)
        } else if self.enclosing.is_some() {
            self.enclosing.unwrap().get_let_ctx(key)
        } else {
            None
        }
    }

    // setters
    pub fn set_value(&self, key: &str, val: Rc<Value>) {
        let _ = self.state.borrow_mut().insert(key.to_string(), val);
    }
    pub fn try_set_type(&self, key: &str, val: Rc<Type>) -> Result<(), SyntaxErr> {
        let mut types = self.types.borrow_mut();
        if types.contains_key(key) {
            Err(SyntaxErr::new(
                "can't reassign a value to immutable variable",
                val.span(),
            ))
        } else {
            let _ = types.insert(key.to_string(), val);
            Ok(())
        }
    }
    pub fn set_type(&self, key: &str, val: Rc<Type>) {
        let _ = self.types.borrow_mut().insert(key.to_string(), val);
    }

    // typecheks
    pub fn resolve_type(&self, typedef: Rc<Type>) -> Result<Rc<Type>, SyntaxErr> {
        match typedef.as_ref() {
            Type::Def(name) => {
                if let Some(t) = self.get_type(&name.0) {
                    self.resolve_type(t)
                } else {
                    Err(SyntaxErr::new(
                        format!("type {} not defined", name.0).as_str(),
                        &name.1,
                    ))
                }
            }
            Type::Or(or) => {
                let mut group = Vec::new();
                for t in &or.types {
                    group.push(self.resolve_type(t.clone())?.clone());
                }
                Ok(Rc::new(Type::Or(OrType {
                    types: group,
                    span: or.span.clone(),
                })))
            }
            Type::Fn(f) => {
                let mut types = Vec::new();
                for (n, t) in &f.args {
                    types.push((n.to_owned(), self.resolve_type(t.clone())?.clone()));
                }
                let returns = self.resolve_type(f.returns.clone())?.clone();
                Ok(Rc::new(Type::Fn(FnType {
                    args: types,
                    returns,
                    span: f.span.clone(),
                })))
            }
            Type::Obj(_) | Type::Number(_) | Type::String(_) | Type::Bool(_) | Type::Void => {
                Ok(typedef)
            }
        }
    }

    pub fn get_return_type(&self, expr: &Expr) -> Result<Rc<Type>, SyntaxErr> {
        match expr {
            Expr::Prop(v) => {
                let typedef = match self.get_type(&v.name.0) {
                    Some(x) => x,
                    None => {
                        return Err(SyntaxErr::new(
                            &format!("var {} is not defined", v.name.0),
                            &v.name.1,
                        ))
                    }
                };
                if let Type::Obj(o) = typedef.as_ref() {
                    if let Some(expr) = &v.then {
                        // get object property
                        let ctx = Ctx::form_types(&o.entries);
                        let return_type = ctx.get_return_type(&expr)?;
                        return Ok(return_type);
                    }
                } else if let Some(e) = &v.then {
                    return Err(SyntaxErr::new(
                        "only object properties are supported",
                        &e.as_ref().span(),
                    ));
                }
                self.resolve_type(typedef)
            }
            Expr::Call(c) => match self.get_type(&c.name.0) {
                Some(typedef) => match typedef.as_ref() {
                    Type::Fn(f) => self.resolve_type(Rc::new(f.returns.as_ref().clone())),
                    _ => Err(SyntaxErr::new(
                        "invalid call expression, variable is not a function",
                        &c.span,
                    )),
                },
                _ => Err(SyntaxErr::new("function is not defined", &c.span)),
            },
            Expr::Val(val) => self.resolve_type(val.value.get_type()),
            Expr::Unary(ex) => Ok(Rc::new(match ex.op.0 {
                UnaryOp::Bang => Type::Bool(ex.span.clone()),
                UnaryOp::Minus => Type::Number(ex.span.clone()),
            })),
            Expr::Binary(ex) => Ok(Rc::new(match ex.op.0 {
                BinaryOp::Plus => {
                    if let Type::String(_) = *self.get_return_type(&ex.left)? {
                        Type::String(ex.span.clone())
                    } else {
                        Type::Number(ex.span.clone())
                    }
                }
                BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div => Type::Number(ex.span.clone()),
                _ => Type::Bool(ex.span.clone()),
            })),
            Expr::Block(b) => match b.stmts.last() {
                Some(expr) => self.get_return_type(expr),
                None => Ok(Rc::new(Type::Void)),
            },
            Expr::If(i) => match &i.or {
                None => self.get_return_type(&i.then),
                Some(or) => {
                    let then_type = self.get_return_type(&i.then)?;
                    let or_type = self.get_return_type(or.as_ref())?;
                    Ok(Rc::new(then_type.combine(&or_type)))
                }
            },
            Expr::Fn(f) => self.resolve_type(f.ty.clone()),
            Expr::Let(_) => Ok(Rc::new(Type::Void)),
            Expr::Def(_) => Ok(Rc::new(Type::Void)),
            Expr::Is(is) => {
                let expr_type = self.get_return_type(&is.expr)?;
                let mut return_type = self.get_return_type(&is.default)?.as_ref().clone();
                for case in &is.cases {
                    let returns = match case {
                        MatchCase::Type(c) => self.get_return_type(&c.then)?,
                        MatchCase::Value(c) => {
                            let ctx = self.derive();
                            let _ = ctx.try_set_type("it", expr_type.clone());
                            ctx.get_return_type(&c.expr)?
                        }
                    };

                    return_type = return_type.combine(returns.as_ref());
                }
                Ok(Rc::new(return_type))
            }
            Expr::Obj(o) => {
                let mut entries = HashMap::new();
                for (name, expr) in &o.entries {
                    let return_type = self.get_return_type(expr)?;
                    entries.insert(name.clone(), return_type.clone());
                }
                Ok(Rc::new(Type::Obj(ObjType {
                    entries,
                    span: o.span.clone(),
                })))
            }
        }
    }
}
