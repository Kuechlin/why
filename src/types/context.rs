use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{types::Type, values::Value};

pub struct Ctx<'a> {
    pub enclosing: Option<&'a Self>,
    pub types: RefCell<HashMap<String, Rc<Type>>>,
    pub state: RefCell<HashMap<String, Rc<Value>>>,
}

impl Ctx<'_> {
    pub fn new<'a>(
        types: Option<HashMap<String, Type>>,
        state: Option<HashMap<String, Value>>,
    ) -> Ctx<'a> {
        let mut t = HashMap::new();
        if let Some(type_map) = types {
            for (key, val) in type_map {
                let _ = t.insert(key, Rc::new(val));
            }
        }
        let mut s = HashMap::new();
        if let Some(state_map) = state {
            for (key, val) in state_map {
                let _ = s.insert(key, Rc::new(val));
            }
        }
        Ctx {
            enclosing: None,
            types: RefCell::new(t),
            state: RefCell::new(s),
        }
    }
    pub fn derive<'a>(&'a self) -> Ctx<'a> {
        Ctx {
            enclosing: Some(self),
            types: RefCell::new(HashMap::new()),
            state: RefCell::new(HashMap::new()),
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
    pub fn set_value(&self, key: &str, val: Value) {
        let _ = self
            .state
            .borrow_mut()
            .insert(key.to_string(), Rc::new(val));
    }
    pub fn try_set_type(&self, key: &str, val: Type) -> Result<(), &'static str> {
        let mut types = self.types.borrow_mut();
        if types.contains_key(key) {
            Err("can't reassign a value to immutable variable")
        } else {
            let _ = types.insert(key.to_string(), Rc::new(val));
            Ok(())
        }
    }
    pub fn set_type(&self, key: &str, val: Type) {
        let _ = self
            .types
            .borrow_mut()
            .insert(key.to_string(), Rc::new(val));
    }

    // typecheks
    pub fn resolve_type(&self, typedef: &Type) -> Result<Rc<Type>, String> {
        match typedef {
            Type::Def(name) => {
                if let Some(t) = self.get_type(name) {
                    self.resolve_rctype(t)
                } else {
                    Err(format!("type {name} not defined"))
                }
            }
            Type::Or(types) => {
                let mut group = Vec::new();
                for t in types {
                    group.push(self.resolve_type(t)?.as_ref().clone());
                }
                Ok(Rc::new(Type::Or(group)))
            }
            Type::Fn { args, returns } => {
                let mut types = Vec::new();
                for (n, t) in args {
                    types.push((n.to_owned(), self.resolve_type(t)?.as_ref().clone()));
                }
                let returns = Box::new(self.resolve_type(returns)?.as_ref().clone());
                Ok(Rc::new(Type::Fn {
                    args: types,
                    returns,
                }))
            }
            Type::Obj(_) | Type::Number | Type::String | Type::Bool | Type::Void => {
                Ok(Rc::new(typedef.clone()))
            }
        }
    }
    pub fn resolve_rctype(&self, typedef: Rc<Type>) -> Result<Rc<Type>, String> {
        match typedef.as_ref() {
            Type::Def(name) => {
                if let Some(t) = self.get_type(name) {
                    self.resolve_rctype(t)
                } else {
                    Err(format!("type {name} not defined"))
                }
            }
            Type::Or(types) => {
                let mut group = Vec::new();
                for t in types {
                    group.push(self.resolve_type(t)?.as_ref().clone());
                }
                Ok(Rc::new(Type::Or(group)))
            }
            Type::Fn { args, returns } => {
                let mut types = Vec::new();
                for (n, t) in args {
                    types.push((n.to_owned(), self.resolve_type(t)?.as_ref().clone()));
                }
                let returns = Box::new(self.resolve_type(returns)?.as_ref().clone());
                Ok(Rc::new(Type::Fn {
                    args: types,
                    returns,
                }))
            }
            Type::Obj(_) | Type::Number | Type::String | Type::Bool | Type::Void => Ok(typedef),
        }
    }

    pub fn type_of(&self, val: &Value) -> Type {
        match val {
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Bool(_) => Type::Bool,
            Value::Fn { typedef, expr: _ } => typedef.clone(),
            Value::Obj {
                typedef,
                entries: _,
            } => typedef.clone(),
            Value::Void => Type::Void,
        }
    }
}
