use std::{borrow::Cow, collections::HashMap, sync::Mutex};

use super::{types::Type, values::Value};

pub struct Ctx<'a> {
    pub enclosing: Option<&'a Self>,
    pub types: Mutex<HashMap<String, Type>>,
    pub state: Mutex<HashMap<String, Value>>,
}

impl Ctx<'_> {
    pub fn new<'a>(
        types: Option<HashMap<String, Type>>,
        state: Option<HashMap<String, Value>>,
    ) -> Ctx<'a> {
        Ctx {
            enclosing: None,
            types: Mutex::new(types.unwrap_or(HashMap::new())),
            state: Mutex::new(state.unwrap_or(HashMap::new())),
        }
    }
    pub fn derive<'a>(&'a self) -> Ctx<'a> {
        Ctx {
            enclosing: Some(self),
            types: Mutex::new(HashMap::new()),
            state: Mutex::new(HashMap::new()),
        }
    }
    pub fn derive_with_state<'a>(&'a self, state: HashMap<String, Value>) -> Ctx<'a> {
        Ctx {
            enclosing: Some(self),
            types: Mutex::new(HashMap::new()),
            state: Mutex::new(state),
        }
    }

    // getters
    pub fn get_value(&self, key: &str) -> Value {
        match self.state.lock().unwrap().get(key) {
            Some(val) => val.clone(),
            None => match self.enclosing {
                Some(parent) => parent.get_value(key),
                None => Value::Void,
            },
        }
    }
    pub fn get_type(&self, key: &str) -> Option<Type> {
        match self.types.lock().unwrap().get(key) {
            Some(val) => Some(val.to_owned()),
            None => match self.enclosing {
                Some(parent) => parent.get_type(key),
                None => None,
            },
        }
    }
    pub fn get_let_ctx(&self, key: &str) -> Option<&Self> {
        if self.state.lock().unwrap().contains_key(key) {
            Some(self)
        } else if self.enclosing.is_some() {
            self.enclosing.unwrap().get_let_ctx(key)
        } else {
            None
        }
    }

    // setters
    pub fn set_value(&self, key: &str, val: Value) {
        let _ = self.state.lock().unwrap().insert(key.to_string(), val);
    }
    pub fn try_set_type(&self, key: &str, val: Type) -> Result<(), &'static str> {
        let mut types = self.types.lock().unwrap();
        if types.contains_key(key) {
            Err("can't reassign a value to immutable variable")
        } else {
            let _ = types.insert(key.to_string(), val);
            Ok(())
        }
    }
    pub fn set_type(&self, key: &str, val: Type) {
        let _ = self.types.lock().unwrap().insert(key.to_string(), val);
    }

    // typecheks
    pub fn resolve_type(&self, typedef: &Type) -> Result<Cow<'_, Type>, String> {
        match typedef {
            Type::Def(name) => {
                if let Some(t) = self.get_type(name) {
                    self.resolve_type(&t)
                } else {
                    Err(format!("type {name} not defined"))
                }
            }
            Type::Or(types) => {
                let mut group = Vec::new();
                for t in types {
                    group.push(self.resolve_type(t)?.into_owned());
                }
                Ok(Cow::Owned(Type::Or(group)))
            }
            Type::Fn { args, returns } => {
                let mut types = Vec::new();
                for (n, t) in args {
                    types.push((n.to_owned(), self.resolve_type(t)?.into_owned()));
                }
                let returns = Box::new(self.resolve_type(returns)?.into_owned());
                Ok(Cow::Owned(Type::Fn {
                    args: types,
                    returns,
                }))
            }
            Type::Obj(_) | Type::Number | Type::String | Type::Bool | Type::Void => {
                Ok(Cow::Owned(typedef.clone()))
            }
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
