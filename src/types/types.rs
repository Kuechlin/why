use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone)]
pub enum Type {
    Number,
    String,
    Bool,
    Fn {
        args: Vec<(String, Self)>,
        returns: Box<Self>,
    },
    Obj(HashMap<String, Self>),
    Or(Vec<Self>),
    Def(String),
    Void,
}

impl Type {
    pub fn combine(&self, other: &Self) -> Type {
        match (self, other) {
            (Type::Or(left), Type::Or(right)) => {
                let mut types = left.clone();
                for t in right {
                    if !types.contains(t) {
                        types.push(t.clone());
                    }
                }
                Type::Or(types)
            }
            (left, Type::Or(right)) => {
                if right.contains(left) {
                    return Type::Or(right.clone());
                }
                let mut types = right.clone();
                types.push(left.clone());
                Type::Or(types)
            }
            (Type::Or(left), right) => {
                if left.contains(right) {
                    return Type::Or(left.clone());
                }
                let mut types = left.clone();
                types.push(right.clone());
                Type::Or(types)
            }
            (left, right) => {
                if *left == *right {
                    left.clone()
                } else {
                    Type::Or(vec![left.clone(), right.clone()])
                }
            }
        }
    }

    pub fn includes(&self, other: &Self) -> bool {
        println!("{self} {other}");
        match (self, other) {
            (Type::Or(left), Type::Or(right)) => {
                let l = Type::Or(left.to_vec());
                for r in right {
                    if !l.includes(r) {
                        return false;
                    }
                }
                true
            }
            (_, Type::Or(_)) => false,
            (Type::Or(left), right) => {
                for l in left {
                    if l.includes(right) {
                        return true;
                    }
                }
                false
            }
            (Type::Obj(left), Type::Obj(right)) => {
                for (name, r_type) in right {
                    match left.get(name) {
                        Some(l_type) => {
                            if !l_type.includes(r_type) {
                                return false;
                            }
                        }
                        None => return false,
                    }
                }
                true
            }
            (Type::String, Type::String) => true,
            (Type::Number, Type::Number) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Void, Type::Void) => true,
            _ => false,
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Fn {
                    args: l_args,
                    returns: l_returns,
                },
                Self::Fn {
                    args: r_args,
                    returns: r_returns,
                },
            ) => {
                for (i, l) in l_args.iter().enumerate() {
                    let r = match r_args.get(i) {
                        Some(x) => x,
                        None => return false,
                    };
                    if r.0 != l.0 || r.1 != l.1 {
                        return false;
                    }
                }
                *l_returns == *r_returns
            }
            (Self::Obj(l), Self::Obj(r)) => {
                for (key, type_l) in l {
                    let type_r = match r.get(key) {
                        Some(x) => x,
                        None => return false,
                    };
                    if *type_r != *type_l {
                        return false;
                    }
                }
                true
            }
            (Self::Or(l), Self::Or(r)) => {
                for (i, type_l) in l.iter().enumerate() {
                    let type_r = match r.get(i) {
                        Some(x) => x,
                        None => return false,
                    };
                    if *type_r != *type_l {
                        return false;
                    }
                }
                true
            }
            (Self::Def(l), Self::Def(r)) => *l == *r,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let val = match self {
            Type::Number => "Num",
            Type::String => "Str",
            Type::Bool => "Bool",
            Type::Void => "Void",
            Type::Def(val) => val,
            Type::Or(types) => {
                return write!(
                    f,
                    "({})",
                    types
                        .iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<String>>()
                        .join(" | ")
                )
            }
            Type::Fn { args, returns } => {
                let mut list = args
                    .iter()
                    .map(|a| format!("{}: {}", a.0, a.1))
                    .collect::<Vec<String>>()
                    .join(", ");

                if !list.is_empty() {
                    list += " ";
                }
                return write!(f, "fn {list}-> {}", returns);
            }
            Type::Obj(entries) => {
                let list = entries
                    .iter()
                    .map(|(name, value)| format!("{name}: {value}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                return write!(f, "{{ {list} }}");
            }
        };
        write!(f, "{val}")
    }
}
