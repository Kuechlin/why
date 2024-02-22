use std::{collections::HashMap, fmt::Display, rc::Rc};

use spanned::Spannable;

use super::{Span, Spannable, Spanned};

#[derive(Debug, Clone)]
pub enum Type {
    Number(Span),
    String(Span),
    Bool(Span),
    Fn(FnType),
    Obj(ObjType),
    Or(OrType),
    Def(Spanned<String>),
    Void,
}
impl Spannable for Type {
    fn span(&self) -> &Span {
        match self {
            Type::Number(span) => span,
            Type::String(span) => span,
            Type::Bool(span) => span,
            Type::Fn(f) => &f.span,
            Type::Obj(o) => &o.span,
            Type::Or(o) => &o.span,
            Type::Def(d) => &d.1,
            Type::Void => panic!("Void has no span"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct FnType {
    pub args: Vec<(Spanned<String>, Rc<Type>)>,
    pub returns: Rc<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct ObjType {
    pub entries: HashMap<Spanned<String>, Rc<Type>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct OrType {
    pub types: Vec<Rc<Type>>,
    pub span: Span,
}

impl Type {
    pub fn combine(&self, other: &Self) -> Type {
        match (self, other) {
            (Type::Or(left), Type::Or(right)) => {
                let mut types = left.types.clone();
                for t in &right.types {
                    if !types.contains(&t) {
                        types.push(t.clone());
                    }
                }
                Type::Or(OrType {
                    types,
                    span: left.span.clone(),
                })
            }
            (left, Type::Or(right)) => {
                if right.types.iter().any(|t| t.as_ref() == left) {
                    return Type::Or(right.clone());
                }
                let mut types = right.types.clone();
                types.push(Rc::new(left.clone()));
                Type::Or(OrType {
                    types,
                    span: right.span.clone(),
                })
            }
            (Type::Or(left), right) => {
                if left.types.iter().any(|t| t.as_ref() == right) {
                    return Type::Or(left.clone());
                }
                let mut types = left.types.clone();
                types.push(Rc::new(right.clone()));
                Type::Or(OrType {
                    types,
                    span: left.span.clone(),
                })
            }
            (left, right) => {
                if *left == *right {
                    left.clone()
                } else {
                    Type::Or(OrType {
                        types: vec![Rc::new(left.clone()), Rc::new(right.clone())],
                        span: left.span().clone(),
                    })
                }
            }
        }
    }

    pub fn includes(&self, other: &Self) -> bool {
        println!("{self} {other}");
        match (self, other) {
            (Type::Or(left), Type::Or(right)) => {
                let l = Type::Or(OrType {
                    types: left.types.to_vec(),
                    span: left.span.clone(),
                });
                for r in &right.types {
                    if !l.includes(&r) {
                        return false;
                    }
                }
                true
            }
            (_, Type::Or(_)) => false,
            (Type::Or(left), right) => {
                for l in &left.types {
                    if l.includes(right) {
                        return true;
                    }
                }
                false
            }
            (Type::Obj(left), Type::Obj(right)) => {
                for (name, r_type) in &right.entries {
                    match left.entries.get(&name) {
                        Some(l_type) => {
                            if !l_type.includes(&r_type) {
                                return false;
                            }
                        }
                        None => return false,
                    }
                }
                true
            }
            (Type::Fn(l), Type::Fn(r)) => {
                for (i, l_arg) in l.args.iter().enumerate() {
                    if let Some(r_arg) = r.args.get(i) {
                        if r_arg.1.includes(&l_arg.1) {
                            continue;
                        }
                    }
                    return false;
                }
                l.returns.includes(&r.returns)
            }
            (Type::Def(left), Type::Def(right)) => *left == *right,
            (Type::String(_), Type::String(_)) => true,
            (Type::Number(_), Type::Number(_)) => true,
            (Type::Bool(_), Type::Bool(_)) => true,
            (Type::Void, Type::Void) => true,
            _ => false,
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        let res = match (self, other) {
            (Self::Fn(l), Self::Fn(r)) => {
                for (i, l) in l.args.iter().enumerate() {
                    let r = match r.args.get(i) {
                        Some(x) => x,
                        None => return false,
                    };
                    if r.0 != l.0 || r.1 != l.1 {
                        return false;
                    }
                }
                *l.returns == *r.returns
            }
            (Self::Obj(l), Self::Obj(r)) => {
                for (key, type_l) in l.entries.iter() {
                    let type_r = match r.entries.get(key) {
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
                for (i, type_l) in l.types.iter().enumerate() {
                    let type_r = match r.types.get(i) {
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
            (Self::String(_), Self::String(_)) => true,
            (Self::Number(_), Self::Number(_)) => true,
            (Self::Bool(_), Self::Bool(_)) => true,
            (Self::Void, Self::Void) => true,
            _ => false,
        };
        println!("{:?} == {:?} = {}", self, other, res);
        res
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let val = match self {
            Type::Number(_) => "Num",
            Type::String(_) => "Str",
            Type::Bool(_) => "Bool",
            Type::Void => "Void",
            Type::Def(val) => &val.0,
            Type::Or(or) => {
                return write!(
                    f,
                    "({})",
                    or.types
                        .iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<String>>()
                        .join(" | ")
                )
            }
            Type::Fn(fun) => {
                let mut list = fun
                    .args
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name.0, ty))
                    .collect::<Vec<String>>()
                    .join(", ");

                if !list.is_empty() {
                    list += " ";
                }
                return write!(f, "fn {list}-> {}", fun.returns);
            }
            Type::Obj(o) => {
                let list = o
                    .entries
                    .iter()
                    .map(|(name, value)| format!("{}: {value}", name.0))
                    .collect::<Vec<String>>()
                    .join(", ");
                return write!(f, "{{ {list} }}");
            }
        };
        write!(f, "{val}")
    }
}
