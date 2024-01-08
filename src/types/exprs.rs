use std::fmt::Display;

use super::{values::Value, Span, Spanned};

#[derive(PartialEq, Clone)]
pub enum Type {
    Number,
    String,
    Bool,
    Fn {
        args: Vec<(String, Type)>,
        returns: Box<Type>,
    },
    Or(Vec<Self>),
    Def(String),
    Void,
}

#[derive(Clone, PartialEq)]
pub enum Expr {
    Literal(Spanned<Value>),
    Var(Spanned<String>),
    Unary {
        op: Spanned<UnaryOp>,
        expr: Box<Self>,
        span: Span,
    },
    Binary {
        op: Spanned<BinaryOp>,
        left: Box<Self>,
        right: Box<Self>,
        span: Span,
    },
    Block {
        stmts: Vec<Self>,
        span: Span,
    },
    Let {
        name: Spanned<String>,
        expr: Box<Self>,
        span: Span,
    },
    If {
        cond: Box<Self>,
        then: Box<Self>,
        or: Option<Box<Self>>,
        span: Span,
    },
    Fn {
        typedef: Spanned<Type>,
        block: Box<Self>,
        span: Span,
    },
    Call {
        name: Spanned<String>,
        args: Vec<Self>,
        span: Span,
    },
    Def {
        name: Spanned<String>,
        typedef: Spanned<Type>,
        span: Span,
    },
}

#[derive(Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Bang,
    Minus,
}

impl Expr {
    pub fn get_span(&self) -> &Span {
        match self {
            Expr::Literal(x) => &x.1,
            Expr::Var(x) => &x.1,
            Expr::Unary {
                op: _,
                expr: _,
                span,
            } => span,
            Expr::Binary {
                op: _,
                left: _,
                right: _,
                span,
            } => span,
            Expr::Block { stmts: _, span } => span,
            Expr::Let {
                name: _,
                expr: _,
                span,
            } => span,
            Expr::If {
                cond: _,
                then: _,
                or: _,
                span,
            } => span,
            Expr::Fn {
                typedef: _,
                block: _,
                span,
            } => span,
            Expr::Call {
                name: _,
                args: _,
                span,
            } => span,
            Expr::Def {
                name: _,
                typedef: _,
                span,
            } => span,
        }
    }
}

impl Type {
    pub fn combine(&self, other: &Self) -> Type {
        match (self, other) {
            (Type::Or(left), Type::Or(right)) => {
                let mut types = left.clone();
                types.append(&mut right.clone());
                Type::Or(types)
            }
            (left, Type::Or(right)) => {
                let mut types = right.clone();
                types.push(left.clone());
                Type::Or(types)
            }
            (Type::Or(left), right) => {
                let mut types = left.clone();
                types.push(right.clone());
                Type::Or(types)
            }
            (left, right) => Type::Or(vec![left.clone(), right.clone()]),
        }
    }

    pub fn includes(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Or(left), Type::Or(right)) => {
                for r in right {
                    if !left.contains(r) {
                        return false;
                    }
                }
                true
            }
            (_, Type::Or(_)) => false,
            (Type::Or(left), right) => left.contains(right),
            (left, right) => *left == *right,
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Plus => "+",
                BinaryOp::Minus => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
                BinaryOp::Equal => "==",
                BinaryOp::NotEqual => "!=",
                BinaryOp::Greater => ">",
                BinaryOp::GreaterEqual => ">=",
                BinaryOp::Less => "<",
                BinaryOp::LessEqual => "<=",
            }
        )
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Bang => "!",
                UnaryOp::Minus => "-",
            }
        )
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name.0),
            Expr::Literal(val) => write!(f, "{}", val.0.to_string()),
            Expr::Unary { op, expr, span: _ } => write!(f, "{}{expr}", op.0),
            Expr::Binary {
                op,
                left,
                right,
                span: _,
            } => write!(f, "{left} {} {right}", op.0),
            Expr::Block { stmts, span: _ } => {
                let list = stmts
                    .iter()
                    .map(|s| format!("\t{s}"))
                    .collect::<Vec<String>>()
                    .join("\n");
                write!(f, "{{\n{list}\n}}")
            }
            Expr::Let {
                name,
                expr,
                span: _,
            } => {
                write!(f, "let {} = {expr};", name.0)
            }
            Expr::If {
                cond,
                then,
                or,
                span: _,
            } => match or {
                None => write!(f, "if {cond} {then}"),
                Some(x) => write!(f, "if {cond} {then} else {x}"),
            },
            Expr::Fn {
                block,
                typedef,
                span: _,
            } => write!(f, "{} {block}", typedef.0),
            Expr::Call {
                name,
                args,
                span: _,
            } => {
                let list = args
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}({list})", name.0)
            }
            Expr::Def {
                name,
                typedef,
                span: _,
            } => write!(f, "def {}: {}", name.0, typedef.0),
        }
    }
}
