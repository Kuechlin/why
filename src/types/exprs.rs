use std::{borrow::Cow, fmt::Display};

use super::values::{Type, Value};

#[derive(Clone, PartialEq)]
pub enum Expr {
    Var {
        name: String,
        typedef: Type,
    },
    Literal(Value),
    Unary {
        op: UnaryOp,
        expr: Box<Self>,
        typedef: Type,
    },
    Binary {
        op: BinaryOp,
        left: Box<Self>,
        right: Box<Self>,
        typedef: Type,
    },
    Block {
        stmts: Vec<Self>,
    },
    Let {
        name: String,
        expr: Box<Self>,
    },
    If {
        cond: Box<Self>,
        then: Box<Self>,
        or: Option<Box<Self>>,
        typedef: Type,
    },
    Fn {
        block: Box<Self>,
        typedef: Type,
    },
    Call {
        name: String,
        args: Vec<Self>,
        typedef: Type,
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
    Mins,
}

impl Expr {
    pub fn get_return(&self) -> Cow<'_, Type> {
        match self {
            Expr::Var { name: _, typedef } => Cow::Borrowed(typedef),
            Expr::Literal(val) => Cow::Owned(val.get_type()),
            Expr::Unary {
                op: _,
                expr: _,
                typedef,
            } => Cow::Borrowed(typedef),
            Expr::Binary {
                op: _,
                left: _,
                right: _,
                typedef,
            } => Cow::Borrowed(typedef),
            Expr::Block { stmts } => match stmts.last() {
                Some(expr) => expr.get_return(),
                None => Cow::Owned(Type::Void),
            },
            Expr::Let { name: _, expr } => expr.get_return(),
            Expr::If {
                cond: _,
                then: _,
                or: _,
                typedef,
            } => Cow::Borrowed(typedef),
            Expr::Call {
                name: _,
                args: _,
                typedef,
            } => Cow::Borrowed(typedef),
            Expr::Fn { block: _, typedef } => Cow::Borrowed(typedef),
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
                UnaryOp::Mins => "-",
            }
        )
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Var { name, typedef: _ } => write!(f, "{name}"),
            Expr::Literal(val) => write!(f, "{}", val.to_string()),
            Expr::Unary {
                op,
                expr,
                typedef: _,
            } => write!(f, "{op}{expr}"),
            Expr::Binary {
                op,
                left,
                right,
                typedef: _,
            } => write!(f, "{left} {op} {right}"),
            Expr::Block { stmts } => {
                let list = stmts
                    .iter()
                    .map(|s| format!("\t{s}"))
                    .collect::<Vec<String>>()
                    .join("\n");
                write!(f, "{{\n{list}\n}}")
            }
            Expr::Let { name, expr } => {
                write!(f, "let {name}: {} = {expr};", expr.get_return())
            }
            Expr::If {
                cond,
                then,
                or,
                typedef: _,
            } => match or {
                None => write!(f, "if {cond} {then}"),
                Some(x) => write!(f, "if {cond} {then} else {x}"),
            },
            Expr::Fn { block, typedef } => write!(f, "{typedef} {block}"),
            Expr::Call {
                name,
                args,
                typedef: _,
            } => {
                let list = args
                    .iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{name}({list})")
            }
        }
    }
}
