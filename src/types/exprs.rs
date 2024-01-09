use std::{collections::HashMap, fmt::Display};

use super::{types::Type, values::Value, Span, Spanned};

#[derive(Clone, PartialEq)]
pub enum Expr {
    Literal(Spanned<Value>),
    Var {
        name: Spanned<String>,
        then: Option<Box<Self>>,
        span: Span,
    },
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
    Is {
        expr: Box<Self>,
        cases: Vec<Self>,
        default: Box<Expr>,
        span: Span,
    },
    Match {
        op: Spanned<BinaryOp>,
        expr: Box<Self>,
        then: Box<Self>,
        span: Span,
    },
    MatchType {
        typedef: Spanned<Type>,
        then: Box<Self>,
        span: Span,
    },
    New {
        entries: HashMap<Spanned<String>, Expr>,
        typedef: Option<Spanned<String>>,
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
            Expr::Var {
                name: _,
                then: _,
                span,
            } => span,
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
            Expr::Is {
                expr: _,
                cases: _,
                default: _,
                span,
            } => span,
            Expr::Match {
                op: _,
                expr: _,
                then: _,
                span,
            } => span,
            Expr::MatchType {
                typedef: _,
                then: _,
                span,
            } => span,
            Expr::New {
                entries: _,
                typedef: _,
                span,
            } => span,
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
            Expr::Var {
                name,
                span: _,
                then,
            } => match then {
                Some(expr) => write!(f, "{}.{}", name.0, expr.as_ref()),
                None => write!(f, "{}", name.0),
            },
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
                    .map(|s| format!("{s}"))
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
            Expr::Is {
                expr,
                cases,
                default,
                span: _,
            } => {
                let list = cases
                    .iter()
                    .map(|x| format!("{x},"))
                    .collect::<Vec<String>>()
                    .join("\n");
                write!(
                    f,
                    "{} is {{\n{list}\n-> {}}}",
                    expr.as_ref(),
                    default.as_ref()
                )
            }
            Expr::Match {
                op,
                expr: cond,
                then,
                span: _,
            } => write!(f, "{} {} -> {}", op.0, cond.as_ref(), then.as_ref()),
            Expr::MatchType {
                typedef,
                then,
                span: _,
            } => write!(f, ": {} -> {}", typedef.0, then.as_ref()),
            Expr::New {
                entries,
                typedef,
                span: _,
            } => {
                let list = entries
                    .iter()
                    .map(|(name, value)| format!("{} = {value},", name.0))
                    .collect::<Vec<String>>()
                    .join("\n");

                match typedef {
                    Some(name) => write!(f, "new {} {{\n{list}\n}}", name.0),
                    _ => write!(f, "new {{\n{list}\n}}"),
                }
            }
        }
    }
}
