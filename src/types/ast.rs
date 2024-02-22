use std::{collections::HashMap, rc::Rc};

use super::{types::Type, values::Value, Spannable};
use spanned::Spannable;

use super::{Span, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Bang,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Val(ValEx),
    Let(LetEx),
    Def(DefEx),
    Block(BlockEx),
    Unary(UnaryEx),
    Binary(BinaryEx),
    If(IfEx),
    Fn(FnEx),
    Call(CallEx),
    Is(IsEx),
    Var(VarEx),
    Obj(ObjEx),
    Tmpl(TmplEx),
}
impl Spannable for Expr {
    fn span(&self) -> &Span {
        match self {
            Expr::Val(v) => v.span(),
            Expr::Let(v) => v.span(),
            Expr::Def(v) => v.span(),
            Expr::Block(v) => v.span(),
            Expr::Unary(v) => v.span(),
            Expr::Binary(v) => v.span(),
            Expr::If(v) => v.span(),
            Expr::Fn(v) => v.span(),
            Expr::Call(v) => v.span(),
            Expr::Is(v) => v.span(),
            Expr::Var(v) => v.span(),
            Expr::Obj(v) => v.span(),
            Expr::Tmpl(v) => v.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct ValEx {
    pub value: Rc<Value>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct BlockEx {
    pub stmts: Vec<Expr>,
    pub span: Span,
}
#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct LetEx {
    pub name: Spanned<String>,
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct UnaryEx {
    pub op: Spanned<UnaryOp>,
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct BinaryEx {
    pub op: Spanned<BinaryOp>,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct IfEx {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub or: Option<Box<Expr>>,
    pub span: Span,
}
#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct FnEx {
    pub ty: Rc<Type>,
    pub block: Box<Expr>,
    pub span: Span,
}
#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct CallEx {
    pub name: Spanned<String>,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct DefEx {
    pub name: Spanned<String>,
    pub ty: Rc<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct IsEx {
    pub expr: Box<Expr>,
    pub cases: Vec<MatchCase>,
    pub default: Box<Expr>,
    pub span: Span,
}
#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct VarEx {
    pub name: Spanned<String>,
    pub then: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchCase {
    Value(MatchValue),
    Type(MatchType),
}
impl Spannable for MatchCase {
    fn span(&self) -> &Span {
        match self {
            MatchCase::Value(v) => &v.span,
            MatchCase::Type(v) => &v.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct MatchValue {
    pub op: Spanned<BinaryOp>,
    pub expr: Box<Expr>,
    pub then: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct MatchType {
    pub ty: Rc<Type>,
    pub then: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct ObjEx {
    pub entries: HashMap<Spanned<String>, Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spannable)]
pub struct TmplEx {
    pub parts: Vec<Expr>,
    pub span: Span,
}
