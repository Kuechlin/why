use super::{
    tokens::Token,
    values::{Type, Value},
    Span, Spanned,
};

#[derive(Clone)]
pub enum Node {
    Literal(Spanned<Value>),
    Identifier(Spanned<String>),
    Unary {
        op: Spanned<Token>,
        node: Box<Self>,
        span: Span,
    },
    Binary {
        op: Spanned<Token>,
        left: Box<Self>,
        right: Box<Self>,
        span: Span,
    },
    Block {
        nodes: Vec<Self>,
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
}

impl Node {
    pub fn get_span(&self) -> &Span {
        match self {
            Node::Literal(x) => &x.1,
            Node::Identifier(x) => &x.1,
            Node::Unary {
                op: _,
                node: _,
                span,
            } => span,
            Node::Binary {
                op: _,
                left: _,
                right: _,
                span,
            } => span,
            Node::Block { nodes: _, span } => span,
            Node::Let {
                name: _,
                expr: _,
                span,
            } => span,
            Node::If {
                cond: _,
                then: _,
                or: _,
                span,
            } => span,
            Node::Fn {
                typedef: _,
                block: _,
                span,
            } => span,
            Node::Call {
                name: _,
                args: _,
                span,
            } => span,
        }
    }
}
