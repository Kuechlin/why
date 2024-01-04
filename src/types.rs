#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    Comma,
    Dot,
    Minus,
    Arrow,
    Plus,
    Star,
    Slash,
    DotDot,
    Semicolon,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(String),
    String(String),
    Number(f64),
    Bool(bool),

    Fn,
    If,
    Else,
    Let,
}

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

pub struct SyntaxErr {
    pub message: String,
    pub source: Span,
}

pub struct RuntimeErr {
    pub message: String,
}

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
        init: Option<Box<Self>>,
        typedef: Option<Spanned<Type>>,
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
                init: _,
                typedef: _,
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

#[derive(PartialEq, Clone)]
pub enum Type {
    Number,
    String,
    Bool,
    Fn {
        args: Vec<(String, Type)>,
        returns: Box<Type>,
    },
    Void,
}
impl Type {
    pub fn get_default(&self) -> Value {
        match self {
            Type::Number => Value::Number(0.0),
            Type::String => Value::String("".to_owned()),
            Type::Bool => Value::Bool(false),
            Type::Fn { args: _, returns } => Value::Fn {
                typedef: self.clone(),
                expr: Box::new(Expr::Literal(returns.get_default())),
            },
            Type::Void => Value::Void,
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Fn { typedef: Type, expr: Box<Expr> },
    Void,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Number(x) => *x != 0.0,
            Value::String(x) => !x.is_empty(),
            Value::Bool(x) => *x,
            Value::Fn {
                expr: _,
                typedef: _,
            } => true,
            Value::Void => false,
        }
    }
    pub fn get_type(&self) -> Type {
        match self {
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Bool(_) => Type::Bool,
            Value::Fn { expr: _, typedef } => typedef.clone(),
            Value::Void => Type::Void,
        }
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Number(x) => x.to_string(),
            Value::String(x) => x.clone(),
            Value::Bool(x) => x.to_string(),
            Value::Fn {
                typedef: _,
                expr: _,
            } => "fn".to_owned(),
            Value::Void => "".to_owned(),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0.partial_cmp(r0),
            (Self::String(l0), Self::String(r0)) => l0.partial_cmp(r0),
            (Self::Bool(l0), Self::Bool(r0)) => l0.partial_cmp(r0),
            _ => Some(std::cmp::Ordering::Equal),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Expr {
    Var {
        name: String,
        typedef: Type,
    },
    Literal(Value),
    Unary {
        op: Token,
        expr: Box<Self>,
        typedef: Type,
    },
    Binary {
        op: Token,
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

impl Expr {
    pub fn get_return(&self) -> Type {
        match self {
            Expr::Var { name: _, typedef } => typedef.clone(),
            Expr::Literal(val) => val.get_type(),
            Expr::Unary {
                op: _,
                expr: _,
                typedef,
            } => typedef.clone(),
            Expr::Binary {
                op: _,
                left: _,
                right: _,
                typedef,
            } => typedef.clone(),
            Expr::Block { stmts } => match stmts.last() {
                Some(expr) => expr.get_return(),
                None => Type::Void,
            },
            Expr::Let { name: _, expr } => expr.get_return(),
            Expr::If {
                cond: _,
                then: _,
                or: _,
                typedef,
            } => typedef.clone(),
            Expr::Call {
                name: _,
                args: _,
                typedef,
            } => typedef.clone(),
            Expr::Fn { block: _, typedef } => typedef.clone(),
        }
    }
}
