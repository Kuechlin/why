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
    Plus,
    Star,
    Slash,
    DotDot,
    Semicolon,
    NewLine,

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
    Let,
}

#[derive(Debug, Clone)]
pub struct TokenData {
    pub token: Token,
    pub source: SourceMap,
}

#[derive(Debug, Clone, Copy)]
pub struct SourceMap {
    pub start: usize,
    pub len: usize,
    pub line: usize,
}

pub struct SyntaxErr {
    pub message: String,
    pub source: SourceMap,
}

pub struct RuntimeErr {
    pub message: String,
}

pub enum Node {
    Literal(TokenData),
    Unary {
        op: TokenData,
        node: Box<Node>,
    },
    Binary {
        op: TokenData,
        left: Box<Node>,
        right: Box<Node>,
    },
    Let {
        name: TokenData,
        node: Box<Node>,
    },
    Block(Vec<Box<Node>>),
}

#[derive(PartialEq, Clone, Copy)]
pub enum Type {
    Number,
    String,
    Bool,
    Void,
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Void,
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Number(x) => *x != 0.0,
            Value::String(x) => !x.is_empty(),
            Value::Bool(x) => *x,
            Value::Void => false,
        }
    }
    pub fn get_type(&self) -> Type {
        match self {
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Bool(_) => Type::Bool,
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
            Value::Void => "".to_owned(),
        }
    }
}

pub enum Expr {
    Var {
        name: String,
        typedef: Type,
    },
    Literal(Value),
    Unary {
        op: Token,
        expr: Box<Expr>,
        typedef: Type,
    },
    Binary {
        op: Token,
        left: Box<Expr>,
        right: Box<Expr>,
        typedef: Type,
    },
    Let {
        name: String,
        expr: Box<Expr>,
    },
    Block {
        stmts: Vec<Box<Expr>>,
    },
}

impl Expr {
    pub fn get_type(&self) -> Type {
        match self {
            Expr::Var { name: _, typedef } => *typedef,
            Expr::Literal(val) => val.get_type(),
            Expr::Unary {
                op: _,
                expr: _,
                typedef,
            } => *typedef,
            Expr::Binary {
                op: _,
                left: _,
                right: _,
                typedef,
            } => *typedef,
            Expr::Let { name: _, expr } => expr.get_type(),
            Expr::Block { stmts } => match stmts.last() {
                Some(expr) => expr.as_ref().get_type(),
                None => Type::Void,
            },
        }
    }
}
