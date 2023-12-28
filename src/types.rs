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

#[derive(Clone, Copy)]
pub struct SourceMap {
    pub start: usize,
    pub len: usize,
    pub line: usize,
}

pub struct SyntaxErr {
    pub message: String,
    pub pos: SourceMap,
}

#[derive(Debug)]
pub enum Expr {
    Var(String),
    Literal(Value),
    Unary {
        op: Token,
        expr: Box<Expr>,
    },
    Binary {
        op: Token,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

pub enum Type {
    Number,
    String,
    Bool,
}

pub enum Stmt {
    Let {
        identifier: String,
        initializer: Box<Expr>,
    },
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Void,
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
