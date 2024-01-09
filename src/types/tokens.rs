use std::fmt::Display;

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
    Or,
    And,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(String),
    TypeIdentifier(String),
    String(String),
    Number(f64),
    Bool(bool),

    Fn,
    If,
    Else,
    Is,
    Def,
    Let,
    New,

    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let val = match self {
            Token::LeftParen => "(".to_owned(),
            Token::RightParen => ")".to_owned(),
            Token::LeftBrace => "{".to_owned(),
            Token::RightBrace => "}".to_owned(),
            Token::LeftBracket => "[".to_owned(),
            Token::RightBracket => "]".to_owned(),
            Token::Comma => ",".to_owned(),
            Token::Dot => ".".to_owned(),
            Token::Minus => "-".to_owned(),
            Token::Arrow => "->".to_owned(),
            Token::Plus => "+".to_owned(),
            Token::Star => "*".to_owned(),
            Token::Slash => "/".to_owned(),
            Token::DotDot => ":".to_owned(),
            Token::Semicolon => ";".to_owned(),
            Token::Or => "|".to_owned(),
            Token::And => "&".to_owned(),
            Token::Bang => "!".to_owned(),
            Token::BangEqual => "!=".to_owned(),
            Token::Equal => "=".to_owned(),
            Token::EqualEqual => "==".to_owned(),
            Token::Greater => ">".to_owned(),
            Token::GreaterEqual => ">=".to_owned(),
            Token::Less => "<".to_owned(),
            Token::LessEqual => "<=".to_owned(),
            Token::Identifier(val) => val.to_owned(),
            Token::TypeIdentifier(val) => val.to_owned(),
            Token::String(str) => format!("\"{str}\"").to_owned(),
            Token::Number(num) => num.to_string(),
            Token::Bool(val) => val.to_string(),
            Token::Fn => "fn".to_owned(),
            Token::If => "if".to_owned(),
            Token::Else => "else".to_owned(),
            Token::Is => "is".to_owned(),
            Token::Def => "def".to_owned(),
            Token::Let => "let".to_owned(),
            Token::New => "new".to_owned(),
            Token::Eof => "".to_owned(),
        };
        write!(f, "{val}")
    }
}
