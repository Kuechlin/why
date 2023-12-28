use crate::types::{SourceMap, SyntaxErr, Token};

struct LexerCtx {
    source: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
    tokens: Vec<Token>,
    source_map: Vec<SourceMap>,
}

impl LexerCtx {
    fn is_end(&self) -> bool {
        self.current >= self.source.len()
    }
    fn previous(&self) -> char {
        self.source[self.current - 1]
    }
    fn peek(&self) -> char {
        match self.is_end() {
            true => '\0',
            false => self.source[self.current],
        }
    }
    fn peek_next(&self) -> char {
        match self.current + 1 >= self.source.len() {
            true => '\0',
            false => self.source[self.current + 1],
        }
    }
    fn advance(&mut self) -> char {
        let val = self.source[self.current];
        self.current += 1;
        return val;
    }
    fn add(&mut self, t: Token) {
        self.source_map.push(SourceMap {
            start: self.start,
            len: self.current - self.start,
            line: self.line,
        });
        self.tokens.push(t);
    }
    fn get_current(&self) -> String {
        self.get_string(self.start, self.current)
    }
    fn get_string(&self, start: usize, end: usize) -> String {
        self.source[start..end].iter().collect()
    }
    fn operator(&mut self, op: Token, op_with_eq: Token) {
        if self.peek() != '=' {
            self.add(op)
        } else {
            self.add(op_with_eq);
            self.current += 1;
        }
    }
    fn string(&mut self) -> Result<(), SyntaxErr> {
        while self.peek() != '"' && !self.is_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.current += 1;
        }

        if self.is_end() {
            return Err(SyntaxErr {
                message: "unterminated string".to_owned(),
                pos: SourceMap {
                    line: match self.previous() == '\n' {
                        true => self.line - 1,
                        false => self.line,
                    },
                    start: self.start,
                    len: self.current - 1 - self.start,
                },
            });
        }

        self.current += 1;

        let value = self.get_string(self.start + 1, self.current - 1);

        self.add(Token::String(value));
        Ok(())
    }
    fn number(&mut self) -> Result<(), SyntaxErr> {
        while self.peek().is_digit(10) {
            self.current += 1;
        }
        // parce decimals
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            while self.peek().is_digit(10) {
                self.current += 1;
            }
        }
        match self.get_current().parse::<f64>() {
            Ok(value) => {
                self.add(Token::Number(value));
                Ok(())
            }
            Err(_) => Err(SyntaxErr {
                message: "invalid float".to_owned(),
                pos: SourceMap {
                    line: self.line,
                    start: self.start,
                    len: self.current - self.start,
                },
            }),
        }
    }
    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.current += 1;
        }
        let value = self.get_current();

        match value.as_str() {
            "fn" => self.add(Token::Fn),
            "if" => self.add(Token::If),
            "let" => self.add(Token::Let),
            "true" => self.add(Token::Bool(true)),
            "false" => self.add(Token::Bool(false)),
            _ => self.add(Token::Identifier(value)),
        }
    }
    fn get_next(&mut self) -> Result<(), SyntaxErr> {
        let c = self.advance();
        match c {
            '(' => self.add(Token::LeftParen),
            ')' => self.add(Token::RightParen),
            '{' => self.add(Token::LeftBrace),
            '}' => self.add(Token::RightBrace),
            '[' => self.add(Token::LeftBracket),
            ']' => self.add(Token::RightBracket),
            ',' => self.add(Token::Comma),
            '.' => self.add(Token::Dot),
            '-' => self.add(Token::Minus),
            '+' => self.add(Token::Plus),
            '*' => self.add(Token::Star),
            '/' => self.add(Token::Slash),
            ';' => self.add(Token::Semicolon),
            // operators
            '!' => self.operator(Token::Bang, Token::BangEqual),
            '=' => self.operator(Token::Equal, Token::EqualEqual),
            '>' => self.operator(Token::Greater, Token::GreaterEqual),
            '<' => self.operator(Token::Less, Token::LessEqual),
            // string
            '"' => self.string()?,
            // ignore whitespace
            ' ' => (),
            '\t' => (),
            '\r' => (),
            // new line
            '\n' => self.line += 1,
            // default
            c => {
                // number
                if c.is_digit(10) {
                    self.number()?;
                } else if c.is_alphabetic() {
                    self.identifier();
                } else {
                    // error
                    return Err(SyntaxErr {
                        message: format!("invalid token '{c}'"),
                        pos: SourceMap {
                            line: self.line,
                            start: self.current,
                            len: 1,
                        },
                    });
                }
            }
        };
        Ok(())
    }
}

pub fn scan(input: &str) -> Result<(Vec<Token>, Vec<SourceMap>), SyntaxErr> {
    let mut ctx = LexerCtx {
        start: 0,
        current: 0,
        line: 0,
        tokens: Vec::new(),
        source: input.chars().collect(),
        source_map: Vec::new(),
    };
    while !ctx.is_end() {
        ctx.start = ctx.current;
        ctx.get_next()?
    }

    return Ok((ctx.tokens, ctx.source_map));
}
