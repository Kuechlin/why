use crate::types::{tokens::Token, Spanned, SyntaxErr};

struct LexerCtx {
    source: Vec<char>,
    start: usize,
    current: usize,
    tokens: Vec<Spanned<Token>>,
}

impl LexerCtx {
    fn is_end(&self) -> bool {
        self.current >= self.source.len()
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
        self.tokens.push((t, self.start..self.current));
    }
    fn get_current(&self) -> String {
        self.get_string(self.start, self.current)
    }
    fn get_string(&self, start: usize, end: usize) -> String {
        self.source[start..end].iter().collect()
    }

    // parser functions
    fn comment(&mut self) {
        while self.peek() != '\n' && !self.is_end() {
            self.current += 1;
        }
    }
    fn operator(&mut self, op: Token, op_with_eq: Token) {
        if self.peek() != '=' {
            self.add(op)
        } else {
            self.add(op_with_eq);
            self.current += 1;
        }
    }
    fn add_str(&mut self) {
        let value = self.get_current();
        if !value.is_empty() {
            self.add(Token::String(self.get_current()));
        }
    }
    fn tmpl(&mut self) -> Result<(), SyntaxErr> {
        self.add(Token::Quotation);
        self.start += 1;

        while self.peek() != '"' && !self.is_end() {
            //if self.peek() == '\\' {
            //    self.add(Token::BackSalsh);
            //}
            if self.peek() == '{' {
                self.add_str();
                self.start = self.current;
                while self.peek() != '}' && !self.is_end() {
                    self.start = self.current;
                    self.get_next()?;
                }
                if self.is_end() {
                    return Err(SyntaxErr {
                        message: "Expected '}' after template part".to_owned(),
                        source: self.start..self.current - 1,
                    });
                }
                self.add(Token::RightBrace);
                self.start = self.current + 1;
            }
            self.current += 1;
        }

        if self.is_end() {
            return Err(SyntaxErr {
                message: "unterminated string".to_owned(),
                source: self.start..self.current - 1,
            });
        }

        self.add_str();
        self.current += 1;
        self.add(Token::Quotation);
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
                source: (self.start..self.current),
            }),
        }
    }
    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.current += 1;
        }
        let value = self.get_current();

        match value.as_str() {
            "fn" => self.add(Token::Fn),
            "if" => self.add(Token::If),
            "is" => self.add(Token::Is),
            "def" => self.add(Token::Def),
            "let" => self.add(Token::Let),
            "else" => self.add(Token::Else),
            "true" => self.add(Token::Bool(true)),
            "false" => self.add(Token::Bool(false)),
            _ => {
                if value.chars().next().unwrap().is_ascii_uppercase() {
                    self.add(Token::TypeIdentifier(value))
                } else {
                    self.add(Token::Identifier(value))
                }
            }
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
            '-' => match self.peek() == '>' {
                true => {
                    self.add(Token::Arrow);
                    self.current += 1;
                }
                false => self.add(Token::Minus),
            },
            '+' => self.add(Token::Plus),
            '*' => self.add(Token::Star),
            '/' => match self.peek() == '/' {
                true => self.comment(),
                false => self.add(Token::Slash),
            },
            ':' => self.add(Token::DotDot),
            ';' => self.add(Token::Semicolon),
            // operators
            '!' => self.operator(Token::Bang, Token::BangEqual),
            '=' => self.operator(Token::Equal, Token::EqualEqual),
            '>' => self.operator(Token::Greater, Token::GreaterEqual),
            '<' => self.operator(Token::Less, Token::LessEqual),
            '|' => self.add(Token::Or),
            '&' => self.add(Token::And),
            // string
            '"' => self.tmpl()?,
            // ignore whitespace
            ' ' => (),
            '\t' => (),
            '\r' => (),
            '\n' => (),
            // default
            c => {
                // number
                if c.is_digit(10) {
                    self.number()?;
                } else if c.is_alphabetic() || c == '_' {
                    self.identifier();
                } else {
                    // error
                    return Err(SyntaxErr {
                        message: format!("invalid token '{c}'"),
                        source: self.current..self.current + 1,
                    });
                }
            }
        };
        Ok(())
    }
}

pub fn scan(input: &str) -> Result<Vec<Spanned<Token>>, SyntaxErr> {
    let mut ctx = LexerCtx {
        start: 0,
        current: 0,
        tokens: Vec::new(),
        source: input.chars().collect(),
    };
    while !ctx.is_end() {
        ctx.start = ctx.current;
        ctx.get_next()?
    }

    return Ok(ctx.tokens);
}
