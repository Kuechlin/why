use crate::types::{Expr, SourceMap, SyntaxErr, Token, Value};

type ParserResult = Result<Box<Expr>, SyntaxErr>;

struct ParserCtx<'a, 'b> {
    tokens: &'a Vec<Token>,
    source_map: &'b Vec<SourceMap>,
    current: usize,
}

impl ParserCtx<'_, '_> {
    fn is_end(&self) -> bool {
        self.current >= self.tokens.len()
    }
    fn check(&self, t: Token) -> bool {
        if self.is_end() {
            return false;
        }
        self.tokens[self.current] == t
    }
    fn advance(&mut self) -> Token {
        if self.is_end() {
            return Token::Semicolon;
        }
        let value = self.tokens[self.current].clone();
        self.current += 1;
        return value;
    }
    fn is(&mut self, tokens: &[Token]) -> bool {
        for t in tokens {
            if self.check(t.clone()) {
                self.current += 1;
                return true;
            }
        }
        return false;
    }
    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }
    fn error(&self, msg: &'static str) -> SyntaxErr {
        let pos = match self.source_map.get(self.current) {
            Some(x) => x,
            None => &SourceMap {
                start: 0,
                len: 0,
                line: 0,
            },
        };
        SyntaxErr {
            message: msg.to_owned(),
            pos: *pos,
        }
    }

    // expressions
    fn expression(&mut self) -> ParserResult {
        self.equality()
    }
    // comparison (!= | ==) comparison
    fn equality(&mut self) -> ParserResult {
        let mut expr = self.comparison()?;

        while self.is(&[Token::BangEqual, Token::EqualEqual]) {
            let op = self.previous();
            let right = self.comparison()?;
            expr = Box::new(Expr::Binary {
                op,
                left: expr,
                right,
            });
        }
        Ok(expr)
    }
    // term (< | <= | > | >=) term
    fn comparison(&mut self) -> ParserResult {
        let mut expr = self.term()?;

        while self.is(&[
            Token::Greater,
            Token::GreaterEqual,
            Token::Less,
            Token::LessEqual,
        ]) {
            let op = self.previous();
            let right = self.term()?;
            expr = Box::new(Expr::Binary {
                op,
                left: expr,
                right,
            });
        }
        Ok(expr)
    }
    // factory (+ | -) factory
    fn term(&mut self) -> ParserResult {
        let mut expr = self.factory()?;

        while self.is(&[Token::Plus, Token::Minus]) {
            let op = self.previous();
            let right = self.factory()?;
            expr = Box::new(Expr::Binary {
                op,
                left: expr,
                right,
            });
        }
        Ok(expr)
    }
    // unary (* | /) unary
    fn factory(&mut self) -> ParserResult {
        let mut expr = self.unary()?;

        while self.is(&[Token::Star, Token::Slash]) {
            let op = self.previous();
            let right = self.unary()?;
            expr = Box::new(Expr::Binary {
                op,
                left: expr,
                right,
            });
        }
        Ok(expr)
    }
    // (! | -) unary | primary
    fn unary(&mut self) -> ParserResult {
        if self.is(&[Token::Bang, Token::Minus]) {
            let op = self.previous();
            let expr = self.primary()?;
            return Ok(Box::new(Expr::Unary { op, expr }));
        }
        self.primary()
    }
    // int, float, string, bool, expression
    fn primary(&mut self) -> ParserResult {
        match self.advance() {
            Token::Bool(val) => Ok(Box::new(Expr::Literal(Value::Bool(val)))),
            Token::Number(val) => Ok(Box::new(Expr::Literal(Value::Number(val)))),
            Token::String(val) => Ok(Box::new(Expr::Literal(Value::String(val.to_string())))),
            Token::LeftParen => {
                // group
                let expr = self.expression()?;
                if !self.is(&[Token::RightParen]) {
                    Err(self.error("Expect ')' after expression."))
                } else {
                    Ok(expr)
                }
            }
            t => panic!("invalid token {:?}", t),
        }
    }
}

pub fn parse(tokens: &Vec<Token>, source_map: &Vec<SourceMap>) -> ParserResult {
    let mut ctx = ParserCtx {
        tokens,
        source_map,
        current: 0,
    };

    ctx.expression()
}
