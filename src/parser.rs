use crate::expressions::{Expr, Value};
use crate::lexer::Token;

struct ParserCtx<'a> {
    tokens: &'a Vec<Token>,
    current: usize,
}

impl ParserCtx<'_> {
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

    // expressions
    fn expression(&mut self) -> Box<Expr> {
        self.equality()
    }
    // comparison (!= | ==) comparison
    fn equality(&mut self) -> Box<Expr> {
        let mut expr = self.comparison();

        while self.is(&[Token::BangEqual, Token::EqualEqual]) {
            let op = self.previous();
            let right = self.comparison();
            expr = Box::new(Expr::Binary {
                op,
                left: expr,
                right,
            });
        }

        return expr;
    }
    // term (< | <= | > | >=) term
    fn comparison(&mut self) -> Box<Expr> {
        let mut expr = self.term();

        while self.is(&[
            Token::Greater,
            Token::GreaterEqual,
            Token::Less,
            Token::LessEqual,
        ]) {
            let op = self.previous();
            let right = self.term();
            expr = Box::new(Expr::Binary {
                op,
                left: expr,
                right,
            });
        }

        return expr;
    }
    // factory (+ | -) factory
    fn term(&mut self) -> Box<Expr> {
        let mut expr = self.factory();

        while self.is(&[Token::Plus, Token::Minus]) {
            let op = self.previous();
            let right = self.term();
            expr = Box::new(Expr::Binary {
                op,
                left: expr,
                right,
            });
        }
        return expr;
    }
    // unary (* | /) unary
    fn factory(&mut self) -> Box<Expr> {
        let mut expr = self.unary();

        while self.is(&[Token::Star, Token::Slash]) {
            let op = self.previous();
            let right = self.unary();
            expr = Box::new(Expr::Binary {
                op,
                left: expr,
                right,
            });
        }
        return expr;
    }
    // (! | -) unary | primary
    fn unary(&mut self) -> Box<Expr> {
        if self.is(&[Token::Bang, Token::Minus]) {
            let op = self.previous();
            let expr = self.primary();
            return Box::new(Expr::Unary { op, expr });
        }
        return self.primary();
    }
    // int, float, string, bool, expression
    fn primary(&mut self) -> Box<Expr> {
        match self.advance() {
            Token::Bool(val) => Box::new(Expr::Literal(Value::Bool(val))),
            Token::Number(val) => Box::new(Expr::Literal(Value::Number(val))),
            Token::String(val) => Box::new(Expr::Literal(Value::String(val.to_string()))),
            Token::LeftParen => {
                // group
                let expr = self.expression();
                if !self.is(&[Token::RightParen]) {
                    panic!("Expect ')' after expression.")
                }
                expr
            }
            t => panic!("invalid token {:?}", t),
        }
    }
}

pub fn parse(tokens: &Vec<Token>) -> Box<Expr> {
    let mut ctx = ParserCtx { tokens, current: 0 };

    ctx.expression()
}
