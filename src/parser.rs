use std::any::Any;

use crate::types::{Expr, SourceMap, Stmt, SyntaxErr, Token, Value};

type ParserResult = Result<Box<dyn Any>, SyntaxErr>;
type ExprResult = Result<Box<Expr>, SyntaxErr>;

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
    fn error(&self, msg: &'static str, i: usize) -> SyntaxErr {
        let pos = match self.source_map.get(i) {
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

    // statement
    fn statement(&mut self) -> ParserResult {
        self.block()
    }

    fn block(&mut self) -> ParserResult {
        if !self.is(&[Token::LeftBrace]) {
            return Ok(self.variable()?);
        }

        let mut stmts = Vec::new();
        while !self.check(Token::RightBrace) && !self.is_end() {
            stmts.push(self.statement()?)
        }
        if self.is(&[Token::RightBrace]) {
            Ok(Box::new(Stmt::Block(stmts)))
        } else {
            Err(self.error("Expect '}' at end of block", self.current))
        }
    }

    fn variable(&mut self) -> ParserResult {
        if !self.is(&[Token::Let]) {
            return Ok(self.expression()?);
        }

        let identifier = match self.advance() {
            Token::Identifier(name) => name,
            _ => return Err(self.error("Identifier expected", self.current - 1)),
        };
        if self.is(&[Token::DotDot]) {
            // parse type def
        }
        let initializer = match self.is(&[Token::Equal]) {
            true => self.expression()?,
            false => Box::new(Expr::Literal(Value::Void)),
        };

        if self.is(&[Token::Semicolon, Token::NewLine]) {
            Ok(Box::new(Stmt::Let {
                name: identifier,
                expr: initializer,
            }))
        } else {
            Err(self.error("Expect ';' or new line after statement", self.current))
        }
    }

    // expressions
    fn expression(&mut self) -> ExprResult {
        self.equality()
    }
    // comparison (!= | ==) comparison
    fn equality(&mut self) -> ExprResult {
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
    fn comparison(&mut self) -> ExprResult {
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
    fn term(&mut self) -> ExprResult {
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
    fn factory(&mut self) -> ExprResult {
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
    fn unary(&mut self) -> ExprResult {
        if self.is(&[Token::Bang, Token::Minus]) {
            let op = self.previous();
            let expr = self.unary()?;
            return Ok(Box::new(Expr::Unary { op, expr }));
        }
        self.primary()
    }
    // int, float, string, bool, expression
    fn primary(&mut self) -> ExprResult {
        match self.advance() {
            Token::Bool(val) => Ok(Box::new(Expr::Literal(Value::Bool(val)))),
            Token::Number(val) => Ok(Box::new(Expr::Literal(Value::Number(val)))),
            Token::String(val) => Ok(Box::new(Expr::Literal(Value::String(val.to_string())))),
            Token::Identifier(val) => Ok(Box::new(Expr::Var(val))),
            Token::LeftParen => {
                // group
                let expr = self.expression()?;
                if !self.is(&[Token::RightParen]) {
                    Err(self.error("Expect ')' after expression.", self.current))
                } else {
                    Ok(expr)
                }
            }
            _ => Err(self.error("invalid token", self.current - 1)),
        }
    }
}

pub fn parse(tokens: &Vec<Token>, source_map: &Vec<SourceMap>) -> ParserResult {
    let mut ctx = ParserCtx {
        tokens,
        source_map,
        current: 0,
    };

    let mut stmts = Vec::new();
    while !ctx.is_end() {
        if ctx.is(&[Token::NewLine]) {
            continue;
        }
        stmts.push(ctx.statement()?)
    }
    Ok(Box::new(Stmt::Block(stmts)))
}
