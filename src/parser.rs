use crate::types::{Node, SyntaxErr, Token, TokenData};

type ParserResult = Result<Box<Node>, SyntaxErr>;

struct ParserCtx<'a> {
    tokens: &'a Vec<TokenData>,
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
        self.tokens[self.current].token == t
    }
    fn advance(&mut self) -> TokenData {
        let value = &self.tokens[self.current];
        self.current += 1;
        value.clone()
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
    fn previous(&self) -> TokenData {
        self.tokens[self.current - 1].clone()
    }
    fn current(&self) -> TokenData {
        self.tokens[self.current].clone()
    }
    fn error(&self, msg: &'static str, token: &TokenData) -> SyntaxErr {
        SyntaxErr {
            message: msg.to_owned(),
            source: token.source,
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
            Ok(Box::new(Node::Block(stmts)))
        } else {
            Err(self.error("Expect '}' at end of block", &self.current()))
        }
    }

    fn variable(&mut self) -> ParserResult {
        if !self.is(&[Token::Let]) {
            return Ok(self.expression()?);
        }

        let name = self.advance();
        match name.token {
            Token::Identifier(_) => (),
            _ => return Err(self.error("Identifier expected", &self.previous())),
        };
        if self.is(&[Token::DotDot]) {
            // parse type def
        }
        let expr = match self.is(&[Token::Equal]) {
            true => self.expression()?,
            _ => return Err(self.error("Initializer expected", &self.current())),
        };

        if self.is(&[Token::Semicolon, Token::NewLine]) {
            Ok(Box::new(Node::Let { name, node: expr }))
        } else {
            Err(self.error("Expect ';' or new line after statement", &self.current()))
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
            expr = Box::new(Node::Binary {
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
            expr = Box::new(Node::Binary {
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
            expr = Box::new(Node::Binary {
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
            expr = Box::new(Node::Binary {
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
            let expr = self.unary()?;
            return Ok(Box::new(Node::Unary { op, node: expr }));
        }
        self.primary()
    }
    // int, float, string, bool, expression
    fn primary(&mut self) -> ParserResult {
        let current = self.advance();
        match current.token {
            Token::Bool(_) => Ok(Box::new(Node::Literal(current))),
            Token::Number(_) => Ok(Box::new(Node::Literal(current))),
            Token::String(_) => Ok(Box::new(Node::Literal(current))),
            Token::Identifier(_) => Ok(Box::new(Node::Literal(current))),
            Token::LeftParen => {
                // group
                let expr = self.expression()?;
                if !self.is(&[Token::RightParen]) {
                    Err(self.error("Expect ')' after expression.", &self.current()))
                } else {
                    Ok(expr)
                }
            }
            _ => Err(self.error("invalid token", &current)),
        }
    }
}

pub fn parse(tokens: &Vec<TokenData>) -> ParserResult {
    let mut ctx = ParserCtx { tokens, current: 0 };

    let mut stmts = Vec::new();
    while !ctx.is_end() {
        if ctx.is(&[Token::NewLine]) {
            continue;
        }
        stmts.push(ctx.statement()?)
    }
    Ok(Box::new(Node::Block(stmts)))
}
