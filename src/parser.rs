use crate::types::{Node, Span, Spanned, SyntaxErr, Token, Type, Value};

type ParserResult = Result<Node, SyntaxErr>;
type TypeResult = Result<Spanned<Type>, SyntaxErr>;

struct ParserCtx<'a> {
    tokens: &'a Vec<Spanned<Token>>,
    current: usize,
}

fn error(msg: &'static str, span: &Span) -> ParserResult {
    Err(SyntaxErr {
        message: msg.to_owned(),
        source: span.clone(),
    })
}

impl ParserCtx<'_> {
    fn is_end(&self) -> bool {
        self.current >= self.tokens.len()
    }
    fn check(&self, t: Token) -> bool {
        if self.is_end() {
            return false;
        }
        self.tokens[self.current].0 == t
    }
    fn advance(&mut self) -> Spanned<Token> {
        let value = self.tokens[self.current].clone();
        self.current += 1;
        value
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
    fn previous(&self) -> Spanned<Token> {
        self.tokens[self.current - 1].clone()
    }
    fn current(&self) -> Spanned<Token> {
        self.tokens[self.current].clone()
    }
    fn end(&self) -> usize {
        self.previous().1.end
    }

    // statement
    fn statement(&mut self) -> ParserResult {
        self.variable()
    }

    fn variable(&mut self) -> ParserResult {
        if !self.is(&[Token::Let]) {
            return Ok(self.condition()?);
        }
        let start = self.previous().1.start;

        let name_token = self.advance();
        let name = match name_token.0 {
            Token::Identifier(name) => (name, name_token.1),
            _ => return error("Identifier expected", &self.previous().1),
        };

        let mut typedef = None;
        let mut init = None;
        // type def
        if self.check(Token::DotDot) {
            // parse type def
            typedef = Some(self.typedef()?);
        }
        // init expression
        else if self.is(&[Token::Equal]) {
            init = Some(Box::new(self.statement()?));
        }
        // error
        else {
            return error("Initializer or typedef expected", &self.current().1);
        }
        // check end
        if !self.is(&[Token::Semicolon]) {
            return error("Expect ';' after statement", &self.current().1);
        }
        Ok(Node::Let {
            name,
            init,
            typedef,
            span: start..self.end(),
        })
    }

    fn condition(&mut self) -> ParserResult {
        if !self.is(&[Token::If]) {
            return Ok(self.function()?);
        }
        let start = self.previous().1.start;

        let cond = self.expression()?;
        if !self.check(Token::LeftBrace) {
            return error("Expected block statement", &self.current().1);
        }
        let then = self.block()?;
        let or = match self.is(&[Token::Else]) {
            true => Some(Box::new(self.statement()?)),
            false => None,
        };

        return Ok(Node::If {
            cond: Box::new(cond),
            then: Box::new(then),
            or,
            span: start..self.end(),
        });
    }

    fn function(&mut self) -> ParserResult {
        let start = self.current().1.start;
        if !self.check(Token::Fn) {
            return Ok(self.block()?);
        }
        let typedef = self.fn_type()?;
        let block = self.block()?;

        Ok(Node::Fn {
            typedef,
            block: Box::new(block),
            span: start..self.end(),
        })
    }

    fn block(&mut self) -> ParserResult {
        if !self.is(&[Token::LeftBrace]) {
            return Ok(self.expression()?);
        }
        let start = self.previous().1.start;

        let mut nodes = Vec::new();
        while !self.check(Token::RightBrace) && !self.is_end() {
            nodes.push(self.statement()?)
        }
        if self.is(&[Token::RightBrace]) {
            Ok(Node::Block {
                nodes,
                span: start..self.end(),
            })
        } else {
            error("Expect '}' at end of block", &self.current().1)
        }
    }

    // expressions
    fn expression(&mut self) -> ParserResult {
        self.equality()
    }
    // comparison (!= | ==) comparison
    fn equality(&mut self) -> ParserResult {
        let start = self.current().1.start;
        let mut expr = self.comparison()?;

        while self.is(&[Token::BangEqual, Token::EqualEqual]) {
            let op = self.previous();
            let right = self.comparison()?;
            expr = Node::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
                span: start..self.end(),
            };
        }
        Ok(expr)
    }
    // term (< | <= | > | >=) term
    fn comparison(&mut self) -> ParserResult {
        let start = self.current().1.start;
        let mut expr = self.term()?;

        while self.is(&[
            Token::Greater,
            Token::GreaterEqual,
            Token::Less,
            Token::LessEqual,
        ]) {
            let op = self.previous();
            let right = self.term()?;
            expr = Node::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
                span: start..self.end(),
            };
        }
        Ok(expr)
    }
    // factory (+ | -) factory
    fn term(&mut self) -> ParserResult {
        let start = self.current().1.start;
        let mut expr = self.factory()?;

        while self.is(&[Token::Plus, Token::Minus]) {
            let op = self.previous();
            let right = self.factory()?;
            expr = Node::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
                span: start..self.end(),
            };
        }
        Ok(expr)
    }
    // unary (* | /) unary
    fn factory(&mut self) -> ParserResult {
        let start = self.current().1.start;
        let mut expr = self.unary()?;

        while self.is(&[Token::Star, Token::Slash]) {
            let op = self.previous();
            let right = self.unary()?;
            expr = Node::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
                span: start..self.end(),
            };
        }
        Ok(expr)
    }
    // (! | -) unary | primary
    fn unary(&mut self) -> ParserResult {
        let start = self.current().1.start;
        if self.is(&[Token::Bang, Token::Minus]) {
            let op = self.previous();
            let expr = self.unary()?;
            return Ok(Node::Unary {
                op,
                node: Box::new(expr),
                span: start..self.end(),
            });
        }
        self.primary()
    }
    // int, float, string, bool, expression
    fn primary(&mut self) -> ParserResult {
        let current = self.advance();
        match current.0 {
            Token::Bool(val) => Ok(Node::Literal((Value::Bool(val), current.1))),
            Token::Number(val) => Ok(Node::Literal((Value::Number(val), current.1))),
            Token::String(val) => Ok(Node::Literal((Value::String(val), current.1))),
            Token::Identifier(val) => match self.check(Token::LeftParen) {
                true => self.call((val, current.1)),
                false => Ok(Node::Identifier((val, current.1))),
            },
            Token::LeftParen => {
                // group
                let expr = self.expression()?;
                if !self.is(&[Token::RightParen]) {
                    error("Expect ')' after expression.", &self.current().1)
                } else {
                    Ok(expr)
                }
            }
            _ => error("invalid token", &current.1),
        }
    }

    fn call(&mut self, name: Spanned<String>) -> ParserResult {
        let start = self.current().1.start;
        if !self.is(&[Token::LeftParen]) {
            return error("Expect '(' after identifer", &name.1);
        }
        let mut args = Vec::new();
        // fn args
        while !self.check(Token::RightParen) && !self.is_end() {
            // get expr
            let expr = self.statement()?;
            args.push(expr);
            // break when no comma
            if !self.is(&[Token::Comma]) {
                break;
            }
        }
        if !self.is(&[Token::RightParen]) {
            return error("Expect ')' at end of args", &self.current().1);
        }
        Ok(Node::Call {
            name,
            args,
            span: start..self.end(),
        })
    }

    // types
    fn typedef(&mut self) -> TypeResult {
        if !self.is(&[Token::DotDot, Token::Arrow]) {
            return Err(SyntaxErr {
                message: "type definition expected".to_owned(),
                source: self.current().1,
            });
        }

        self.fn_type()
    }

    fn fn_type(&mut self) -> TypeResult {
        if !self.is(&[Token::Fn]) {
            return self.value_type();
        }
        let start = self.previous().1.start;
        // parse args
        let mut args = Vec::new();
        // fn args
        while !self.check(Token::Arrow) && !self.is_end() {
            // get name
            let current = self.advance();
            let name = match current.0 {
                Token::Identifier(name) => name,
                _ => {
                    return Err(SyntaxErr {
                        message: "identifier exprected".to_owned(),
                        source: current.1,
                    })
                }
            };
            // get type
            let typedef = self.typedef()?;
            args.push((name, typedef.0));
            // break when no comma
            if !self.is(&[Token::Comma]) {
                break;
            }
        }
        if !self.check(Token::Arrow) {
            return Err(SyntaxErr {
                message: "Expect '->' at end of args to define return value".to_owned(),
                source: self.current().1,
            });
        }
        // fn return type
        let returns = self.typedef()?;
        Ok((
            Type::Fn {
                args,
                returns: Box::new(returns.0),
            },
            start..self.end(),
        ))
    }

    fn value_type(&mut self) -> TypeResult {
        let current = self.advance();
        return match current.0 {
            Token::Identifier(name) => match name.as_str() {
                "number" => Ok((Type::Number, current.1)),
                "string" => Ok((Type::String, current.1)),
                "boolean" => Ok((Type::Bool, current.1)),
                _ => Err(SyntaxErr {
                    message: "type identifier exprected".to_owned(),
                    source: current.1,
                }),
            },
            _ => Err(SyntaxErr {
                message: "type identifier exprected".to_owned(),
                source: current.1,
            }),
        };
    }
}

pub fn parse(tokens: &Vec<Spanned<Token>>) -> Result<Vec<Node>, SyntaxErr> {
    let mut ctx = ParserCtx { tokens, current: 0 };

    let mut nodes = Vec::new();
    while !ctx.is_end() {
        nodes.push(ctx.statement()?)
    }
    Ok(nodes)
}
