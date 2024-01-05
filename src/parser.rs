use crate::types::{
    nodes::Node, tokens::Token, values::Type, values::Value, Span, Spanned, SyntaxErr,
};

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
    fn start(&self) -> usize {
        self.current().1.start
    }
    fn end(&self) -> usize {
        self.previous().1.end
    }

    // statement
    fn statement(&mut self) -> ParserResult {
        self.condition()
    }

    fn condition(&mut self) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::If]) {
            return Ok(self.function()?);
        }

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
        let start = self.start();
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
        let start = self.start();
        if !self.is(&[Token::LeftBrace]) {
            return Ok(self.expression()?);
        }

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
        // equality
        self.binary(&[Token::BangEqual, Token::EqualEqual], |n| {
            // comparisson
            n.binary(
                &[
                    Token::Greater,
                    Token::GreaterEqual,
                    Token::Less,
                    Token::LessEqual,
                ],
                |n| {
                    // plus and minus
                    n.binary(&[Token::Plus, Token::Minus], |n| {
                        // multiply and divide
                        n.binary(&[Token::Star, Token::Slash], |n| {
                            // unary or primary
                            n.unary()
                        })
                    })
                },
            )
        })
    }

    fn binary(&mut self, tokens: &[Token], nested: fn(&mut Self) -> ParserResult) -> ParserResult {
        let start = self.start();
        let mut expr = nested(self)?;

        while self.is(tokens) {
            let op = self.previous();
            let right = nested(self)?;
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
        let start = self.start();
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
            Token::Identifier(val) => {
                if self.check(Token::LeftParen) {
                    self.call((val, current.1))
                } else if self.check(Token::Equal) {
                    self.variable((val, current.1))
                } else {
                    Ok(Node::Identifier((val, current.1)))
                }
            }
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

    fn variable(&mut self, name: Spanned<String>) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::Equal]) {
            return error("initializer expected", &name.1);
        }
        let expr = Box::new(self.statement()?);
        // check end
        if !self.is(&[Token::Semicolon]) {
            return error("Expect ';' after statement", &self.current().1);
        }
        Ok(Node::Let {
            name,
            expr,
            span: start..self.end(),
        })
    }

    fn call(&mut self, name: Spanned<String>) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::LeftParen]) {
            return error("Expect '(' after identifer", &name.1);
        }
        let mut args = Vec::new();
        // fn args
        while !self.check(Token::RightParen) && !self.is_end() {
            // get expr
            let expr = match self.statement()? {
                Node::Block { nodes, span } => Node::Fn {
                    typedef: (Type::Void, 0..0),
                    block: Box::new(Node::Block {
                        nodes,
                        span: span.clone(),
                    }),
                    span: span.clone(),
                },
                node => node,
            };
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

    fn identifer(&mut self) -> Result<Spanned<String>, SyntaxErr> {
        let token = self.advance();
        match token.0 {
            Token::Identifier(name) => Ok((name, token.1)),
            _ => Err(SyntaxErr {
                message: "Identifier expected".to_owned(),
                source: self.previous().1.clone(),
            }),
        }
    }

    // types
    fn typedef(&mut self) -> TypeResult {
        if !self.is(&[Token::LeftParen]) {
            return self.value_type();
        }
        let t = self.fn_type()?;
        if self.is(&[Token::RightParen]) {
            Ok(t)
        } else {
            Err(SyntaxErr {
                message: "Expected ')' after type group".to_owned(),
                source: self.current().1.clone(),
            })
        }
    }

    fn fn_type(&mut self) -> TypeResult {
        let start = self.start();
        if !self.is(&[Token::Fn]) {
            return self.value_type();
        }
        // parse args
        let mut args = Vec::new();
        // fn args
        while !self.check(Token::Arrow) && !self.is_end() {
            // get name
            let name = self.identifer()?;
            // get type
            if !self.is(&[Token::DotDot]) {
                return Err(SyntaxErr {
                    message: "type definition expected".to_owned(),
                    source: self.current().1,
                });
            }
            let typedef = self.typedef()?;
            args.push((name.0, typedef.0));
            // break when no comma
            if !self.is(&[Token::Comma]) {
                break;
            }
        }
        if !self.is(&[Token::Arrow]) {
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
                "num" => Ok((Type::Number, current.1)),
                "str" => Ok((Type::String, current.1)),
                "bool" => Ok((Type::Bool, current.1)),
                _ => Err(SyntaxErr {
                    message: format!("type identifier exprected, found {name}"),
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
