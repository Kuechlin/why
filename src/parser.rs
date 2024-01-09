use crate::types::{
    exprs::{BinaryOp, Expr, Type, UnaryOp},
    tokens::Token,
    values::Value,
    Span, Spanned, SyntaxErr,
};

type ParserResult = Result<Expr, SyntaxErr>;
type TypeResult = Result<Spanned<Type>, SyntaxErr>;

struct ParserCtx<'a> {
    tokens: &'a Vec<Spanned<Token>>,
    current: usize,
}

fn err(msg: &'static str, span: &Span) -> SyntaxErr {
    SyntaxErr {
        message: msg.to_owned(),
        source: span.clone(),
    }
}
fn error<'a>(msg: &'a str, span: &Span) -> ParserResult {
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
        if self.is_end() {
            (Token::Eof, (self.current - 1)..self.current)
        } else {
            self.tokens[self.current].clone()
        }
    }
    fn start(&self) -> usize {
        self.current().1.start
    }
    fn end(&self) -> usize {
        self.previous().1.end
    }

    // statement
    fn statement(&mut self) -> ParserResult {
        self.stmt_def()
    }

    fn stmt_def(&mut self) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::Def]) {
            return self.stmt_let();
        }
        let token = self.advance();
        let name = match token.0 {
            Token::TypeIdentifier(name) => name,
            _ => return error("Type Identifier expected", &token.1),
        };
        if !self.is(&[Token::DotDot]) {
            return error("Expect ':' after definition identifier", &self.current().1);
        }
        let typedef = self.typedef()?;
        if !self.is(&[Token::Semicolon]) {
            return error("Expect ';' after statement", &self.current().1);
        }
        Ok(Expr::Def {
            name: (name, token.1),
            typedef,
            span: start..self.end(),
        })
    }

    fn stmt_let(&mut self) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::Let]) {
            return self.expression();
        }
        let name = self.identifer()?;
        if !self.is(&[Token::Equal]) {
            return error("initializer expected", &name.1);
        }
        let expr = Box::new(self.statement()?);
        // check end
        if !self.is(&[Token::Semicolon]) {
            return error("Expect ';' after statement", &self.current().1);
        }
        Ok(Expr::Let {
            name,
            expr,
            span: start..self.end(),
        })
    }

    // expressions
    fn expression(&mut self) -> ParserResult {
        self.expr_if()
    }

    fn expr_if(&mut self) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::If]) {
            return Ok(self.expr_fn()?);
        }

        let cond = self.expression()?;
        if !self.check(Token::LeftBrace) {
            return error("Expected block statement", &self.current().1);
        }
        let then = self.expr_block()?;
        let or = match self.is(&[Token::Else]) {
            true => Some(Box::new(self.statement()?)),
            false => None,
        };

        return Ok(Expr::If {
            cond: Box::new(cond),
            then: Box::new(then),
            or,
            span: start..self.end(),
        });
    }

    fn expr_fn(&mut self) -> ParserResult {
        let start = self.start();
        if !self.check(Token::Fn) {
            return Ok(self.expr_block()?);
        }
        let typedef = self.type_fn()?;
        let block = self.expr_block()?;

        Ok(Expr::Fn {
            typedef,
            block: Box::new(block),
            span: start..self.end(),
        })
    }

    fn expr_block(&mut self) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::LeftBrace]) {
            return Ok(self.equality()?);
        }

        let mut nodes = Vec::new();
        while !self.check(Token::RightBrace) && !self.is_end() {
            nodes.push(self.statement()?)
        }
        if self.is(&[Token::RightBrace]) {
            Ok(Expr::Block {
                stmts: nodes,
                span: start..self.end(),
            })
        } else {
            error("Expect '}' at end of block", &self.current().1)
        }
    }

    fn equality(&mut self) -> ParserResult {
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
            expr = Expr::Binary {
                op: match op.0 {
                    Token::Minus => (BinaryOp::Minus, op.1),
                    Token::Plus => (BinaryOp::Plus, op.1),
                    Token::Star => (BinaryOp::Mul, op.1),
                    Token::Slash => (BinaryOp::Div, op.1),
                    Token::BangEqual => (BinaryOp::NotEqual, op.1),
                    Token::EqualEqual => (BinaryOp::Equal, op.1),
                    Token::Greater => (BinaryOp::Greater, op.1),
                    Token::GreaterEqual => (BinaryOp::GreaterEqual, op.1),
                    Token::Less => (BinaryOp::Less, op.1),
                    Token::LessEqual => (BinaryOp::LessEqual, op.1),
                    _ => return error("invalid binary operator", &op.1),
                },
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
            return Ok(Expr::Unary {
                op: match op.0 {
                    Token::Bang => (UnaryOp::Bang, op.1),
                    Token::Minus => (UnaryOp::Minus, op.1),
                    _ => return error("invalid unary operator", &op.1),
                },
                expr: Box::new(expr),
                span: start..self.end(),
            });
        }
        self.primary()
    }
    // int, float, string, bool, expression
    fn primary(&mut self) -> ParserResult {
        let current = self.advance();
        let expr = match current.0 {
            Token::Bool(val) => Expr::Literal((Value::Bool(val), current.1)),
            Token::Number(val) => Expr::Literal((Value::Number(val), current.1)),
            Token::String(val) => Expr::Literal((Value::String(val), current.1)),
            Token::Identifier(val) => {
                if self.check(Token::LeftParen) {
                    self.expr_call((val, current.1))?
                } else {
                    Expr::Var((val, current.1))
                }
            }
            Token::LeftParen => {
                // group
                let expr = self.expression()?;
                if !self.is(&[Token::RightParen]) {
                    return error("Expect ')' after expression.", &self.current().1);
                }
                expr
            }
            _ => return error("invalid token", &current.1),
        };
        // check if is match expression
        self.expr_is(expr)
    }

    fn expr_is(&mut self, expr: Expr) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::Is]) {
            return Ok(expr);
        }
        if !self.is(&[Token::LeftBrace]) {
            return error("Expect '{' after is keyword", &self.current().1);
        }
        let mut default = Expr::Literal((Value::Void, 0..0));
        let mut cases = Vec::new();
        while !self.check(Token::RightBrace) || !self.is_end() {
            let current = self.advance();
            let (op, cond) = match current.0 {
                Token::Comma => continue,
                Token::RightBrace => {
                    self.current -= 1;
                    break;
                }
                // match equal
                Token::String(val) => (
                    BinaryOp::Equal,
                    Expr::Literal((Value::String(val), current.1.clone())),
                ),
                Token::Number(val) => (
                    BinaryOp::Equal,
                    Expr::Literal((Value::Number(val), current.1.clone())),
                ),
                Token::Bool(val) => (
                    BinaryOp::Equal,
                    Expr::Literal((Value::Bool(val), current.1.clone())),
                ),
                // match compare
                Token::Bang => (BinaryOp::NotEqual, self.expression()?),
                Token::GreaterEqual => (BinaryOp::GreaterEqual, self.expression()?),
                Token::Greater => (BinaryOp::Greater, self.expression()?),
                Token::LessEqual => (BinaryOp::LessEqual, self.expression()?),
                Token::Less => (BinaryOp::Less, self.expression()?),
                // match type
                Token::DotDot => {
                    let typedef = self.typedef()?;

                    if !self.is(&[Token::Arrow]) {
                        return error("expected '->' after match condition", &self.current().1);
                    }
                    let then = self.expr_block()?;
                    cases.push(Expr::MatchType {
                        typedef,
                        then: Box::new(then),
                        span: current.1.start..self.end(),
                    });
                    continue;
                }
                // match default
                Token::Arrow => {
                    default = self.expr_block()?;
                    continue;
                }
                _ => {
                    return error(
                        format!("match case expected, found {}", current.0).as_str(),
                        &current.1,
                    )
                }
            };
            // build match condition
            if !self.is(&[Token::Arrow]) {
                return error("expected '->' after match condition", &self.current().1);
            }

            let then = self.expr_block()?;

            cases.push(Expr::Match {
                op: (op, current.1.clone()),
                expr: Box::new(cond),
                then: Box::new(then),
                span: current.1.start..self.end(),
            });
        }
        if !self.is(&[Token::RightBrace]) {
            return error("Expect '}' at end of match", &self.current().1);
        }
        Ok(Expr::Is {
            expr: Box::new(expr),
            cases,
            default: Box::new(default),
            span: start..self.end(),
        })
    }

    fn expr_call(&mut self, name: Spanned<String>) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::LeftParen]) {
            return error("Expect '(' after identifer", &name.1);
        }
        let mut args = Vec::new();
        // fn args
        while !self.check(Token::RightParen) && !self.is_end() {
            // get expr
            let expr = match self.statement()? {
                Expr::Block { stmts: nodes, span } => Expr::Fn {
                    typedef: (Type::Void, 0..0),
                    block: Box::new(Expr::Block {
                        stmts: nodes,
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
        Ok(Expr::Call {
            name,
            args,
            span: start..self.end(),
        })
    }

    fn identifer(&mut self) -> Result<Spanned<String>, SyntaxErr> {
        let token = self.advance();
        match token.0 {
            Token::Identifier(name) => Ok((name, token.1)),
            _ => Err(err("Identifier expected", &token.1)),
        }
    }

    // types
    fn typedef(&mut self) -> TypeResult {
        if !self.is(&[Token::LeftParen]) {
            return self.type_value();
        }
        let t = self.type_fn()?;
        if self.is(&[Token::RightParen]) {
            Ok(t)
        } else {
            Err(SyntaxErr {
                message: "Expected ')' after type group".to_owned(),
                source: self.current().1.clone(),
            })
        }
    }

    fn type_fn(&mut self) -> TypeResult {
        let start = self.start();
        if !self.is(&[Token::Fn]) {
            return self.type_or();
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

    fn type_or(&mut self) -> TypeResult {
        let start = self.start();
        let mut types = Vec::new();
        while !self.check(Token::RightParen) && !self.is_end() {
            // get type
            types.push(self.typedef()?.0);
            // break when no comma
            if !self.is(&[Token::Or]) {
                break;
            }
        }
        let result = match types.len() {
            1 => types.get(0).unwrap().clone(),
            _ => Type::Or(types),
        };
        Ok((result, start..self.end()))
    }

    fn type_value(&mut self) -> TypeResult {
        let current = self.advance();
        Ok((
            match current.0 {
                Token::TypeIdentifier(name) => match name.as_str() {
                    "Num" => Type::Number,
                    "Str" => Type::String,
                    "Bool" => Type::Bool,
                    _ => Type::Def(name),
                },
                _ => return Err(err("type identifier exprected", &current.1)),
            },
            current.1.clone(),
        ))
    }
}

pub fn parse(tokens: &Vec<Spanned<Token>>) -> Result<Vec<Expr>, SyntaxErr> {
    let mut ctx = ParserCtx { tokens, current: 0 };

    let mut nodes = Vec::new();
    while !ctx.is_end() {
        nodes.push(ctx.statement()?)
    }
    Ok(nodes)
}
