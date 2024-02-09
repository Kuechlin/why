use std::{collections::HashMap, sync::Mutex};

use crate::types::{
    exprs::{BinaryOp, Expr, UnaryOp},
    tokens::Token,
    types::Type,
    values::Value,
    Span, Spanned, SyntaxErr,
};

type ParserResult = Result<Expr, SyntaxErr>;
type TypeResult = Result<Spanned<Type>, SyntaxErr>;

struct ParserCtx<'a> {
    tokens: &'a Vec<Spanned<Token>>,
    current: Mutex<usize>,
}

fn error<'a>(msg: &'a str, span: &Span) -> ParserResult {
    Err(SyntaxErr {
        message: msg.to_owned(),
        source: span.clone(),
    })
}

impl ParserCtx<'_> {
    fn index(&self) -> usize {
        self.current.lock().unwrap().to_owned()
    }
    fn is_end(&self) -> bool {
        *self.current.lock().unwrap() >= self.tokens.len()
    }
    fn check(&self, t: Token) -> bool {
        if self.is_end() {
            return false;
        }
        self.tokens[self.index()].0 == t
    }
    fn advance(&self) -> &Spanned<Token> {
        let value = &self.tokens[self.index()];
        *self.current.lock().unwrap() += 1;
        value
    }
    fn is(&self, tokens: &[Token]) -> bool {
        for t in tokens {
            if self.check(t.clone()) {
                *self.current.lock().unwrap() += 1;
                return true;
            }
        }
        return false;
    }
    fn previous(&self) -> Result<&Spanned<Token>, SyntaxErr> {
        if *self.current.lock().unwrap() == 0 {
            Err(SyntaxErr::new("no previous token found", &(0..1)))
        } else {
            Ok(&self.tokens[self.index() - 1])
        }
    }
    fn current(&self) -> Result<&Spanned<Token>, SyntaxErr> {
        if self.is_end() {
            Err(SyntaxErr::new(
                "unexprected end of file",
                &((self.index() - 1)..self.index()),
            ))
        } else {
            Ok(&self.tokens[self.index()])
        }
    }
    fn start(&self) -> usize {
        match self.current() {
            Ok(c) => c.1.start,
            Err(_) => self.tokens.len(),
        }
    }
    fn end(&self) -> usize {
        match self.previous() {
            Ok(c) => c.1.end,
            Err(_) => 0,
        }
    }

    // statement
    fn statement(&self) -> ParserResult {
        let stmt = self.stmt_def()?;
        let _ = self.is(&[Token::Semicolon]);
        Ok(stmt)
    }

    fn stmt_def(&self) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::Def]) {
            return self.stmt_let();
        }
        let token = self.advance();
        let name = match &token.0 {
            Token::TypeIdentifier(name) => name,
            _ => return error("Type Identifier expected", &token.1),
        };
        if !self.is(&[Token::DotDot]) {
            return error("Expect ':' after definition identifier", &self.current()?.1);
        }
        let typedef = self.typedef()?;
        Ok(Expr::Def {
            name: (name.to_owned(), token.1.to_owned()),
            typedef,
            span: start..self.end(),
        })
    }

    fn stmt_let(&self) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::Let]) {
            return self.expression();
        }
        let name = self.identifer()?;
        if !self.is(&[Token::Equal]) {
            return error("initializer expected", &name.1);
        }
        let expr = Box::new(self.statement()?);
        Ok(Expr::Let {
            name,
            expr,
            span: start..self.end(),
        })
    }

    // expressions
    fn expression(&self) -> ParserResult {
        self.expr_new()
    }

    fn expr_new(&self) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::New]) {
            return Ok(self.expr_if()?);
        }
        let token = self.current()?;
        let name = match &token.0 {
            Token::TypeIdentifier(name) => {
                _ = self.advance();
                Some((name.to_owned(), token.1.to_owned()))
            }
            _ => None,
        };
        // object
        if self.is(&[Token::LeftBrace]) {
            let mut entries = HashMap::new();
            while !self.check(Token::RightBrace) && !self.is_end() {
                // get name
                let name = self.identifer()?;
                // get type
                if !self.is(&[Token::Equal]) {
                    return Err(SyntaxErr::new(
                        "type definition expected",
                        &self.current()?.1,
                    ));
                }
                let expr = self.expression()?;
                entries.insert(name, expr);
                // break when no comma
                if !self.is(&[Token::Comma]) {
                    break;
                }
            }
            if !self.is(&[Token::RightBrace]) {
                return error("Expect '}' at end of block", &self.current()?.1);
            }
            Ok(Expr::New {
                typedef: name,
                entries,
                span: start..self.end(),
            })
        } else {
            error("invalid new expression", &self.current()?.1)
        }
    }

    fn expr_if(&self) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::If]) {
            return Ok(self.expr_fn()?);
        }

        let cond = self.expression()?;
        if !self.check(Token::LeftBrace) {
            return error("Expected block statement", &self.current()?.1);
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

    fn expr_fn(&self) -> ParserResult {
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

    fn expr_block(&self) -> ParserResult {
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
            error("Expect '}' at end of block", &self.current()?.1)
        }
    }

    fn equality(&self) -> ParserResult {
        // equality
        self.binary(&[Token::BangEqual, Token::EqualEqual], |n| {
            // comparisson
            n.binary(
                &[
                    Token::Greater,
                    Token::GreaterEqual,
                    Token::Less,
                    Token::LessEqual,
                    Token::And,
                    Token::Or,
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

    fn binary(&self, tokens: &[Token], nested: fn(&Self) -> ParserResult) -> ParserResult {
        let start = self.start();
        let mut expr = nested(self)?;

        while self.is(tokens) {
            let op = self.previous()?;
            let right = nested(self)?;
            expr = Expr::Binary {
                op: match op.0 {
                    Token::Minus => (BinaryOp::Minus, op.1.clone()),
                    Token::Plus => (BinaryOp::Plus, op.1.clone()),
                    Token::Star => (BinaryOp::Mul, op.1.clone()),
                    Token::Slash => (BinaryOp::Div, op.1.clone()),
                    Token::And => (BinaryOp::And, op.1.clone()),
                    Token::Or => (BinaryOp::Or, op.1.clone()),
                    Token::BangEqual => (BinaryOp::NotEqual, op.1.clone()),
                    Token::EqualEqual => (BinaryOp::Equal, op.1.clone()),
                    Token::Greater => (BinaryOp::Greater, op.1.clone()),
                    Token::GreaterEqual => (BinaryOp::GreaterEqual, op.1.clone()),
                    Token::Less => (BinaryOp::Less, op.1.clone()),
                    Token::LessEqual => (BinaryOp::LessEqual, op.1.clone()),
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
    fn unary(&self) -> ParserResult {
        let start = self.start();
        if self.is(&[Token::Bang, Token::Minus]) {
            let op = self.previous()?;
            let expr = self.unary()?;
            return Ok(Expr::Unary {
                op: match op.0 {
                    Token::Bang => (UnaryOp::Bang, op.1.clone()),
                    Token::Minus => (UnaryOp::Minus, op.1.clone()),
                    _ => return error("invalid unary operator", &op.1),
                },
                expr: Box::new(expr),
                span: start..self.end(),
            });
        }
        self.primary()
    }
    // int, float, string, bool, expression
    fn primary(&self) -> ParserResult {
        let current = self.advance();
        let expr = match &current.0 {
            Token::Bool(val) => Expr::Literal((Value::Bool(val.to_owned()), current.1.to_owned())),
            Token::Number(val) => {
                Expr::Literal((Value::Number(val.to_owned()), current.1.to_owned()))
            }
            Token::String(val) => {
                Expr::Literal((Value::String(val.to_owned()), current.1.to_owned()))
            }
            Token::Identifier(val) => {
                if self.check(Token::LeftParen) {
                    self.expr_call((val.to_owned(), current.1.to_owned()))?
                } else {
                    self.expr_var((val.to_owned(), current.1.to_owned()))?
                }
            }
            Token::LeftParen => {
                // group
                let expr = self.expression()?;
                if !self.is(&[Token::RightParen]) {
                    return error("Expect ')' after expression.", &self.current()?.1);
                }
                expr
            }
            _ => return error("invalid token", &current.1),
        };
        // check if is match expression
        self.expr_is(expr)
    }

    fn expr_var(&self, name: Spanned<String>) -> ParserResult {
        let mut then: Option<Box<Expr>> = None;
        if self.is(&[Token::Dot]) {
            then = Some(Box::new(self.primary()?));
        }
        let span = name.1.start..self.end();
        Ok(Expr::Var { name, then, span })
    }

    fn expr_is(&self, expr: Expr) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::Is]) {
            return Ok(expr);
        }
        if !self.is(&[Token::LeftBrace]) {
            return error("Expect '{' after is keyword", &self.current()?.1);
        }
        let mut default = Expr::Literal((Value::Void, 0..0));
        let mut cases = Vec::new();
        while !self.check(Token::RightBrace) || !self.is_end() {
            let current = self.advance();
            let (op, cond) = match &current.0 {
                Token::Comma => continue,
                Token::RightBrace => {
                    *self.current.lock().unwrap() -= 1;
                    break;
                }
                // match equal
                Token::String(val) => (
                    BinaryOp::Equal,
                    Expr::Literal((Value::String(val.to_owned()), current.1.to_owned())),
                ),
                Token::Number(val) => (
                    BinaryOp::Equal,
                    Expr::Literal((Value::Number(val.to_owned()), current.1.to_owned())),
                ),
                Token::Bool(val) => (
                    BinaryOp::Equal,
                    Expr::Literal((Value::Bool(val.to_owned()), current.1.to_owned())),
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
                        return error("expected '->' after match condition", &self.current()?.1);
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
                return error("expected '->' after match condition", &self.current()?.1);
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
            return error("Expect '}' at end of match", &self.current()?.1);
        }
        Ok(Expr::Is {
            expr: Box::new(expr),
            cases,
            default: Box::new(default),
            span: start..self.end(),
        })
    }

    fn expr_call(&self, name: Spanned<String>) -> ParserResult {
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
            return error("Expect ')' at end of args", &self.current()?.1);
        }
        let span = name.1.start..self.end();
        Ok(Expr::Call { name, args, span })
    }

    fn identifer(&self) -> Result<Spanned<String>, SyntaxErr> {
        let token = self.advance();
        match &token.0 {
            Token::Identifier(name) => Ok((name.to_owned(), token.1.to_owned())),
            _ => Err(SyntaxErr::new("Identifier expected", &token.1)),
        }
    }

    // types
    fn typedef(&self) -> TypeResult {
        if !self.is(&[Token::LeftParen]) {
            return self.type_obj();
        }
        let t = self.type_fn()?;
        if self.is(&[Token::RightParen]) {
            Ok(t)
        } else {
            Err(SyntaxErr {
                message: "Expected ')' after type group".to_owned(),
                source: self.current()?.1.clone(),
            })
        }
    }

    fn type_obj(&self) -> TypeResult {
        let start = self.start();
        if !self.is(&[Token::LeftBrace]) {
            return self.type_value();
        }
        let mut entries = HashMap::new();
        while !self.check(Token::RightBrace) && !self.is_end() {
            // get name
            let name = self.identifer()?;
            // get type
            if !self.is(&[Token::DotDot]) {
                return Err(SyntaxErr::new(
                    "type definition expected",
                    &self.current()?.1,
                ));
            }
            let typedef = self.typedef()?;
            entries.insert(name.0, typedef.0);
            // break when no comma
            if !self.is(&[Token::Comma]) {
                break;
            }
        }
        if !self.is(&[Token::RightBrace]) {
            return Err(SyntaxErr::new(
                "Expect '}' at end of obj",
                &self.current()?.1,
            ));
        }
        Ok((Type::Obj(entries), start..self.end()))
    }

    fn type_fn(&self) -> TypeResult {
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
                return Err(SyntaxErr::new(
                    "type definition expected",
                    &self.current()?.1,
                ));
            }
            let typedef = self.typedef()?;
            args.push((name.0, typedef.0));
            // break when no comma
            if !self.is(&[Token::Comma]) {
                break;
            }
        }
        if !self.is(&[Token::Arrow]) {
            return Err(SyntaxErr::new(
                "Expect '->' at end of args to define return value",
                &self.current()?.1,
            ));
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

    fn type_or(&self) -> TypeResult {
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

    fn type_value(&self) -> TypeResult {
        let current = self.advance();
        Ok((
            match &current.0 {
                Token::TypeIdentifier(name) => match name.as_str() {
                    "Num" => Type::Number,
                    "Str" => Type::String,
                    "Bool" => Type::Bool,
                    _ => Type::Def(name.to_owned()),
                },
                _ => return Err(SyntaxErr::new("type identifier exprected", &current.1)),
            },
            current.1.clone(),
        ))
    }
}

pub fn parse(tokens: &Vec<Spanned<Token>>) -> Result<Vec<Expr>, SyntaxErr> {
    let ctx = ParserCtx {
        tokens,
        current: Mutex::new(0),
    };

    let mut nodes = Vec::new();
    while !ctx.is_end() {
        nodes.push(ctx.statement()?)
    }
    Ok(nodes)
}
