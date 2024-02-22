use std::{collections::HashMap, rc::Rc, sync::Mutex};

use crate::types::{
    ast::{
        BinaryEx, BinaryOp, BlockEx, CallEx, DefEx, Expr, FnEx, IfEx, IsEx, LetEx, MatchCase,
        MatchType, MatchValue, ObjEx, PropEx, UnaryEx, UnaryOp, ValEx,
    },
    tokens::Token,
    types::{FnType, ObjType, OrType, Type},
    values::Value,
    Span, Spanned, SyntaxErr,
};

type ParserResult = Result<Expr, SyntaxErr>;
type TypeResult = Result<Rc<Type>, SyntaxErr>;

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
    fn advance(&self) -> Result<&Spanned<Token>, SyntaxErr> {
        if self.is_end() {
            let i = self.end();
            return Err(SyntaxErr::new("unexprected end of file", &(i - 1..i)));
        }
        let value = &self.tokens[self.index()];
        *self.current.lock().unwrap() += 1;
        Ok(value)
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
        let token = self.advance()?;
        let name = match &token.0 {
            Token::TypeIdentifier(name) => name,
            _ => return error("Type Identifier expected", &token.1),
        };
        if !self.is(&[Token::DotDot]) {
            return error("Expect ':' after definition identifier", &self.current()?.1);
        }
        let typedef = self.typedef()?;
        Ok(Expr::Def(DefEx {
            name: (name.to_owned(), token.1.to_owned()),
            ty: typedef,
            span: start..self.end(),
        }))
    }

    fn stmt_let(&self) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::Let]) {
            return self.stmt_block();
        }
        let name = self.identifer()?;
        if !self.is(&[Token::Equal]) {
            return error("initializer expected", &name.1);
        }
        let expr = Box::new(self.expression()?);
        Ok(Expr::Let(LetEx {
            name,
            expr,
            span: start..self.end(),
        }))
    }

    fn stmt_block(&self) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::LeftBrace]) {
            return Ok(self.expression()?);
        }

        let mut nodes = Vec::new();
        while !self.check(Token::RightBrace) && !self.is_end() {
            nodes.push(self.statement()?)
        }
        if self.is(&[Token::RightBrace]) {
            Ok(Expr::Block(BlockEx {
                stmts: nodes,
                span: start..self.end(),
            }))
        } else {
            error("Expect '}' at end of block", &self.current()?.1)
        }
    }

    // expressions
    fn expression(&self) -> ParserResult {
        self.expr_if()
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
        let then = self.stmt_block()?;
        let or = match self.is(&[Token::Else]) {
            true => Some(Box::new(self.statement()?)),
            false => None,
        };

        Ok(Expr::If(IfEx {
            cond: Box::new(cond),
            then: Box::new(then),
            or,
            span: start..self.end(),
        }))
    }

    fn expr_fn(&self) -> ParserResult {
        let start = self.start();
        if !self.check(Token::Fn) {
            return Ok(self.equality()?);
        }
        let ty = self.type_fn()?;
        let block = self.stmt_block()?;

        Ok(Expr::Fn(FnEx {
            ty,
            block: Box::new(block),
            span: start..self.end(),
        }))
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
            expr = Expr::Binary(BinaryEx {
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
            });
        }
        Ok(expr)
    }

    // (! | -) unary | primary
    fn unary(&self) -> ParserResult {
        let start = self.start();
        if self.is(&[Token::Bang, Token::Minus]) {
            let op = self.previous()?;
            let expr = self.unary()?;
            return Ok(Expr::Unary(UnaryEx {
                op: match op.0 {
                    Token::Bang => (UnaryOp::Bang, op.1.clone()),
                    Token::Minus => (UnaryOp::Minus, op.1.clone()),
                    _ => return error("invalid unary operator", &op.1),
                },
                expr: Box::new(expr),
                span: start..self.end(),
            }));
        }
        self.primary()
    }
    // int, float, string, bool, expression
    fn primary(&self) -> ParserResult {
        let current = self.advance()?;
        let expr = match &current.0 {
            Token::Bool(val) => Expr::Val(ValEx {
                value: Rc::new(Value::Bool(val.to_owned())),
                span: current.1.to_owned(),
            }),
            Token::Number(val) => Expr::Val(ValEx {
                value: Rc::new(Value::Number(val.to_owned())),
                span: current.1.to_owned(),
            }),
            Token::String(val) => Expr::Val(ValEx {
                value: Rc::new(Value::String(val.to_owned())),
                span: current.1.to_owned(),
            }),
            Token::Identifier(val) => {
                if self.check(Token::LeftParen) {
                    self.expr_call((val.to_owned(), current.1.to_owned()))?
                } else {
                    self.expr_prop((val.to_owned(), current.1.to_owned()))?
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
            Token::LeftBrace => {
                return self.expr_obj();
            }
            _ => return error(format!("invalid token {}", current.0).as_str(), &current.1),
        };
        // check if is match expression
        self.expr_is(expr)
    }

    fn expr_obj(&self) -> ParserResult {
        let start = self.start();
        // object
        let mut entries = HashMap::new();
        while !self.check(Token::RightBrace) && !self.is_end() {
            // get name
            let name = self.identifer()?;
            // get value
            if !self.is(&[Token::Equal]) {
                return Err(SyntaxErr::new(
                    "expression definition expected",
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
        Ok(Expr::Obj(ObjEx {
            entries,
            span: start..self.end(),
        }))
    }

    fn expr_prop(&self, name: Spanned<String>) -> ParserResult {
        let mut then: Option<Box<Expr>> = None;
        if self.is(&[Token::Dot]) {
            then = Some(Box::new(self.primary()?));
        }
        let span = name.1.start..self.end();
        Ok(Expr::Prop(PropEx { name, then, span }))
    }

    fn expr_is(&self, expr: Expr) -> ParserResult {
        let start = self.start();
        if !self.is(&[Token::Is]) {
            return Ok(expr);
        }
        if !self.is(&[Token::LeftBrace]) {
            return error("Expect '{' after is keyword", &self.current()?.1);
        }
        let mut default = Expr::Val(ValEx {
            value: Rc::new(Value::Void),
            span: Span::default(),
        });
        let mut cases = Vec::new();
        while !self.check(Token::RightBrace) || !self.is_end() {
            let current = self.advance()?;
            let (op, cond) = match &current.0 {
                Token::Comma => continue,
                Token::RightBrace => {
                    *self.current.lock().unwrap() -= 1;
                    break;
                }
                // match equal
                Token::String(val) => (
                    BinaryOp::Equal,
                    Expr::Val(ValEx {
                        value: Rc::new(Value::String(val.to_owned())),
                        span: current.1.to_owned(),
                    }),
                ),
                Token::Number(val) => (
                    BinaryOp::Equal,
                    Expr::Val(ValEx {
                        value: Rc::new(Value::Number(val.to_owned())),
                        span: current.1.to_owned(),
                    }),
                ),
                Token::Bool(val) => (
                    BinaryOp::Equal,
                    Expr::Val(ValEx {
                        value: Rc::new(Value::Bool(val.to_owned())),
                        span: current.1.to_owned(),
                    }),
                ),
                // match compare
                Token::Bang => (BinaryOp::NotEqual, self.stmt_block()?),
                Token::GreaterEqual => (BinaryOp::GreaterEqual, self.stmt_block()?),
                Token::Greater => (BinaryOp::Greater, self.stmt_block()?),
                Token::LessEqual => (BinaryOp::LessEqual, self.stmt_block()?),
                Token::Less => (BinaryOp::Less, self.stmt_block()?),
                // match type
                Token::DotDot => {
                    let ty = self.typedef()?;

                    if !self.is(&[Token::Arrow]) {
                        return error("expected '->' after match condition", &self.current()?.1);
                    }
                    let then = self.stmt_block()?;
                    cases.push(MatchCase::Type(MatchType {
                        ty,
                        then: Box::new(then),
                        span: current.1.start..self.end(),
                    }));
                    continue;
                }
                // match default
                Token::Arrow => {
                    default = self.stmt_block()?;
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

            let then = self.stmt_block()?;

            cases.push(MatchCase::Value(MatchValue {
                op: (op, current.1.clone()),
                expr: Box::new(cond),
                then: Box::new(then),
                span: current.1.start..self.end(),
            }));
        }
        if !self.is(&[Token::RightBrace]) {
            return error("Expect '}' at end of match", &self.current()?.1);
        }
        Ok(Expr::Is(IsEx {
            expr: Box::new(expr),
            cases,
            default: Box::new(default),
            span: start..self.end(),
        }))
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
                Expr::Block(b) => Expr::Fn(FnEx {
                    ty: Rc::new(Type::Void),
                    block: Box::new(Expr::Block(BlockEx {
                        stmts: b.stmts,
                        span: b.span.clone(),
                    })),
                    span: b.span.clone(),
                }),
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
        Ok(Expr::Call(CallEx { name, args, span }))
    }

    fn identifer(&self) -> Result<Spanned<String>, SyntaxErr> {
        let token = self.advance()?;
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
            entries.insert(name, typedef);
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
        Ok(Rc::new(Type::Obj(ObjType {
            entries,
            span: start..self.end(),
        })))
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
            args.push((name, typedef));
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
        Ok(Rc::new(Type::Fn(FnType {
            args,
            returns,
            span: start..self.end(),
        })))
    }

    fn type_or(&self) -> TypeResult {
        let start = self.start();
        let mut types = Vec::new();
        while !self.check(Token::RightParen) && !self.is_end() {
            // get type
            types.push(self.typedef()?);
            // break when no comma
            if !self.is(&[Token::Or]) {
                break;
            }
        }
        let result = match types.len() {
            1 => types.get(0).unwrap().clone(),
            _ => Rc::new(Type::Or(OrType {
                types,
                span: start..self.end(),
            })),
        };
        Ok(result)
    }

    fn type_value(&self) -> TypeResult {
        let current = self.advance()?;
        Ok(match &current.0 {
            Token::TypeIdentifier(name) => match name.as_str() {
                "Num" => Rc::new(Type::Number(current.1.clone())),
                "Str" => Rc::new(Type::String(current.1.clone())),
                "Bool" => Rc::new(Type::Bool(current.1.clone())),
                _ => Rc::new(Type::Def((name.to_owned(), current.1.clone()))),
            },
            _ => return Err(SyntaxErr::new("type identifier exprected", &current.1)),
        })
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
