use std::collections::HashMap;

use crate::types::{Expr, Node, SyntaxErr, Token, TokenData, Type, Value};

type AnalyserResult = Result<Box<Expr>, SyntaxErr>;

struct AnalyserCtx<'a> {
    pub enclosing: Option<&'a AnalyserCtx<'a>>,
    pub state: HashMap<String, Type>,
}

fn error(msg: &'static str, token: &TokenData) -> AnalyserResult {
    Err(SyntaxErr {
        message: msg.to_owned(),
        source: token.source,
    })
}
fn ok(expr: Expr) -> AnalyserResult {
    Ok(Box::new(expr))
}

impl AnalyserCtx<'_> {
    fn derive(&self) -> AnalyserCtx {
        AnalyserCtx {
            enclosing: Some(self),
            state: HashMap::new(),
        }
    }
    fn get(&self, key: &str) -> Option<Type> {
        if self.state.contains_key(key) {
            return Some(*self.state.get(key).unwrap());
        }
        if self.enclosing.is_some() {
            return self.enclosing.unwrap().get(key);
        }
        return None;
    }
    fn set(&mut self, key: &str, val: Type) -> Result<(), &'static str> {
        if self.state.contains_key(key) {
            Err("can't reassign a value to immutable variable")
        } else {
            let _ = self.state.insert(key.to_string(), val);
            Ok(())
        }
    }

    // statements
    fn visit(&mut self, node: &Box<Node>) -> AnalyserResult {
        match node.as_ref() {
            Node::Let { name, node } => self.visit_let(name, node),
            Node::Block(nodes) => self.visit_block(nodes),
            _ => Ok(self.visit_expr(node)?),
        }
    }

    fn visit_block(&mut self, nodes: &Vec<Box<Node>>) -> AnalyserResult {
        let mut ctx = self.derive();
        let mut stmts = Vec::new();
        for node in nodes {
            stmts.push(ctx.visit(node)?);
        }
        Ok(Box::new(Expr::Block { stmts }))
    }

    fn visit_let(&mut self, name: &TokenData, node: &Box<Node>) -> AnalyserResult {
        let key = match &name.token {
            Token::Identifier(x) => x.to_owned(),
            _ => return error("Identifier expected.", name),
        };
        let expr = self.visit_expr(node)?;
        match self.set(&key, expr.get_type()) {
            Ok(_) => Ok(Box::new(Expr::Let { name: key, expr })),
            Err(err) => error(err, name),
        }
    }

    // expression
    fn visit_expr(&self, node: &Box<Node>) -> AnalyserResult {
        match node.as_ref() {
            Node::Literal(token) => self.visit_literal(token),
            Node::Unary { op, node } => self.visit_unary(op, node),
            Node::Binary { op, left, right } => self.visit_binary(op, left, right),
            _ => panic!("invalid expression"),
        }
    }

    fn visit_unary(&self, op: &TokenData, node: &Box<Node>) -> AnalyserResult {
        let expr = self.visit_expr(node)?;

        match op.token {
            Token::Bang => Ok(Box::new(Expr::Unary {
                op: op.token.to_owned(),
                expr,
                typedef: Type::Bool,
            })),
            Token::Minus => match expr.get_type() {
                Type::Number => Ok(Box::new(Expr::Unary {
                    op: op.token.to_owned(),
                    expr,
                    typedef: Type::Number,
                })),
                _ => Err(SyntaxErr {
                    message: "minus can only be applyed to numbers".to_owned(),
                    source: op.source,
                }),
            },
            _ => Err(SyntaxErr {
                message: "invalid operator".to_owned(),
                source: op.source,
            }),
        }
    }

    fn visit_binary(&self, op: &TokenData, left: &Box<Node>, right: &Box<Node>) -> AnalyserResult {
        let left_expr = self.visit_expr(left)?;
        let right_expr = self.visit_expr(right)?;

        fn check_math(op: &TokenData, l: Box<Expr>, r: Box<Expr>) -> AnalyserResult {
            if l.get_type() != Type::Number || r.get_type() != Type::Number {
                error("operator can only be used for numbers", op)
            } else {
                ok(Expr::Binary {
                    op: op.token.to_owned(),
                    left: l,
                    right: r,
                    typedef: Type::Number,
                })
            }
        }
        fn check_compare(op: &TokenData, l: Box<Expr>, r: Box<Expr>) -> AnalyserResult {
            if l.get_type() != r.get_type() {
                error("can only compare same types", op)
            } else {
                ok(Expr::Binary {
                    op: op.token.to_owned(),
                    left: l,
                    right: r,
                    typedef: Type::Bool,
                })
            }
        }
        match op.token {
            Token::Plus => match left_expr.get_type() {
                Type::String => ok(Expr::Binary {
                    op: op.token.to_owned(),
                    left: left_expr,
                    right: right_expr,
                    typedef: Type::String,
                }),
                _ => check_math(op, left_expr, right_expr),
            },
            Token::Minus => check_math(op, left_expr, right_expr),
            Token::Star => check_math(op, left_expr, right_expr),
            Token::Slash => check_math(op, left_expr, right_expr),
            Token::BangEqual => check_compare(op, left_expr, right_expr),
            Token::EqualEqual => check_compare(op, left_expr, right_expr),
            Token::Greater => check_compare(op, left_expr, right_expr),
            Token::GreaterEqual => check_compare(op, left_expr, right_expr),
            Token::Less => check_compare(op, left_expr, right_expr),
            Token::LessEqual => check_compare(op, left_expr, right_expr),
            _ => error("invalid operator", op),
        }
    }

    fn visit_literal(&self, token: &TokenData) -> AnalyserResult {
        match &token.token {
            Token::Identifier(name) => ok(Expr::Var {
                name: name.to_owned(),
                typedef: match self.get(&name) {
                    Some(t) => t,
                    None => return error("variable not defined", token),
                },
            }),
            Token::String(val) => ok(Expr::Literal(Value::String(val.to_owned()))),
            Token::Number(val) => ok(Expr::Literal(Value::Number(val.to_owned()))),
            Token::Bool(val) => ok(Expr::Literal(Value::Bool(val.to_owned()))),
            _ => error("invalid literal", token),
        }
    }
}

pub fn analyse(node: &Box<Node>) -> AnalyserResult {
    let mut ctx = AnalyserCtx {
        enclosing: None,
        state: HashMap::new(),
    };

    ctx.visit(node)
}
