use std::collections::HashMap;

use crate::types::{BinaryOp, Expr, Node, Span, Spanned, SyntaxErr, Token, Type, UnaryOp};

type AnalyserResult = Result<Expr, SyntaxErr>;

pub struct AnalyserCtx<'a> {
    pub enclosing: Option<&'a AnalyserCtx<'a>>,
    pub state: HashMap<String, Type>,
}

fn error(msg: &str, span: &Span) -> AnalyserResult {
    Err(SyntaxErr {
        message: msg.to_owned(),
        source: span.clone(),
    })
}

impl AnalyserCtx<'_> {
    pub fn new<'a>() -> AnalyserCtx<'a> {
        AnalyserCtx {
            enclosing: None,
            state: HashMap::new(),
        }
    }
    pub fn analyse(&mut self, nodes: &[Node]) -> Result<Vec<Expr>, SyntaxErr> {
        let mut stmts = Vec::new();
        for node in nodes {
            stmts.push(self.visit(node)?);
        }
        Ok(stmts)
    }

    fn derive(&self) -> AnalyserCtx {
        AnalyserCtx {
            enclosing: Some(self),
            state: HashMap::new(),
        }
    }
    fn get(&self, key: &str) -> Option<&Type> {
        if self.state.contains_key(key) {
            return Some(self.state.get(key).unwrap());
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
    fn visit(&mut self, node: &Node) -> AnalyserResult {
        match node {
            Node::Block { nodes, span: _ } => self.visit_block(nodes),
            Node::Let {
                name,
                expr,
                span: _,
            } => self.visit_let(name, expr),
            Node::If {
                cond,
                then,
                or,
                span: _,
            } => self.visit_if(cond, then, or),
            Node::Fn {
                typedef,
                block,
                span: _,
            } => self.visit_fn(typedef, block),
            _ => Ok(self.visit_expr(node)?),
        }
    }

    fn visit_block(&mut self, nodes: &Vec<Node>) -> AnalyserResult {
        let mut ctx = self.derive();
        let mut stmts = Vec::new();
        for node in nodes {
            stmts.push(ctx.visit(node)?);
        }
        Ok(Expr::Block { stmts })
    }

    fn visit_let(&mut self, name: &Spanned<String>, node: &Box<Node>) -> AnalyserResult {
        let expr = self.visit(node)?;
        let typedef = expr.get_return();

        match self.set(&name.0, typedef) {
            Ok(_) => Ok(Expr::Let {
                name: name.0.to_owned(),
                expr: Box::new(expr),
            }),
            Err(err) => error(err, &name.1),
        }
    }

    fn visit_if(
        &mut self,
        cond: &Box<Node>,
        then: &Box<Node>,
        or: &Option<Box<Node>>,
    ) -> AnalyserResult {
        let cond_expr = self.visit_expr(cond)?;
        let then_expr = self.visit(then)?;
        let typedef = then_expr.get_return();

        match or {
            Some(node) => {
                let expr = self.visit(node)?;
                if expr.get_return() != typedef {
                    error(
                        "if and else arms need to return the same value",
                        node.get_span(),
                    )
                } else {
                    Ok(Expr::If {
                        cond: Box::new(cond_expr),
                        then: Box::new(then_expr),
                        or: Some(Box::new(expr)),
                        typedef,
                    })
                }
            }
            None => Ok(Expr::If {
                cond: Box::new(cond_expr),
                then: Box::new(then_expr),
                or: None,
                typedef,
            }),
        }
    }

    fn visit_fn(&mut self, typedef: &Spanned<Type>, block: &Box<Node>) -> AnalyserResult {
        let (args, returns) = match &typedef.0 {
            Type::Fn { args, returns } => (args, returns),
            _ => return error("invalid function type", &typedef.1),
        };

        // create function scope
        let mut ctx = AnalyserCtx::new();
        for arg in args {
            ctx.set(&arg.0, arg.1.clone()).or(Err(SyntaxErr {
                message: "argument already exists".to_owned(),
                source: typedef.1.clone(),
            }))?;
        }
        // validate expression
        let expr = ctx.visit(&block)?;
        if &expr.get_return() != returns.as_ref() {
            return error("function block has invalid return type", block.get_span());
        }

        Ok(Expr::Fn {
            block: Box::new(expr),
            typedef: typedef.0.clone(),
        })
    }

    // expression
    fn visit_expr(&mut self, node: &Node) -> AnalyserResult {
        match node {
            Node::Literal(val) => Ok(Expr::Literal(val.0.clone())),
            Node::Identifier(name) => self.visit_identifier(name),
            Node::Call {
                name,
                args,
                span: _,
            } => self.visit_call(name, args),
            Node::Unary { op, node, span: _ } => self.visit_unary(op, node),
            Node::Binary {
                op,
                left,
                right,
                span: _,
            } => self.visit_binary(op, left, right),
            _ => panic!("invalid expression"),
        }
    }

    fn visit_unary(&mut self, op: &Spanned<Token>, node: &Box<Node>) -> AnalyserResult {
        let expr = self.visit_expr(node)?;

        match op.0 {
            Token::Bang => Ok(Expr::Unary {
                op: UnaryOp::Bang,
                expr: Box::new(expr),
                typedef: Type::Bool,
            }),
            Token::Minus => match expr.get_return() {
                Type::Number => Ok(Expr::Unary {
                    op: UnaryOp::Mins,
                    expr: Box::new(expr),
                    typedef: Type::Number,
                }),
                _ => Err(SyntaxErr {
                    message: "minus can only be applyed to numbers".to_owned(),
                    source: op.1.clone(),
                }),
            },
            _ => Err(SyntaxErr {
                message: "invalid operator".to_owned(),
                source: op.1.clone(),
            }),
        }
    }

    fn visit_binary(
        &mut self,
        op: &Spanned<Token>,
        left: &Box<Node>,
        right: &Box<Node>,
    ) -> AnalyserResult {
        let left_expr = self.visit_expr(left)?;
        let right_expr = self.visit_expr(right)?;

        fn check_math(op: BinaryOp, span: &Span, l: Expr, r: Expr) -> AnalyserResult {
            if l.get_return() != Type::Number || r.get_return() != Type::Number {
                error("operator can only be used for numbers", span)
            } else {
                Ok(Expr::Binary {
                    op,
                    left: Box::new(l),
                    right: Box::new(r),
                    typedef: Type::Number,
                })
            }
        }
        fn check_compare(op: BinaryOp, span: &Span, l: Expr, r: Expr) -> AnalyserResult {
            if l.get_return() != r.get_return() {
                error("can only compare same types", span)
            } else {
                Ok(Expr::Binary {
                    op,
                    left: Box::new(l),
                    right: Box::new(r),
                    typedef: Type::Bool,
                })
            }
        }
        match op.0 {
            Token::Plus => match left_expr.get_return() {
                Type::String => Ok(Expr::Binary {
                    op: BinaryOp::Plus,
                    left: Box::new(left_expr),
                    right: Box::new(right_expr),
                    typedef: Type::String,
                }),
                _ => check_math(BinaryOp::Plus, &op.1, left_expr, right_expr),
            },
            Token::Minus => check_math(BinaryOp::Minus, &op.1, left_expr, right_expr),
            Token::Star => check_math(BinaryOp::Mul, &op.1, left_expr, right_expr),
            Token::Slash => check_math(BinaryOp::Div, &op.1, left_expr, right_expr),
            Token::BangEqual => check_compare(BinaryOp::NotEqual, &op.1, left_expr, right_expr),
            Token::EqualEqual => check_compare(BinaryOp::Equal, &op.1, left_expr, right_expr),
            Token::Greater => check_compare(BinaryOp::Greater, &op.1, left_expr, right_expr),
            Token::GreaterEqual => {
                check_compare(BinaryOp::GreaterEqual, &op.1, left_expr, right_expr)
            }
            Token::Less => check_compare(BinaryOp::Less, &op.1, left_expr, right_expr),
            Token::LessEqual => check_compare(BinaryOp::LessEqual, &op.1, left_expr, right_expr),
            _ => error("invalid operator", &op.1),
        }
    }

    fn visit_identifier(&self, name: &Spanned<String>) -> AnalyserResult {
        Ok(Expr::Var {
            name: name.0.to_owned(),
            typedef: match self.get(&name.0) {
                Some(t) => t.clone(),
                None => return error("variable not defined", &name.1),
            },
        })
    }

    fn visit_call(&mut self, name: &Spanned<String>, args: &Vec<Node>) -> AnalyserResult {
        let (arg_types, return_type) = match self.get(&name.0) {
            Some(t) => match t {
                Type::Fn { args, returns } => (args.clone(), returns.as_ref().to_owned()),
                _ => return error(format!("{} is not a function", &name.0).as_str(), &name.1),
            },
            None => return error("function not defined", &name.1),
        };

        let mut epxr_args = Vec::new();
        for (i, arg_type) in arg_types.iter().enumerate() {
            let arg = match args.get(i) {
                Some(node) => node,
                None => return error(format!("missing fn arg {}", &arg_type.0).as_str(), &name.1),
            };
            let arg_expr = self.visit(arg)?;
            if arg_expr.get_return() != arg_type.1 {
                return error("invalid arg type", arg.get_span());
            }
            epxr_args.push(arg_expr);
        }

        Ok(Expr::Call {
            name: name.0.to_owned(),
            args: epxr_args,
            typedef: return_type,
        })
    }
}
