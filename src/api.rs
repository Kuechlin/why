use std::rc::Rc;

use crate::{
    lexer, parser,
    types::{context::Ctx, values::Value, SyntaxErr},
};

pub fn eval(value: &str) -> Result<Rc<Value>, Vec<SyntaxErr>> {
    eval_with_ctx(&Ctx::new(), value)
}

pub fn eval_with_ctx(ctx: &Ctx, value: &str) -> Result<Rc<Value>, Vec<SyntaxErr>> {
    let tokens = match lexer::scan(value) {
        Ok(token) => token,
        Err(err) => return Err(vec![err]),
    };

    let stmts = match parser::parse(&tokens) {
        Ok(nodes) => nodes,
        Err(err) => return Err(vec![err]),
    };

    match ctx.analyse(&stmts) {
        Ok(stmts) => stmts,
        Err(err) => return Err(err.to_vec()),
    };

    ctx.eval(&stmts).or_else(|x| {
        Err(vec![SyntaxErr {
            message: x.message,
            source: 0..value.len(),
        }])
    })
}

pub fn analyse(value: &str) -> Result<(), Vec<SyntaxErr>> {
    let tokens = match lexer::scan(value) {
        Ok(token) => token,
        Err(err) => return Err(vec![err]),
    };

    let nodes = match parser::parse(&tokens) {
        Ok(nodes) => nodes,
        Err(err) => return Err(vec![err]),
    };

    match Ctx::new().analyse(&nodes) {
        Ok(_) => Ok(()),
        Err(err) => Err(err.to_vec()),
    }
}
