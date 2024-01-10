use types::{context::Ctx, values::Value, SyntaxErr};
use wasm_bindgen::prelude::*;

mod analyser;
mod interpretor;
mod lexer;
mod parser;
mod types;

#[wasm_bindgen]
pub struct WhyErr {
    message: String,
    pub start: usize,
    pub end: usize,
}

#[wasm_bindgen]
impl WhyErr {
    #[wasm_bindgen(getter)]
    pub fn message(&self) -> String {
        self.message.clone()
    }
}

fn to_err(e: &SyntaxErr) -> WhyErr {
    WhyErr {
        message: e.message.to_owned(),
        start: e.source.start,
        end: e.source.end,
    }
}

#[wasm_bindgen]
pub fn analyse(value: &str) -> Vec<WhyErr> {
    match check(value) {
        Ok(_) => Vec::new(),
        Err(err) => err.iter().map(to_err).collect(),
    }
}

fn check(value: &str) -> Result<(), Vec<SyntaxErr>> {
    let tokens = match lexer::scan(value) {
        Ok(token) => token,
        Err(err) => return Err(vec![err]),
    };

    let nodes = match parser::parse(&tokens) {
        Ok(nodes) => nodes,
        Err(err) => return Err(vec![err]),
    };

    match Ctx::new(None, None).analyse(&nodes) {
        Ok(_) => Ok(()),
        Err(err) => Err(err.to_vec()),
    }
}

fn value_to_js(value: &Value) -> JsValue {
    match value {
        Value::Number(val) => JsValue::from_f64(*val),
        Value::String(val) => JsValue::from_str(val),
        Value::Bool(val) => JsValue::from_bool(*val),
        Value::Fn {
            typedef: _,
            expr: _,
        } => JsValue::from_str(format!("{value}").as_str()),
        Value::Obj {
            typedef: _,
            entries: _,
        } => JsValue::from_str(format!("{value}").as_str()),
        Value::Void => JsValue::UNDEFINED,
    }
}

#[wasm_bindgen]
pub fn why(value: &str) -> Result<JsValue, WhyErr> {
    match eval(value) {
        Ok(val) => Ok(value_to_js(&val)),
        Err(err) => Err(to_err(err.last().unwrap())),
    }
}

fn eval(value: &str) -> Result<Value, Vec<SyntaxErr>> {
    let tokens = match lexer::scan(value) {
        Ok(token) => token,
        Err(err) => return Err(vec![err]),
    };

    let stmts = match parser::parse(&tokens) {
        Ok(nodes) => nodes,
        Err(err) => return Err(vec![err]),
    };

    let mut ctx = Ctx::new(None, None);

    match ctx.analyse(&stmts) {
        Ok(stmts) => stmts,
        Err(err) => return Err(err.to_vec()),
    };

    ctx.execute(&stmts).map(|x| x).or_else(|x| {
        Err(vec![SyntaxErr {
            message: x.message,
            source: 0..value.len(),
        }])
    })
}
