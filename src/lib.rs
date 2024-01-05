use analyser::AnalyserCtx;
use interpretor::ExecCtx;
use types::{values::Value, SyntaxErr};
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

#[wasm_bindgen]
pub fn analyse(value: &str) -> Vec<WhyErr> {
    match eval(value) {
        Ok(_) => Vec::new(),
        Err(err) => err
            .iter()
            .map(|e| WhyErr {
                message: e.message.to_owned(),
                start: e.source.start,
                end: e.source.end,
            })
            .collect(),
    }
}

#[wasm_bindgen]
pub fn why(value: &str) -> Result<JsValue, WhyErr> {
    match eval(value) {
        Ok(val) => Ok(match &val {
            Value::Number(val) => JsValue::from_f64(*val),
            Value::String(val) => JsValue::from_str(val),
            Value::Bool(val) => JsValue::from_bool(*val),
            Value::Fn {
                typedef: _,
                expr: _,
            } => JsValue::from_str(format!("{val}").as_str()),
            Value::Void => JsValue::UNDEFINED,
        }),
        Err(err) => {
            let e = err.last().unwrap();
            Err(WhyErr {
                message: e.message.to_owned(),
                start: e.source.start,
                end: e.source.end,
            })
        }
    }
}

fn eval(value: &str) -> Result<Value, Vec<SyntaxErr>> {
    let tokens = match lexer::scan(value) {
        Ok(token) => token,
        Err(err) => return Err(vec![err]),
    };

    let nodes = match parser::parse(&tokens) {
        Ok(nodes) => nodes,
        Err(err) => return Err(vec![err]),
    };

    let stmts = match AnalyserCtx::new().analyse(&nodes) {
        Ok(stmts) => stmts,
        Err(err) => return Err(err.to_vec()),
    };

    ExecCtx::new().execute(&stmts).map(|x| x).or_else(|x| {
        Err(vec![SyntaxErr {
            message: x.message,
            source: 0..value.len(),
        }])
    })
}
