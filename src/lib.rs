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
        Err(err) => Err(WhyErr {
            message: err.message,
            start: err.source.start,
            end: err.source.end,
        }),
    }
}

fn eval(value: &str) -> Result<Value, SyntaxErr> {
    let tokens = lexer::scan(value)?;

    let nodes = parser::parse(&tokens)?;

    let stmts = AnalyserCtx::new().analyse(&nodes)?;

    ExecCtx::new().execute(&stmts).map(|x| x).or_else(|x| {
        Err(SyntaxErr {
            message: x.message,
            source: 0..value.len(),
        })
    })
}
