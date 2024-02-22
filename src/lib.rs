use types::{values::Value, SyntaxErr};
use wasm_bindgen::prelude::*;

mod analyser;
mod api;
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
    match api::analyse(value) {
        Ok(_) => Vec::new(),
        Err(err) => err.iter().map(to_err).collect(),
    }
}

#[wasm_bindgen]
pub fn why(value: &str) -> Result<JsValue, WhyErr> {
    match api::eval(value) {
        Ok(val) => Ok(value_to_js(&val)),
        Err(err) => Err(to_err(err.last().unwrap())),
    }
}

fn value_to_js(value: &Value) -> JsValue {
    match value {
        Value::Number(val) => JsValue::from_f64(*val),
        Value::String(val) => JsValue::from_str(val),
        Value::Bool(val) => JsValue::from_bool(*val),
        Value::Fn(val) => JsValue::from_str(format!("{}", val.ty).as_str()),
        Value::Obj(val) => JsValue::from_str(format!("{:?}", val).as_str()),
        Value::Void => JsValue::UNDEFINED,
    }
}
