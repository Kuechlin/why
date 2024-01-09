pub mod exprs;
pub mod tokens;
pub mod types;
pub mod values;

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Clone)]
pub struct SyntaxErr {
    pub message: String,
    pub source: Span,
}

pub struct RuntimeErr {
    pub message: String,
}
