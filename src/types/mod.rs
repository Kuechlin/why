use colored::Colorize;

pub mod ast;
pub mod context;
pub mod tokens;
pub mod types;
pub mod values;

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);
pub trait Spannable {
    fn span(&self) -> &Span;
}

#[derive(Clone)]
pub struct SyntaxErr {
    pub message: String,
    pub source: Span,
}

impl SyntaxErr {
    pub fn new<'a>(msg: &'a str, span: &Span) -> Self {
        Self {
            message: msg.to_owned(),
            source: span.clone(),
        }
    }

    pub fn print(&self, buffer: &str) {
        let mut value = "";
        let mut count = 0;
        let mut len = 0;
        for line in buffer.split('\n') {
            count += 1;
            value = line;
            if len + line.len() > self.source.start {
                len = self.source.start - len;
                break;
            }
            len += line.len();
        }

        println!("{}{}", "error: ".red().bold(), self.message.red());
        let num = count.to_string();
        println!("{}{}{value}", num.bold().blue(), " | ".bold().blue());
        let space = times(len + num.len() + 3, ' ');
        let line = times(self.source.len(), '^');
        println!("{space}{}", line.red());
    }
}

fn times(x: usize, v: char) -> String {
    (0..x).map(|_| v).collect()
}
