/*
fn add(a: int, b: int) {
    a + b
}
enum LoopResult {
    Continue,
    Break
}
fn loop(fn: fn LoopResult) {
    let r = fn()
    if r {
        Continue -> loop(fn),
    }
}

let text = "hello world"
let x = 0;
loop {
    x = add(x, 5)
    if x {
        > 10 -> Break,
        _ -> Continue
    }
}
*/

use std::io::{self, Write};

mod expressions;
mod interpretor;
mod lexer;
mod parser;

fn main() {
    println!("why?");

    loop {
        print!(">");
        let _ = io::stdout().flush();
        // read
        let mut buffer = String::new();
        let stdin = io::stdin();
        let _ = stdin.read_line(&mut buffer);

        print!("run: {}", buffer);
        let tokens = lexer::scan(&buffer);
        println!("tokens: {:?}", tokens);
        let expr = parser::parse(&tokens);

        let result = interpretor::eval(&expr);

        print!("result: ");
        println!("{}", result.to_string())
    }
}
