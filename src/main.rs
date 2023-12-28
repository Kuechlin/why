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

use colored::Colorize;
use types::SyntaxErr;

mod interpretor;
mod lexer;
mod parser;
mod types;

fn main() {
    println!("why?");

    loop {
        print!(">");
        let _ = io::stdout().flush();
        // read
        let mut buffer = String::new();
        let stdin = io::stdin();
        let _ = stdin.read_line(&mut buffer);

        let lines: Vec<&str> = buffer.split('\n').collect();

        print!("{}{}", "run: ".blue(), buffer);
        let (tokens, map) = match lexer::scan(&buffer) {
            Ok(e) => e,
            Err(err) => {
                print_err(&lines, &err);
                continue;
            }
        };

        println!("tokens: {:?}", tokens);
        let expr = match parser::parse(&tokens, &map) {
            Ok(e) => e,
            Err(err) => {
                print_err(&lines, &err);
                continue;
            }
        };

        let result = match interpretor::eval(&expr) {
            Ok(e) => e,
            Err(err) => {
                println!("{}{}", "error: ".red().bold(), err.red());
                continue;
            }
        };

        print!("{}", "result: ".green());
        println!("{}", result.to_string());
        println!("");
    }
}

fn times(x: usize, v: char) -> String {
    (0..x).map(|_| v).collect()
}

fn print_err(lines: &Vec<&str>, err: &SyntaxErr) {
    let line = match lines.get(err.pos.line) {
        Some(x) => x,
        None => "",
    };
    let nr = err.pos.line.to_string();

    println!("{}{}", "error: ".red().bold(), err.message.red());

    println!(" {}{}{line}", nr.bold().blue(), " | ".bold().blue());
    let space = times(err.pos.start + 4 + nr.len(), ' ');
    let line = times(err.pos.len, '^');
    println!("{space}{}", line.red());
}
