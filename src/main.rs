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

use std::{
    collections::HashMap,
    io::{self, Write},
};

use colored::Colorize;
use types::SyntaxErr;

use crate::{interpretor::Context, types::Value};

mod analyser;
mod interpretor;
mod lexer;
mod parser;
mod types;

fn main() {
    println!(
        "{}",
        "
                __                  
               /\\ \\                 
     __  __  __\\ \\ \\___   __  __
    /\\ \\/\\ \\/\\ \\\\ \\  _ `\\/\\ \\/\\ \\
    \\ \\ \\_/ \\_/ \\\\ \\ \\ \\ \\ \\ \\_\\ \\
     \\ \\___x___/' \\ \\_\\ \\_\\/`____ \\
      \\/__//__/    \\/_/\\/_/`/___/> \\
                              /\\___/
                              \\/__/
"
        .blue()
    );
    let mut ctx = Context::new();
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    loop {
        print!("> ");
        let _ = stdout.flush();
        // read
        let mut buffer = String::new();
        let _ = stdin.read_line(&mut buffer);

        print!("{}{}", "run: ".blue(), buffer);
        let tokens = match lexer::scan(&buffer) {
            Ok(e) => e,
            Err(err) => {
                print_err(&buffer, &err);
                continue;
            }
        };

        let node = match parser::parse(&tokens) {
            Ok(e) => e,
            Err(err) => {
                print_err(&buffer, &err);
                continue;
            }
        };

        let expr = match analyser::analyse(&node) {
            Ok(e) => e,
            Err(err) => {
                print_err(&buffer, &err);
                continue;
            }
        };

        let result = match ctx.eval(&expr) {
            Ok(e) => e,
            Err(err) => {
                println!("{}{}", "error: ".red().bold(), err.message.red());
                continue;
            }
        };

        match result {
            Value::Void => {
                println!("{}", "void".cyan())
            }
            res => {
                print!("{}", "result: ".green());
                println!("{}", res.to_string());
            }
        }
        println!("");
    }
}

fn times(x: usize, v: char) -> String {
    (0..x).map(|_| v).collect()
}

fn print_err(buffer: &str, err: &SyntaxErr) {
    let line = &buffer[err.source.start..err.source.end];
    println!("{}{}", "error: ".red().bold(), err.message.red());
    println!("{}{line}", " | ".bold().blue());
    let line = times(err.source.len(), '^');
    println!("{}", line.red());
}
