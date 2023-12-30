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
    let mut ctx = Context {
        enclosing: None,
        state: HashMap::new(),
    };
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    loop {
        print!("> ");
        let _ = stdout.flush();
        // read
        let mut buffer = String::new();
        while !buffer.trim_end().ends_with(";;") {
            let _ = stdin.read_line(&mut buffer);
            print!("| ");
            let _ = stdout.flush();
        }
        buffer = buffer[0..buffer.len() - 3].to_string();

        let lines: Vec<&str> = buffer.split('\n').collect();

        print!("{}{}", "run: ".blue(), buffer);
        let tokens = match lexer::scan(&buffer) {
            Ok(e) => e,
            Err(err) => {
                print_err(&lines, &err);
                continue;
            }
        };

        let node = match parser::parse(&tokens) {
            Ok(e) => e,
            Err(err) => {
                print_err(&lines, &err);
                continue;
            }
        };

        let expr = match analyser::analyse(&node) {
            Ok(e) => e,
            Err(err) => {
                print_err(&lines, &err);
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

fn print_err(lines: &Vec<&str>, err: &SyntaxErr) {
    let line = match lines.get(err.source.line) {
        Some(x) => x,
        None => "",
    };
    let nr = err.source.line.to_string();

    println!("{}{}", "error: ".red().bold(), err.message.red());

    println!(" {}{}{line}", nr.bold().blue(), " | ".bold().blue());
    let space = times(err.source.start + 4 + nr.len(), ' ');
    let line = times(err.source.len, '^');
    println!("{space}{}", line.red());
}
