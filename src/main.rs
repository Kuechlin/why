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

use crate::types::{context::Ctx, values::Value};

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
    let ctx = Ctx::new(None, None);
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

        let stmts = match parser::parse(&tokens) {
            Ok(e) => e,
            Err(err) => {
                print_err(&buffer, &err);
                continue;
            }
        };

        match ctx.analyse(&stmts) {
            Ok(_) => (),
            Err(err) => {
                for e in err {
                    print_err(&buffer, &e);
                }
                continue;
            }
        };

        let result = match ctx.execute(&stmts) {
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
    let mut value = "";
    let mut count = 0;
    let mut len = 0;
    for line in buffer.split('\n') {
        count += 1;
        value = line;
        if len + line.len() > err.source.start {
            len = err.source.start - len;
            break;
        }
        len += line.len();
    }

    println!("{}{}", "error: ".red().bold(), err.message.red());
    let num = count.to_string();
    println!("{}{}{value}", num.bold().blue(), " | ".bold().blue());
    let space = times(len + num.len() + 3, ' ');
    let line = times(err.source.len(), '^');
    println!("{space}{}", line.red());
}
