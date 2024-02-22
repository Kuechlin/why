use std::{
    env,
    io::{self, Write},
};

use colored::Colorize;

use crate::{
    api::{eval, eval_with_ctx},
    types::context::Ctx,
};

mod analyser;
mod api;
mod interpretor;
mod lexer;
mod parser;
mod types;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        repl();
    } else {
        let filename = &args[1];
        println!("{}{}", "run: ".blue(), filename);
        match std::fs::read_to_string(filename) {
            Ok(buffer) => run(&buffer),
            Err(err) => println!("{}{}", "file error: ".red().bold(), err.to_string().red()),
        };
    }
}

fn logo() {
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
}

fn repl() {
    logo();
    let ctx = Ctx::new();
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    loop {
        print!("> ");
        let _ = stdout.flush();
        // read
        let mut buffer = String::new();
        let _ = stdin.read_line(&mut buffer);

        print!("{}{}", "run: ".blue(), buffer);
        match eval_with_ctx(&ctx, &buffer) {
            Ok(e) => e.as_ref().print(),
            Err(err) => {
                for e in err {
                    e.print(&buffer);
                }
            }
        };
        println!("");
    }
}

fn run(buffer: &str) {
    match eval(&buffer) {
        Ok(e) => e.as_ref().print(),
        Err(err) => {
            for e in err {
                e.print(&buffer);
            }
        }
    };
    println!("");
}
