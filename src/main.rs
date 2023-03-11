use std::io;

mod ast;
mod eval;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    println!("Starting REPL!");
    repl::start(io::stdin(), io::stdout());
}
