use std::io;

mod ast;
// mod eval;
// mod object;
mod parser;
mod repl;
mod token;
mod util;

fn main() {
    println!("Starting REPL!");
    repl::start(io::stdin(), io::stdout());
}
