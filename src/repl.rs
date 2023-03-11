use std::io::{self, BufRead, BufReader};

use crate::{eval::eval_statements, lexer::Lexer, parser::get_ast};

const PROMPT: &[u8] = b">> ";

pub fn start(inp: impl io::Read, mut out: impl io::Write) {
    print_prompt(&mut out);

    let mut scanner = BufReader::new(inp);
    let mut line = vec![];
    while scanner.read_until(b'\n', &mut line).is_ok() {
        let l = Lexer::new(&line);
        let (statements, errors) = get_ast(l);
        if !errors.is_empty() {
            println!("Error detected while parsing!");
            for error in errors {
                println!("- {error:#}");
            }
        } else {
            match eval_statements(statements) {
                Ok(obj) => println!("{obj:?}"),
                Err(error) => println!("Error detected! {error:#}"),
            };
        }

        print_prompt(&mut out);
        line.clear();
    }
}

fn print_prompt(out: &mut impl io::Write) {
    out.write_all(PROMPT).unwrap();
    out.flush().unwrap();
}
