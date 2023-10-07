use std::io::{self, Write};
use colored::Colorize;

use crate::parse::parser::{Parser, Program};
use crate::evaluate::evaluator::Evaluator;

mod diagnostics;
mod parse;
mod evaluate;

fn main() -> io::Result<()> {
    loop {
        let mut buffer = String::new();
        print!("> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut buffer)?;

        if buffer.trim().is_empty() || buffer.trim() == "exit" {
            break;
        }

        let tree = Program::parse(buffer.to_owned());
        let mut evaluator = Evaluator::new(tree);
        let result = evaluator.evaluate();

        if evaluator.errors.is_empty() {
            println!("{}", result);
        } else {
            for error in evaluator.errors {
                println!("{}", error.message);
                let span = error.span;
                let pre = &buffer[..span.start];
                let highlight = &buffer[span.start..span.end].red();
                let post = &buffer[span.end..];
                println!("{}{}{}", pre, highlight, post);
            }
        }
    }

    Ok(())
}
