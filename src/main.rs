use std::io::{self, Write};
use colored::Colorize;

use crate::parse::parser::Parser;

mod diagnostics;
mod parse;

fn main() -> io::Result<()> {
    loop {
        let mut buffer = String::new();
        print!("> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut buffer)?;

        if buffer.trim().is_empty() || buffer.trim() == "exit" {
            break;
        }

        let result = Parser::parse(buffer.to_owned());

        if result.errors.is_empty() {
            println!("{}", result.root);
        } else {
            for error in result.errors {
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
