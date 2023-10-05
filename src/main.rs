use std::{
    io::{self, Write},
};

mod syntax;
mod parse;
use crate::{syntax::{TokenKind, Lexer}, parse::Parser};

fn main() -> io::Result<()> {
    loop {
        let mut buffer = String::new();
        print!("> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut buffer)?;

        if buffer.trim().is_empty() || buffer.trim() == "exit" {
            break
        }

        let tree = Parser::parse(&buffer);
        println!("{}", tree);
    }

    Ok(())
}
