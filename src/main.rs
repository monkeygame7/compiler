use std::io::{self, Write};

use crate::parse::parser::Parser;

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

        let result = Parser::parse(buffer);

        println!("{}", result);
    }

    Ok(())
}
