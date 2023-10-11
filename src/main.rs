use colored::Colorize;
use std::io::{self, Write};

use crate::{ast::parser::Parser, evaluator::Evaluator};

mod ast;
mod diagnostics;
mod evaluator;
mod visitor;

fn main() -> io::Result<()> {
    let mut show_tree = true;
    loop {
        let mut buffer = String::new();
        print!("» ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut buffer)?;

        if buffer.trim().is_empty() || buffer.trim() == "exit" {
            break;
        } else if buffer.trim() == "#tree" {
            show_tree = !show_tree;
            let msg = if show_tree { "" } else { "Not " };
            println!("{}showing tree", msg);
            continue;
        } else if buffer.trim() == "#clear" {
            print!("\x1B[2J\x1B[1;1H");
            continue;
        }

        let tree = Parser::parse(buffer.to_owned());
        if show_tree {
            println!("{}", tree.root);
        }
        let evaluator = Evaluator::new(tree);
        let result = evaluator.evaluate();
        match result {
            Ok(r) => println!("{}", r),
            Err(errors) => {
                for error in errors {
                    println!("{}", error.message);
                    let span = error.span;
                    let pre = &buffer[..span.start];
                    let highlight = &buffer[span.start..span.end].red();
                    let post = &buffer[span.end..];
                    println!("{}{}{}", pre, highlight, post);
                }
            }
        }
    }

    Ok(())
}
