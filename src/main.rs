use colored::Colorize;
use std::io::{self, Write};

use crate::{ast::parser::Parser, evaluator::Evaluator};

mod ast;
mod diagnostics;
mod evaluator;
mod text;

fn main() -> io::Result<()> {
    let mut show_tree = false;
    let mut buffer = String::new();
    loop {
        let mut line_buffer = String::new();

        if buffer.is_empty() {
            print!("» ");
        } else {
            print!("  ")
        }
        io::stdout().flush()?;
        io::stdin().read_line(&mut line_buffer)?;

        if buffer.is_empty() {
            if line_buffer.trim() == "#exit" {
                break;
            } else if line_buffer.trim() == "#tree" {
                show_tree = !show_tree;
                let msg = if show_tree { "" } else { "Not " };
                println!("{}showing tree", msg);
                continue;
            } else if line_buffer.trim() == "#clear" {
                print!("\x1B[2J\x1B[1;1H");
                continue;
            }
        }
        buffer += &line_buffer;

        let tree = Parser::parse(&buffer).unwrap();
        if show_tree {
            println!("{}", tree.root);
        }
        let evaluator = Evaluator::new(tree);
        let result = evaluator.evaluate();
        match result {
            Ok(r) => {
                println!("\n{}", r);
                buffer.clear()
            }
            Err(errors) if line_buffer.trim().is_empty() => {
                for error in errors {
                    println!("{}", error);
                    let span = error.span;
                    let pre = &buffer[..span.start].trim_start();
                    let highlight = &buffer[span.start..span.end].red();
                    let post = &buffer[span.end..].trim_end();
                    println!("{}{}{}", pre, highlight, post);
                }
                println!();
                buffer.clear()
            }
            _ => continue,
        }
    }

    Ok(())
}
