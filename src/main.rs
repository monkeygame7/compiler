use colored::Colorize;
use std::io::{self, Write};

use crate::{ast::parser::Parser, evaluator::Evaluator};

mod ast;
mod diagnostics;
mod evaluator;
mod id;
mod scope;
mod text;

fn main() -> io::Result<()> {
    let mut show_tree = false;
    let mut buffer = String::new();
    loop {
        let mut line_buffer = String::new();

        if buffer.is_empty() {
            print!("{}", "» ".green());
        } else {
            print!("{}", "· ".green());
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

        let mut evaluator = Evaluator::new(&tree);
        let result = evaluator.evaluate();
        if line_buffer.trim().is_empty() && tree.diagnostics.has_errors() {
            let mut diagnostics = tree.diagnostics.into_iter().collect::<Vec<_>>();
            diagnostics.sort_by(|a, b| a.span.start.partial_cmp(&b.span.start).unwrap());
            for error in diagnostics {
                let line = tree.src.get_line(error.span.start);
                let span = tree.src.relative_span(error.span);
                println!("{}", error.to_string().red());
                if line.len() > 0 {
                    let pre = &line[..span.start].trim_start();
                    let highlight = &line[span.start..span.end].to_string().red();
                    let post = &line[span.end..].trim_end();
                    // TODO: Include line number in output
                    println!("{}{}{}\n", pre, highlight, post);
                }
            }
            println!();
            buffer.clear()
        } else if !tree.diagnostics.has_errors() {
            println!("\n{}", result.to_string().purple());
            buffer.clear();
        }
    }

    Ok(())
}
