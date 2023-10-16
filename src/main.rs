use colored::Colorize;
use diagnostics::DiagnosticBag;
use std::{
    io::{self, Write},
    rc::Rc,
};
use text::SourceText;

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
            print!("{}", "» ".bright_green());
        } else {
            print!("{}", "· ".bright_green());
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

        let src = SourceText::from(&buffer).unwrap();
        let diagnostics = Rc::new(DiagnosticBag::new());
        let mut tree = Parser::parse(&src, diagnostics.clone()).unwrap();
        if show_tree {
            tree.print();
        }

        let result = Evaluator::evaluate(&mut tree, diagnostics.clone());
        if line_buffer.trim().is_empty() && diagnostics.has_errors() {
            let mut diagnostics = diagnostics
                .messages
                .borrow()
                .iter()
                .map(|d| (*d).clone())
                .collect::<Vec<_>>();
            diagnostics.sort_by(|a, b| a.span.start.partial_cmp(&b.span.start).unwrap());
            for error in diagnostics {
                let line = src.get_line(error.span.start);
                let span = src.relative_span(error.span);
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
        } else if !diagnostics.has_errors() {
            match result {
                evaluator::ResultType::Void => println!(""),
                _ => println!("\n{}", result.to_string().purple()),
            }
            buffer.clear();
        }
    }

    Ok(())
}
