use colored::Colorize;
use compilation::CompilationUnit;
use diagnostics::DiagnosticBag;
use std::{
    io::{self, Write},
    rc::Rc,
};
use text::SourceText;

use crate::evaluator::Evaluator;

mod ast;
mod compilation;
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

        let compilation = CompilationUnit::compile(&buffer, show_tree);
        match compilation {
            Ok(mut program) => {
                let result = Evaluator::evaluate(&mut program.ast);
                match result {
                    evaluator::ResultType::Void => println!(""),
                    _ => println!("\n{}", result.to_string().purple()),
                }
                buffer.clear();
            }
            Err((src, diagnostics)) if line_buffer.trim().is_empty() => {
                print_diagnostics(diagnostics, &src);
                buffer.clear();
            }
            _ => continue,
        }
    }

    Ok(())
}

fn print_diagnostics(diagnostics: Rc<DiagnosticBag>, src: &SourceText) {
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
}
