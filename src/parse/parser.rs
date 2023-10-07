use std::fmt::Display;

use crate::diagnostics::{DiagnosticMessage, TextSpan};

use super::lexer::{Lexer, SyntaxToken, TokenKind};

pub struct Parser {
    tokens: Vec<SyntaxToken>,
    current_position: usize,
    errors: Vec<DiagnosticMessage>,
}

#[derive(Debug)]
pub enum BinaryOperatorKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

impl BinaryOperatorKind {
    fn priority(&self) -> usize {
        match self {
            BinaryOperatorKind::Addition => 1,
            BinaryOperatorKind::Subtraction => 1,
            BinaryOperatorKind::Multiplication => 2,
            BinaryOperatorKind::Division => 2,
        }
    }
}

pub struct BinaryOperatorToken {
    kind: BinaryOperatorKind,
    token: SyntaxToken,
}

pub enum SyntaxKind {
    BadExpression,
    IntegerExpression(i32),
    BinaryExpression(Box<SyntaxNode>, BinaryOperatorToken, Box<SyntaxNode>),
    UnaryExpression(UnaryOperatorToken, Box<SyntaxNode>), // TODO!
    GroupExpression(SyntaxToken, Box<SyntaxNode>, SyntaxToken),
}

pub struct SyntaxNode {
    kind: SyntaxKind,
    span: TextSpan,
}

pub struct Program {
    pub root: SyntaxNode,
    pub errors: Vec<DiagnosticMessage>,
}

impl Parser {
    pub fn parse(text: String) -> Program {
        let mut lexer = Lexer::new(text);
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            match token.kind {
                TokenKind::WhiteSpace(_) => continue,
                _ => (),
            }
            tokens.push(token);
            let token = tokens.last().unwrap();
            match token.kind {
                TokenKind::EOF => break,
                _ => continue,
            }
        }

        let parser = Parser::new(tokens, lexer.errors);

        parser.parse_program()
    }

    fn new(tokens: Vec<SyntaxToken>, errors: Vec<DiagnosticMessage>) -> Parser {
        assert!(tokens.len() > 0, "Tokens must not be empty");

        Parser {
            tokens,
            current_position: 0,
            errors,
        }
    }

    fn current(&mut self) -> &SyntaxToken {
        &self.tokens[self.current_position]
    }

    fn next(&mut self) -> SyntaxToken {
        let current = &self.tokens[self.current_position];
        if self.current_position < self.tokens.len() - 1 {
            self.current_position += 1;
        }
        current.clone()
    }

    fn parse_program(mut self) -> Program {
        let program = self.parse_expression(0);

        let eof_token = self.next();
        if !matches!(eof_token.kind, TokenKind::EOF) {
            let mut last_token;
            loop {
                last_token = self.next();
                if matches!(last_token.kind, TokenKind::EOF) {
                    break;
                }
            }
            while !matches!(self.next().kind, TokenKind::EOF) {}
            self.errors.push(DiagnosticMessage::for_range(
                "Expected EOF".to_owned(),
                eof_token.span.to(last_token.span),
            ));
        }
        Program {
            root: program,
            errors: self.errors,
        }
    }

    fn parse_expression(&mut self, priority: usize) -> SyntaxNode {
        self.parse_binary_expression(priority)
    }

    fn parse_binary_expression(&mut self, priority: usize) -> SyntaxNode {
        let mut left = self.parse_primary_expression();

        loop {
            let kind = match self.current().kind {
                TokenKind::PlusToken(_) => BinaryOperatorKind::Addition,
                TokenKind::DashToken(_) => BinaryOperatorKind::Subtraction,
                TokenKind::StarToken(_) => BinaryOperatorKind::Multiplication,
                TokenKind::SlashToken(_) => BinaryOperatorKind::Division,
                _ => break,
            };
            if kind.priority() <= priority {
                break;
            }
            let operator = BinaryOperatorToken {
                kind,
                token: self.next(),
            };
            let right = self.parse_expression(operator.kind.priority());
            let span = left.span.to(right.span);
            left = SyntaxNode {
                kind: SyntaxKind::BinaryExpression(Box::new(left), operator, Box::new(right)),
                span,
            }
        }

        left
    }

    fn parse_primary_expression(&mut self) -> SyntaxNode {
        match self.current().kind {
            TokenKind::Integer(i) => self.parse_number_expression(i),
            TokenKind::LeftParenthesisToken(_) => self.parse_group_expression(),
            _ => {
                let start_span = self.current().span;
                let bad_token = self.next();
                let span = start_span.to(bad_token.span);
                self.errors.push(DiagnosticMessage::for_range(
                    format!("Expected primary expression, found: {}", bad_token.kind),
                    span,
                ));
                SyntaxNode {
                    kind: SyntaxKind::BadExpression,
                    span,
                }
            }
        }
    }

    fn parse_group_expression(&mut self) -> SyntaxNode {
        let open = self.next();
        let expression = self.parse_expression(0);
        let close = self.next();
        let span = open.span.to(close.span);
        match close.kind {
            TokenKind::RightParenthesisToken(_) => SyntaxNode {
                kind: SyntaxKind::GroupExpression(open, Box::new(expression), close),
                span,
            },
            _ => {
                self.errors.push(DiagnosticMessage::for_range(
                    format!("Expected ')', but found {}", close.kind),
                    close.span,
                ));
                SyntaxNode {
                    kind: SyntaxKind::BadExpression,
                    span,
                }
            }
        }
    }

    fn parse_number_expression(&mut self, i: i32) -> SyntaxNode {
        let token = self.next();
        SyntaxNode {
            kind: SyntaxKind::IntegerExpression(i),
            span: token.span,
        }
    }
}

impl Display for SyntaxNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_helper(self, f, "", true)
    }
}

fn display_helper(
    node: &SyntaxNode,
    f: &mut std::fmt::Formatter<'_>,
    padding: &str,
    is_last: bool,
) -> std::fmt::Result {
    let last_marker = " └──";
    let middle_marker = " ├──";
    let marker = if is_last { last_marker } else { middle_marker };
    let child_padding = if is_last {
        padding.to_owned() + "    "
    } else {
        padding.to_owned() + " │  "
    };

    match &node.kind {
        SyntaxKind::BadExpression => f.write_fmt(format_args!("{}{}\n", padding, marker)),
        SyntaxKind::IntegerExpression(i) => {
            f.write_fmt(format_args!("{}{} IntegerExpression\n", padding, marker))?;
            f.write_fmt(format_args!("{}{} {}\n", &child_padding, last_marker, i))
        }
        SyntaxKind::BinaryExpression(l, op, r) => {
            f.write_fmt(format_args!("{}{} BinaryExpression\n", padding, marker))?;
            display_helper(&l, f, &child_padding, false)?;
            f.write_fmt(format_args!(
                "{}{} {}\n",
                &child_padding, middle_marker, op.token.kind
            ))?;
            display_helper(&r, f, &child_padding, true)
        }
        SyntaxKind::GroupExpression(_, exp, _) => {
            f.write_fmt(format_args!("{}{} GroupExpression\n", padding, marker))?;
            display_helper(exp, f, &child_padding, true)
        }
    }
}
