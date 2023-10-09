use std::fmt::Display;

use crate::diagnostics::{TextSpan, DiagnosticBag};

use super::lexer::{Lexer, SyntaxToken, TokenKind};

pub enum SyntaxKind {
    BadExpression,
    IntegerExpression(SyntaxToken, i32),
    BinaryExpression(Box<SyntaxNode>, SyntaxToken, Box<SyntaxNode>),
    UnaryExpression(SyntaxToken, Box<SyntaxNode>),
    GroupExpression(SyntaxToken, Box<SyntaxNode>, SyntaxToken),
}

pub struct SyntaxNode {
    pub kind: SyntaxKind,
    pub span: TextSpan,
}

pub struct SyntaxTree {
    pub root: SyntaxNode,
    pub diagnostics: DiagnosticBag,
}

pub struct Parser {
    tokens: Vec<SyntaxToken>,
    current_position: usize,
    diagnostics: DiagnosticBag,
}

impl SyntaxTree {
    pub fn parse(text: String) -> SyntaxTree {
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

        let parser = Parser::new(tokens, lexer.diagnostics);

        parser.parse_program()
    }
}

impl Parser {
    fn new(tokens: Vec<SyntaxToken>, diagnostics: DiagnosticBag) -> Parser {
        assert!(tokens.len() > 0, "Tokens must not be empty");

        Parser {
            tokens,
            current_position: 0,
            diagnostics,
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

    fn parse_program(mut self) -> SyntaxTree {
        let program = self.parse_expression(0);

        let eof_token = self.next();
        if !matches!(eof_token.kind, TokenKind::EOF) {
            self.diagnostics.report_unexpected_token(eof_token, TokenKind::EOF);
        }
        SyntaxTree {
            root: program,
            diagnostics: self.diagnostics,
        }
    }

    fn parse_expression(&mut self, priority: usize) -> SyntaxNode {
        self.parse_binary_expression(priority)
    }

    fn parse_unary_expression(&mut self, priority: usize) -> SyntaxNode {
        let operator_token = self.next();
        let expression = self.parse_expression(unary_operator_priority(&operator_token));
        let span = operator_token.span.to(expression.span);
        SyntaxNode {
            kind: SyntaxKind::UnaryExpression(operator_token, Box::new(expression)),
            span,
        }
    }

    fn parse_binary_expression(&mut self, priority: usize) -> SyntaxNode {
        let current = self.current();
        let mut left = match current.kind {
            TokenKind::DashToken(_) | TokenKind::PlusToken(_) 
                // is this necessary?
                if unary_operator_priority(current) >= priority =>
            {
                self.parse_unary_expression(priority)
            }
            _ => self.parse_primary_expression(),
        };

        loop {
            match self.current().kind {
                TokenKind::PlusToken(_)
                | TokenKind::DashToken(_)
                | TokenKind::StarToken(_)
                | TokenKind::SlashToken(_) => (),
                _ => break,
            };
            let operator_priority = binary_operator_priority(self.current());
            if operator_priority <= priority {
                break;
            }
            let operator_token = self.next();
            let right = self.parse_expression(operator_priority);
            let span = left.span.to(right.span);
            left = SyntaxNode {
                kind: SyntaxKind::BinaryExpression(Box::new(left), operator_token, Box::new(right)),
                span,
            }
        }

        left
    }

    fn parse_primary_expression(&mut self) -> SyntaxNode {
        match self.current().kind {
            TokenKind::Integer(i) => self.parse_integer_expression(i),
            TokenKind::LeftParenthesisToken(_) => self.parse_group_expression(),
            _ => {
                let start_span = self.current().span;
                let bad_token = self.next();
                let span = start_span.to(bad_token.span);
                self.diagnostics.report_unexpected_token(bad_token, "<primary expression>");
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
                self.diagnostics.report_unexpected_token(close, TokenKind::RightParenthesisToken(")".to_string()));
                SyntaxNode {
                    kind: SyntaxKind::BadExpression,
                    span,
                }
            }
        }
    }

    fn parse_integer_expression(&mut self, value: i32) -> SyntaxNode {
        let token = self.next();
        let span = token.span;
        SyntaxNode {
            kind: SyntaxKind::IntegerExpression(token, value),
            span,
        }
    }
}

fn unary_operator_priority(token: &SyntaxToken) -> usize {
    match token.kind {
        TokenKind::PlusToken(_) | TokenKind::DashToken(_) => 3,
        _ => 0,
    }
}

fn binary_operator_priority(token: &SyntaxToken) -> usize {
    match token.kind {
        TokenKind::StarToken(_) | TokenKind::SlashToken(_) => 2,
        TokenKind::PlusToken(_) | TokenKind::DashToken(_) => 1,
        _ => 0,
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
        SyntaxKind::IntegerExpression(_, i) => {
            f.write_fmt(format_args!("{}{} IntegerExpression\n", padding, marker))?;
            f.write_fmt(format_args!(
                "{}{} {}\n",
                &child_padding, last_marker, i
            ))
        }
        SyntaxKind::BinaryExpression(l, op, r) => {
            f.write_fmt(format_args!("{}{} BinaryExpression\n", padding, marker))?;
            display_helper(&l, f, &child_padding, false)?;
            f.write_fmt(format_args!(
                "{}{} {}\n",
                &child_padding, middle_marker, op.kind
            ))?;
            display_helper(&r, f, &child_padding, true)
        }
        SyntaxKind::UnaryExpression(op, exp) => {
            f.write_fmt(format_args!("{}{} UnaryExpression\n", padding, marker))?;
            f.write_fmt(format_args!(
                "{}{} {}\n",
                &child_padding, middle_marker, op.kind
            ))?;
            display_helper(exp, f, &child_padding, true)
        }
        SyntaxKind::GroupExpression(_, exp, _) => {
            f.write_fmt(format_args!("{}{} GroupExpression\n", padding, marker))?;
            display_helper(exp, f, &child_padding, true)
        }
    }
}
