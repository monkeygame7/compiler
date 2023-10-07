use std::fmt::Display;

use super::lexer::{Lexer, SyntaxToken, TokenKind};

pub struct Parser {
    tokens: Vec<SyntaxToken>,
    current_position: usize,
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
    Integer(i32),
    BinaryExpression(Box<SyntaxNode>, BinaryOperatorToken, Box<SyntaxNode>),
    GroupExpression(SyntaxToken, Box<SyntaxNode>, SyntaxToken),
}

pub struct SyntaxNode {
    kind: SyntaxKind,
    position: usize,
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
        SyntaxKind::Integer(i) => {
            f.write_fmt(format_args!("{}{} IntegerExpression\n", padding, marker))?;
            f.write_fmt(format_args!("{}{} {}\n", &child_padding, last_marker, i))
        }
        SyntaxKind::BinaryExpression(l, op, r) => {
            f.write_fmt(format_args!("{}{} BinaryExpression\n", padding, marker))?;
            display_helper(&l, f, &child_padding, false)?;
            f.write_fmt(format_args!("{}{} {}\n", &child_padding, middle_marker, op.token.kind))?;
            display_helper(&r, f, &child_padding, true)
        }
        SyntaxKind::GroupExpression(_, exp, _) => {
            f.write_fmt(format_args!("{}{} GroupExpression\n", padding, marker))?;
            display_helper(exp, f, &child_padding, true)
        }
    }
}

impl Display for SyntaxNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_helper(self, f, "", true)
    }
}

impl Parser {
    pub fn parse(text: String) -> SyntaxNode {
        let mut lexer = Lexer::new(text);
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            match token.kind {
                TokenKind::BadToken(_) | TokenKind::WhiteSpace(_) => continue,
                _ => (),
            }
            tokens.push(token);
            let token = tokens.last().unwrap();
            match token.kind {
                TokenKind::EOF => break,
                _ => continue,
            }
        }

        let mut parser = Parser::new(tokens);

        parser.parse_program()
    }

    fn new(tokens: Vec<SyntaxToken>) -> Parser {
        assert!(tokens.len() > 0, "Tokens must not be empty");

        Parser {
            tokens,
            current_position: 0,
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

    fn parse_program(&mut self) -> SyntaxNode {
        let program = self.parse_expression(0);

        if !matches!(self.next().kind, TokenKind::EOF) {
            todo!("Error handling for extra input");
        }

        program
    }

    fn parse_expression(&mut self, priority: usize) -> SyntaxNode {
        self.parse_binary_expression(priority).unwrap()
    }

    fn parse_binary_expression(&mut self, priority: usize) -> Option<SyntaxNode> {
        let mut left = self.parse_primary_expression()?;

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
            let position = left.position;
            left = SyntaxNode {
                kind: SyntaxKind::BinaryExpression(Box::new(left), operator, Box::new(right)),
                position,
            }
        }

        Some(left)
    }

    fn parse_primary_expression(&mut self) -> Option<SyntaxNode> {
        match self.current().kind {
            TokenKind::Integer(i) => Some(self.parse_number_expression(i)),
            TokenKind::LeftParenthesisToken(_) => {
                let open = self.next();
                let expression = self.parse_expression(0);
                let close = match self.current().kind {
                    TokenKind::RightParenthesisToken(_) => Some(self.next()),
                    _ => None,
                };
                let position = open.position;
                close.map(|close| SyntaxNode {
                    kind: SyntaxKind::GroupExpression(open, Box::new(expression), close),
                    position,
                })
            }
            _ => panic!("expected int"),
        }
    }

    fn parse_number_expression(&mut self, i: i32) -> SyntaxNode {
        let token = self.next();
        SyntaxNode {
            kind: SyntaxKind::Integer(i),
            position: token.position,
        }
    }
}
