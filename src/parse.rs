use crate::syntax::{Lexer, SyntaxToken, TokenKind};
use std::{
    fmt::{Debug, Display},
    iter::Peekable,
};

pub struct Parser<'a> {
    lexer: Peekable<Box<dyn Iterator<Item = SyntaxToken> + 'a>>,
}

pub enum OperatorKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

pub struct OperatorToken {
    kind: OperatorKind,
    token: SyntaxToken,
}

pub enum SyntaxNode {
    BadNode(SyntaxToken),
    PrimaryExpression(SyntaxToken),
    BinaryExpression(Box<SyntaxNode>, OperatorToken, Box<SyntaxNode>),
}

impl Display for SyntaxNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_helper(f, "")
    }
}

impl SyntaxNode {
    fn fmt_helper(&self, f: &mut std::fmt::Formatter<'_>, padding: &str) -> std::fmt::Result {
        match self {
            SyntaxNode::PrimaryExpression(t) => {
                f.write_fmt(format_args!("{}<PrimaryExpression> {}\n", padding, t))?;
            }
            SyntaxNode::BadNode(t) => {
                f.write_fmt(format_args!("{}<BadNode> {}\n", padding, t))?;
            }
            SyntaxNode::BinaryExpression(left, op, right) => {
                let child_padding = padding.to_owned() + "   ";
                f.write_fmt(format_args!("{}<BinaryExpression> {}\n", padding, op.token))?;
                left.fmt_helper(f, &child_padding)?;
                right.fmt_helper(f, &child_padding)?;
            }
        }
        Ok(())
    }
}

impl Parser<'_> {
    pub fn parse(text: &str) -> SyntaxNode {
        let lexer = Lexer::parse(text).filter(|t| match t.kind {
            TokenKind::EOF | TokenKind::BadToken | TokenKind::WhiteSpace => false,
            _ => true,
        });
        let lexer: Box<dyn Iterator<Item = SyntaxToken>> = Box::new(lexer);

        let mut parser = Parser {
            lexer: lexer.peekable(),
        };

        parser.parse_expression()
    }

    fn parse_expression(&mut self) -> SyntaxNode {
        self.parse_binary_expression()
    }

    fn parse_binary_expression(&mut self) -> SyntaxNode {
        let mut left = self.parse_primary_expression();

        while let Some(token) = self.lexer.peek() {
            let operator_kind = match token.kind {
                TokenKind::Plus => OperatorKind::Addition,
                TokenKind::Dash => OperatorKind::Subtraction,
                TokenKind::Star => OperatorKind::Multiplication,
                TokenKind::Slash => OperatorKind::Division,
                _ => break,
            };
            let operator = OperatorToken {
                kind: operator_kind,
                token: self.lexer.next().unwrap(),
            };
            let right = self.parse_primary_expression();
            left = SyntaxNode::BinaryExpression(Box::new(left), operator, Box::new(right));
        }

        left
    }

    fn parse_primary_expression(&mut self) -> SyntaxNode {
        let token = self.lexer.next();
        token
            .map(|t| match t.kind {
                TokenKind::Number(_) => SyntaxNode::PrimaryExpression(t),
                _ => SyntaxNode::BadNode(t),
            })
            .unwrap()
    }
}
