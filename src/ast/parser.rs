use crate::diagnostics::DiagnosticBag;

use super::{
    lexer::{Lexer, SyntaxToken, TokenKind},
    Ast, AstNode, AstNodeKind, BinaryOperatorKind, UnaryOperatorKind,
};

pub struct Parser {
    tokens: Vec<SyntaxToken>,
    current_position: usize,
    pub diagnostics: DiagnosticBag,
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

    pub fn parse(text: String) -> Ast {
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

        parser.parse_ast()
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

    fn parse_ast(mut self) -> Ast {
        let program = self.parse_expression(0);

        let eof_token = self.next();
        if !matches!(eof_token.kind, TokenKind::EOF) {
            self.diagnostics
                .report_unexpected_token(eof_token, TokenKind::EOF);
        }
        Ast {
            root: program,
            diagnostics: self.diagnostics,
        }
    }

    fn parse_expression(&mut self, priority: usize) -> AstNode {
        self.parse_binary_expression(priority)
    }

    fn parse_unary_expression(&mut self, priority: usize) -> Option<AstNode> {
        let operator = match self.current().kind {
            TokenKind::Plus => UnaryOperatorKind::Identity,
            TokenKind::Dash => UnaryOperatorKind::Negate,
            TokenKind::Bang => UnaryOperatorKind::LogicalNot,
            _ => return None,
        };
        let next_token = self.next();

        let operator_prioirty = unary_operator_priority(&next_token);
        if operator_prioirty < priority {
            return None;
        }

        let expression = self.parse_expression(operator_prioirty);
        let span = next_token.span.to(expression.span);
        Some(AstNode {
            kind: AstNodeKind::UnaryExpression(operator, Box::new(expression)),
            span,
        })
    }

    fn parse_binary_expression(&mut self, priority: usize) -> AstNode {
        let mut left = self
            .parse_unary_expression(priority)
            .unwrap_or_else(|| self.parse_primary_expression());

        loop {
            let operator_token = match self.current().kind {
                TokenKind::Plus => BinaryOperatorKind::Add,
                TokenKind::Dash => BinaryOperatorKind::Subtract,
                TokenKind::Star => BinaryOperatorKind::Mulitply,
                TokenKind::Slash => BinaryOperatorKind::Divide,
                TokenKind::AmpersandAmpersand => BinaryOperatorKind::LogicalAnd,
                TokenKind::Ampersand => BinaryOperatorKind::BitwiseAnd,
                TokenKind::PipePipe => BinaryOperatorKind::LogicalOr,
                TokenKind::Pipe => BinaryOperatorKind::BitwiseOr,
                TokenKind::EqualsEquals => BinaryOperatorKind::Equals,
                TokenKind::BangEquals => BinaryOperatorKind::NotEquals,
                TokenKind::LeftAngleEquals => BinaryOperatorKind::LessThanOrEquals,
                TokenKind::LeftAngleBracket => BinaryOperatorKind::LessThan,
                TokenKind::RightAngleEquals => BinaryOperatorKind::GreaterThanOrEquals,
                TokenKind::RightAngleBracket => BinaryOperatorKind::GreaterThan,
                _ => break,
            };
            let operator_priority = binary_operator_priority(self.current());
            if operator_priority <= priority {
                break;
            }
            // Important to consume token here
            self.next();
            let right = self.parse_expression(operator_priority);
            let span = left.span.to(right.span);
            left = AstNode {
                kind: AstNodeKind::BinaryExpression(
                    Box::new(left),
                    operator_token,
                    Box::new(right),
                ),
                span,
            }
        }

        left
    }

    fn parse_primary_expression(&mut self) -> AstNode {
        let next_token = self.next();
        let span = next_token.span;
        match next_token.kind {
            TokenKind::Integer(i) => AstNode {
                kind: AstNodeKind::IntegerLiteral(i),
                span,
            },
            TokenKind::Boolean(b) => AstNode {
                kind: AstNodeKind::BooleanLiteral(b),
                span,
            },
            TokenKind::Identifier(s) => AstNode {
                kind: AstNodeKind::Identifier(s),
                span,
            },
            TokenKind::LeftParenthesis => self.parse_group_expression(next_token),
            _ => {
                self.diagnostics
                    .report_unexpected_token(next_token, "<primary expression>");
                AstNode {
                    kind: AstNodeKind::BadNode,
                    span,
                }
            }
        }
    }

    fn parse_group_expression(&mut self, open: SyntaxToken) -> AstNode {
        let expression = self.parse_expression(0);
        let close = self.next();
        let span = open.span.to(close.span);
        match close.kind {
            TokenKind::RightParenthesis => expression,
            _ => {
                self.diagnostics
                    .report_unexpected_token(close, TokenKind::RightParenthesis);
                AstNode {
                    kind: AstNodeKind::BadNode,
                    span,
                }
            }
        }
    }
}

fn unary_operator_priority(token: &SyntaxToken) -> usize {
    match token.kind {
        TokenKind::Plus | TokenKind::Dash | TokenKind::Bang => 13,
        _ => 0,
    }
}

fn binary_operator_priority(token: &SyntaxToken) -> usize {
    match token.kind {
        TokenKind::Star | TokenKind::Slash => 12,
        TokenKind::Plus | TokenKind::Dash => 11,
        TokenKind::RightAngleEquals
        | TokenKind::RightAngleBracket
        | TokenKind::LeftAngleBracket
        | TokenKind::LeftAngleEquals
        | TokenKind::EqualsEquals
        | TokenKind::BangEquals => 8,
        TokenKind::Ampersand => 7,
        TokenKind::Pipe => 5,
        TokenKind::AmpersandAmpersand => 4,
        TokenKind::PipePipe => 3,
        _ => 0,
    }
}
