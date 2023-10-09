use crate::diagnostics::DiagnosticBag;

use super::{lexer::{SyntaxToken, Lexer, TokenKind}, Ast, AstNode, UnaryOperatorKind, AstNodeKind, BinaryOperatorKind};

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
            TokenKind::PlusToken => UnaryOperatorKind::Identity,
            TokenKind::DashToken => UnaryOperatorKind::Negate,
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
                TokenKind::PlusToken => BinaryOperatorKind::Add,
                TokenKind::DashToken => BinaryOperatorKind::Subtract,
                TokenKind::StarToken => BinaryOperatorKind::Mulitply,
                TokenKind::SlashToken => BinaryOperatorKind::Divide,
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
        match self.current().kind {
            TokenKind::Integer(i) => self.parse_integer_expression(i),
            TokenKind::LeftParenthesisToken => self.parse_group_expression(),
            _ => {
                let start_span = self.current().span;
                let bad_token = self.next();
                let span = start_span.to(bad_token.span);
                self.diagnostics
                    .report_unexpected_token(bad_token, "<primary expression>");
                AstNode {
                    kind: AstNodeKind::BadNode,
                    span,
                }
            }
        }
    }

    fn parse_group_expression(&mut self) -> AstNode {
        let open = self.next();
        let expression = self.parse_expression(0);
        let close = self.next();
        let span = open.span.to(close.span);
        match close.kind {
            TokenKind::RightParenthesisToken => expression,
            _ => {
                self.diagnostics
                    .report_unexpected_token(close, TokenKind::RightParenthesisToken);
                AstNode {
                    kind: AstNodeKind::BadNode,
                    span,
                }
            }
        }
    }

    fn parse_integer_expression(&mut self, value: i32) -> AstNode {
        let token = self.next();
        let span = token.span;
        AstNode {
            kind: AstNodeKind::IntegerLiteral(value),
            span,
        }
    }
}

fn unary_operator_priority(token: &SyntaxToken) -> usize {
    match token.kind {
        TokenKind::PlusToken | TokenKind::DashToken => 3,
        _ => 0,
    }
}

fn binary_operator_priority(token: &SyntaxToken) -> usize {
    match token.kind {
        TokenKind::StarToken | TokenKind::SlashToken => 2,
        TokenKind::PlusToken | TokenKind::DashToken => 1,
        _ => 0,
    }
}
