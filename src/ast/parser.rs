use std::fmt::Display;

use ascii::AsAsciiStrError;

use crate::{
    diagnostics::DiagnosticBag,
    scope::Scope,
    text::{SourceText, TextSpan},
};

use super::{
    lexer::{Lexer, SyntaxToken, TokenKind},
    Ast, AstNode, AstNodeKind, BinaryOperator, BinaryOperatorKind, UnaryOperator,
    UnaryOperatorKind,
};

pub struct Parser<'a> {
    tokens: Vec<SyntaxToken>,
    current_position: usize,
    pub diagnostics: DiagnosticBag,
    current_scope: Scope<'a>,
}

impl<'a> Parser<'a> {
    fn new(tokens: Vec<SyntaxToken>, diagnostics: DiagnosticBag) -> Parser<'a> {
        assert!(tokens.len() > 0, "Tokens must not be empty");

        Parser {
            tokens,
            current_position: 0,
            diagnostics,
            current_scope: Scope::new(None),
        }
    }

    pub fn parse(text: &str) -> Result<Ast, AsAsciiStrError> {
        let diagnostics = DiagnosticBag::new();
        let src = SourceText::from(&text)?;
        let mut lexer = Lexer::new(&src);
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

        let mut parser = Parser::new(tokens, diagnostics);

        let root = parser.parse_ast();
        Ok(Ast {
            src,
            root,
            diagnostics: parser.diagnostics,
        })
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

    fn parse_ast(&mut self) -> AstNode {
        let program = self.parse_expression(0);

        let eof_token = self.next();
        if !matches!(eof_token.kind, TokenKind::EOF) {
            self.diagnostics
                .report_unexpected_token(&eof_token, TokenKind::EOF);
        }
        program
    }

    fn parse_expression(&mut self, priority: usize) -> AstNode {
        let current = self.current();
        match current.kind {
            TokenKind::LeftCurly => {
                let curly = self.next();
                self.parse_scope(curly)
            }
            TokenKind::Let => {
                let let_token = self.next();
                self.parse_let_declaration(let_token, priority)
            }
            _ => self.parse_binary_expression(priority),
        }
    }

    fn parse_let_declaration(&mut self, let_token: SyntaxToken, priority: usize) -> AstNode {
        let next = self.next();
        let identifier_node = match next.kind {
            TokenKind::Identifier(s) => self.make_node(AstNodeKind::Identifier(s), next.span),
            _ => {
                let span = let_token.span.to(next.span);
                return self.make_bad_node(next, &"<identifier>", span);
            }
        };

        let next = self.next();
        match next.kind {
            TokenKind::Equals => (),
            _ => {
                let span = let_token.span.to(next.span);
                return self.make_bad_node(next, &TokenKind::Equals, span);
            }
        }

        let expr = self.parse_expression(priority);
        let span = let_token.span.to(expr.span);
        self.make_node(AstNodeKind::LetDeclaration(identifier_node, expr), span)
    }

    fn parse_unary_expression(&mut self, priority: usize) -> Option<AstNode> {
        let operator_kind = match self.current().kind {
            TokenKind::Plus => UnaryOperatorKind::Identity,
            TokenKind::Dash => UnaryOperatorKind::Negate,
            TokenKind::Bang => UnaryOperatorKind::LogicalNot,
            _ => return None,
        };
        let operator_token = self.next();

        let operator_prioirty = unary_operator_priority(&operator_token);
        if operator_prioirty < priority {
            return None;
        }
        let operator = UnaryOperator {
            kind: operator_kind,
            span: operator_token.span,
        };

        let expression = self.parse_expression(operator_prioirty);
        let span = operator_token.span.to(expression.span);
        Some(self.make_node(AstNodeKind::UnaryExpression(operator, expression), span))
    }

    fn parse_binary_expression(&mut self, priority: usize) -> AstNode {
        let mut left = self
            .parse_unary_expression(priority)
            .unwrap_or_else(|| self.parse_primary_expression());

        loop {
            let operator_kind = match self.current().kind {
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
            let operator_token = self.next();
            let operator = BinaryOperator {
                kind: operator_kind,
                span: operator_token.span,
            };
            let right = self.parse_expression(operator_priority);
            let span = left.span.to(right.span);
            left = self.make_node(AstNodeKind::BinaryExpression(left, operator, right), span)
        }

        left
    }

    fn parse_primary_expression(&mut self) -> AstNode {
        let next_token = self.next();
        let span = next_token.span;
        match next_token.kind {
            TokenKind::LeftParenthesis => self.parse_group_expression(next_token),
            TokenKind::Integer(i) => self.make_node(AstNodeKind::IntegerLiteral(i), span),
            TokenKind::Boolean(b) => self.make_node(AstNodeKind::BooleanLiteral(b), span),
            TokenKind::Identifier(s) => self.make_node(AstNodeKind::Identifier(s), span),
            _ => self.make_bad_node(next_token, &"<primary expression>", span),
        }
    }

    fn parse_group_expression(&mut self, open: SyntaxToken) -> AstNode {
        let expression = self.parse_expression(0);
        let close = self.next();
        let span = open.span.to(close.span);
        match close.kind {
            TokenKind::RightParenthesis => expression,
            _ => self.make_bad_node(close, &TokenKind::RightParenthesis, span),
        }
    }

    fn parse_scope(&mut self, open: SyntaxToken) -> AstNode {
        let expression = self.parse_expression(0);
        let close = self.next();
        let span = open.span.to(close.span);
        match close.kind {
            TokenKind::RightCurly => self.make_node(AstNodeKind::Scope(expression), span),
            _ => self.make_bad_node(close, &TokenKind::RightCurly, span),
        }
    }

    fn make_node(&mut self, kind: AstNodeKind, span: TextSpan) -> AstNode {
        AstNode {
            kind: Box::new(kind),
            span,
        }
    }

    fn make_bad_node(
        &mut self,
        next_token: SyntaxToken,
        expected_kind: &dyn Display,
        span: TextSpan,
    ) -> AstNode {
        self.diagnostics
            .report_unexpected_token(&next_token, expected_kind);
        AstNode {
            kind: Box::new(AstNodeKind::BadNode),
            span,
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

#[cfg(test)]
mod test {
    use std::fmt::Display;

    use super::*;

    #[derive(PartialEq, Eq, Debug, Clone)]
    enum Matcher {
        Bad,
        Int(i32),
        Bool(bool),
        Ident(String),
        Binary(BinaryOperatorKind),
        Unary(UnaryOperatorKind),
        Scope,
        LetDecl,
    }

    impl Display for Matcher {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let s = match self {
                Matcher::Int(i) => i.to_string(),
                Matcher::Bool(b) => b.to_string(),
                Matcher::Ident(s) => s.to_string(),
                _ => "".to_string(),
            };
            f.write_str(&s)
        }
    }

    struct AssertingIterator {
        text: String,
        nodes: Vec<Matcher>,
        current: usize,
    }

    impl AssertingIterator {
        fn new(text: String) -> Self {
            let mut nodes = vec![];
            let mut stack = vec![];
            let ast = Parser::parse(&text).unwrap();

            let errors: Vec<_> = ast.diagnostics.into_iter().map(|dm| dm.message).collect();
            assert_eq!(errors.len(), 0, "Had errors parsing: {:?}", errors);

            stack.push(ast.root);

            while let Some(next) = stack.pop() {
                let matcher = match *next.kind {
                    AstNodeKind::BadNode => Matcher::Bad,
                    AstNodeKind::IntegerLiteral(i) => Matcher::Int(i),
                    AstNodeKind::BooleanLiteral(b) => Matcher::Bool(b),
                    AstNodeKind::Identifier(s) => Matcher::Ident(s),
                    AstNodeKind::Scope(expr) => {
                        stack.push(expr);
                        Matcher::Scope
                    }
                    AstNodeKind::BinaryExpression(l, op, r) => {
                        stack.push(r);
                        stack.push(l);
                        Matcher::Binary(op.kind)
                    }
                    AstNodeKind::UnaryExpression(op, expr) => {
                        stack.push(expr);
                        Matcher::Unary(op.kind)
                    }
                    AstNodeKind::LetDeclaration(identifier, expr) => {
                        stack.push(expr);
                        stack.push(identifier);
                        Matcher::LetDecl
                    }
                };
                nodes.push(matcher);
            }

            AssertingIterator {
                text,
                nodes,
                current: 0,
            }
        }

        fn assert(&mut self, matcher: Matcher) {
            assert!(
                self.current < self.nodes.len(),
                "Unable to match {:?}, ran out of tokens",
                matcher
            );
            assert_eq!(
                self.nodes[self.current], matcher,
                "'{}'\n{:?}",
                self.text, self.nodes
            );
            self.current += 1;
        }

        fn finish(&mut self) {
            assert_eq!(self.current, self.nodes.len(), "Did not match all nodes");
        }
    }

    #[test]
    fn test_unary_operator_stops_at_binary() {
        binary_operators()
            .into_iter()
            .flat_map(|bin| unary_operators().into_iter().map(move |una| (bin, una)))
            .for_each(|(bin, una)| {
                primary_matchers()
                    .into_iter()
                    .flat_map(move |id1| {
                        primary_matchers()
                            .into_iter()
                            .map(move |id2| (id1.clone(), id2.clone()))
                    })
                    .for_each(|(id1, id2)| {
                        let s = format!("{}{} {} {}", una, id1, bin, id2);
                        let mut asserter = AssertingIterator::new(s);
                        asserter.assert(Matcher::Binary(bin));
                        asserter.assert(Matcher::Unary(una));
                        asserter.assert(id1);
                        asserter.assert(id2);

                        asserter.finish();
                    })
            })
    }

    #[test]
    fn test_binary_operator_consumes_unary() {
        binary_operators()
            .into_iter()
            .flat_map(|bin| unary_operators().into_iter().map(move |una| (bin, una)))
            .for_each(|(bin, una)| {
                primary_matchers()
                    .into_iter()
                    .flat_map(move |id1| {
                        primary_matchers()
                            .into_iter()
                            .map(move |id2| (id1.clone(), id2.clone()))
                    })
                    .for_each(|(id1, id2)| {
                        let s = format!("{} {} {}{}", id1, bin, una, id2);
                        let mut asserter = AssertingIterator::new(s);
                        asserter.assert(Matcher::Binary(bin));
                        asserter.assert(id1);
                        asserter.assert(Matcher::Unary(una));
                        asserter.assert(id2);

                        asserter.finish();
                    })
            })
    }

    #[test]
    fn test_scope() {
        let text = "{1 + 2} - 4".to_string();
        let mut asserter = AssertingIterator::new(text);

        asserter.assert(Matcher::Binary(BinaryOperatorKind::Subtract));
        asserter.assert(Matcher::Scope);
        asserter.assert(Matcher::Binary(BinaryOperatorKind::Add));
        asserter.assert(Matcher::Int(1));
        asserter.assert(Matcher::Int(2));
        asserter.assert(Matcher::Int(4));
    }

    #[test]
    fn test_full() {
        let text = "a + 2 * (!true && false) | (test / -+3)".to_string();
        let mut asserter = AssertingIterator::new(text);

        asserter.assert(Matcher::Binary(BinaryOperatorKind::BitwiseOr));
        asserter.assert(Matcher::Binary(BinaryOperatorKind::Add));
        asserter.assert(Matcher::Ident("a".to_string()));
        asserter.assert(Matcher::Binary(BinaryOperatorKind::Mulitply));
        asserter.assert(Matcher::Int(2));
        asserter.assert(Matcher::Binary(BinaryOperatorKind::LogicalAnd));
        asserter.assert(Matcher::Unary(UnaryOperatorKind::LogicalNot));
        asserter.assert(Matcher::Bool(true));
        asserter.assert(Matcher::Bool(false));
        asserter.assert(Matcher::Binary(BinaryOperatorKind::Divide));
        asserter.assert(Matcher::Ident("test".to_string()));
        asserter.assert(Matcher::Unary(UnaryOperatorKind::Negate));
        asserter.assert(Matcher::Unary(UnaryOperatorKind::Identity));
        asserter.assert(Matcher::Int(3));

        asserter.finish();
    }

    fn binary_operators() -> Vec<BinaryOperatorKind> {
        vec![
            BinaryOperatorKind::Add,
            BinaryOperatorKind::Subtract,
            BinaryOperatorKind::Mulitply,
            BinaryOperatorKind::Divide,
            BinaryOperatorKind::LogicalAnd,
            BinaryOperatorKind::LogicalOr,
            BinaryOperatorKind::BitwiseAnd,
            BinaryOperatorKind::BitwiseOr,
            BinaryOperatorKind::Equals,
            BinaryOperatorKind::NotEquals,
            BinaryOperatorKind::LessThan,
            BinaryOperatorKind::LessThanOrEquals,
            BinaryOperatorKind::GreaterThan,
            BinaryOperatorKind::GreaterThanOrEquals,
        ]
    }

    fn unary_operators() -> Vec<UnaryOperatorKind> {
        vec![
            UnaryOperatorKind::Identity,
            UnaryOperatorKind::Negate,
            UnaryOperatorKind::LogicalNot,
        ]
    }

    fn primary_matchers() -> Vec<Matcher> {
        vec![
            Matcher::Int(1),
            Matcher::Int(2),
            Matcher::Bool(false),
            Matcher::Bool(true),
            Matcher::Ident("a".to_string()),
            Matcher::Ident("b".to_string()),
        ]
    }
}
