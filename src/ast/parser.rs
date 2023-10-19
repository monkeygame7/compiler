use std::rc::Rc;

use crate::diagnostics::DiagnosticBag;

use super::{
    lexer::{Lexer, SyntaxToken, TokenKind},
    Ast, BinaryOperator, BinaryOperatorKind, ElseClause, ExprId, ItemId, ItemKind, StmtId,
    UnaryOperator, UnaryOperatorKind,
};

pub struct Parser<'a> {
    ast: &'a mut Ast,
    tokens: Vec<SyntaxToken>,
    current_position: usize,
    pub diagnostics: Rc<DiagnosticBag>,
}

impl<'a> Parser<'a> {
    fn new(
        ast: &'a mut Ast,
        tokens: Vec<SyntaxToken>,
        diagnostics: Rc<DiagnosticBag>,
    ) -> Parser<'a> {
        assert!(tokens.len() > 0, "Tokens must not be empty");

        Parser {
            ast,
            tokens,
            current_position: 0,
            diagnostics,
        }
    }

    pub fn parse(mut lexer: Lexer, diagnostics: Rc<DiagnosticBag>) -> Ast {
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

        let mut ast = Ast::new();
        let mut parser = Parser::new(&mut ast, tokens, diagnostics);

        parser.parse_ast();

        ast
    }

    fn is_done(&self) -> bool {
        self.current().kind == TokenKind::EOF
    }

    fn current(&self) -> &SyntaxToken {
        self.peek(0)
    }

    fn peek(&self, offset: usize) -> &SyntaxToken {
        let idx = self.current_position + offset;
        if idx >= self.tokens.len() {
            self.tokens.last().unwrap()
        } else {
            &self.tokens[idx]
        }
    }

    fn next(&mut self) -> SyntaxToken {
        let current = &self.tokens[self.current_position];
        if self.current_position < self.tokens.len() - 1 {
            self.current_position += 1;
        }
        current.clone()
    }

    fn parse_ast(&mut self) {
        while !self.is_done() {
            self.parse_item();
        }
    }

    fn parse_item(&mut self) -> ItemId {
        let stmt = self.parse_stmt();
        self.ast.create_item(ItemKind::Stmt(stmt))
    }

    fn parse_stmt(&mut self) -> StmtId {
        let id = match self.current().kind {
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            _ => self.parse_expr_stmt(),
        };

        id
    }

    fn parse_expr_stmt(&mut self) -> StmtId {
        let id = self.parse_expr(0);
        self.ast.create_expr_stmt(id)
        // TODO: consume semicolon
    }

    fn parse_let_stmt(&mut self) -> StmtId {
        let keyword = self.expect(TokenKind::Let);
        let identifier = self.expect(TokenKind::Identifier);
        let equals_token = self.expect(TokenKind::Equals);
        let expr = self.parse_expr(0);

        self.ast
            .create_let_stmt(keyword, identifier, equals_token, expr)
    }

    fn parse_while_stmt(&mut self) -> StmtId {
        let keyword = self.expect(TokenKind::While);
        let condition = self.parse_expr(0);
        let body = self.parse_expr(0);

        self.ast.create_while_stmt(keyword, condition, body)
    }

    fn parse_expr(&mut self, priority: usize) -> ExprId {
        let current_kind = &self.current().kind;
        let next_kind = &self.peek(1).kind;
        match (current_kind, next_kind) {
            (TokenKind::LeftCurly, _) => self.parse_block_expr(),
            (TokenKind::Identifier, TokenKind::Equals) => self.parse_assign_expr(),
            (TokenKind::If, _) => self.parse_if_expr(),
            _ => self.parse_binary_expr(priority),
        }
    }

    fn parse_block_expr(&mut self) -> ExprId {
        let open_token = self.expect(TokenKind::LeftCurly);
        let mut stmts = vec![];
        while self.current().kind != TokenKind::RightCurly && !self.is_done() {
            let stmt_id = self.parse_stmt();
            stmts.push(stmt_id);
        }

        let close_token = self.expect(TokenKind::RightCurly);
        self.ast.create_block_expr(open_token, stmts, close_token)
    }

    fn parse_assign_expr(&mut self) -> ExprId {
        let lhs = self.expect(TokenKind::Identifier);
        let equals = self.expect(TokenKind::Equals);
        let rhs = self.parse_expr(0);

        self.ast.create_assign_expr(lhs, equals, rhs)
    }

    fn parse_if_expr(&mut self) -> ExprId {
        let identifier = self.expect(TokenKind::If);
        let condition = self.parse_expr(0);
        let then_clause = self.parse_expr(0);
        let else_clause = self.parse_else_clause();

        self.ast
            .create_if_expr(identifier, condition, then_clause, else_clause)
    }

    fn parse_else_clause(&mut self) -> Option<ElseClause> {
        if self.current().kind == TokenKind::Else {
            let identifier = self.expect(TokenKind::Else);
            let body = self.parse_expr(0);
            let body_span = self.ast.query_expr(body).span;
            let span = identifier.span.to(body_span);

            Some(ElseClause {
                keyword: identifier,
                body,
                span,
            })
        } else {
            None
        }
    }

    fn parse_binary_expr(&mut self, priority: usize) -> ExprId {
        let mut left = self
            .parse_unary_expr(priority)
            .unwrap_or_else(|| self.parse_primary_expr());

        while let Some(operator) = self.parse_binary_operator(priority) {
            let right = self.parse_expr(operator.kind.priority());
            left = self.ast.create_binary_expr(left, operator, right);
        }

        left
    }

    fn parse_binary_operator(&mut self, priority: usize) -> Option<BinaryOperator> {
        let kind = match self.current().kind {
            TokenKind::Plus => Some(BinaryOperatorKind::Add),
            TokenKind::Dash => Some(BinaryOperatorKind::Subtract),
            TokenKind::Star => Some(BinaryOperatorKind::Mulitply),
            TokenKind::Slash => Some(BinaryOperatorKind::Divide),
            TokenKind::AmpersandAmpersand => Some(BinaryOperatorKind::LogicalAnd),
            TokenKind::Ampersand => Some(BinaryOperatorKind::BitwiseAnd),
            TokenKind::PipePipe => Some(BinaryOperatorKind::LogicalOr),
            TokenKind::Pipe => Some(BinaryOperatorKind::BitwiseOr),
            TokenKind::EqualsEquals => Some(BinaryOperatorKind::Equals),
            TokenKind::BangEquals => Some(BinaryOperatorKind::NotEquals),
            TokenKind::LeftAngleEquals => Some(BinaryOperatorKind::LessThanOrEquals),
            TokenKind::LeftAngleBracket => Some(BinaryOperatorKind::LessThan),
            TokenKind::RightAngleEquals => Some(BinaryOperatorKind::GreaterThanOrEquals),
            TokenKind::RightAngleBracket => Some(BinaryOperatorKind::GreaterThan),
            _ => None,
        };

        kind.filter(|kind| kind.priority() >= priority)
            .map(|kind| BinaryOperator {
                kind,
                token: self.next(),
            })
    }

    fn parse_unary_expr(&mut self, priority: usize) -> Option<ExprId> {
        self.parse_unary_operator(priority).map(|op| {
            let operand = self.parse_expr(op.kind.priority());
            self.ast.create_unary_expr(op, operand)
        })
    }

    fn parse_unary_operator(&mut self, priority: usize) -> Option<UnaryOperator> {
        let kind = match self.current().kind {
            TokenKind::Plus => Some(UnaryOperatorKind::Identity),
            TokenKind::Dash => Some(UnaryOperatorKind::Negate),
            TokenKind::Bang => Some(UnaryOperatorKind::LogicalNot),
            _ => None,
        };

        kind.filter(|kind| kind.priority() >= priority)
            .map(|kind| UnaryOperator {
                kind,
                token: self.next(),
            })
    }

    fn parse_primary_expr(&mut self) -> ExprId {
        let token = self.next();
        let id = match token.kind {
            TokenKind::LeftParenthesis => {
                let open = token;
                let expr = self.parse_expr(0);
                let close = self.expect(TokenKind::RightParenthesis);
                self.ast.create_paren_expr(open, expr, close)
            }
            TokenKind::Integer(value) => self.ast.create_integer_expr(value, token),
            TokenKind::Boolean(value) => self.ast.create_boolean_expr(value, token),
            TokenKind::Identifier => self.ast.create_variable_expr(token),
            _ => {
                self.diagnostics.report_expected_expression(&token);
                self.ast.create_error_expr(token.span)
            }
        };

        id
    }

    fn expect(&mut self, expected_kind: TokenKind) -> SyntaxToken {
        let next = self.next();
        if next.kind != expected_kind {
            self.diagnostics
                .report_unexpected_token(&next, &expected_kind)
        }
        next
    }
}

#[cfg(test)]
mod test {
    use std::fmt::Display;

    use crate::{
        ast::{
            visitor::AstVisitor, BinaryExpr, BooleanExpr, Expr, IntegerExpr, LetStmt, Stmt,
            UnaryExpr, VariableExpr,
        },
        text::SourceText,
    };

    use super::*;

    #[derive(PartialEq, Eq, Debug, Clone)]
    enum Matcher {
        Bad,
        Int(i32),
        Bool(bool),
        Ident(String),
        Binary(BinaryOperatorKind),
        Unary(UnaryOperatorKind),
        LetDecl(String),
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
        text: SourceText,
        nodes: Vec<Matcher>,
        current: usize,
    }

    impl AssertingIterator {
        fn new(text: String) -> Self {
            let src = SourceText::from(&text).unwrap();
            let diagnostics = Rc::new(DiagnosticBag::new());
            let lexer = Lexer::new(&src);
            let mut ast = Parser::parse(lexer, diagnostics.clone());

            let errors: Vec<_> = diagnostics
                .messages
                .borrow()
                .iter()
                .map(|dm| dm.message.clone())
                .collect();
            assert_eq!(errors.len(), 0, "Had errors parsing: {:?}", errors);

            let mut iter = AssertingIterator {
                text: src,
                nodes: vec![],
                current: 0,
            };

            ast.visit(&mut iter);

            iter
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
                self.text.text, self.nodes
            );
            self.current += 1;
        }

        fn finish(&mut self) {
            assert_eq!(self.current, self.nodes.len(), "Did not match all nodes");
        }
    }

    impl AstVisitor for AssertingIterator {
        fn visit_let_stmt(&mut self, ast: &mut Ast, let_stmt: &LetStmt, _stmt: &Stmt) {
            self.nodes
                .push(Matcher::LetDecl(let_stmt.identifier.literal.clone()));
            self.visit_expr(ast, let_stmt.initial);
        }

        fn visit_error(&mut self, _ast: &mut Ast, _span: &crate::text::TextSpan, _expr: &Expr) {
            self.nodes.push(Matcher::Bad);
        }

        fn visit_integer_expr(&mut self, _ast: &mut Ast, int_expr: &IntegerExpr, _expr: &Expr) {
            self.nodes.push(Matcher::Int(int_expr.value));
        }

        fn visit_boolean_expr(&mut self, _ast: &mut Ast, bool_expr: &BooleanExpr, _expr: &Expr) {
            self.nodes.push(Matcher::Bool(bool_expr.value));
        }

        fn visit_binary_expr(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, _expr: &Expr) {
            self.nodes.push(Matcher::Binary(binary_expr.operator.kind));
            self.visit_expr(ast, binary_expr.left);
            self.visit_expr(ast, binary_expr.right);
        }

        fn visit_unary_expr(&mut self, ast: &mut Ast, unary_expr: &UnaryExpr, _expr: &Expr) {
            self.nodes.push(Matcher::Unary(unary_expr.operator.kind));
            self.visit_expr(ast, unary_expr.operand);
        }

        fn visit_variable_expr(
            &mut self,
            _ast: &mut Ast,
            variable_expr: &VariableExpr,
            _expr: &Expr,
        ) {
            self.nodes
                .push(Matcher::Ident(variable_expr.token.literal.clone()));
        }

        fn visit_assign_expr(
            &mut self,
            _ast: &mut Ast,
            _assign_expr: &crate::ast::AssignExpr,
            _expr: &Expr,
        ) {
            todo!()
        }

        fn visit_if_expr(&mut self, _ast: &mut Ast, _if_expr: &crate::ast::IfExpr, _expr: &Expr) {
            todo!()
        }

        fn visit_while_stmt(
            &mut self,
            _ast: &mut Ast,
            _while_stmt: &crate::ast::WhileStmt,
            _stmt: &Stmt,
        ) {
            todo!()
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
                        let s = format!("{}{} {} {}", una.1, id1, bin.1, id2);
                        let mut asserter = AssertingIterator::new(s);
                        asserter.assert(Matcher::Binary(bin.0));
                        asserter.assert(Matcher::Unary(una.0));
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
                        let s = format!("{} {} {}{}", id1, bin.1, una.1, id2);
                        let mut asserter = AssertingIterator::new(s);
                        asserter.assert(Matcher::Binary(bin.0));
                        asserter.assert(id1);
                        asserter.assert(Matcher::Unary(una.0));
                        asserter.assert(id2);

                        asserter.finish();
                    })
            })
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

    fn binary_operators() -> Vec<(BinaryOperatorKind, &'static str)> {
        vec![
            (BinaryOperatorKind::Add, "+"),
            (BinaryOperatorKind::Subtract, "-"),
            (BinaryOperatorKind::Mulitply, "*"),
            (BinaryOperatorKind::Divide, "/"),
            (BinaryOperatorKind::LogicalAnd, "&&"),
            (BinaryOperatorKind::LogicalOr, "||"),
            (BinaryOperatorKind::BitwiseAnd, "&"),
            (BinaryOperatorKind::BitwiseOr, "|"),
            (BinaryOperatorKind::Equals, "=="),
            (BinaryOperatorKind::NotEquals, "!="),
            (BinaryOperatorKind::LessThan, "<"),
            (BinaryOperatorKind::LessThanOrEquals, "<="),
            (BinaryOperatorKind::GreaterThan, ">"),
            (BinaryOperatorKind::GreaterThanOrEquals, ">="),
        ]
    }

    fn unary_operators() -> Vec<(UnaryOperatorKind, &'static str)> {
        vec![
            (UnaryOperatorKind::Identity, "+"),
            (UnaryOperatorKind::Negate, "-"),
            (UnaryOperatorKind::LogicalNot, "!"),
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
