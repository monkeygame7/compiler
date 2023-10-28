use std::rc::Rc;

use crate::{
    ast::{
        nodes::{
            BinaryOperator, BinaryOperatorKind, ElseClause, ItemKind, TypeDecl, UnaryOperator,
            UnaryOperatorKind,
        },
        Ast, ExprId, ItemId, StmtId,
    },
    diagnostics::DiagnosticBag,
};

use super::{Lexer, SyntaxToken, TokenKind};

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
        let type_decl = self.parse_optional_type_decl();
        let equals_token = self.expect(TokenKind::Equals);
        let expr = self.parse_expr(0);

        self.ast
            .create_let_stmt(keyword, identifier, type_decl, equals_token, expr)
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

    fn parse_optional_type_decl(&mut self) -> Option<TypeDecl> {
        let current_kind = &self.current().kind;
        match current_kind {
            TokenKind::Colon => {
                let colon = self.next();
                let typ = self.expect(TokenKind::Identifier);
                Some(TypeDecl { colon, typ })
            }
            _ => None,
        }
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
            nodes::{
                AssignExpr, BinaryExpr, BooleanExpr, Expr, IfExpr, IntegerExpr, LetStmt, Stmt,
                UnaryExpr, VariableExpr, WhileStmt,
            },
            AstVisitor,
        },
        diagnostics::{SourceText, TextSpan},
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
        Assign(String),
        TypeDecl(String),
        If,
        Else,
        While,
    }

    impl Matcher {
        fn ident(s: &str) -> Self {
            Matcher::Ident(s.to_string())
        }

        fn let_decl(s: &str) -> Self {
            Matcher::LetDecl(s.to_string())
        }

        fn assign(s: &str) -> Self {
            Matcher::Assign(s.to_string())
        }

        fn type_decl(s: &str) -> Self {
            Matcher::TypeDecl(s.to_string())
        }
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
        ast_s: String,
    }

    impl AssertingIterator {
        fn new(text: &str) -> Self {
            let src = SourceText::from(&text).unwrap();
            let diagnostics = Rc::new(DiagnosticBag::new());
            let lexer = Lexer::new(&src);
            let ast = Parser::parse(lexer, diagnostics.clone());

            let errors: Vec<_> = diagnostics
                .messages
                .borrow()
                .iter()
                .map(|dm| dm.message.clone())
                .collect();
            let ast_s = format!("{:?}", ast);
            assert_eq!(
                errors.len(),
                0,
                "Had errors parsing: {:?}\n{}\n",
                errors,
                ast_s
            );

            let mut iter = AssertingIterator {
                text: src,
                nodes: vec![],
                current: 0,
                ast_s,
            };

            ast.visit(&mut iter);

            iter
        }

        fn assert(&mut self, matcher: Matcher) {
            assert!(
                self.current < self.nodes.len(),
                "Unable to match {:?}, ran out of tokens\n{}\n",
                matcher,
                self.ast_s
            );
            assert_eq!(
                self.nodes[self.current], matcher,
                "'{}'\n{:?}",
                self.text.text, self.nodes
            );
            self.current += 1;
        }

        fn finish(&mut self) {
            assert_eq!(
                self.current,
                self.nodes.len(),
                "Did not match all nodes: {:?}\n{}\n",
                &self.nodes[self.current..],
                self.ast_s
            );
        }
    }

    impl AstVisitor for AssertingIterator {
        fn visit_func_decl(&mut self, _ast: &Ast, _func: &crate::ast::nodes::FunctionDecl) {
            todo!()
        }

        fn visit_let_stmt(&mut self, ast: &Ast, let_stmt: &LetStmt, _stmt: &Stmt) {
            self.nodes
                .push(Matcher::let_decl(&let_stmt.identifier.literal));
            if let Some(typ) = &let_stmt.type_decl {
                self.nodes.push(Matcher::type_decl(&typ.typ.literal))
            }
            self.visit_expr(ast, let_stmt.initial);
        }

        fn visit_while_stmt(&mut self, ast: &Ast, while_stmt: &WhileStmt, _stmt: &Stmt) {
            self.nodes.push(Matcher::While);
            self.visit_expr(ast, while_stmt.condition);
            self.visit_expr(ast, while_stmt.body);
        }

        fn visit_error(&mut self, _ast: &Ast, _span: &TextSpan, _expr: &Expr) {
            self.nodes.push(Matcher::Bad);
        }

        fn visit_integer_expr(&mut self, _ast: &Ast, int_expr: &IntegerExpr, _expr: &Expr) {
            self.nodes.push(Matcher::Int(int_expr.value));
        }

        fn visit_boolean_expr(&mut self, _ast: &Ast, bool_expr: &BooleanExpr, _expr: &Expr) {
            self.nodes.push(Matcher::Bool(bool_expr.value));
        }

        fn visit_assign_expr(&mut self, ast: &Ast, assign_expr: &AssignExpr, _expr: &Expr) {
            self.nodes
                .push(Matcher::assign(&assign_expr.identifier.literal));
            self.visit_expr(ast, assign_expr.rhs);
        }

        fn visit_binary_expr(&mut self, ast: &Ast, binary_expr: &BinaryExpr, _expr: &Expr) {
            self.nodes.push(Matcher::Binary(binary_expr.operator.kind));
            self.visit_expr(ast, binary_expr.left);
            self.visit_expr(ast, binary_expr.right);
        }

        fn visit_unary_expr(&mut self, ast: &Ast, unary_expr: &UnaryExpr, _expr: &Expr) {
            self.nodes.push(Matcher::Unary(unary_expr.operator.kind));
            self.visit_expr(ast, unary_expr.operand);
        }

        fn visit_variable_expr(&mut self, _ast: &Ast, variable_expr: &VariableExpr, _expr: &Expr) {
            self.nodes
                .push(Matcher::ident(&variable_expr.token.literal));
        }

        fn visit_if_expr(&mut self, ast: &Ast, if_expr: &IfExpr, _expr: &Expr) {
            self.nodes.push(Matcher::If);
            self.visit_expr(ast, if_expr.condition);
            self.visit_expr(ast, if_expr.then_clause);
            if let Some(else_clause) = &if_expr.else_clause {
                self.nodes.push(Matcher::Else);
                self.visit_expr(ast, else_clause.body);
            }
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
                        let mut asserter = AssertingIterator::new(&s);
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
                        let mut asserter = AssertingIterator::new(&s);
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
        let text = "a + 2 * (!true && false) | (test / -+3)";
        let mut asserter = AssertingIterator::new(text);

        asserter.assert(Matcher::Binary(BinaryOperatorKind::BitwiseOr));
        asserter.assert(Matcher::Binary(BinaryOperatorKind::Add));
        asserter.assert(Matcher::ident("a"));
        asserter.assert(Matcher::Binary(BinaryOperatorKind::Mulitply));
        asserter.assert(Matcher::Int(2));
        asserter.assert(Matcher::Binary(BinaryOperatorKind::LogicalAnd));
        asserter.assert(Matcher::Unary(UnaryOperatorKind::LogicalNot));
        asserter.assert(Matcher::Bool(true));
        asserter.assert(Matcher::Bool(false));
        asserter.assert(Matcher::Binary(BinaryOperatorKind::Divide));
        asserter.assert(Matcher::ident("test"));
        asserter.assert(Matcher::Unary(UnaryOperatorKind::Negate));
        asserter.assert(Matcher::Unary(UnaryOperatorKind::Identity));
        asserter.assert(Matcher::Int(3));

        asserter.finish();
    }

    #[test]
    fn test_assignment() {
        let text = "x = 4";
        let mut asserter = AssertingIterator::new(text);

        asserter.assert(Matcher::assign("x"));
        asserter.assert(Matcher::Int(4));
    }

    #[test]
    fn test_let() {
        let text = "let x = 4";
        let mut asserter = AssertingIterator::new(text);

        asserter.assert(Matcher::let_decl("x"));
        asserter.assert(Matcher::Int(4));
    }

    #[test]
    fn test_let_with_type() {
        let text = "let x: int = 4";
        let mut asserter = AssertingIterator::new(text);

        asserter.assert(Matcher::let_decl("x"));
        asserter.assert(Matcher::type_decl("int"));
        asserter.assert(Matcher::Int(4));
    }

    #[test]
    fn test_if() {
        let text = "if (a < 2) 1 else 3 + 2";
        let mut asserter = AssertingIterator::new(text);

        asserter.assert(Matcher::If);
        asserter.assert(Matcher::Binary(BinaryOperatorKind::LessThan));
        asserter.assert(Matcher::ident("a"));
        asserter.assert(Matcher::Int(2));
        asserter.assert(Matcher::Int(1));
        asserter.assert(Matcher::Else);
        asserter.assert(Matcher::Binary(BinaryOperatorKind::Add));
        asserter.assert(Matcher::Int(3));
        asserter.assert(Matcher::Int(2));
    }

    #[test]
    fn test_while() {
        let text = "while (i < 10) { i = i + 1 }";
        let mut asserter = AssertingIterator::new(text);

        asserter.assert(Matcher::While);
        asserter.assert(Matcher::Binary(BinaryOperatorKind::LessThan));
        asserter.assert(Matcher::ident("i"));
        asserter.assert(Matcher::Int(10));

        // Inside the while block
        asserter.assert(Matcher::assign("i"));
        asserter.assert(Matcher::Binary(BinaryOperatorKind::Add));
        asserter.assert(Matcher::ident("i"));
        asserter.assert(Matcher::Int(1));

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
            Matcher::ident("a"),
            Matcher::ident("b"),
        ]
    }
}
