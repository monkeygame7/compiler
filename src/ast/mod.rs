use std::fmt::Display;

use crate::{compilation::Type, id::Idx, id::IdxVec, idx, scope::VariableId, text::TextSpan};

use self::{lexer::SyntaxToken, printer::AstPrinter, visitor::AstVisitor};

pub mod lexer;
pub mod parser;
pub mod printer;
pub mod visitor;

idx!(ItemId);
idx!(StmtId);
idx!(ExprId);

pub struct Ast {
    statements: IdxVec<StmtId, Stmt>,
    expressions: IdxVec<ExprId, Expr>,
    items: IdxVec<ItemId, Item>,
}

impl Ast {
    pub fn new() -> Self {
        Ast {
            statements: IdxVec::new(),
            expressions: IdxVec::new(),
            items: IdxVec::new(),
        }
    }

    pub fn visit(&mut self, visitor: &mut dyn AstVisitor) {
        for item in self.items.clone().iter() {
            visitor.visit_item(self, item.id);
        }
    }

    pub fn print(&mut self) {
        let mut printer = AstPrinter::new();
        self.visit(&mut printer);
        println!("{}", printer.result);
    }

    pub fn query_item(&self, id: ItemId) -> &Item {
        &self.items[id]
    }

    pub fn query_stmt(&self, id: StmtId) -> &Stmt {
        &self.statements[id]
    }

    pub fn query_stmt_mut(&mut self, id: StmtId) -> &mut Stmt {
        &mut self.statements[id]
    }

    pub fn query_expr(&self, id: ExprId) -> &Expr {
        &self.expressions[id]
    }

    pub fn query_expr_mut(&mut self, id: ExprId) -> &mut Expr {
        &mut self.expressions[id]
    }

    pub fn set_type(&mut self, id: ExprId, typ: Type) {
        let expr = self.query_expr_mut(id);
        expr.typ = typ;
    }

    pub fn set_variable_for_stmt(&mut self, var: VariableId, stmt: StmtId) {
        let stmt = self.query_stmt_mut(stmt);
        match &mut stmt.kind {
            StmtKind::Let(let_stmt) => let_stmt.variable = var,
            _ => unreachable!("only let statments have variables"),
        }
    }

    pub fn set_variable_for_expr(&mut self, var: VariableId, expr: ExprId) {
        let expr = self.query_expr_mut(expr);
        match &mut expr.kind {
            ExprKind::Assign(assign_expr) => assign_expr.variable = var,
            ExprKind::Variable(var_expr) => var_expr.id = var,
            _ => unreachable!("only assignment expressions have variables"),
        }
    }

    fn create_item(&mut self, kind: ItemKind) -> ItemId {
        let item = Item::new(kind);
        let id = self.items.push(item);
        self.items[id].id = id;
        id
    }

    fn create_stmt(&mut self, kind: StmtKind, span: TextSpan) -> StmtId {
        let stmt = Stmt::new(kind, span);
        let id = self.statements.push(stmt);
        self.statements[id].id = id;
        id
    }

    fn create_expr(&mut self, kind: ExprKind, span: TextSpan) -> ExprId {
        let expr = Expr::new(kind, span);
        let id = self.expressions.push(expr);
        self.expressions[id].id = id;
        id
    }

    fn create_expr_stmt(&mut self, expr: ExprId) -> StmtId {
        let expr_span = self.query_expr(expr).span;
        self.create_stmt(StmtKind::Expr(expr), expr_span)
    }

    fn create_let_stmt(
        &mut self,
        keyword: SyntaxToken,
        identifier: SyntaxToken,
        equals_token: SyntaxToken,
        expr: ExprId,
    ) -> StmtId {
        let expr_span = self.query_expr(expr).span;
        let span = keyword.span.to(expr_span);
        self.create_stmt(
            StmtKind::Let(LetStmt {
                keyword,
                identifier,
                variable: VariableId::default(),
                equals_token,
                initial: expr,
            }),
            span,
        )
    }

    fn create_while_stmt(
        &mut self,
        keyword: SyntaxToken,
        condition: ExprId,
        body: ExprId,
    ) -> StmtId {
        let span = keyword.span.to(self.query_expr(body).span);
        self.create_stmt(
            StmtKind::While(WhileStmt {
                keyword,
                condition,
                body,
            }),
            span,
        )
    }

    fn create_error_expr(&mut self, span: TextSpan) -> ExprId {
        self.create_expr(ExprKind::Error(span), span)
    }

    fn create_block_expr(
        &mut self,
        open_token: SyntaxToken,
        stmts: Vec<StmtId>,
        close_token: SyntaxToken,
    ) -> ExprId {
        let span = open_token.span.to(close_token.span);
        self.create_expr(
            ExprKind::Block(BlockExpr {
                open_token,
                stmts,
                close_token,
            }),
            span,
        )
    }

    fn create_binary_expr(
        &mut self,
        left: ExprId,
        operator: BinaryOperator,
        right: ExprId,
    ) -> ExprId {
        let left_span = self.query_expr(left).span;
        let right_span = self.query_expr(right).span;
        self.create_expr(
            ExprKind::Binary(BinaryExpr {
                left,
                operator,
                right,
            }),
            left_span.to(right_span),
        )
    }

    fn create_unary_expr(&mut self, operator: UnaryOperator, expr: ExprId) -> ExprId {
        let expr_span = self.query_expr(expr).span;
        let span = operator.token.span.to(expr_span);
        self.create_expr(
            ExprKind::Unary(UnaryExpr {
                operator,
                operand: expr,
            }),
            span,
        )
    }

    fn create_assign_expr(&mut self, lhs: SyntaxToken, equals: SyntaxToken, rhs: ExprId) -> ExprId {
        let rhs_span = self.query_expr(rhs).span;
        let span = lhs.span.to(rhs_span);
        self.create_expr(
            ExprKind::Assign(AssignExpr {
                identifier: lhs,
                equals,
                rhs,
                variable: VariableId::default(),
            }),
            span,
        )
    }

    fn create_paren_expr(&mut self, open: SyntaxToken, expr: ExprId, close: SyntaxToken) -> ExprId {
        let span = open.span.to(close.span);
        self.create_expr(ExprKind::Paren(ParenExpr { open, expr, close }), span)
    }

    fn create_integer_expr(&mut self, value: i32, token: SyntaxToken) -> ExprId {
        let span = token.span;
        self.create_expr(ExprKind::Integer(IntegerExpr { token, value }), span)
    }

    fn create_boolean_expr(&mut self, value: bool, token: SyntaxToken) -> ExprId {
        let span = token.span;
        self.create_expr(ExprKind::Boolean(BooleanExpr { token, value }), span)
    }

    fn create_variable_expr(&mut self, token: SyntaxToken) -> ExprId {
        let span = token.span;
        self.create_expr(
            ExprKind::Variable(VariableExpr {
                token,
                id: VariableId::default(),
                typ: Type::Unresolved,
            }),
            span,
        )
    }

    fn create_if_expr(
        &mut self,
        keyword: SyntaxToken,
        condition: ExprId,
        then_clause: ExprId,
        else_clause: Option<ElseClause>,
    ) -> ExprId {
        let end_span = else_clause
            .as_ref()
            .map(|els| els.span)
            .unwrap_or_else(|| self.query_expr(then_clause).span);

        let span = keyword.span.to(end_span);
        self.create_expr(
            ExprKind::If(IfExpr {
                keyword,
                condition,
                then_clause,
                else_clause,
            }),
            span,
        )
    }
}

#[derive(Debug, Clone)]
pub struct Item {
    kind: ItemKind,
    id: ItemId,
}

impl Item {
    fn new(kind: ItemKind) -> Self {
        Self {
            kind,
            id: ItemId::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Stmt(StmtId),
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub id: StmtId,
    pub span: TextSpan,
}

impl Stmt {
    fn new(kind: StmtKind, span: TextSpan) -> Self {
        Self {
            kind,
            id: StmtId::default(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expr(ExprId),
    Let(LetStmt),
    While(WhileStmt),
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub keyword: SyntaxToken,
    pub identifier: SyntaxToken,
    pub variable: VariableId,
    pub equals_token: SyntaxToken,
    pub initial: ExprId,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub keyword: SyntaxToken,
    pub condition: ExprId,
    pub body: ExprId,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub id: ExprId,
    pub typ: Type,
    pub span: TextSpan,
}

impl Expr {
    fn new(kind: ExprKind, span: TextSpan) -> Self {
        Self {
            kind,
            id: ExprId::default(),
            typ: Type::Unresolved,
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Error(TextSpan),
    Integer(IntegerExpr),
    Boolean(BooleanExpr),
    Paren(ParenExpr),
    Assign(AssignExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Block(BlockExpr),
    Variable(VariableExpr),
    If(IfExpr),
}

#[derive(Debug, Clone)]
pub struct IntegerExpr {
    pub token: SyntaxToken,
    pub value: i32,
}

#[derive(Debug, Clone)]
pub struct BooleanExpr {
    pub token: SyntaxToken,
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct ParenExpr {
    pub open: SyntaxToken,
    pub expr: ExprId,
    pub close: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub identifier: SyntaxToken,
    pub equals: SyntaxToken,
    pub rhs: ExprId,
    pub variable: VariableId,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: ExprId,
    pub operator: BinaryOperator,
    pub right: ExprId,
}

#[derive(Debug, Clone)]
pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
    pub token: SyntaxToken,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum BinaryOperatorKind {
    Add,
    Subtract,
    Mulitply,
    Divide,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,
}

impl BinaryOperatorKind {
    pub fn priority(&self) -> usize {
        match self {
            BinaryOperatorKind::Mulitply | BinaryOperatorKind::Divide => 12,
            BinaryOperatorKind::Add | BinaryOperatorKind::Subtract => 11,
            BinaryOperatorKind::Equals
            | BinaryOperatorKind::NotEquals
            | BinaryOperatorKind::LessThan
            | BinaryOperatorKind::LessThanOrEquals
            | BinaryOperatorKind::GreaterThan
            | BinaryOperatorKind::GreaterThanOrEquals => 8,
            BinaryOperatorKind::BitwiseAnd => 7,
            BinaryOperatorKind::BitwiseOr => 5,
            BinaryOperatorKind::LogicalAnd => 4,
            BinaryOperatorKind::LogicalOr => 3,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub operand: ExprId,
}

#[derive(Debug, Clone)]
pub struct UnaryOperator {
    pub kind: UnaryOperatorKind,
    pub token: SyntaxToken,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum UnaryOperatorKind {
    Identity,
    Negate,
    LogicalNot,
}

impl UnaryOperatorKind {
    pub fn priority(&self) -> usize {
        match self {
            UnaryOperatorKind::Identity
            | UnaryOperatorKind::Negate
            | UnaryOperatorKind::LogicalNot => 13,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub open_token: SyntaxToken,
    pub stmts: Vec<StmtId>,
    pub close_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub token: SyntaxToken,
    pub id: VariableId,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub keyword: SyntaxToken,
    pub condition: ExprId,
    pub then_clause: ExprId,
    pub else_clause: Option<ElseClause>,
}

#[derive(Debug, Clone)]
pub struct ElseClause {
    pub keyword: SyntaxToken,
    pub body: ExprId,
    pub span: TextSpan,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.token.literal)
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.token.literal)
    }
}
