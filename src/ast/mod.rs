mod id;
pub mod nodes;
mod printer;
mod visitor;
use std::fmt::Debug;

pub use id::{Idx, IdxVec};
use nodes::*;

pub use visitor::AstVisitor;
pub use visitor::AstVisitorMut;

use crate::{
    compilation::{FunctionId, Type, VariableId},
    diagnostics::TextSpan,
    idx,
    parsing::SyntaxToken,
};
use printer::AstPrinter;

idx!(ItemId);
idx!(StmtId);
idx!(ExprId);

pub struct Ast {
    statements: IdxVec<StmtId, Stmt>,
    expressions: IdxVec<ExprId, Expr>,
    items: IdxVec<ItemId, Item>,
}

impl Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut printer = AstPrinter::new();
        self.visit(&mut printer);
        f.write_str(&printer.result)
    }
}

impl Ast {
    pub fn new() -> Self {
        Ast {
            statements: IdxVec::new(),
            expressions: IdxVec::new(),
            items: IdxVec::new(),
        }
    }

    pub fn visit(&self, visitor: &mut dyn AstVisitor) {
        for item in self.items.clone().iter() {
            visitor.visit_item(self, item.id);
        }
    }

    pub fn visit_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        for item in self.items.clone().iter() {
            visitor.visit_item(self, item.id);
        }
    }

    pub fn print(&mut self) {
        println!("{:?}", self);
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
            StmtKind::Decl(variable_decl) => variable_decl.variable = var,
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

    pub fn create_item(&mut self, kind: ItemKind) -> ItemId {
        let item = Item::new(kind);
        let id = self.items.push(item);
        self.items[id].id = id;
        id
    }

    pub fn create_stmt_item(&mut self, stmt: StmtId) -> ItemId {
        self.create_item(ItemKind::Stmt(stmt))
    }

    pub fn create_function_item(
        &mut self,
        keyword: SyntaxToken,
        name: SyntaxToken,
        return_type: Option<TypeDecl>,
        open_paren: SyntaxToken,
        params: DelimitedSequence<FunctionParam>,
        close_paren: SyntaxToken,
        body: ExprId,
    ) -> ItemId {
        self.create_item(ItemKind::Func(FunctionDecl {
            keyword,
            name,
            return_type,
            open_paren,
            params,
            close_paren,
            body,
            id: FunctionId::default(),
        }))
    }

    pub fn create_stmt(&mut self, kind: StmtKind, span: TextSpan) -> StmtId {
        let stmt = Stmt::new(kind, span);
        let id = self.statements.push(stmt);
        self.statements[id].id = id;
        id
    }

    pub fn create_expr(&mut self, kind: ExprKind, span: TextSpan) -> ExprId {
        let expr = Expr::new(kind, span);
        let id = self.expressions.push(expr);
        self.expressions[id].id = id;
        id
    }

    pub fn create_expr_stmt(&mut self, expr: ExprId, semicolon: Option<SyntaxToken>) -> StmtId {
        let mut span = self.query_expr(expr).span;
        if let Some(token) = &semicolon {
            span = span.to(token.span);
        }
        self.create_stmt(StmtKind::Expr(ExprStmt { expr, semicolon }), span)
    }

    pub fn create_if_stmt(&mut self, if_expr: ExprId) -> StmtId {
        let span = self.query_expr(if_expr).span;
        self.create_stmt(StmtKind::If(if_expr), span)
    }

    pub fn create_variable_decl(
        &mut self,
        keyword: SyntaxToken,
        is_mutable: bool,
        identifier: SyntaxToken,
        type_decl: Option<TypeDecl>,
        equals_token: SyntaxToken,
        expr: ExprId,
        semicolon: SyntaxToken,
    ) -> StmtId {
        let expr_span = self.query_expr(expr).span;
        let span = keyword.span.to(expr_span);
        self.create_stmt(
            StmtKind::Decl(VariableDecl {
                keyword,
                is_mutable,
                identifier,
                variable: VariableId::default(),
                type_decl,
                equals_token,
                initial: expr,
                semicolon,
            }),
            span,
        )
    }

    pub fn create_while_stmt(
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

    pub fn create_return_stmt(
        &mut self,
        keyword: SyntaxToken,
        value: Option<ExprId>,
        semicolon: SyntaxToken,
    ) -> StmtId {
        let mut span = keyword.span;
        if let Some(expr) = value {
            span = span.to(self.query_expr(expr).span);
        }
        self.create_stmt(
            StmtKind::Return(ReturnStmt {
                keyword,
                value,
                semicolon,
                typ: Type::Unresolved,
            }),
            span,
        )
    }

    pub fn create_error_expr(&mut self, span: TextSpan) -> ExprId {
        self.create_expr(ExprKind::Error(span), span)
    }

    pub fn create_block_expr(
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

    pub fn create_binary_expr(
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

    pub fn create_unary_expr(&mut self, operator: UnaryOperator, expr: ExprId) -> ExprId {
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

    pub fn create_assign_expr(
        &mut self,
        lhs: SyntaxToken,
        equals: SyntaxToken,
        rhs: ExprId,
    ) -> ExprId {
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

    pub fn create_paren_expr(
        &mut self,
        open: SyntaxToken,
        expr: ExprId,
        close: SyntaxToken,
    ) -> ExprId {
        let span = open.span.to(close.span);
        self.create_expr(ExprKind::Paren(ParenExpr { open, expr, close }), span)
    }

    pub fn create_integer_expr(&mut self, value: i32, token: SyntaxToken) -> ExprId {
        let span = token.span;
        self.create_expr(ExprKind::Integer(IntegerExpr { token, value }), span)
    }

    pub fn create_boolean_expr(&mut self, value: bool, token: SyntaxToken) -> ExprId {
        let span = token.span;
        self.create_expr(ExprKind::Boolean(BooleanExpr { token, value }), span)
    }

    pub fn create_variable_expr(&mut self, token: SyntaxToken) -> ExprId {
        let span = token.span;
        self.create_expr(
            ExprKind::Variable(VariableExpr {
                token,
                id: VariableId::default(),
            }),
            span,
        )
    }

    pub fn create_if_expr(
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

    pub fn create_call_expr(
        &mut self,
        callee: ExprId,
        open_paren: SyntaxToken,
        args: DelimitedSequence<ExprId>,
        close_paren: SyntaxToken,
    ) -> ExprId {
        let span = self.query_expr(callee).span.to(close_paren.span);
        self.create_expr(
            ExprKind::CallExpr(CallExpr::new(callee, open_paren, args, close_paren)),
            span,
        )
    }
}
