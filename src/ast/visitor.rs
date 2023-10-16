use crate::text::TextSpan;

use super::{
    Ast, BinaryExpr, BlockExpr, Expr, ExprId, ExprKind, IntegerExpr, ItemId, ParenExpr, StmtId,
    UnaryExpr, VariableExpr,
};

pub trait AstVisitor {
    fn visit_item(&mut self, ast: &mut Ast, item: ItemId) {
        let item = ast.query_item(item).clone();
        match &item.kind {
            super::ItemKind::Stmt(stmt) => self.visit_stmt(ast, *stmt),
        }
    }

    fn visit_stmt(&mut self, ast: &mut Ast, stmt: StmtId) {
        let stmt = ast.query_stmt(stmt).clone();
        match &stmt.kind {
            super::StmtKind::Expr(expr) => self.visit_expr(ast, *expr),
            super::StmtKind::Let(let_stmt) => todo!(),
        }
    }

    fn visit_expr(&mut self, ast: &mut Ast, expr: ExprId) {
        self.do_visit_expr(ast, expr);
    }

    fn do_visit_expr(&mut self, ast: &mut Ast, expr: ExprId) {
        let expr = ast.query_expr(expr).clone();
        match &expr.kind {
            ExprKind::Error(span) => self.visit_error(ast, span, &expr),
            ExprKind::Integer(int_expr) => self.visit_integer_expr(ast, &int_expr, &expr),
            ExprKind::Paren(paren_expr) => self.visit_paren_expr(ast, &paren_expr, &expr),
            ExprKind::Binary(binary_expr) => self.visit_binary_expr(ast, &binary_expr, &expr),
            ExprKind::Unary(unary_expr) => self.visit_unary_expr(ast, &unary_expr, &expr),
            ExprKind::Block(block_expr) => self.visit_block_expr(ast, &block_expr, &expr),
            ExprKind::Variable(variable_expr) => {
                self.visit_variable_expr(ast, &variable_expr, &expr)
            }
        }
    }

    fn visit_error(&mut self, ast: &mut Ast, span: &TextSpan, expr: &Expr);

    fn visit_integer_expr(&mut self, ast: &mut Ast, int_expr: &IntegerExpr, expr: &Expr);

    fn visit_paren_expr(&mut self, ast: &mut Ast, paren_expr: &ParenExpr, expr: &Expr);

    fn visit_binary_expr(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, expr: &Expr);

    fn visit_unary_expr(&mut self, ast: &mut Ast, unary_expr: &UnaryExpr, expr: &Expr);

    fn visit_block_expr(&mut self, ast: &mut Ast, block_expr: &BlockExpr, expr: &Expr);

    fn visit_variable_expr(&mut self, ast: &mut Ast, variable_expr: &VariableExpr, expr: &Expr);
}
