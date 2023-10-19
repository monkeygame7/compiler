use crate::text::TextSpan;

use super::{
    AssignExpr, Ast, BinaryExpr, BlockExpr, BooleanExpr, Expr, ExprId, ExprKind, IfExpr,
    IntegerExpr, ItemId, LetStmt, ParenExpr, Stmt, StmtId, StmtKind, UnaryExpr, VariableExpr,
    WhileStmt,
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
            StmtKind::Expr(expr) => self.visit_expr_stmt(ast, *expr, &stmt),
            StmtKind::Let(let_stmt) => self.visit_let_stmt(ast, let_stmt, &stmt),
            StmtKind::While(while_stmt) => self.visit_while_stmt(ast, while_stmt, &stmt),
        }
    }

    fn visit_expr_stmt(&mut self, ast: &mut Ast, expr: ExprId, _stmt: &Stmt) {
        self.visit_expr(ast, expr);
    }

    fn visit_let_stmt(&mut self, ast: &mut Ast, let_stmt: &LetStmt, stmt: &Stmt);

    fn visit_while_stmt(&mut self, ast: &mut Ast, while_stmt: &WhileStmt, stmt: &Stmt);

    fn visit_expr(&mut self, ast: &mut Ast, expr: ExprId) {
        let expr = ast.query_expr(expr).clone();
        self.do_visit_expr(ast, &expr);
    }

    fn do_visit_expr(&mut self, ast: &mut Ast, expr: &Expr) {
        match &expr.kind {
            ExprKind::Error(span) => self.visit_error(ast, span, &expr),
            ExprKind::Integer(int_expr) => self.visit_integer_expr(ast, &int_expr, &expr),
            ExprKind::Boolean(bool_expr) => self.visit_boolean_expr(ast, &bool_expr, &expr),
            ExprKind::Paren(paren_expr) => self.visit_paren_expr(ast, &paren_expr, &expr),
            ExprKind::Assign(assign_expr) => self.visit_assign_expr(ast, &assign_expr, &expr),
            ExprKind::Binary(binary_expr) => self.visit_binary_expr(ast, &binary_expr, &expr),
            ExprKind::Unary(unary_expr) => self.visit_unary_expr(ast, &unary_expr, &expr),
            ExprKind::Block(block_expr) => self.visit_block_expr(ast, &block_expr, &expr),
            ExprKind::Variable(variable_expr) => {
                self.visit_variable_expr(ast, &variable_expr, &expr)
            }
            ExprKind::If(if_expr) => self.visit_if_expr(ast, &if_expr, &expr),
        }
    }

    fn visit_error(&mut self, ast: &mut Ast, span: &TextSpan, expr: &Expr);

    fn visit_integer_expr(&mut self, ast: &mut Ast, int_expr: &IntegerExpr, expr: &Expr);

    fn visit_boolean_expr(&mut self, ast: &mut Ast, bool_expr: &BooleanExpr, expr: &Expr);

    fn visit_assign_expr(&mut self, ast: &mut Ast, assign_expr: &AssignExpr, expr: &Expr);

    fn visit_paren_expr(&mut self, ast: &mut Ast, paren_expr: &ParenExpr, _expr: &Expr) {
        self.visit_expr(ast, paren_expr.expr);
    }

    fn visit_binary_expr(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, expr: &Expr);

    fn visit_unary_expr(&mut self, ast: &mut Ast, unary_expr: &UnaryExpr, expr: &Expr);

    fn visit_block_expr(&mut self, ast: &mut Ast, block_expr: &BlockExpr, expr: &Expr) {
        self.do_visit_block_expr(ast, block_expr, expr);
    }

    fn do_visit_block_expr(&mut self, ast: &mut Ast, block_expr: &BlockExpr, _expr: &Expr) {
        for stmt in block_expr.stmts.iter() {
            self.visit_stmt(ast, *stmt);
        }
    }

    fn visit_variable_expr(&mut self, ast: &mut Ast, variable_expr: &VariableExpr, expr: &Expr);

    fn visit_if_expr(&mut self, ast: &mut Ast, if_expr: &IfExpr, expr: &Expr);
}
