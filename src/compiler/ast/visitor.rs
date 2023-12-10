use crate::compiler::diagnostics::TextSpan;

use super::{node::*, Ast, ExprId, ItemId, StmtId};

pub trait AstVisitor {
    fn visit_item(&mut self, ast: &Ast, item: ItemId) {
        let item = ast.query_item(item).clone();
        match &item.kind {
            ItemKind::Stmt(stmt) => self.visit_stmt(ast, *stmt),
            ItemKind::Func(func) => self.visit_func_decl(ast, func, &item),
        }
    }

    fn visit_func_decl(&mut self, ast: &Ast, func: &FunctionDecl, item: &Item);

    fn visit_stmt(&mut self, ast: &Ast, stmt: StmtId) {
        let stmt = ast.query_stmt(stmt);
        self.do_visit_stmt(ast, &stmt);
    }

    fn do_visit_stmt(&mut self, ast: &Ast, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Expr(expr_stmt) => self.visit_expr_stmt(ast, expr_stmt, &stmt),
            StmtKind::If(if_expr) => self.visit_if_stmt(ast, *if_expr, &stmt),
            StmtKind::Decl(variable_decl) => self.visit_variable_decl(ast, variable_decl, &stmt),
            StmtKind::While(while_stmt) => self.visit_while_stmt(ast, while_stmt, &stmt),
            StmtKind::Return(return_stmt) => self.visit_return_stmt(ast, return_stmt, &stmt),
        }
    }

    fn visit_expr_stmt(&mut self, ast: &Ast, expr_stmt: &ExprStmt, _stmt: &Stmt) {
        self.visit_expr(ast, expr_stmt.expr);
    }

    fn visit_if_stmt(&mut self, ast: &Ast, if_expr: ExprId, _stmt: &Stmt) {
        self.visit_expr(ast, if_expr);
    }

    fn visit_variable_decl(&mut self, ast: &Ast, variable_decl: &VariableDecl, stmt: &Stmt);

    fn visit_while_stmt(&mut self, ast: &Ast, while_stmt: &WhileStmt, stmt: &Stmt);

    fn visit_return_stmt(&mut self, ast: &Ast, return_stmt: &ReturnStmt, stmt: &Stmt);

    fn visit_expr(&mut self, ast: &Ast, expr: ExprId) {
        let expr = ast.query_expr(expr);
        self.do_visit_expr(ast, &expr);
    }

    fn do_visit_expr(&mut self, ast: &Ast, expr: &Expr) {
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
            ExprKind::CallExpr(call_expr) => self.visit_call_expr(ast, &call_expr, &expr),
        }
    }

    fn visit_error(&mut self, ast: &Ast, span: &TextSpan, expr: &Expr);

    fn visit_integer_expr(&mut self, ast: &Ast, int_expr: &IntegerExpr, expr: &Expr);

    fn visit_boolean_expr(&mut self, ast: &Ast, bool_expr: &BooleanExpr, expr: &Expr);

    fn visit_assign_expr(&mut self, ast: &Ast, assign_expr: &AssignExpr, expr: &Expr);

    fn visit_paren_expr(&mut self, ast: &Ast, paren_expr: &ParenExpr, _expr: &Expr) {
        self.visit_expr(ast, paren_expr.expr);
    }

    fn visit_binary_expr(&mut self, ast: &Ast, binary_expr: &BinaryExpr, expr: &Expr);

    fn visit_unary_expr(&mut self, ast: &Ast, unary_expr: &UnaryExpr, expr: &Expr);

    fn visit_block_expr(&mut self, ast: &Ast, block_expr: &BlockExpr, expr: &Expr) {
        self.do_visit_block_expr(ast, block_expr, expr);
    }

    fn do_visit_block_expr(&mut self, ast: &Ast, block_expr: &BlockExpr, _expr: &Expr) {
        for stmt in block_expr.stmts.iter() {
            self.visit_stmt(ast, *stmt);
        }
    }

    fn visit_variable_expr(&mut self, ast: &Ast, variable_expr: &VariableExpr, expr: &Expr);

    fn visit_if_expr(&mut self, ast: &Ast, if_expr: &IfExpr, expr: &Expr);

    fn visit_call_expr(&mut self, ast: &Ast, call_expr: &CallExpr, expr: &Expr);
}

pub trait AstVisitorMut {
    fn visit_item(&mut self, ast: &mut Ast, item: ItemId) {
        let item = ast.query_item(item).clone();
        match &item.kind {
            ItemKind::Stmt(stmt) => self.visit_stmt(ast, *stmt),
            ItemKind::Func(func) => self.visit_func_decl(ast, func, &item),
        }
    }

    fn visit_func_decl(&mut self, ast: &mut Ast, func: &FunctionDecl, item: &Item);

    fn visit_stmt(&mut self, ast: &mut Ast, stmt: StmtId) {
        let stmt = ast.query_stmt(stmt).clone();
        self.do_visit_stmt(ast, &stmt);
    }

    fn do_visit_stmt(&mut self, ast: &mut Ast, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Expr(expr_stmt) => self.visit_expr_stmt(ast, expr_stmt, &stmt),
            StmtKind::If(if_expr) => self.visit_if_stmt(ast, *if_expr, &stmt),
            StmtKind::Decl(variable_decl) => self.visit_variable_decl(ast, variable_decl, &stmt),
            StmtKind::While(while_stmt) => self.visit_while_stmt(ast, while_stmt, &stmt),
            StmtKind::Return(return_stmt) => self.visit_return_stmt(ast, return_stmt, &stmt),
        }
    }

    fn visit_expr_stmt(&mut self, ast: &mut Ast, expr_stmt: &ExprStmt, _stmt: &Stmt) {
        self.visit_expr(ast, expr_stmt.expr);
    }

    fn visit_if_stmt(&mut self, ast: &mut Ast, if_expr: ExprId, _stmt: &Stmt) {
        self.visit_expr(ast, if_expr);
    }

    fn visit_variable_decl(&mut self, ast: &mut Ast, variable_decl: &VariableDecl, stmt: &Stmt);

    fn visit_while_stmt(&mut self, ast: &mut Ast, while_stmt: &WhileStmt, stmt: &Stmt);

    fn visit_return_stmt(&mut self, ast: &mut Ast, return_stmt: &ReturnStmt, stmt: &Stmt);

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
            ExprKind::CallExpr(call_expr) => self.visit_call_expr(ast, &call_expr, &expr),
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

    fn visit_call_expr(&mut self, ast: &mut Ast, call_expr: &CallExpr, expr: &Expr);
}
