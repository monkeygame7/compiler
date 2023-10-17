use std::rc::Rc;

use crate::{
    ast::{
        visitor::AstVisitor, AssignExpr, Ast, BinaryExpr, BooleanExpr, Expr, IntegerExpr, LetStmt,
        Stmt, Type, UnaryExpr, VariableExpr,
    },
    diagnostics::DiagnosticBag,
    scope::GlobalScope,
    text::TextSpan,
};

pub struct CompilationUnit {
    ast: Ast,
    diagnostics: Rc<DiagnosticBag>,
    scope: GlobalScope,
}

pub struct Resolver {}

impl AstVisitor for Resolver {
    fn visit_let_stmt(&mut self, ast: &mut Ast, let_stmt: &LetStmt, stmt: &Stmt) {
        todo!()
    }

    fn visit_error(&mut self, ast: &mut Ast, span: &TextSpan, expr: &Expr) {
        todo!()
    }

    fn visit_integer_expr(&mut self, ast: &mut Ast, int_expr: &IntegerExpr, expr: &Expr) {
        ast.set_type(expr.id, Type::Int);
    }

    fn visit_boolean_expr(&mut self, ast: &mut Ast, bool_expr: &BooleanExpr, expr: &Expr) {
        ast.set_type(expr.id, Type::Bool);
    }

    fn visit_assign_expr(&mut self, ast: &mut Ast, assign_expr: &AssignExpr, expr: &Expr) {
        todo!()
    }

    fn visit_binary_expr(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, expr: &Expr) {
        todo!()
    }

    fn visit_unary_expr(&mut self, ast: &mut Ast, unary_expr: &UnaryExpr, expr: &Expr) {
        todo!()
    }

    fn visit_variable_expr(&mut self, ast: &mut Ast, variable_expr: &VariableExpr, expr: &Expr) {
        todo!()
    }
}
