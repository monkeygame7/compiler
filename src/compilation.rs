use std::rc::Rc;

use crate::{
    ast::{
        visitor::AstVisitor, AssignExpr, Ast, BinaryExpr, BooleanExpr, Expr, IntegerExpr, LetStmt,
        Stmt, Type, UnaryExpr, VariableExpr,
    },
    diagnostics::DiagnosticBag,
    scope::{GlobalScope, Scopes},
    text::TextSpan,
};

pub struct CompilationUnit {
    ast: Ast,
    diagnostics: Rc<DiagnosticBag>,
    scope: GlobalScope,
}

pub struct Resolver {
    diagnostics: Rc<DiagnosticBag>,
    scopes: Scopes,
}

impl Resolver {
    pub fn expect_type(&self, expected: Type, actual: Type) -> Type {
        if expected != actual {
            todo!("unexpected type")
        }
        expected
    }
}

impl AstVisitor for Resolver {
    fn visit_let_stmt(&mut self, ast: &mut Ast, let_stmt: &LetStmt, stmt: &Stmt) {
        self.visit_expr(ast, let_stmt.initial);
        let intial_expr = ast.query_expr(let_stmt.initial);
        let var = self
            .scopes
            .declare_variable(&let_stmt.identifier, intial_expr.typ);
        ast.set_variable_for_stmt(var, stmt.id);
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
        self.visit_expr(ast, assign_expr.rhs);
        let var = self.scopes.lookup_variable(&assign_expr.identifier.literal);

        let typ = match var {
            Some(var) => {
                let rhs_expr = ast.query_expr(assign_expr.rhs);
                let var_typ = self.scopes.lookup_type(var);
                self.expect_type(rhs_expr.typ, var_typ);
                var_typ
            }
            None => {
                todo!("Undeclared variable")
            }
        };

        ast.set_type(expr.id, typ);
    }

    fn visit_binary_expr(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, expr: &Expr) {
        todo!()
    }

    fn visit_unary_expr(&mut self, ast: &mut Ast, unary_expr: &UnaryExpr, expr: &Expr) {
        todo!()
    }

    fn visit_variable_expr(&mut self, ast: &mut Ast, variable_expr: &VariableExpr, expr: &Expr) {
        let var = self.scopes.lookup_variable(&variable_expr.token.literal);
        let typ = match var {
            Some(var) => self.scopes.lookup_type(var),
            None => todo!("undeclared variable"),
        };
        ast.set_type(expr.id, typ);
    }

    fn visit_block_expr(&mut self, ast: &mut Ast, block_expr: &crate::ast::BlockExpr, expr: &Expr) {
        self.do_visit_block_expr(ast, block_expr, expr);
    }
}
