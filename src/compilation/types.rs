use std::{fmt::Display, rc::Rc};

use crate::{
    ast::{nodes::*, Ast, AstVisitor},
    diagnostics::{DiagnosticBag, TextSpan},
};

use super::scope::Scopes;

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Int,
    Bool,
    Unresolved,
    Void,
}

pub struct Resolver {
    diagnostics: Rc<DiagnosticBag>,
    pub scopes: Scopes,
}

impl Resolver {
    pub fn new(diagnostics: Rc<DiagnosticBag>) -> Self {
        Resolver {
            diagnostics,
            scopes: Scopes::new(),
        }
    }

    pub fn expect_type(&self, expected: &Type, actual: &Type, span: TextSpan) -> Type {
        // ignore if type is unresolved because that indicates type binding failed, which should
        // have already been reported as another error
        if expected != actual && *actual != Type::Unresolved {
            self.diagnostics
                .report_unexpected_type(&expected, actual, span);
        }
        expected.clone()
    }

    pub fn resolve_binary_expr(&self, op: &BinaryOperator, left: &Expr, right: &Expr) -> Type {
        // this might be aggressive
        if left.typ == Type::Unresolved || right.typ == Type::Unresolved {
            return Type::Unresolved;
        }

        let (expected_left, expected_right, result) = match op.kind {
            BinaryOperatorKind::Add => (Type::Int, Type::Int, Type::Int),
            BinaryOperatorKind::Subtract => (Type::Int, Type::Int, Type::Int),
            BinaryOperatorKind::Mulitply => (Type::Int, Type::Int, Type::Int),
            BinaryOperatorKind::Divide => (Type::Int, Type::Int, Type::Int),
            BinaryOperatorKind::LogicalAnd => (Type::Bool, Type::Bool, Type::Bool),
            BinaryOperatorKind::LogicalOr => (Type::Bool, Type::Bool, Type::Bool),
            BinaryOperatorKind::BitwiseAnd => (Type::Int, Type::Int, Type::Int),
            BinaryOperatorKind::BitwiseOr => (Type::Int, Type::Int, Type::Int),
            BinaryOperatorKind::Equals => (Type::Int, Type::Int, Type::Bool),
            BinaryOperatorKind::NotEquals => (Type::Int, Type::Int, Type::Bool),
            BinaryOperatorKind::LessThan => (Type::Int, Type::Int, Type::Bool),
            BinaryOperatorKind::LessThanOrEquals => (Type::Int, Type::Int, Type::Bool),
            BinaryOperatorKind::GreaterThan => (Type::Int, Type::Int, Type::Bool),
            BinaryOperatorKind::GreaterThanOrEquals => (Type::Int, Type::Int, Type::Bool),
        };

        if left.typ != expected_left || right.typ != expected_right {
            self.diagnostics
                .report_unsupported_binary_operator(&left.typ, op, &right.typ);
            Type::Unresolved
        } else {
            result
        }
    }

    pub fn resolve_unary_expr(&self, op: &UnaryOperator, operand: &Expr) -> Type {
        if operand.typ == Type::Unresolved {
            return Type::Unresolved;
        }

        let (expected_operand, result) = match op.kind {
            UnaryOperatorKind::Identity => (Type::Int, Type::Int),
            UnaryOperatorKind::Negate => (Type::Int, Type::Int),
            UnaryOperatorKind::LogicalNot => (Type::Bool, Type::Bool),
        };

        if operand.typ != expected_operand {
            self.diagnostics
                .report_unsupported_unary_operator(op, &operand.typ);
            Type::Unresolved
        } else {
            result
        }
    }
}

impl AstVisitor for Resolver {
    fn visit_let_stmt(&mut self, ast: &mut Ast, let_stmt: &LetStmt, stmt: &Stmt) {
        self.visit_expr(ast, let_stmt.initial);
        let initial_expr = ast.query_expr(let_stmt.initial);

        let typ = let_stmt
            .type_decl
            .as_ref()
            .and_then(|d| match d.typ.literal.as_str() {
                "int" => Some(Type::Int),
                "bool" => Some(Type::Bool),
                _ => {
                    self.diagnostics.report_undefined_type(&d.typ);
                    None
                }
            })
            .map(|typ| self.expect_type(&typ, &initial_expr.typ, initial_expr.span))
            .unwrap_or_else(|| initial_expr.typ.clone());

        let var = self.scopes.declare_variable(&let_stmt.identifier, typ);
        match var {
            Some(var) => ast.set_variable_for_stmt(var, stmt.id),
            None => self
                .diagnostics
                .report_already_declared(&let_stmt.identifier),
        }
    }

    fn visit_while_stmt(&mut self, ast: &mut Ast, while_stmt: &WhileStmt, _stmt: &Stmt) {
        self.visit_expr(ast, while_stmt.condition);

        let condition = ast.query_expr(while_stmt.condition);
        self.expect_type(&Type::Bool, &condition.typ, condition.span);

        self.visit_expr(ast, while_stmt.body);
    }

    fn visit_paren_expr(&mut self, ast: &mut Ast, paren_expr: &ParenExpr, expr: &Expr) {
        self.visit_expr(ast, paren_expr.expr);
        let inner_expr = ast.query_expr(paren_expr.expr);
        ast.set_type(expr.id, inner_expr.typ.clone());
    }

    fn visit_error(&mut self, ast: &mut Ast, _span: &TextSpan, expr: &Expr) {
        ast.set_type(expr.id, Type::Unresolved);
    }

    fn visit_integer_expr(&mut self, ast: &mut Ast, _int_expr: &IntegerExpr, expr: &Expr) {
        ast.set_type(expr.id, Type::Int);
    }

    fn visit_boolean_expr(&mut self, ast: &mut Ast, _bool_expr: &BooleanExpr, expr: &Expr) {
        ast.set_type(expr.id, Type::Bool);
    }

    fn visit_assign_expr(&mut self, ast: &mut Ast, assign_expr: &AssignExpr, expr: &Expr) {
        self.visit_expr(ast, assign_expr.rhs);
        let var = self.scopes.lookup_variable(&assign_expr.identifier.literal);

        let typ = match var {
            Some(var) => {
                let rhs_expr = ast.query_expr(assign_expr.rhs);
                let typ = self.expect_type(&var.typ, &rhs_expr.typ, rhs_expr.span);
                ast.set_variable_for_expr(var.id, expr.id);
                typ
            }
            None => {
                self.diagnostics
                    .report_identifier_not_found(&assign_expr.identifier);
                ast.query_expr(assign_expr.rhs).typ.clone()
            }
        };
        ast.set_type(expr.id, typ);
    }

    fn visit_binary_expr(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, expr: &Expr) {
        self.visit_expr(ast, binary_expr.left);
        self.visit_expr(ast, binary_expr.right);
        let left = ast.query_expr(binary_expr.left);
        let right = ast.query_expr(binary_expr.right);

        let typ = self.resolve_binary_expr(&binary_expr.operator, left, right);

        ast.set_type(expr.id, typ);
    }

    fn visit_unary_expr(&mut self, ast: &mut Ast, unary_expr: &UnaryExpr, expr: &Expr) {
        self.visit_expr(ast, unary_expr.operand);
        let operand = ast.query_expr(unary_expr.operand);

        let typ = self.resolve_unary_expr(&unary_expr.operator, operand);

        ast.set_type(expr.id, typ);
    }

    fn visit_variable_expr(&mut self, ast: &mut Ast, variable_expr: &VariableExpr, expr: &Expr) {
        let var = self.scopes.lookup_variable(&variable_expr.token.literal);
        let typ = match var {
            Some(var) => {
                ast.set_variable_for_expr(var.id, expr.id);
                var.typ.clone()
            }
            None => {
                self.diagnostics
                    .report_identifier_not_found(&variable_expr.token);
                Type::Unresolved
            }
        };

        ast.set_type(expr.id, typ);
    }

    fn visit_block_expr(&mut self, ast: &mut Ast, block_expr: &BlockExpr, expr: &Expr) {
        self.scopes.enter_scope();
        self.do_visit_block_expr(ast, block_expr, expr);
        self.scopes.exit_scope();

        let typ = block_expr
            .stmts
            .last()
            .map(|id| ast.query_stmt(*id))
            .and_then(|stmt| match stmt.kind {
                // TODO: only if no semicolon
                StmtKind::Expr(expr) => Some(ast.query_expr(expr).typ.clone()),
                _ => None,
            })
            .unwrap_or(Type::Void);

        ast.set_type(expr.id, typ);
    }

    fn visit_if_expr(&mut self, ast: &mut Ast, if_expr: &IfExpr, expr: &Expr) {
        self.visit_expr(ast, if_expr.condition);

        let condition = ast.query_expr(if_expr.condition);
        self.expect_type(&Type::Bool, &condition.typ, condition.span);

        self.visit_expr(ast, if_expr.then_clause);
        let then_typ = ast.query_expr(if_expr.then_clause).typ.clone();

        if let Some(else_clause) = &if_expr.else_clause {
            self.visit_expr(ast, else_clause.body);
            let else_body = &ast.query_expr(else_clause.body);
            self.expect_type(&then_typ, &else_body.typ, else_body.span);
        }

        ast.set_type(expr.id, then_typ);
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Int => "<int>",
            Type::Bool => "<bool>",
            Type::Void => "<void>",
            Type::Unresolved => "UNRESOLVED",
        };

        write!(f, "{}", s)
    }
}
