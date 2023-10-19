use std::{fmt::Display, rc::Rc};

use crate::{
    ast::{
        lexer::Lexer, parser::Parser, visitor::AstVisitor, AssignExpr, Ast, BinaryExpr,
        BinaryOperatorKind, BlockExpr, BooleanExpr, Expr, IfExpr, IntegerExpr, LetStmt, ParenExpr,
        Stmt, StmtKind, UnaryExpr, UnaryOperatorKind, VariableExpr,
    },
    diagnostics::DiagnosticBag,
    scope::{GlobalScope, Scopes},
    text::{SourceText, TextSpan},
};

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Type {
    Int,
    Bool,
    Unresolved,
    Void,
}

pub struct CompilationUnit {
    pub src: SourceText,
    pub ast: Ast,
    pub diagnostics: Rc<DiagnosticBag>,
    scope: GlobalScope,
}

impl CompilationUnit {
    pub fn compile(text: &str, print_tree: bool) -> Result<Self, (SourceText, Rc<DiagnosticBag>)> {
        let src = SourceText::from(text).unwrap();
        let diagnostics = Rc::new(DiagnosticBag::new());

        let lexer = Lexer::new(&src);
        let mut ast = Parser::parse(lexer, diagnostics.clone());

        let mut resolver = Resolver::new(diagnostics.clone());
        ast.visit(&mut resolver);

        if print_tree {
            ast.print();
        }

        if diagnostics.has_errors() {
            Err((src, diagnostics))
        } else {
            Ok(CompilationUnit {
                src,
                ast,
                diagnostics,
                scope: resolver.scopes.global_scope,
            })
        }
    }
}

pub struct Resolver {
    diagnostics: Rc<DiagnosticBag>,
    scopes: Scopes,
}

impl Resolver {
    fn new(diagnostics: Rc<DiagnosticBag>) -> Self {
        Resolver {
            diagnostics,
            scopes: Scopes::new(),
        }
    }

    pub fn expect_type(&self, expected: Type, actual: Type, span: TextSpan) -> Type {
        // ignore if type is unresolved because that indicates type binding failed, which should
        // have already been reported as another error
        if expected != actual && actual != Type::Unresolved {
            self.diagnostics
                .report_unexpected_type(expected, actual, span);
        }
        expected
    }

    fn resolve_binary_expr(&self, op: BinaryOperatorKind, left: &Expr, right: &Expr) -> Type {
        let (expected_left, expected_right, result) = match op {
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
        self.expect_type(expected_left, left.typ, left.span);
        self.expect_type(expected_right, right.typ, right.span);

        result
    }

    fn resolve_unary_expr(&self, op: UnaryOperatorKind, operand: &Expr) -> Type {
        let (expected_operand, result) = match op {
            UnaryOperatorKind::Identity => (Type::Int, Type::Int),
            UnaryOperatorKind::Negate => (Type::Int, Type::Int),
            UnaryOperatorKind::LogicalNot => (Type::Bool, Type::Bool),
        };
        self.expect_type(expected_operand, operand.typ, operand.span);

        result
    }
}

impl AstVisitor for Resolver {
    fn visit_let_stmt(&mut self, ast: &mut Ast, let_stmt: &LetStmt, stmt: &Stmt) {
        self.visit_expr(ast, let_stmt.initial);
        let intial_expr = ast.query_expr(let_stmt.initial);
        let var = self
            .scopes
            .declare_variable(&let_stmt.identifier, intial_expr.typ);
        match var {
            Some(var) => ast.set_variable_for_stmt(var, stmt.id),
            None => self
                .diagnostics
                .report_already_declared(&let_stmt.identifier),
        }
    }

    fn visit_paren_expr(&mut self, ast: &mut Ast, paren_expr: &ParenExpr, expr: &Expr) {
        self.visit_expr(ast, paren_expr.expr);
        let inner_expr = ast.query_expr(paren_expr.expr);
        ast.set_type(expr.id, inner_expr.typ);
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
                self.expect_type(var.typ, rhs_expr.typ, rhs_expr.span);
                ast.set_variable_for_expr(var.id, expr.id);
                var.typ
            }
            None => {
                self.diagnostics
                    .report_identifier_not_found(&assign_expr.identifier);
                ast.query_expr(assign_expr.rhs).typ
            }
        };
        ast.set_type(expr.id, typ);
    }

    fn visit_binary_expr(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, expr: &Expr) {
        self.visit_expr(ast, binary_expr.left);
        self.visit_expr(ast, binary_expr.right);
        let left = ast.query_expr(binary_expr.left);
        let right = ast.query_expr(binary_expr.right);

        let typ = self.resolve_binary_expr(binary_expr.operator.kind, left, right);

        ast.set_type(expr.id, typ);
    }

    fn visit_unary_expr(&mut self, ast: &mut Ast, unary_expr: &UnaryExpr, expr: &Expr) {
        self.visit_expr(ast, unary_expr.operand);
        let operand = ast.query_expr(unary_expr.operand);

        let typ = self.resolve_unary_expr(unary_expr.operator.kind, operand);

        ast.set_type(expr.id, typ);
    }

    fn visit_variable_expr(&mut self, ast: &mut Ast, variable_expr: &VariableExpr, expr: &Expr) {
        let var = self.scopes.lookup_variable(&variable_expr.token.literal);
        let typ = match var {
            Some(var) => {
                ast.set_variable_for_expr(var.id, expr.id);
                var.typ
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
                StmtKind::Expr(expr) => Some(ast.query_expr(expr).typ),
                _ => None,
            })
            .unwrap_or(Type::Void);

        ast.set_type(expr.id, typ);
    }

    fn visit_if_expr(&mut self, ast: &mut Ast, if_expr: &IfExpr, expr: &Expr) {
        self.visit_expr(ast, if_expr.condition);

        let condition = ast.query_expr(if_expr.condition);
        self.expect_type(Type::Bool, condition.typ, condition.span);

        self.visit_expr(ast, if_expr.then_clause);
        let typ = if let Some(else_clause) = &if_expr.else_clause {
            self.visit_expr(ast, else_clause.body);
            ast.query_expr(else_clause.body).typ
        } else {
            ast.query_expr(if_expr.then_clause).typ
        };

        ast.set_type(expr.id, typ);
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
