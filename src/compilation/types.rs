use std::{fmt::Display, rc::Rc};

use crate::{
    ast::{nodes::*, Ast, AstVisitorMut},
    diagnostics::{DiagnosticBag, TextSpan},
};

use super::scope::Scopes;

#[derive(PartialEq, Debug, Clone, Eq)]
pub enum Type {
    Int,
    Bool,
    Unresolved,
    Void,
    Func(Box<Signature>),
}

impl Type {
    pub fn func(sig: &Signature) -> Self {
        Type::Func(Box::new(sig.clone()))
    }
}

#[derive(PartialEq, Debug, Clone, Eq)]
pub struct Types(Vec<Type>);

impl From<Vec<Type>> for Types {
    fn from(value: Vec<Type>) -> Self {
        Self(value)
    }
}

#[derive(PartialEq, Debug, Clone, Eq)]
pub struct Signature {
    pub return_type: Type,
    pub params: Types,
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
        if expected != actual && *actual != Type::Unresolved && *expected != Type::Unresolved {
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

    fn extract_type(&mut self, d: &TypeDecl) -> Option<Type> {
        match d.typ.literal.as_str() {
            "int" => Some(Type::Int),
            "bool" => Some(Type::Bool),
            _ => {
                self.diagnostics.report_undefined_type(&d.typ);
                None
            }
        }
    }
}

impl AstVisitorMut for Resolver {
    fn visit_func_decl(&mut self, ast: &mut Ast, func: &FunctionDecl) {
        if self.scopes.lookup_function(&func.name.literal).is_some() {
            self.diagnostics.report_already_declared(&func.name);
        }

        let return_type = func
            .return_type
            .as_ref()
            .map(|d| self.extract_type(d).unwrap_or(Type::Unresolved))
            .unwrap_or(Type::Void);
        let params = func
            .params
            .items
            .iter()
            .map(|item| &item.item)
            .map(|param| {
                let typ = self
                    .extract_type(&param.type_decl)
                    .unwrap_or(Type::Unresolved);
                // function params are immutable (at least currently)
                self.scopes
                    .create_unscoped_variable(&param.token, typ, false)
            })
            .collect();
        let id = self
            .scopes
            .declare_function(&func.name, return_type.clone(), params, func.body);

        self.scopes.enter_function_scope(id);
        self.visit_expr(ast, func.body);
        self.scopes.exit_function_scope();

        let expr = ast.query_expr(func.body);
        self.expect_type(&return_type, &expr.typ, expr.span);
    }

    fn visit_expr_stmt(&mut self, ast: &mut Ast, expr_stmt: &ExprStmt, _stmt: &Stmt) {
        self.visit_expr(ast, expr_stmt.expr);
        if let Some(_) = expr_stmt.semicolon {
            ast.set_type(expr_stmt.expr, Type::Void);
        }
    }

    fn visit_variable_decl(&mut self, ast: &mut Ast, variable_decl: &VariableDecl, stmt: &Stmt) {
        self.visit_expr(ast, variable_decl.initial);
        let initial_expr = ast.query_expr(variable_decl.initial);

        let typ = variable_decl
            .type_decl
            .as_ref()
            .and_then(|d| self.extract_type(d))
            .map(|typ| self.expect_type(&typ, &initial_expr.typ, initial_expr.span))
            .unwrap_or_else(|| initial_expr.typ.clone());

        let var =
            self.scopes
                .declare_variable(&variable_decl.identifier, typ, variable_decl.is_mutable);
        match var {
            Some(var) => ast.set_variable_for_stmt(var, stmt.id),
            None => self
                .diagnostics
                .report_already_declared(&variable_decl.identifier),
        }
    }

    fn visit_while_stmt(&mut self, ast: &mut Ast, while_stmt: &WhileStmt, _stmt: &Stmt) {
        self.visit_expr(ast, while_stmt.condition);

        let condition = ast.query_expr(while_stmt.condition);
        self.expect_type(&Type::Bool, &condition.typ, condition.span);

        self.visit_expr(ast, while_stmt.body);
    }

    fn visit_return_stmt(&mut self, ast: &mut Ast, return_stmt: &ReturnStmt, stmt: &Stmt) {
        let return_type = if let Some(value) = return_stmt.value {
            self.visit_expr(ast, value);
            ast.query_expr(value).typ.clone()
        } else {
            Type::Void
        };

        if let Some(func) = self.scopes.get_current_function() {
            let func_type = &self.scopes.global_scope.functions[func].sig.return_type;
            let span = match return_stmt.value {
                Some(id) => ast.query_expr(id).span,
                None => stmt.span,
            };
            let StmtKind::Return(ret_stmt_mut) = &mut ast.query_stmt_mut(stmt.id).kind else {
                panic!()
            };
            ret_stmt_mut.typ = self.expect_type(&func_type, &return_type, span);
        } else {
            self.diagnostics
                .report_return_outside_function(&return_stmt.keyword);
        }
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
            Some(var) if var.is_mutable => {
                let rhs_expr = ast.query_expr(assign_expr.rhs);
                let typ = self.expect_type(&var.typ, &rhs_expr.typ, rhs_expr.span);
                ast.set_variable_for_expr(var.id, expr.id);
                typ
            }
            Some(var) => {
                self.diagnostics
                    .report_immutable(&var.identifier, assign_expr.equals.span);
                var.typ.clone()
            }
            None => {
                self.diagnostics
                    .report_identifier_not_found(&assign_expr.identifier);
                ast.query_expr(assign_expr.rhs).typ.clone()
            }
        };
        ast.set_type(expr.id, typ);
    }

    fn visit_paren_expr(&mut self, ast: &mut Ast, paren_expr: &ParenExpr, expr: &Expr) {
        self.visit_expr(ast, paren_expr.expr);
        let inner_expr = ast.query_expr(paren_expr.expr);
        ast.set_type(expr.id, inner_expr.typ.clone());
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

    fn visit_block_expr(&mut self, ast: &mut Ast, block_expr: &BlockExpr, expr: &Expr) {
        self.scopes.enter_scope();
        self.do_visit_block_expr(ast, block_expr, expr);
        self.scopes.exit_scope();

        let mut typ = Type::Void;
        let mut has_return = false;
        block_expr.stmts.iter().enumerate().for_each(|(idx, id)| {
            let stmt = ast.query_stmt(*id);
            let is_last = idx == block_expr.stmts.len() - 1;
            match &stmt.kind {
                StmtKind::Expr(expr_stmt) => {
                    if is_last && !has_return {
                        typ = ast.query_expr(expr_stmt.expr).typ.clone();
                    } else {
                        if expr_stmt.semicolon.is_none() {
                            self.diagnostics.report_expected_semicolon(stmt.span);
                        }
                    }
                }
                StmtKind::If(if_expr) => {
                    if is_last && !has_return {
                        typ = ast.query_expr(*if_expr).typ.clone();
                    }
                }
                StmtKind::Return(return_stmt) => {
                    has_return = true;
                    typ = return_stmt.typ.clone();
                    if !is_last {
                        todo!("return not last");
                    }
                }
                _ => (),
            }
        });

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

    fn visit_if_expr(&mut self, ast: &mut Ast, if_expr: &IfExpr, expr: &Expr) {
        self.visit_expr(ast, if_expr.condition);

        let condition = ast.query_expr(if_expr.condition);
        self.expect_type(&Type::Bool, &condition.typ, condition.span);

        self.visit_expr(ast, if_expr.then_clause);

        let typ = match &if_expr.else_clause {
            Some(else_clause) => {
                let then_typ = ast.query_expr(if_expr.then_clause).typ.clone();
                self.visit_expr(ast, else_clause.body);
                let else_body = &ast.query_expr(else_clause.body);
                self.expect_type(&then_typ, &else_body.typ, else_body.span);
                then_typ
            }
            _ => Type::Void,
        };

        ast.set_type(expr.id, typ);
    }

    fn visit_call_expr(&mut self, ast: &mut Ast, call_expr: &CallExpr, expr: &Expr) {
        self.visit_expr(ast, call_expr.callee);
        let callee = ast.query_expr(call_expr.callee);

        let Type::Func(sig) = &callee.typ else {
            self.diagnostics
                .report_not_callable(&callee.typ, callee.span);
            return;
        };

        let params = sig.params.clone();
        let return_type = sig.return_type.clone();

        let mut arg_types: Types = vec![].into();

        for item in &call_expr.args.items {
            let id = item.item;
            self.visit_expr(ast, id);
            arg_types.0.push(ast.query_expr(id).typ.clone());
        }

        if params != arg_types {
            let span = call_expr.open_paren.span.to(call_expr.close_paren.span);
            self.diagnostics
                .report_incorrect_arguments(&params, &arg_types, span);
        }
        ast.set_type(expr.id, return_type);
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Int => "<int>".to_string(),
            Type::Bool => "<bool>".to_string(),
            Type::Void => "<void>".to_string(),
            Type::Unresolved => "UNRESOLVED".to_string(),
            Type::Func(sig) => sig.to_string(),
        };

        write!(f, "{}", s)
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.return_type, self.params)
    }
}

impl Display for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut params_s = String::from("(");

        let mut param_iter = self.0.iter();
        if let Some(param) = param_iter.next() {
            params_s += &param.to_string();
        }

        for param in param_iter {
            params_s += ", ";
            params_s += &param.to_string();
        }

        params_s += ")";

        f.write_str(&params_s)
    }
}
