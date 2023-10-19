use std::fmt::Display;

use colored::Colorize;

use super::{
    lexer::SyntaxToken, visitor::AstVisitor, Ast, BinaryExpr, BlockExpr, Expr, IfExpr, IntegerExpr,
    ParenExpr, UnaryExpr, VariableExpr, WhileStmt,
};

pub struct AstPrinter {
    indent: Vec<&'static str>,
    pub result: String,
    is_last: bool,
}

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter {
            indent: vec![],
            result: String::new(),
            is_last: true,
        }
    }

    fn indent(&mut self) {
        let padding = if self.indent.is_empty() {
            "  "
        } else if self.is_last {
            "   "
        } else {
            " │ "
        };

        self.indent.push(padding);
    }

    fn unindent(&mut self) {
        self.indent.pop();
    }

    fn append_item(&mut self, s: impl Display) {
        let indentation = self.indent.iter().map(|s| *s).collect::<String>();
        let indentation = indentation.truecolor(100, 100, 100);
        let marker = self.marker().truecolor(100, 100, 100);
        self.result += &format!("{}{} {}\n", indentation, marker, s);
    }

    fn marker(&self) -> &'static str {
        if self.indent.is_empty() {
            "──"
        } else if self.is_last {
            " └─"
        } else {
            " ├─"
        }
    }

    fn append_keyword(&mut self, token: &SyntaxToken) {
        self.append_item(format!("<{}>", token.literal).cyan());
    }

    fn append_operator(&mut self, token: impl Display) {
        self.append_item(token.to_string().black().on_truecolor(100, 100, 100));
    }

    fn append_structural(&mut self, label: &str) {
        self.append_item(
            format!("({})", label)
                .truecolor(60, 60, 60)
                .on_truecolor(30, 30, 30),
        );
    }
}

impl AstVisitor for AstPrinter {
    fn visit_error(&mut self, _ast: &mut Ast, _span: &crate::text::TextSpan, _expr: &Expr) {
        self.append_item("(ERROR)".red());
    }

    fn visit_integer_expr(&mut self, _ast: &mut Ast, int_expr: &IntegerExpr, _expr: &Expr) {
        self.append_item(&int_expr.token.to_string().bright_blue());
    }

    fn visit_boolean_expr(&mut self, _ast: &mut Ast, bool_expr: &super::BooleanExpr, _expr: &Expr) {
        self.append_item(&bool_expr.token.to_string().bright_yellow());
    }

    fn visit_assign_expr(&mut self, ast: &mut Ast, assign_expr: &super::AssignExpr, _expr: &Expr) {
        self.append_operator("=");

        let was_last = self.is_last;
        self.indent();

        self.is_last = false;
        self.append_item(assign_expr.identifier.to_string().green());
        self.is_last = true;
        self.visit_expr(ast, assign_expr.rhs);

        self.is_last = was_last;
        self.unindent();
    }

    fn visit_paren_expr(&mut self, ast: &mut Ast, paren_expr: &ParenExpr, _expr: &Expr) {
        self.append_structural("paren");

        let was_last = self.is_last;
        self.indent();

        self.is_last = true;
        self.visit_expr(ast, paren_expr.expr);

        self.is_last = was_last;
        self.unindent();
    }

    fn visit_binary_expr(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, _expr: &Expr) {
        self.append_operator(&binary_expr.operator.token);

        let was_last = self.is_last;
        self.indent();

        self.is_last = false;
        self.visit_expr(ast, binary_expr.left);
        self.is_last = true;
        self.visit_expr(ast, binary_expr.right);

        self.is_last = was_last;
        self.unindent();
    }

    fn visit_unary_expr(&mut self, ast: &mut Ast, unary_expr: &UnaryExpr, _expr: &Expr) {
        self.append_operator(&unary_expr.operator.token);

        let was_last = self.is_last;
        self.indent();

        self.is_last = true;
        self.visit_expr(ast, unary_expr.operand);

        self.is_last = was_last;
        self.unindent();
    }

    fn visit_block_expr(&mut self, ast: &mut Ast, block_expr: &BlockExpr, _expr: &Expr) {
        self.append_structural("block");

        let was_last = self.is_last;
        self.indent();

        for (idx, stmt) in block_expr.stmts.iter().enumerate() {
            self.is_last = idx == block_expr.stmts.len() - 1;
            self.visit_stmt(ast, *stmt);
        }

        self.is_last = was_last;
        self.unindent();
    }

    fn visit_variable_expr(&mut self, _ast: &mut Ast, variable_expr: &VariableExpr, _expr: &Expr) {
        self.append_item(&variable_expr.token.to_string().green());
    }

    fn visit_let_stmt(&mut self, ast: &mut Ast, let_stmt: &super::LetStmt, _stmt: &super::Stmt) {
        self.append_keyword(&let_stmt.keyword);

        let was_last = self.is_last;
        self.indent();

        self.is_last = false;
        self.append_item(&let_stmt.identifier.literal.green());
        self.is_last = true;
        self.visit_expr(ast, let_stmt.initial);

        self.is_last = was_last;
        self.unindent();
    }

    fn visit_if_expr(&mut self, ast: &mut Ast, if_expr: &super::IfExpr, _expr: &Expr) {
        self.append_keyword(&if_expr.keyword);

        let was_last = self.is_last;
        self.indent();

        self.is_last = false;
        self.visit_expr(ast, if_expr.condition);
        if if_expr.else_clause.is_none() {
            self.is_last = true;
        }
        self.visit_expr(ast, if_expr.then_clause);
        if let Some(else_clause) = &if_expr.else_clause {
            self.is_last = true;
            self.visit_expr(ast, else_clause.body);
        }

        self.is_last = was_last;
        self.unindent();
    }
}
