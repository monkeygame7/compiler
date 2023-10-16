use std::fmt::Display;

use colored::Colorize;

use super::{
    visitor::AstVisitor, Ast, BinaryExpr, BlockExpr, Expr, IntegerExpr, ParenExpr, UnaryExpr,
    VariableExpr,
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
}

impl AstVisitor for AstPrinter {
    fn visit_error(&mut self, ast: &mut Ast, span: &crate::text::TextSpan, expr: &Expr) {
        self.append_item("(ERROR)".red());
    }

    fn visit_integer_expr(&mut self, ast: &mut Ast, int_expr: &IntegerExpr, expr: &Expr) {
        self.append_item(&int_expr.token.to_string().bright_blue());
    }

    fn visit_paren_expr(&mut self, ast: &mut Ast, paren_expr: &ParenExpr, expr: &Expr) {
        self.append_item("(paren)".truecolor(100, 100, 100));

        let was_last = self.is_last;
        self.indent();

        self.is_last = true;
        self.visit_expr(ast, paren_expr.expr);

        self.is_last = was_last;
        self.unindent();
    }

    fn visit_binary_expr(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, expr: &Expr) {
        self.append_item(
            binary_expr
                .operator
                .to_string()
                .black()
                .on_truecolor(50, 50, 50),
        );

        let was_last = self.is_last;
        self.indent();

        self.is_last = false;
        self.visit_expr(ast, binary_expr.left);
        self.is_last = true;
        self.visit_expr(ast, binary_expr.right);

        self.is_last = was_last;
        self.unindent();
    }

    fn visit_unary_expr(&mut self, ast: &mut Ast, unary_expr: &UnaryExpr, expr: &Expr) {
        self.append_item(
            unary_expr
                .operator
                .to_string()
                .black()
                .on_truecolor(50, 50, 50),
        );

        let was_last = self.is_last;
        self.indent();

        self.is_last = true;
        self.visit_expr(ast, unary_expr.operand);

        self.is_last = was_last;
        self.unindent();
    }

    fn visit_block_expr(&mut self, ast: &mut Ast, block_expr: &BlockExpr, expr: &Expr) {
        self.append_item("(block)".truecolor(100, 100, 100));

        let was_last = self.is_last;
        self.indent();

        for (idx, stmt) in block_expr.stmts.iter().enumerate() {
            self.is_last = idx == block_expr.stmts.len();
            self.visit_stmt(ast, *stmt);
        }

        self.is_last = was_last;
        self.unindent();
    }

    fn visit_variable_expr(&mut self, ast: &mut Ast, variable_expr: &VariableExpr, expr: &Expr) {
        self.append_item(&variable_expr.token.to_string().green());
    }
}