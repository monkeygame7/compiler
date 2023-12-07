use std::fmt::Display;

use colored::Colorize;

use crate::{diagnostics::TextSpan, parsing::SyntaxToken};

use super::{nodes::*, visitor::AstVisitor, Ast};

pub struct AstPrinter {
    indent: Vec<&'static str>,
    pub result: String,
    is_last: bool,
}
const VERBOSE: bool = false;

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
    fn visit_func_decl(&mut self, ast: &Ast, func: &FunctionDecl, _item: &Item) {
        self.append_item(format!(
            "{} {}",
            func.keyword.literal.truecolor(60, 60, 60),
            func.name.literal.yellow()
        ));

        nested(self, |printer| {
            printer.is_last = false;
            printer.append_item(format!(
                "return ({})",
                func.return_type
                    .as_ref()
                    .map(|t| t.typ.literal.as_str())
                    .unwrap_or("void")
                    .yellow()
            ));
            if func.params.len() > 0 {
                printer.append_structural("params");
                nested(printer, |printer| {
                    printer.is_last = false;
                    nested(printer, |printer| {
                        for (i, param) in func.params.iter_items().enumerate() {
                            printer.is_last = i == func.params.len() - 1;
                            printer.append_item(format!(
                                "{} ({})",
                                param.token.literal.green(),
                                param.type_decl.typ.literal.yellow()
                            ))
                        }
                    });
                });
            }
            printer.is_last = true;
            printer.visit_expr(ast, func.body);
        });
    }

    fn visit_stmt(&mut self, ast: &Ast, stmt: super::StmtId) {
        if VERBOSE {
            self.append_structural("stmt");
            nested(self, |printer| {
                printer.is_last = true;
                let stmt = ast.query_stmt(stmt);
                printer.do_visit_stmt(ast, stmt);
            });
        } else {
            let stmt = ast.query_stmt(stmt);
            self.do_visit_stmt(ast, stmt);
        }
    }

    fn visit_variable_decl(
        &mut self,
        ast: &Ast,
        variable_decl: &super::VariableDecl,
        _stmt: &super::Stmt,
    ) {
        self.append_keyword(&variable_decl.keyword);

        nested(self, |printer| {
            printer.is_last = false;
            printer.append_item(&variable_decl.identifier.literal.green());
            printer.is_last = true;
            printer.visit_expr(ast, variable_decl.initial);
        });
    }

    fn visit_while_stmt(&mut self, ast: &Ast, while_stmt: &WhileStmt, _stmt: &super::Stmt) {
        self.append_keyword(&while_stmt.keyword);

        nested(self, |printer| {
            printer.is_last = false;
            printer.visit_expr(ast, while_stmt.condition);
            printer.is_last = true;
            printer.visit_expr(ast, while_stmt.body);
        });
    }

    fn visit_return_stmt(&mut self, ast: &Ast, return_stmt: &ReturnStmt, _stmt: &Stmt) {
        self.append_keyword(&return_stmt.keyword);

        if let Some(expr) = return_stmt.value {
            nested(self, |printer| {
                printer.is_last = true;
                printer.visit_expr(ast, expr);
            })
        }
    }

    fn visit_error(&mut self, _ast: &Ast, _span: &TextSpan, _expr: &Expr) {
        self.append_item("(ERROR)".red());
    }

    fn visit_integer_expr(&mut self, _ast: &Ast, int_expr: &IntegerExpr, _expr: &Expr) {
        self.append_item(&int_expr.token.to_string().bright_blue());
    }

    fn visit_boolean_expr(&mut self, _ast: &Ast, bool_expr: &super::BooleanExpr, _expr: &Expr) {
        self.append_item(&bool_expr.token.to_string().bright_yellow());
    }

    fn visit_assign_expr(&mut self, ast: &Ast, assign_expr: &super::AssignExpr, _expr: &Expr) {
        self.append_operator("=");

        nested(self, |printer| {
            printer.is_last = false;
            printer.append_item(assign_expr.identifier.to_string().green());
            printer.is_last = true;
            printer.visit_expr(ast, assign_expr.rhs);
        });
    }

    fn visit_paren_expr(&mut self, ast: &Ast, paren_expr: &ParenExpr, _expr: &Expr) {
        self.append_structural("paren");

        nested(self, |printer| {
            printer.is_last = true;
            printer.visit_expr(ast, paren_expr.expr);
        });
    }

    fn visit_binary_expr(&mut self, ast: &Ast, binary_expr: &BinaryExpr, _expr: &Expr) {
        self.append_operator(&binary_expr.operator.token);

        nested(self, |printer| {
            printer.is_last = false;
            printer.visit_expr(ast, binary_expr.left);
            printer.is_last = true;
            printer.visit_expr(ast, binary_expr.right);
        });
    }

    fn visit_unary_expr(&mut self, ast: &Ast, unary_expr: &UnaryExpr, _expr: &Expr) {
        self.append_operator(&unary_expr.operator.token);

        nested(self, |printer| {
            printer.is_last = true;
            printer.visit_expr(ast, unary_expr.operand);
        });
    }

    fn visit_block_expr(&mut self, ast: &Ast, block_expr: &BlockExpr, _expr: &Expr) {
        self.append_structural("block");

        nested(self, |printer| {
            for (idx, stmt) in block_expr.stmts.iter().enumerate() {
                printer.is_last = idx == block_expr.stmts.len() - 1;
                printer.visit_stmt(ast, *stmt);
            }
        });
    }

    fn visit_variable_expr(&mut self, _ast: &Ast, variable_expr: &VariableExpr, _expr: &Expr) {
        self.append_item(&variable_expr.token.to_string().green());
    }

    fn visit_if_expr(&mut self, ast: &Ast, if_expr: &IfExpr, _expr: &Expr) {
        self.append_keyword(&if_expr.keyword);

        nested(self, |printer| {
            printer.is_last = false;
            printer.visit_expr(ast, if_expr.condition);
            if if_expr.else_clause.is_none() {
                printer.is_last = true;
            }
            printer.append_structural("then");
            nested(printer, |printer| {
                printer.is_last = true;
                printer.visit_expr(ast, if_expr.then_clause);
            });
            if let Some(else_clause) = &if_expr.else_clause {
                printer.is_last = true;
                printer.append_structural("else");
                nested(printer, |printer| {
                    printer.is_last = true;
                    printer.visit_expr(ast, else_clause.body);
                });
            }
        });
    }

    fn visit_call_expr(&mut self, ast: &Ast, call_expr: &CallExpr, _expr: &Expr) {
        self.append_structural("call");

        nested(self, |p| {
            p.is_last = call_expr.args.is_empty();
            p.append_structural("target");
            nested(p, |p| {
                p.is_last = true;
                p.visit_expr(ast, call_expr.callee);
            });
            if !p.is_last {
                p.is_last = true;
                p.append_structural("args");
                nested(p, |p| {
                    p.is_last = false;
                    for arg in call_expr.args.iter_items() {
                        if arg == call_expr.args.last_item().unwrap() {
                            p.is_last = true;
                        }
                        p.visit_expr(ast, *arg);
                    }
                });
            }
        })
    }
}

fn nested(printer: &mut AstPrinter, mut func: impl FnMut(&mut AstPrinter)) {
    let was_last = printer.is_last;
    printer.indent();

    func(&mut *printer);

    printer.unindent();
    printer.is_last = was_last;
}
