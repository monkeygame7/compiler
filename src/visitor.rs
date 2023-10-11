use std::fmt::Display;

use crate::{
    ast::{AstNode, AstNodeKind, BinaryOperatorKind, UnaryOperatorKind},
    diagnostics::TextSpan,
};

impl AstNode {
    pub fn visit(&self, visitor: &mut impl AstVisitor) {
        let span = &self.span;
        match &self.kind {
            AstNodeKind::BadNode => visitor.visit_bad_node(span),
            AstNodeKind::IntegerLiteral(i) => visitor.visit_integer(*i, span),
            AstNodeKind::BooleanLiteral(b) => visitor.visit_boolean(*b, span),
            AstNodeKind::Identifier(s) => visitor.visit_identifier(s, span),
            AstNodeKind::BinaryExpression(left, op, right) => {
                left.visit(visitor);
                right.visit(visitor);
                visitor.visit_binary_expression(&op.kind, &op.span)
            }
            AstNodeKind::UnaryExpression(op, expr) => {
                expr.visit(visitor);
                visitor.visit_unary_expression(&op.kind, &op.span)
            }
            AstNodeKind::Scope(expr) => {
                visitor.visit_scope_enter();
                expr.visit(visitor);
                visitor.visit_scope_exit();
            }
        }
    }
}

pub trait AstVisitor {
    fn visit_integer(&mut self, value: i32, span: &TextSpan);
    fn visit_boolean(&mut self, value: bool, span: &TextSpan);
    fn visit_identifier(&mut self, identifier: &String, span: &TextSpan);
    fn visit_binary_expression(&mut self, op: &BinaryOperatorKind, span: &TextSpan);
    fn visit_unary_expression(&mut self, op: &UnaryOperatorKind, span: &TextSpan);
    fn visit_scope_enter(&mut self);
    fn visit_scope_exit(&mut self);
    fn visit_bad_node(&mut self, span: &TextSpan);
}

impl Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_helper(self, f, "", true)
    }
}

fn display_helper(
    node: &AstNode,
    f: &mut std::fmt::Formatter<'_>,
    padding: &str,
    is_last: bool,
) -> std::fmt::Result {
    let last_marker = " └──";
    let middle_marker = " ├──";
    let marker = if is_last { last_marker } else { middle_marker };
    let child_padding = if is_last {
        padding.to_owned() + "    "
    } else {
        padding.to_owned() + " │  "
    };

    match &node.kind {
        AstNodeKind::BadNode => f.write_fmt(format_args!("{}{} {}\n", padding, marker, "ERROR")),
        AstNodeKind::IntegerLiteral(i) => {
            f.write_fmt(format_args!("{}{} {}\n", padding, marker, i))
        }
        AstNodeKind::BooleanLiteral(b) => {
            f.write_fmt(format_args!("{}{} {}\n", padding, marker, b))
        }
        AstNodeKind::Identifier(s) => f.write_fmt(format_args!("{}{} {}\n", padding, marker, s)),
        AstNodeKind::BinaryExpression(l, op, r) => {
            f.write_fmt(format_args!("{}{} {}\n", padding, marker, op.kind))?;
            display_helper(&l, f, &child_padding, false)?;
            display_helper(&r, f, &child_padding, true)
        }
        AstNodeKind::UnaryExpression(op, expr) => {
            f.write_fmt(format_args!("{}{} {}\n", padding, marker, op.kind))?;
            display_helper(expr, f, &child_padding, true)
        }
        AstNodeKind::Scope(expr) => {
            f.write_fmt(format_args!("{}{} {}\n", padding, marker, "{}"))?;
            display_helper(expr, f, &child_padding, true)
        }
    }
}
