use std::fmt::Display;

use crate::ast::{AstNode, AstNodeKind, BinaryOperatorKind, UnaryOperatorKind};


impl AstNode {
    pub fn visit(&self, visitor: &mut impl AstVisitor) {
        match &self.kind {
            AstNodeKind::BadNode => visitor.visit_bad_node(),
            AstNodeKind::IntegerLiteral(value) => visitor.visit_integer(*value),
            AstNodeKind::BinaryExpression(left, op, right) => {
                left.visit(visitor);
                right.visit(visitor);
                visitor.visit_binary_expression(op)
            }
            AstNodeKind::UnaryExpression(op, expr) => {
                expr.visit(visitor);
                visitor.visit_unary_expression(op)
            }
        }
    }
}

pub trait AstVisitor {
    fn visit_integer(&mut self, value: i32);
    fn visit_binary_expression(&mut self, op: &BinaryOperatorKind);
    fn visit_unary_expression(&mut self, op: &UnaryOperatorKind);
    fn visit_bad_node(&mut self);
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
        AstNodeKind::BinaryExpression(l, op, r) => {
            f.write_fmt(format_args!("{}{} {}\n", padding, marker, op))?;
            display_helper(&l, f, &child_padding, false)?;
            display_helper(&r, f, &child_padding, true)
        }
        AstNodeKind::UnaryExpression(op, expr) => {
            f.write_fmt(format_args!("{}{} {}\n", padding, marker, op))?;
            display_helper(expr, f, &child_padding, true)
        }
    }
}

impl Display for BinaryOperatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOperatorKind::Add => "+",
            BinaryOperatorKind::Subtract => "-",
            BinaryOperatorKind::Mulitply => "*",
            BinaryOperatorKind::Divide => "/",
        };
        f.write_str(s)
    }
}

impl Display for UnaryOperatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            UnaryOperatorKind::Identity => "+",
            UnaryOperatorKind::Negate => "-",
        };
        f.write_str(s)
    }
}
