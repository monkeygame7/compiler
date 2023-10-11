use std::fmt::Display;

use crate::diagnostics::{DiagnosticBag, TextSpan};

pub mod lexer;
pub mod parser;

pub struct Ast {
    pub root: AstNode,
    pub diagnostics: DiagnosticBag,
}

pub enum AstNodeKind {
    BadNode,
    IntegerLiteral(i32),
    BooleanLiteral(bool),
    BinaryExpression(AstNode, BinaryOperator, AstNode),
    UnaryExpression(UnaryOperator, AstNode),
    Identifier(String),
    Scope(AstNode),
}

pub struct AstNode {
    pub kind: Box<AstNodeKind>,
    pub span: TextSpan,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum BinaryOperatorKind {
    Add,
    Subtract,
    Mulitply,
    Divide,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,
}

pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
    pub span: TextSpan,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum UnaryOperatorKind {
    Identity,
    Negate,
    LogicalNot,
}

pub struct UnaryOperator {
    pub kind: UnaryOperatorKind,
    pub span: TextSpan,
}

impl Display for BinaryOperatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOperatorKind::Add => "+",
            BinaryOperatorKind::Subtract => "-",
            BinaryOperatorKind::Mulitply => "*",
            BinaryOperatorKind::Divide => "/",
            BinaryOperatorKind::LogicalAnd => "&&",
            BinaryOperatorKind::LogicalOr => "||",
            BinaryOperatorKind::BitwiseAnd => "&",
            BinaryOperatorKind::BitwiseOr => "|",
            BinaryOperatorKind::Equals => "==",
            BinaryOperatorKind::NotEquals => "!=",
            BinaryOperatorKind::LessThan => "<",
            BinaryOperatorKind::LessThanOrEquals => "<=",
            BinaryOperatorKind::GreaterThan => ">",
            BinaryOperatorKind::GreaterThanOrEquals => ">=",
        };
        f.write_str(s)
    }
}

impl Display for UnaryOperatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            UnaryOperatorKind::Identity => "+",
            UnaryOperatorKind::Negate => "-",
            UnaryOperatorKind::LogicalNot => "!",
        };
        f.write_str(s)
    }
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

    match node.kind.as_ref() {
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
