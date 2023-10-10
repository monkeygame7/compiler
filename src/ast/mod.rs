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
    BinaryExpression(Box<AstNode>, BinaryOperatorKind, Box<AstNode>),
    UnaryExpression(UnaryOperatorKind, Box<AstNode>),
    Identifier(String),
}

pub struct AstNode {
    pub kind: AstNodeKind,
    pub span: TextSpan,
}

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

pub enum UnaryOperatorKind {
    Identity,
    Negate,
    LogicalNot,
}
