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
    BinaryExpression(Box<AstNode>, BinaryOperatorKind, Box<AstNode>),
    UnaryExpression(UnaryOperatorKind, Box<AstNode>),
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
}

pub enum UnaryOperatorKind {
    Identity,
    Negate,
}

