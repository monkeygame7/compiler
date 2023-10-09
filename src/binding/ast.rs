use std::fmt::Display;

use crate::{
    diagnostics::{TextSpan, DiagnosticBag},
    parse::{
        lexer::{SyntaxToken, TokenKind},
        parser::{SyntaxKind, SyntaxNode, SyntaxTree},
    },
};

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

pub struct Binder {
    pub diagnostics: DiagnosticBag,
}

impl Binder {
    fn new(diagnostics: DiagnosticBag) -> Self {
        Self { diagnostics }
    }

    pub fn bind(cst: SyntaxTree) -> Ast {
        let binder = Binder::new(cst.diagnostics);
        let root_span = cst.root.span;
        let root = binder.bind_syntax_node(cst.root, root_span);

        Ast {
            root,
            diagnostics: binder.diagnostics,
        }
    }

    fn bind_syntax_node(&self, node: SyntaxNode, span: TextSpan) -> AstNode {
        match node.kind {
            SyntaxKind::BadExpression => self.bind_bad_expression(span),
            SyntaxKind::IntegerExpression(_, value) => self.bind_integer_expression(value, span),
            SyntaxKind::BinaryExpression(left, op, right) => {
                self.bind_binary_expression(*left, op, *right, span)
            }
            SyntaxKind::UnaryExpression(op, expr) => self.bind_unary_expression(op, *expr, span),
            SyntaxKind::GroupExpression(_, expr, _) => self.bind_syntax_node(*expr, span),
        }
    }

    fn bind_bad_expression(&self, span: TextSpan) -> AstNode {
        self.diagnostics.report_bad_expression(span);
        AstNode{kind: AstNodeKind::BadNode, span}
    }

    fn bind_integer_expression(&self, value: i32, span: TextSpan) -> AstNode {
        AstNode {
            kind: AstNodeKind::IntegerLiteral(value),
            span,
        }
    }

    fn bind_binary_expression(
        &self,
        left: SyntaxNode,
        op: SyntaxToken,
        right: SyntaxNode,
        span: TextSpan,
    ) -> AstNode {
        let left_span = left.span;
        let right_span = right.span;
        let left = self.bind_syntax_node(left, left_span);
        let right = self.bind_syntax_node(right, right_span);

        let operator = match op.kind {
            TokenKind::PlusToken(_) => BinaryOperatorKind::Add,
            TokenKind::DashToken(_) => BinaryOperatorKind::Subtract,
            TokenKind::StarToken(_) => BinaryOperatorKind::Mulitply,
            TokenKind::SlashToken(_) => BinaryOperatorKind::Divide,
            _ => {
                let op_span = op.span;
                self.diagnostics.report_unrecognized_binary_operator(op, op_span);
                return AstNode{kind: AstNodeKind::BadNode, span}
            },
        };

        let kind = AstNodeKind::BinaryExpression(Box::new(left), operator, Box::new(right));

        AstNode { kind, span }
    }

    fn bind_unary_expression(&self, op: SyntaxToken, expr: SyntaxNode, span: TextSpan) -> AstNode {
        let expr_span = expr.span;
        let expr = self.bind_syntax_node(expr, expr_span);

        let operator = match op.kind {
            TokenKind::PlusToken(_) => UnaryOperatorKind::Identity,
            TokenKind::DashToken(_) => UnaryOperatorKind::Negate,
            _ => panic!("unsupported unary operation"),
        };

        let kind = AstNodeKind::UnaryExpression(operator, Box::new(expr));

        AstNode { kind, span }
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

    match &node.kind {
        AstNodeKind::BadNode => {
            f.write_fmt(format_args!("{}{} {}\n", padding, marker, "ERROR"))
        },
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
