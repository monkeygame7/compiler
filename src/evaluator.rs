use std::fmt::Display;

use crate::{
    ast::{Ast, BinaryOperatorKind, UnaryOperatorKind},
    diagnostics::{DiagnosticBag, TextSpan},
    visitor::AstVisitor,
};

pub struct Evaluator {
    root: Ast,
}

pub enum ResultType {
    IntegerResult(i32),
    BooleanResult(bool),
    VoidResult,
}
use ResultType::*;

impl Display for ResultType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IntegerResult(_) => "<int>",
            BooleanResult(_) => "<bool>",
            VoidResult => "<void>",
        };

        f.write_str(s)
    }
}

impl Evaluator {
    pub fn new(program: Ast) -> Evaluator {
        Evaluator { root: program }
    }

    pub fn evaluate(self) -> String {
        let mut visitor = EvaluatingVisitor::new(self.root.diagnostics);

        self.root.root.visit(&mut visitor);

        visitor
            .stack
            .pop()
            .map(|r| match r {
                IntegerResult(i) => i.to_string(),
                BooleanResult(b) => b.to_string(),
                VoidResult => "(void)".to_string(),
            })
            .unwrap_or("(void)".to_string())
    }
}

struct EvaluatingVisitor {
    stack: Vec<ResultType>,
    pub diagnostics: DiagnosticBag,
}

impl EvaluatingVisitor {
    fn new(diagnostics: DiagnosticBag) -> Self {
        Self {
            stack: vec![],
            diagnostics,
        }
    }
}

impl AstVisitor for EvaluatingVisitor {
    fn visit_integer(&mut self, value: i32) {
        self.stack.push(IntegerResult(value))
    }

    fn visit_boolean(&mut self, value: bool) {
        self.stack.push(BooleanResult(value))
    }

    fn visit_identifier(&mut self, value: &String) {
        todo!("visit identifier")
    }

    fn visit_binary_expression(&mut self, op: &BinaryOperatorKind) {
        let right_result = self.stack.pop().unwrap_or(VoidResult);
        let left_result = self.stack.pop().unwrap_or(VoidResult);

        let result = match (&left_result, &right_result) {
            (IntegerResult(l), IntegerResult(r)) => match op {
                BinaryOperatorKind::Add => IntegerResult(l + r),
                BinaryOperatorKind::Subtract => IntegerResult(l - r),
                BinaryOperatorKind::Mulitply => IntegerResult(l * r),
                BinaryOperatorKind::Divide => IntegerResult(l / r),
                BinaryOperatorKind::BitwiseAnd => IntegerResult(l & r),
                BinaryOperatorKind::BitwiseOr => IntegerResult(l | r),
                BinaryOperatorKind::Equals => BooleanResult(l == r),
                BinaryOperatorKind::NotEquals => BooleanResult(l != r),
                BinaryOperatorKind::GreaterThanOrEquals => BooleanResult(l >= r),
                BinaryOperatorKind::GreaterThan => BooleanResult(l > r),
                BinaryOperatorKind::LessThanOrEquals => BooleanResult(l <= r),
                BinaryOperatorKind::LessThan => BooleanResult(l < r),
                _ => VoidResult,
            },
            (BooleanResult(l), BooleanResult(r)) => match op {
                BinaryOperatorKind::LogicalAnd => BooleanResult(*l && *r),
                BinaryOperatorKind::LogicalOr => BooleanResult(*l || *r),
                BinaryOperatorKind::BitwiseAnd => BooleanResult(l & r),
                BinaryOperatorKind::BitwiseOr => BooleanResult(l | r),
                _ => VoidResult,
            },
            _ => VoidResult,
        };

        if matches!(result, VoidResult) {
            self.diagnostics.report_unsupported_binary_operator(
                left_result,
                op,
                right_result,
                TextSpan::new(0, 0),
            );
            todo!("Add span to visitor somehow")
        } else {
            self.stack.push(result)
        }
    }

    fn visit_unary_expression(&mut self, op: &UnaryOperatorKind) {
        let expr_result = self.stack.pop().unwrap_or(VoidResult);
        let result = match expr_result {
            IntegerResult(i) => match op {
                UnaryOperatorKind::Identity => IntegerResult(i),
                UnaryOperatorKind::Negate => IntegerResult(-i),
                _ => VoidResult,
            },
            BooleanResult(b) => match op {
                UnaryOperatorKind::LogicalNot => BooleanResult(!b),
                _ => VoidResult,
            },
            _ => VoidResult,
        };

        if matches!(result, VoidResult) {
            self.diagnostics.report_unsupported_unary_operator(
                op,
                expr_result,
                TextSpan::new(0, 0),
            );
            todo!("Add span to visitor somehow")
        } else {
            self.stack.push(result)
        }
    }

    fn visit_bad_node(&mut self) {}
}
