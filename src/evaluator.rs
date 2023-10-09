use crate::{
    diagnostics::DiagnosticBag,
    visitor::AstVisitor, ast::{Ast, BinaryOperatorKind, UnaryOperatorKind},
};

pub struct Evaluator {
    root: Ast,
}

pub enum ResultType {
    IntegerResult(i32),
    VoidResult,
}
use ResultType::*;

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
        self.stack.push(ResultType::IntegerResult(value))
    }

    fn visit_binary_expression(&mut self, op: &BinaryOperatorKind) {
        let right_result = self.stack.pop();
        let left_result = self.stack.pop();

        match (left_result, right_result) {
            (Some(IntegerResult(l)), Some(IntegerResult(r))) => {
                let result = match op {
                    BinaryOperatorKind::Add => l + r,
                    BinaryOperatorKind::Subtract => l - r,
                    BinaryOperatorKind::Mulitply => l * r,
                    BinaryOperatorKind::Divide => l / r,
                };
                self.stack.push(IntegerResult(result))
            }
            _ => todo!("Unsupported binary operation"),
        }
    }

    fn visit_unary_expression(&mut self, op: &UnaryOperatorKind) {
        let expr_result = self.stack.pop();
        match expr_result {
            Some(IntegerResult(i)) => {
                let result = match op {
                    UnaryOperatorKind::Identity => i,
                    UnaryOperatorKind::Negate => -i,
                };
                self.stack.push(IntegerResult(result))
            }
            _ => todo!("Unsupported unary operation"),
        }
    }

    fn visit_bad_node(&mut self) {}
}
