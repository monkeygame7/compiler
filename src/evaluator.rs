use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use crate::{
    ast::{Ast, BinaryOperatorKind, UnaryOperatorKind},
    diagnostics::{DiagnosticBag, TextSpan},
    visitor::AstVisitor,
};

pub struct Evaluator {
    root: Ast,
}

#[derive(PartialEq, Eq, Clone)]
pub enum ResultType {
    IntegerResult(i32),
    BooleanResult(bool),
    VoidResult,
}
use ResultType::*;

impl Display for ResultType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IntegerResult(i) => i.to_string(),
            BooleanResult(b) => b.to_string(),
            VoidResult => "(void)".to_string(),
        };

        f.write_str(&s)
    }
}

impl Debug for ResultType {
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

    pub fn evaluate(self) -> Result<ResultType, DiagnosticBag> {
        let mut visitor = EvaluatingVisitor::new(self.root.diagnostics);

        self.root.root.visit(&mut visitor);

        let result = visitor.stack.pop().unwrap_or(VoidResult);

        if visitor.diagnostics.has_errors() {
            Err(visitor.diagnostics)
        } else {
            Ok(result)
        }
    }
}

struct EvaluatingVisitor {
    stack: Vec<ResultType>,
    scopes: Vec<HashMap<String, ResultType>>,
    pub diagnostics: DiagnosticBag,
}

impl EvaluatingVisitor {
    fn new(diagnostics: DiagnosticBag) -> Self {
        Self {
            stack: vec![],
            scopes: vec![],
            diagnostics,
        }
    }
}

impl AstVisitor for EvaluatingVisitor {
    fn visit_integer(&mut self, value: i32, span: &TextSpan) {
        self.stack.push(IntegerResult(value))
    }

    fn visit_boolean(&mut self, value: bool, span: &TextSpan) {
        self.stack.push(BooleanResult(value))
    }

    fn visit_identifier(&mut self, identifier: &String, span: &TextSpan) {
        let value = self
            .scopes
            .last()
            .map(|scope| scope.get(identifier))
            .flatten();

        match value {
            Some(result) => self.stack.push(result.clone()),
            None => self
                .diagnostics
                .report_identifier_not_found(identifier, *span),
        }
    }

    fn visit_binary_expression(&mut self, op: &BinaryOperatorKind, span: &TextSpan) {
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
                *span,
            );
        } else {
            self.stack.push(result)
        }
    }

    fn visit_unary_expression(&mut self, op: &UnaryOperatorKind, span: &TextSpan) {
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
            self.diagnostics
                .report_unsupported_unary_operator(op, expr_result, *span);
        } else {
            self.stack.push(result)
        }
    }

    fn visit_scope_enter(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn visit_scope_exit(&mut self) {
        let scope = self.scopes.pop();
        if matches!(scope, None) {
            panic!("wtf how?");
        }
    }

    fn visit_bad_node(&mut self, span: &TextSpan) {}
}

#[cfg(test)]
mod test {
    use std::vec;

    use crate::ast::parser::Parser;

    use super::*;

    struct TestCase {
        input: String,
        actual_input: String,
        expected_spans: Vec<TextSpan>,
    }

    impl TestCase {
        fn run(input: &str, expected_result: ResultType) {
            let case = Self::init(input);
            case.evaluate(expected_result)
        }

        fn init(input: &str) -> Self {
            let mut actual_input = String::new();
            let mut expected_spans = vec![];
            let mut starts_stack = vec![];
            let mut pos = 0;

            input.chars().for_each(|c| {
                if c == '[' {
                    starts_stack.push(pos);
                } else if c == ']' {
                    let start = starts_stack.pop().unwrap();
                    expected_spans.push(TextSpan::new(start, pos));
                } else {
                    pos += 1;
                    actual_input.push(c);
                }
            });
            assert_eq!(pos, actual_input.len(), "Something went wrong...");
            expected_spans.sort();
            TestCase {
                input: input.to_string(),
                actual_input,
                expected_spans,
            }
        }

        fn evaluate(&self, expected_result: ResultType) {
            let tree = Parser::parse(self.actual_input.to_owned());
            let evaluator = Evaluator::new(tree);
            let result = evaluator.evaluate();

            match result {
                Ok(r) => {
                    assert!(
                        self.expected_spans.is_empty(),
                        "Expected errors in '{}' but found none",
                        self.input
                    );
                    assert_eq!(r, expected_result);
                }
                Err(errors) => {
                    let mut messages = vec![];
                    let mut errors: Vec<_> = errors
                        .into_iter()
                        .map(|dm| {
                            messages.push(dm.message);
                            dm.span
                        })
                        .collect();
                    assert!(
                        self.expected_spans.len() > 0,
                        "Expected no errors in '{}' but found:\n{:?}",
                        self.input,
                        messages,
                    );

                    errors.sort();
                    assert_eq!(
                        errors, self.expected_spans,
                        "Unexpected errors in '{}':\n{:?}",
                        self.input, messages
                    );
                }
            }
        }
    }

    #[test]
    fn test_evaluator() {
        test_cases()
            .into_iter()
            .for_each(|(input, expected)| TestCase::run(input, expected))
    }

    fn test_cases() -> Vec<(&'static str, ResultType)> {
        let int_cases = vec![
            ("1", 1),
            ("123", 123),
            ("1 + 1", 2),
            ("-4 + 4", 0),
            ("-(4 + 4)", -8),
            ("3 * -2", -6),
            ("10 / 5", 2),
            ("2 + 4 * 3 - 4 / 2", 12),
            ("123 & -1", 123),
            ("123 & 0", 0),
            ("123 | 0", 123),
            ("123 | -1", -1),
        ];
        let bool_cases = vec![
            ("true", true),
            ("false", false),
            ("!true", !true),
            ("!false", !false),
            ("true & false", false),
            ("true & true", true),
            ("false | false", false),
            ("false | true", true),
            ("true && false", false),
            ("true && true", true),
            ("true || false", true),
            ("false || false", false),
            ("1 == 1", true),
            ("1 == 2", false),
            ("1 != 1", false),
            ("1 != 2", true),
            ("1 > 1", false),
            ("1 >= 1", true),
            ("2 > 1", true),
            ("2 >= 1", true),
            ("1 < 1", false),
            ("1 <= 1", true),
            ("1 < 2", true),
            ("1 <= 2", true),
        ];
        let void_cases = vec![
            "[]",
            "[$]",
            "(1 + 2[]",
            "[-]true",
            "13 [@]",
            "4 [&&] 5",
            "true [+] false",
            "([$] [+] 2) [-] 4",
            "([$]) [+] 4",
        ];

        int_cases
            .into_iter()
            .map(|(s, i)| (s, IntegerResult(i)))
            .chain(bool_cases.into_iter().map(|(s, b)| (s, BooleanResult(b))))
            .chain(void_cases.into_iter().map(|s| (s, VoidResult)))
            .collect()
    }
}
