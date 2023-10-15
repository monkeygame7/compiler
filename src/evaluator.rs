use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    ast::{Ast2, AstNode, AstNodeKind, BinaryOperatorKind, UnaryOperatorKind},
    diagnostics::DiagnosticBag,
    text::TextSpan,
};

pub struct Evaluator<'a> {
    ast: &'a Ast2,
    scopes: Vec<HashMap<String, ResultType>>,
    diagnostics: Rc<DiagnosticBag>,
}

#[derive(PartialEq, Eq, Clone)]
pub enum ResultType {
    Integer(i32),
    Boolean(bool),
    Void,
    Undefined,
}
use ResultType::*;

impl Display for ResultType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Integer(i) => i.to_string(),
            Boolean(b) => b.to_string(),
            Void => "(void)".to_string(),
            Undefined => "UNDEFINED".to_string(),
        };

        f.write_str(&s)
    }
}

impl Debug for ResultType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Integer(_) => "<int>",
            Boolean(_) => "<bool>",
            Void => "<void>",
            Undefined => "<undefined>",
        };

        f.write_str(s)
    }
}

impl<'a> Evaluator<'a> {
    pub fn new(ast: &'a Ast2, diagnostics: Rc<DiagnosticBag>) -> Evaluator {
        Evaluator {
            ast,
            scopes: vec![],
            diagnostics,
        }
    }

    pub fn evaluate(&mut self) -> ResultType {
        todo!();
        // let node = self.root;
        // self.evaluate_node(&node)
    }

    pub fn evaluate_node(&mut self, node: &AstNode) -> ResultType {
        let span = node.span;
        match node.kind.as_ref() {
            AstNodeKind::BadNode => self.evaluate_bad_node(span),
            AstNodeKind::IntegerLiteral(i) => self.evaluate_integer(*i, span),
            AstNodeKind::BooleanLiteral(b) => self.evaluate_boolean(*b, span),
            AstNodeKind::Identifier(s) => self.evaluate_identifier(&s, span),
            AstNodeKind::BinaryExpression(left, op, right) => {
                let left = self.evaluate_node(&left);
                let right = self.evaluate_node(&right);
                self.evaluate_binary_expression(left, op.kind, right, op.token.span)
            }
            AstNodeKind::UnaryExpression(op, expr) => {
                let result = self.evaluate_node(&expr);
                self.evaluate_unary_expression(op.kind, result, op.token.span)
            }
            AstNodeKind::Scope(expr) => {
                self.scopes.push(HashMap::new());
                let result = self.evaluate_node(&expr);
                self.scopes.pop();
                result
            }
            AstNodeKind::LetDeclaration(identifier, expr) => {
                todo!();
            }
            AstNodeKind::Statement => todo!(),
        }
    }

    fn evaluate_integer(&mut self, value: i32, span: TextSpan) -> ResultType {
        Integer(value)
    }

    fn evaluate_boolean(&mut self, value: bool, span: TextSpan) -> ResultType {
        Boolean(value)
    }

    fn evaluate_identifier(&mut self, identifier: &String, span: TextSpan) -> ResultType {
        let value = self
            .scopes
            .last()
            .map(|scope| scope.get(identifier))
            .flatten();

        match value {
            Some(result) => result.clone(),
            None => {
                self.diagnostics
                    .report_identifier_not_found(identifier, span);
                Undefined
            }
        }
    }

    fn evaluate_binary_expression(
        &mut self,
        left_result: ResultType,
        op: BinaryOperatorKind,
        right_result: ResultType,
        span: TextSpan,
    ) -> ResultType {
        let result = match (&left_result, &right_result) {
            (Integer(l), Integer(r)) => match op {
                BinaryOperatorKind::Add => Integer(l + r),
                BinaryOperatorKind::Subtract => Integer(l - r),
                BinaryOperatorKind::Mulitply => Integer(l * r),
                BinaryOperatorKind::Divide => Integer(l / r),
                BinaryOperatorKind::BitwiseAnd => Integer(l & r),
                BinaryOperatorKind::BitwiseOr => Integer(l | r),
                BinaryOperatorKind::Equals => Boolean(l == r),
                BinaryOperatorKind::NotEquals => Boolean(l != r),
                BinaryOperatorKind::GreaterThanOrEquals => Boolean(l >= r),
                BinaryOperatorKind::GreaterThan => Boolean(l > r),
                BinaryOperatorKind::LessThanOrEquals => Boolean(l <= r),
                BinaryOperatorKind::LessThan => Boolean(l < r),
                _ => Undefined,
            },
            (Boolean(l), Boolean(r)) => match op {
                BinaryOperatorKind::LogicalAnd => Boolean(*l && *r),
                BinaryOperatorKind::LogicalOr => Boolean(*l || *r),
                BinaryOperatorKind::BitwiseAnd => Boolean(l & r),
                BinaryOperatorKind::BitwiseOr => Boolean(l | r),
                _ => Undefined,
            },
            _ => Undefined,
        };

        if matches!(result, Undefined) {
            self.diagnostics
                .report_unsupported_binary_operator(left_result, op, right_result, span)
        }
        result
    }

    fn evaluate_unary_expression(
        &mut self,
        op: UnaryOperatorKind,
        expr_result: ResultType,
        span: TextSpan,
    ) -> ResultType {
        let result = match expr_result {
            Integer(i) => match op {
                UnaryOperatorKind::Identity => Integer(i),
                UnaryOperatorKind::Negate => Integer(-i),
                _ => Undefined,
            },
            Boolean(b) => match op {
                UnaryOperatorKind::LogicalNot => Boolean(!b),
                _ => Undefined,
            },
            _ => Undefined,
        };

        if matches!(result, Undefined) {
            self.diagnostics
                .report_unsupported_unary_operator(op, expr_result, span);
        }
        result
    }

    fn evaluate_bad_node(&mut self, span: TextSpan) -> ResultType {
        self.diagnostics.report_invalid_expression(span);
        Undefined
    }
}

#[cfg(test)]
mod test {
    use std::{rc::Rc, vec};

    use crate::{ast::parser::Parser, text::SourceText};

    use super::*;

    struct TestCase {
        input: String,
        actual_input: SourceText,
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
                actual_input: SourceText::from(&actual_input).unwrap(),
                expected_spans,
            }
        }

        fn evaluate(&self, expected_result: ResultType) {
            let diagnostics = Rc::new(DiagnosticBag::new());
            let tree = Parser::parse(&self.actual_input, diagnostics.clone()).unwrap();
            let mut evaluator = Evaluator::new(&tree, diagnostics.clone());
            let result = evaluator.evaluate();

            if diagnostics.has_errors() {
                let mut messages = vec![];
                let mut errors: Vec<_> = diagnostics
                    .messages
                    .borrow()
                    .iter()
                    .map(|dm| {
                        let dm = dm.clone();
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
            } else {
                assert!(
                    self.expected_spans.is_empty(),
                    "Expected errors in '{}' but found none",
                    self.input
                );
                assert_eq!(result, expected_result);
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
        let undefined_cases = vec![
            "[[]]",
            "[[$]]",
            "[(1 + 2[]]",
            "[-]true",
            "13 [@]",
            "4 [&&] 5",
            "true [+] false",
            "([[$]] [+] 2) [-] 4",
            "([[$]]) [+] 4",
            "[foo] [+] [bar]",
        ];

        int_cases
            .into_iter()
            .map(|(s, i)| (s, Integer(i)))
            .chain(bool_cases.into_iter().map(|(s, b)| (s, Boolean(b))))
            .chain(undefined_cases.into_iter().map(|s| (s, Undefined)))
            .collect()
    }
}
