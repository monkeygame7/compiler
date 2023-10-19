use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use crate::{
    ast::{
        visitor::AstVisitor, AssignExpr, Ast, BinaryExpr, BinaryOperatorKind, BlockExpr,
        BooleanExpr, Expr, IntegerExpr, LetStmt, Stmt, UnaryExpr, UnaryOperatorKind, VariableExpr,
    },
    scope::VariableId,
    text::TextSpan,
};

pub struct Evaluator {
    scopes: Vec<HashMap<VariableId, ResultType>>,
    last_result: Option<ResultType>,
}

#[derive(PartialEq, Eq, Clone, Debug, Copy)]
pub enum ResultType {
    Integer(i32),
    Boolean(bool),
    Void,
    Undefined,
}

impl Display for ResultType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

use ResultType::*;

impl AstVisitor for Evaluator {
    fn visit_error(&mut self, _ast: &mut Ast, _span: &TextSpan, _expr: &Expr) {
        self.last_result = Some(Undefined);
    }

    fn visit_integer_expr(&mut self, _ast: &mut Ast, int_expr: &IntegerExpr, _expr: &Expr) {
        self.last_result = Some(Integer(int_expr.value));
    }

    fn visit_boolean_expr(&mut self, _ast: &mut Ast, bool_expr: &BooleanExpr, _expr: &Expr) {
        self.last_result = Some(Boolean(bool_expr.value));
    }

    fn visit_assign_expr(&mut self, ast: &mut Ast, assign_expr: &AssignExpr, _expr: &Expr) {
        self.visit_expr(ast, assign_expr.rhs);
        let value = self.last_result.unwrap();

        let scope = self
            .scopes
            .iter_mut()
            .rev()
            .filter(|scope| scope.contains_key(&assign_expr.variable))
            .nth(0)
            .expect("Variable not found");

        scope.insert(assign_expr.variable, value);

        self.last_result = Some(value)
    }

    fn visit_binary_expr(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, _expr: &Expr) {
        self.visit_expr(ast, binary_expr.left);
        let left = self.last_result.take().unwrap();
        self.visit_expr(ast, binary_expr.right);
        let right = self.last_result.take().unwrap();

        let result = match (&left, &right) {
            (Integer(l), Integer(r)) => match binary_expr.operator.kind {
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
            (Boolean(l), Boolean(r)) => match binary_expr.operator.kind {
                BinaryOperatorKind::LogicalAnd => Boolean(*l && *r),
                BinaryOperatorKind::LogicalOr => Boolean(*l || *r),
                BinaryOperatorKind::BitwiseAnd => Boolean(l & r),
                BinaryOperatorKind::BitwiseOr => Boolean(l | r),
                _ => Undefined,
            },
            _ => Undefined,
        };

        self.last_result = Some(result);
    }

    fn visit_unary_expr(&mut self, ast: &mut Ast, unary_expr: &UnaryExpr, _expr: &Expr) {
        self.visit_expr(ast, unary_expr.operand);
        let expr_result = self.last_result.take().unwrap();
        let result = match expr_result {
            Integer(i) => match unary_expr.operator.kind {
                UnaryOperatorKind::Identity => Integer(i),
                UnaryOperatorKind::Negate => Integer(-i),
                _ => Undefined,
            },
            Boolean(b) => match unary_expr.operator.kind {
                UnaryOperatorKind::LogicalNot => Boolean(!b),
                _ => Undefined,
            },
            _ => Undefined,
        };

        self.last_result = Some(result);
    }

    fn visit_variable_expr(&mut self, _ast: &mut Ast, variable_expr: &VariableExpr, _expr: &Expr) {
        self.last_result = self
            .scopes
            .iter()
            .rev()
            .flat_map(|vars| vars.get(&variable_expr.id))
            .nth(0)
            .cloned();
    }

    fn visit_let_stmt(&mut self, ast: &mut Ast, let_stmt: &LetStmt, _stmt: &Stmt) {
        self.visit_expr(ast, let_stmt.initial);
        let value = self.last_result.take().unwrap();

        let last_scope = self.scopes.last_mut().unwrap();
        assert!(!&last_scope.contains_key(&let_stmt.variable));
        last_scope.insert(let_stmt.variable, value);
        self.last_result = Some(Void);
    }

    fn visit_block_expr(&mut self, ast: &mut Ast, block_expr: &BlockExpr, expr: &Expr) {
        self.scopes.push(HashMap::new());
        self.do_visit_block_expr(ast, block_expr, expr);
        self.scopes.pop().expect("Unexpected empty scopes");
    }
}

impl Evaluator {
    fn new() -> Evaluator {
        Evaluator {
            scopes: vec![HashMap::new()],
            last_result: None,
        }
    }

    pub fn evaluate(ast: &mut Ast) -> ResultType {
        let mut evaluator = Self::new();
        ast.visit(&mut evaluator);
        evaluator.last_result.take().unwrap_or(Void)
    }
}

#[cfg(test)]
mod test {
    use std::vec;

    use crate::compilation::CompilationUnit;

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
            let compilation = CompilationUnit::compile(&self.actual_input, true);
            match compilation {
                Ok(mut unit) => {
                    let result = Evaluator::evaluate(&mut unit.ast);
                    assert!(
                        self.expected_spans.is_empty(),
                        "Expected errors in '{}' but found none",
                        self.input
                    );
                    assert_eq!(result, expected_result, "{}", self.input);
                }
                Err((_, diagnostics)) => {
                    assert!(diagnostics.has_errors());
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

    #[test]
    fn test_multiline() {
        let input = r#"
        {
            let x = 5
            let y = 10
            let z = x + y
            x = x * 2
            y = x + y
            y + z
        }
            "#;
        let expected = Integer(35);
        TestCase::run(input, expected);
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
            ("let x = 4 x = x = x + 5", 9),
            ("let x = 4 x = x + 5", 9),
            ("let x = 4 {let y = x let x = 2 y + x}", 6),
            ("let x = 4 {x + 2}", 6),
            ("let x = 4 + {let x = 2 - {let y = 10 y + 100} x} x", -104),
            ("let x = 4 x + {let y = 3 x + y}", 11),
        ];
        let bool_cases = vec![
            ("true", true),
            ("false", false),
            ("!true", !true),
            ("!false", !false),
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
        let void_cases = vec!["", "let x = 4"];
        let undefined_cases = vec![
            "[$]",
            "[x] + 4",
            "(1 + 2[]",
            "-[true]",
            "13 [@]",
            "[4] && [5]",
            "1 + [true]",
            "[true] + [false]",
            "([$] + 2) - 4",
            "([$]) + 4",
            "[foo] + [bar]",
            "1 + [let] [x] = 4",
            "[true] & [false]",
            "[true] | [false]",
            "let x = 4 + {let x = 2 - {let y = {[x] + 10} y + 100} x}",
            "let x = { let y = 4 } [x] + 5",
            "4 + [{let x = 4}]",
        ];

        int_cases
            .into_iter()
            .map(|(s, i)| (s, Integer(i)))
            .chain(bool_cases.into_iter().map(|(s, b)| (s, Boolean(b))))
            .chain(void_cases.into_iter().map(|s| (s, Void)))
            .chain(undefined_cases.into_iter().map(|s| (s, Undefined)))
            .collect()
    }
}
