use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    ast::{
        visitor::AstVisitor, Ast, BinaryExpr, BinaryOperatorKind, BlockExpr, Expr, IntegerExpr,
        LetStmt, ParenExpr, Stmt, UnaryExpr, UnaryOperatorKind, VariableExpr,
    },
    diagnostics::DiagnosticBag,
    text::TextSpan,
};

pub struct Evaluator {
    scopes: Vec<HashMap<String, ResultType>>,
    diagnostics: Rc<DiagnosticBag>,
    vars: HashMap<String, ResultType>,
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

impl ResultType {
    fn to_integer(&self) -> i32 {
        match self {
            Integer(i) => *i,
            _ => panic!("expected int"),
        }
    }

    fn to_bool(&self) -> bool {
        match self {
            Boolean(b) => *b,
            _ => panic!("expected bool"),
        }
    }

    fn to_void(&self) -> () {
        match self {
            Void => (),
            _ => panic!("expected void"),
        }
    }
}

impl AstVisitor for Evaluator {
    fn visit_error(&mut self, ast: &mut Ast, span: &TextSpan, expr: &Expr) {
        panic!("Evaluated bad node");
    }

    fn visit_integer_expr(&mut self, ast: &mut Ast, int_expr: &IntegerExpr, expr: &Expr) {
        self.last_result = Some(ResultType::Integer(int_expr.value));
    }

    fn visit_boolean_expr(
        &mut self,
        ast: &mut Ast,
        bool_expr: &crate::ast::BooleanExpr,
        expr: &Expr,
    ) {
        self.last_result = Some(ResultType::Boolean(bool_expr.value));
    }

    fn visit_paren_expr(&mut self, ast: &mut Ast, paren_expr: &ParenExpr, expr: &Expr) {
        self.visit_expr(ast, paren_expr.expr);
    }

    fn visit_binary_expr(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, expr: &Expr) {
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

    fn visit_unary_expr(&mut self, ast: &mut Ast, unary_expr: &UnaryExpr, expr: &Expr) {
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

    fn visit_variable_expr(&mut self, ast: &mut Ast, variable_expr: &VariableExpr, expr: &Expr) {
        let value = &self.vars[&variable_expr.token.literal];
        self.last_result = Some(*value);
    }

    fn visit_let_stmt(&mut self, ast: &mut Ast, let_stmt: &LetStmt, stmt: &Stmt) {
        let identifier = &let_stmt.identifier.literal;
        assert!(!self.vars.contains_key(identifier));
        self.visit_expr(ast, let_stmt.expr);
        self.vars
            .insert(identifier.to_string(), self.last_result.take().unwrap());
    }
}

impl Evaluator {
    fn new(diagnostics: Rc<DiagnosticBag>) -> Evaluator {
        Evaluator {
            scopes: vec![],
            diagnostics,
            vars: HashMap::new(),
            last_result: None,
        }
    }

    pub fn evaluate(ast: &mut Ast, diagnostics: Rc<DiagnosticBag>) -> ResultType {
        let mut evaluator = Self::new(diagnostics);
        ast.visit(&mut evaluator);
        evaluator.last_result.take().unwrap_or(ResultType::Void)
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
            let mut tree = Parser::parse(&self.actual_input, diagnostics.clone()).unwrap();
            let result = Evaluator::evaluate(&mut tree, diagnostics.clone());

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
