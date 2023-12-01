use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use crate::{
    ast::{nodes::*, Ast, AstVisitor, ExprId},
    compilation::{FunctionId, VariableId},
    diagnostics::TextSpan,
};

pub struct Evaluator {
    functions: HashMap<FunctionId, FunctionDef>,
    scopes: Vec<HashMap<VariableId, ResultType>>,
    last_result: Option<ResultType>,
    main_body: Option<ExprId>,
}

#[derive(Debug)]
struct FunctionDef {
    name: String,
    entry: ExprId,
    params: Vec<VariableId>,
}

#[derive(PartialEq, Eq, Clone, Debug, Copy)]
pub enum ResultType {
    Integer(i32),
    Boolean(bool),
    Void,
    Undefined,
}

impl ResultType {
    fn to_bool(self) -> bool {
        let Boolean(b) = self else {
            panic!("expected bool")
        };
        b
    }
}

impl Display for ResultType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

use ResultType::*;

impl AstVisitor for Evaluator {
    fn visit_func_decl(&mut self, _ast: &Ast, func: &FunctionDecl, _item: &Item) {
        let params = func.params.items.iter().map(|i| i.item.id).collect();
        let entry = func.body;
        let name = func.name.literal.clone();
        self.functions
            .insert(func.id, FunctionDef { name, entry, params });
        if func.name.literal == "main" {
            self.main_body = Some(entry);
        }
    }

    fn visit_expr_stmt(&mut self, ast: &Ast, expr_stmt: &ExprStmt, _stmt: &Stmt) {
        self.visit_expr(ast, expr_stmt.expr);
        if expr_stmt.semicolon.is_some() {
            self.last_result = None;
        }
    }

    fn visit_variable_decl(&mut self, ast: &Ast, variable_decl: &VariableDecl, _stmt: &Stmt) {
        self.visit_expr(ast, variable_decl.initial);
        let value = self.last_result.take().unwrap();

        let last_scope = self.scopes.last_mut().unwrap();
        assert!(!&last_scope.contains_key(&variable_decl.variable));
        last_scope.insert(variable_decl.variable, value);
        self.last_result = Some(Void);
    }

    fn visit_while_stmt(&mut self, ast: &Ast, while_stmt: &WhileStmt, _stmt: &Stmt) {
        loop {
            self.visit_expr(ast, while_stmt.condition);
            let condition = self.last_result.unwrap().to_bool();
            if !condition {
                break;
            }

            self.visit_expr(ast, while_stmt.body);
        }
    }

    fn visit_return_stmt(&mut self, _ast: &Ast, _return_stmt: &ReturnStmt, _stmt: &Stmt) {
        todo!("evaluate return")
    }

    fn visit_error(&mut self, _ast: &Ast, _span: &TextSpan, _expr: &Expr) {
        self.last_result = Some(Undefined);
    }

    fn visit_integer_expr(&mut self, _ast: &Ast, int_expr: &IntegerExpr, _expr: &Expr) {
        self.last_result = Some(Integer(int_expr.value));
    }

    fn visit_boolean_expr(&mut self, _ast: &Ast, bool_expr: &BooleanExpr, _expr: &Expr) {
        self.last_result = Some(Boolean(bool_expr.value));
    }

    fn visit_assign_expr(&mut self, ast: &Ast, assign_expr: &AssignExpr, _expr: &Expr) {
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

    fn visit_binary_expr(&mut self, ast: &Ast, binary_expr: &BinaryExpr, _expr: &Expr) {
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

        if result == Undefined {
            unreachable!("Tried to evaluate invalid binary expression");
        }

        self.last_result = Some(result);
    }

    fn visit_unary_expr(&mut self, ast: &Ast, unary_expr: &UnaryExpr, _expr: &Expr) {
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

        if result == Undefined {
            unreachable!("Tried to evaluate invalid unary expression");
        }

        self.last_result = Some(result);
    }

    fn visit_block_expr(&mut self, ast: &Ast, block_expr: &BlockExpr, expr: &Expr) {
        self.enter_scope();
        self.do_visit_block_expr(ast, block_expr, expr);
        self.exit_scope();
    }

    fn visit_variable_expr(&mut self, _ast: &Ast, variable_expr: &VariableExpr, _expr: &Expr) {
        self.last_result = self
            .scopes
            .iter()
            .rev()
            .flat_map(|vars| vars.get(&variable_expr.id))
            .nth(0)
            .cloned();
    }

    fn visit_if_expr(&mut self, ast: &Ast, if_expr: &IfExpr, _expr: &Expr) {
        self.visit_expr(ast, if_expr.condition);
        let condition = if let Some(ResultType::Boolean(b)) = self.last_result {
            b
        } else {
            unreachable!("if condition evaluate to boolean");
        };

        if condition {
            self.visit_expr(ast, if_expr.then_clause);
        } else if let Some(else_clause) = &if_expr.else_clause {
            self.visit_expr(ast, else_clause.body);
        }
    }

    fn visit_call_expr(&mut self, ast: &Ast, call_expr: &CallExpr, _expr: &Expr) {
        self.enter_scope();

        let func = self.functions.get(&call_expr.callee_id).unwrap();
        let entry = func.entry;
        assert!(
            func.params.len() == call_expr.args.items.len(),
            "{}({:?}) != {:?}",
            func.name,
            func.params,
            call_expr.args.items
        );
        call_expr
            .args
            .items
            .iter()
            .map(|i| i.item)
            .zip(func.params.clone())
            .for_each(|(arg, param)| {
                self.visit_expr(ast, arg);
                let value = self.last_result.take().unwrap();
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert(param, value);
            });

        self.visit_expr(ast, entry);

        self.exit_scope();
    }
}

impl Evaluator {
    fn new() -> Evaluator {
        Evaluator {
            functions: HashMap::new(),
            scopes: vec![],
            last_result: None,
            main_body: None,
        }
    }

    pub fn evaluate(ast: &Ast) -> ResultType {
        let mut evaluator = Self::new();
        ast.visit(&mut evaluator);
        evaluator.call_main(ast);
        evaluator.last_result.take().unwrap_or(Void)
    }

    fn call_main(&mut self, ast: &Ast) {
        self.enter_scope();
        self.visit_expr(ast, self.main_body.expect("No main function found"));
        self.exit_scope();
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop().expect("Unexpected empty scopes");
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
            let compilation = CompilationUnit::compile(&self.actual_input, false);
            match compilation {
                Ok(mut unit) => {
                    let result = Evaluator::evaluate(&mut unit.ast);
                    assert!(
                        self.expected_spans.is_empty(),
                        "Expected errors in \n{}\nbut found none\n",
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
                        "Expected no errors in \n{}\nbut found:\n{:?}\n",
                        self.input,
                        messages,
                    );

                    errors.sort();
                    assert_eq!(
                        errors, self.expected_spans,
                        "Unexpected errors in \n{}\n{:?}\n",
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
            var x = 5;
            var y = 10;
            var z = x + y;
            x = x * 2;
            y = x + y;
            y + z
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
            ("0 + {if true 1 else 2}", 1),
            (
                "var x = 4;
                 x = x = x + 5",
                9,
            ),
            (
                "var x = 4;
                 x = x + 5",
                9,
            ),
            (
                "const x = 4;
                {
                    const y = x;
                    const x = 2;
                    y + x
                }",
                6,
            ),
            (
                "const x = 4;
                {x + 2}",
                6,
            ),
            (
                "
                const x = 4 + {
                const x = 2 - {
                    const y = 10;
                    y + 100
                };
                x
             };
             x",
                -104,
            ),
            (
                "
                 const x = 4;
                 x + {
                     const y = 3;
                     x + y
                 }",
                11,
            ),
            (
                "
                const x: int = 1;
                const y: int = 2;
                var z = 3;
                if x > y {
                    z = -100
                } else {
                    if x == y {
                        z = 0
                    } else if y == 3 {
                        z = 3
                    } else {
                        z = 4
                    }
                }
                ",
                4,
            ),
            (
                "
                var x = 0;
                var result = 0;
                while x < 100 {
                  if x < 20 {
                    result = result + x
                  } else if x <= 50 {
                    result = result + x / 2
                  } else {
                    result = result + 1
                  }
                  x = x + 1
                }
                result
                ",
                774,
            ),
            (
                "
                const x = 5;
                fn foo: int(x: int) {var x = 0; x = 3}
                x",
                5,
            ),
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
        let void_cases = vec![
            "",
            "{1;2;}",
            "const x = 4;",
            "fn foo() {}",
            "fn foo: int() 1",
            "fn foo() {true; var x = 1;}",
            "fn foo: int(x: int, y: bool) x",
            "var x = true;
             fn foo: int(x: int, y: bool) x",
            "const x = 4;
             fn foo: int() x",
            "fn foo: int() {
                true;
                false;
                1 + 2
            }",
            "fn foo() {
                1;
                2;
                3;
            }",
            "fn foo() {foo()}",
            "fn foo() {
                var y = foo;
                y();
            }",
            "fn foo: int() {
                if true {
                    return 1;
                }
                2
            }",
        ];
        let undefined_cases = vec![
            "[$]",
            "[x] + 4",
            "(1 + 2[]",
            "[-]true",
            "13 [@]",
            "4 [&&] 5",
            "1 [+] true",
            "true [+] false",
            "([$] + 2) - 4",
            "([$]) + 4",
            "[foo] + [bar]",
            "1 + [let] [x] = 4",
            "true [&] false",
            "true [|] false",
            "const x = 4 + {
                const x = 2 - {
                    const y = {
                        [x] + 10
                    };
                    y + 100
                };
                x
            };",
            "const x = {
                const y = 4;
            };
            x [+] 5",
            "4 [+] {
                var x = 4;
            }",
            "{
                var x = 4;
                x = [{
                    4 < 3
                }]
            }",
            "{
                const x = 4;
            }
            [x]",
            "if [100] true else false",
            "if true [var] [x] = 4",
            "while [1] {}",
            "if true 1 else [false]",
            "var[[[[]]]]",
            "const[[[[]]]]",
            "const x: int = [true];",
            "var x: int = [true];
             x + 4",
            "const x: bool = [5];
             x [+] 4",
            "var x: [foo] = 5;",
            "fn test: [foo]() {1}",
            "fn test: int() [{true}]",
            "fn test: int(x: [foo]) {x}",
            "fn test: int(x: int, y: bool) [y]",
            "fn test: int(x: int, y: bool) x [+] y",
            "fn [if]: [foo](x: int, y: [boo]) x",
            "fn test() [1]",
            "fn[[[[]]]]",
            "fn foo[[[]]]",
            "fn foo([[]]",
            "fn foo()[]",
            "const x = 4;
             fn foo: int(x: bool) [x]",
            "fn foo: int() 1
             const x: int = [foo];",
            "fn foo: int(x: int) x [x]",
            "{[1] 2}",
            "[return][]",
            "[return];",
            "[return] 1;",
            "[return] 1[]",
            "var x = 4[]",
            "fn foo: int() [{1;}]",
            "fn foo: int() 1[;]",
            "fn foo: int() {return 1[}]",
            "fn foo() {var x = 4[}]",
            "fn foo(x: int [y]: int [z]: int) {}",
            "fn foo(x: int, y: int) {
                foo[(true, false)]
            }",
            "[1](1, 2, 3)",
            "[true]()",
            "fn foo(x: int) {
                foo[()]
            }",
            "fn foo() {
                foo[(1)]
            }",
            "fn foo(x: int) {
                foo[(foo(1))]
            }",
            "fn foo: int() {
                return 1;
             }
             const x: bool = [foo()];",
            "const x: int = [if true {1}];",
            "const x = if true{1};
             x [+] 1",
            "fn foo: int() {return [false];}",
            "fn foo: int() {
                if true {
                    return [false];
                }
                2
            }",
            "fn foo() {} foo [=] 1",
            "fn foo(x: int) {x [=] 1;}",
            "const x = 1;
             x [=] 2;",
            "fn foo() {}
             foo [=] 1;",
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
