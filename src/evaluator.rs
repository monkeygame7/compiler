use std::fmt::{Debug, Display};

use crate::compiler::{CompilationUnit, Program};

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

use inkwell::context::Context;
use ResultType::*;

pub fn evaluate(unit: CompilationUnit) -> ResultType {
    let context = Context::create();
    let program = Program::build(&context, &unit);

    let engine = program
        .ir
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();

    let func = unsafe { engine.get_function::<unsafe extern "C" fn() -> i32>("main") }.unwrap();

    let value = unsafe { func.call() };

    ResultType::Integer(value)
}

#[cfg(test)]
mod test {
    use std::vec;

    use crate::compiler::{compile, diagnostics::TextSpan};

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
            let compilation = compile(&self.actual_input, false);
            match compilation {
                Ok(unit) => {
                    let result = evaluate(unit);
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
