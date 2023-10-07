use std::fmt::Display;

use crate::{
    diagnostics::DiagnosticMessage,
    parse::parser::{
        BinaryOperatorKind, BinaryOperatorToken, Program, SyntaxKind, SyntaxNode,
        UnaryOperatorToken, UnaryOperatorKind,
    },
};

pub struct Evaluator {
    root: SyntaxNode,
    pub errors: Vec<DiagnosticMessage>,
}

pub enum ResultType {
    IntegerResult(i32),
    VoidResult,
}

impl Evaluator {
    pub fn new(program: Program) -> Evaluator {
        Evaluator {
            root: program.root,
            errors: program.errors,
        }
    }

    pub fn evaluate(&mut self) -> ResultType {
        self.evaluate_expression(&self.root)
    }

    fn evaluate_expression(&self, node: &SyntaxNode) -> ResultType {
        match &node.kind {
            SyntaxKind::BadExpression => self.evaluate_bad_expression(&node),
            SyntaxKind::IntegerExpression(i) => self.evaluate_integer_expression(*i),
            SyntaxKind::BinaryExpression(l, op, r) => self.evaluate_binary_expression(l, op, r),
            SyntaxKind::UnaryExpression(op, exp) => self.evaluate_unary_expression(op, exp),
            SyntaxKind::GroupExpression(_, exp, _) => self.evaluate_expression(exp.as_ref()),
        }
    }

    fn evaluate_bad_expression(&self, node: &SyntaxNode) -> ResultType {
        // self.errors.append(DiagnosticMessage::for_range(
        //     "Unable to evaluate bad expression".to_owned(),
        //     node.span,
        // ));
        todo!("bad expression");
        ResultType::VoidResult
    }

    fn evaluate_integer_expression(&self, i: i32) -> ResultType {
        ResultType::IntegerResult(i)
    }

    fn evaluate_binary_expression(
        &self,
        left: &SyntaxNode,
        operator: &BinaryOperatorToken,
        right: &SyntaxNode,
    ) -> ResultType {
        let left_result = self.evaluate_expression(left);
        let right_result = self.evaluate_expression(right);
        match operator.kind {
            BinaryOperatorKind::Addition => {
                self.evaluate_addition_expression(left_result, right_result)
            }
            BinaryOperatorKind::Subtraction => {
                self.evaluate_subtraction_expression(left_result, right_result)
            }
            BinaryOperatorKind::Multiplication => {
                self.evaluate_multiplication_expression(left_result, right_result)
            }
            BinaryOperatorKind::Division => {
                self.evaluate_division_expression(left_result, right_result)
            }
        }
    }

    fn evaluate_addition_expression(
        &self,
        left_result: ResultType,
        right_result: ResultType,
    ) -> ResultType {
        if let ResultType::IntegerResult(left) = left_result {
            if let ResultType::IntegerResult(right) = right_result {
                return ResultType::IntegerResult(left + right);
            }
        }
        todo!("Invalid type for addition");
        return ResultType::VoidResult;
    }

    fn evaluate_subtraction_expression(
        &self,
        left_result: ResultType,
        right_result: ResultType,
    ) -> ResultType {
        if let ResultType::IntegerResult(left) = left_result {
            if let ResultType::IntegerResult(right) = right_result {
                return ResultType::IntegerResult(left - right);
            }
        }
        todo!("Invalid type for subtraction");
        return ResultType::VoidResult;
    }

    fn evaluate_multiplication_expression(
        &self,
        left_result: ResultType,
        right_result: ResultType,
    ) -> ResultType {
        if let ResultType::IntegerResult(left) = left_result {
            if let ResultType::IntegerResult(right) = right_result {
                return ResultType::IntegerResult(left * right);
            }
        }
        todo!("Invalid type for multiplication");
        return ResultType::VoidResult;
    }

    fn evaluate_division_expression(
        &self,
        left_result: ResultType,
        right_result: ResultType,
    ) -> ResultType {
        if let ResultType::IntegerResult(left) = left_result {
            if let ResultType::IntegerResult(right) = right_result {
                return ResultType::IntegerResult(left / right);
            }
        }
        todo!("Invalid type for division");
        return ResultType::VoidResult;
    }

    fn evaluate_unary_expression(&self, op: &UnaryOperatorToken, exp: &SyntaxNode) -> ResultType {
        let exp_result = self.evaluate_expression(exp);
        match op.kind {
            UnaryOperatorKind::Negative => self.evaluate_negative_expression(exp_result),
            UnaryOperatorKind::Positive => self.evaluate_positive_expression(exp_result),
        }
    }

    fn evaluate_negative_expression(&self, exp_result: ResultType) -> ResultType {
        if let ResultType::IntegerResult(result) = exp_result {
            return ResultType::IntegerResult(-result);
        }
        todo!("Invalid type for negative")
    }

    fn evaluate_positive_expression(&self, exp_result: ResultType) -> ResultType {
        if let ResultType::IntegerResult(_) = exp_result {
            return exp_result;
        }
        todo!("Invalid type for negative")
    }
}

impl Display for ResultType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ResultType::IntegerResult(i) => i.to_string(),
            ResultType::VoidResult => "(void)".to_owned(),
        };

        f.write_str(&s)
    }
}
