mod text;
use std::{
    cell::RefCell,
    fmt::{Debug, Display},
};
pub use text::SourceText;
pub use text::TextSpan;

use crate::ast::nodes::BinaryOperator;
use crate::ast::nodes::UnaryOperator;
use crate::{
    compilation::Type,
    parsing::{SyntaxToken, TokenKind},
};

#[derive(Clone)]
pub struct Diagnostic {
    pub message: String,
    pub span: TextSpan,
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Err: {}", self.message))
    }
}

pub struct DiagnosticBag {
    // TODO: Expose a way to iterate over diagnostics without making this public
    pub messages: RefCell<Vec<Diagnostic>>,
}

impl DiagnosticBag {
    pub fn new() -> Self {
        Self {
            messages: RefCell::new(vec![]),
        }
    }

    pub fn has_errors(&self) -> bool {
        self.messages.borrow().len() > 0
    }

    fn make_diagnostic(&self, message: String, span: TextSpan) -> Diagnostic {
        Diagnostic { message, span }
    }

    fn add_message(&self, message: String, span: TextSpan) {
        let diagnostic = self.make_diagnostic(message, span);
        self.messages.borrow_mut().push(diagnostic);
    }

    pub fn report_unexpected_token(&self, given: &SyntaxToken, expected: &TokenKind) {
        self.add_message(
            format!("Expected '{:?}', but found '{}'", expected, given),
            given.span,
        );
    }

    pub fn report_expected_expression(&self, token: &SyntaxToken) {
        self.add_message(
            format!("Expected expression, but found '{}'", token),
            token.span,
        )
    }

    pub fn report_unexpected_type(&self, expected_type: &Type, actual_type: &Type, span: TextSpan) {
        self.add_message(
            format!("Expected '{}' but found '{}'", expected_type, actual_type),
            span,
        )
    }

    pub fn report_unsupported_binary_operator(
        &self,
        l_type: &impl Debug,
        op: &BinaryOperator,
        r_type: &impl Debug,
    ) {
        self.add_message(
            format!(
                "'{}' is not supported between '{:?}' and '{:?}'",
                op, l_type, r_type
            ),
            op.token.span,
        );
    }

    pub fn report_unsupported_unary_operator(&self, op: &UnaryOperator, operand_type: &impl Debug) {
        self.add_message(
            format!("'{}' is not a supported for '{:?}'", op, operand_type),
            op.token.span,
        );
    }

    pub fn report_identifier_not_found(&self, token: &SyntaxToken) {
        self.add_message(
            format!("'{}' is not defined in this scope", token),
            token.span,
        );
    }

    pub fn report_already_declared(&self, token: &SyntaxToken) {
        self.add_message(
            format!("'{}' is already defined in this scope", token),
            token.span,
        )
    }

    pub fn report_undefined_type(&self, token: &SyntaxToken) {
        self.add_message(format!("Type '{}' is not defined", token), token.span);
    }
    pub fn report_return_outside_function(&self, token: &SyntaxToken) {
        self.add_message("Cannot return outside of function".to_owned(), token.span);
    }
}

impl IntoIterator for DiagnosticBag {
    type Item = Diagnostic;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.messages.take().into_iter()
    }
}

impl Debug for DiagnosticBag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for message in self.messages.borrow().iter() {
            f.write_str(&message.message)?;
        }
        Ok(())
    }
}
