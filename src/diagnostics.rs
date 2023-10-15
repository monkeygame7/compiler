use std::{
    cell::RefCell,
    fmt::{Debug, Display},
};

use crate::{ast::lexer::SyntaxToken, text::TextSpan};

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
    messages: RefCell<Vec<Diagnostic>>,
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

    pub fn report_unexpected_token(&self, given_token: &SyntaxToken, expected_kind: impl Display) {
        self.add_message(
            format!("Expected '{}', but found '{}'", expected_kind, given_token),
            given_token.span,
        );
    }

    pub fn report_unsupported_binary_operator(
        &self,
        l_type: impl Debug,
        op: impl Display,
        r_type: impl Debug,
        span: TextSpan,
    ) {
        self.add_message(
            format!(
                "'{}' is not supported between '{:?}' and '{:?}'",
                op, l_type, r_type
            ),
            span,
        );
    }

    pub fn report_unsupported_unary_operator(
        &self,
        operator: impl Display,
        exp_type: impl Display,
        span: TextSpan,
    ) {
        self.add_message(
            format!("'{}' is not a supported for '{}'", operator, exp_type),
            span,
        );
    }

    pub fn report_identifier_not_found(&self, identifier: &str, span: TextSpan) {
        self.add_message(
            format!("'{}' is not defined in this scope", identifier),
            span,
        );
    }

    pub fn report_invalid_expression(&self, span: TextSpan) {
        self.add_message("Expression is not valid".to_string(), span);
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
