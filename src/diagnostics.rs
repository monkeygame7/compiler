use std::{
    cell::Cell,
    fmt::{Debug, Display},
};

use crate::{ast::lexer::SyntaxToken, text::TextSpan};

pub struct DiagnosticMessage {
    pub message: String,
    pub span: TextSpan,
}

impl Display for DiagnosticMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Err: {}", self.message))
    }
}

pub struct DiagnosticBag {
    messages: Cell<Vec<DiagnosticMessage>>,
}

impl DiagnosticBag {
    pub fn new() -> Self {
        Self {
            messages: Cell::new(vec![]),
        }
    }

    pub fn has_errors(&self) -> bool {
        let messages = self.messages.take();
        let result = messages.len() > 0;
        self.messages.set(messages);
        result
    }

    fn make_diagnostic(&self, message: String, span: TextSpan) -> DiagnosticMessage {
        DiagnosticMessage { message, span }
    }

    fn add_message(&self, message: String, span: TextSpan) {
        let diagnostic = self.make_diagnostic(message, span);
        let mut messages = self.messages.take();
        messages.push(diagnostic);
        self.messages.set(messages);
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
    type Item = DiagnosticMessage;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.messages.take().into_iter()
    }
}

impl Debug for DiagnosticBag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let messages = self.messages.take();
        for message in messages.iter() {
            f.write_str(&message.message)?;
        }

        self.messages.set(messages);
        Ok(())
    }
}
