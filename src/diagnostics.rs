use std::{
    cell::Cell,
    fmt::{Debug, Display},
};

use crate::ast::lexer::SyntaxToken;

#[derive(PartialEq, Eq, Copy, Clone, Debug, PartialOrd, Ord)]
pub struct TextSpan {
    pub start: usize,
    pub end: usize,
}

impl TextSpan {
    pub fn new(start: usize, end: usize) -> Self {
        TextSpan { start, end }
    }

    fn len(&self) -> usize {
        self.end - self.start
    }

    fn contains(&self, position: usize) -> bool {
        self.start <= position && position < self.end
    }

    pub fn to(self, other: TextSpan) -> Self {
        TextSpan {
            start: self.start,
            end: other.end,
        }
    }
}

pub struct DiagnosticMessage {
    pub message: String,
    pub span: TextSpan,
}

impl DiagnosticMessage {
    pub fn for_range(message: String, span: TextSpan) -> Self {
        DiagnosticMessage { message, span }
    }
}

impl Display for DiagnosticMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("ERR: {}", self.message))
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

    fn add_message(&self, message: String, span: TextSpan) {
        let diagnostic = DiagnosticMessage::for_range(message, span);
        let mut messages = self.messages.take();
        messages.push(diagnostic);
        self.messages.set(messages);
    }

    pub fn report_unexpected_token(&self, given_token: SyntaxToken, expected_kind: impl Display) {
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
                "'{:?}' is not supported between '{}' and '{:?}'",
                l_type, op, r_type
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

    pub fn report_bad_expression(&self, span: TextSpan) {
        self.add_message("Not a valid expression".to_string(), span);
    }
}

impl IntoIterator for DiagnosticBag {
    type Item = DiagnosticMessage;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.messages.take().into_iter()
    }
}
