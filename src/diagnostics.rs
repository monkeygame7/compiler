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

    pub fn to(self, other: TextSpan) -> Self {
        TextSpan::new(self.start, other.end)
    }
}

pub struct DiagnosticMessage {
    pub message: String,
    pub span: TextSpan,
    pub line_number: usize,
}

impl Display for DiagnosticMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("ERR L{}: {}", self.line_number, self.message))
    }
}

pub struct DiagnosticBag {
    messages: Cell<Vec<DiagnosticMessage>>,
    line_break_spans: Vec<TextSpan>,
}

impl DiagnosticBag {
    pub fn new(text: &str) -> Self {
        let line_break_positions: Vec<_> = text
            .chars()
            .enumerate()
            .filter_map(|(pos, c)| Some(pos).filter(|_| c == '\n'))
            .collect();
        let mut line_break_spans = vec![];
        let mut start = 0;
        for end in line_break_positions {
            line_break_spans.push(TextSpan::new(start, end));
            start = end;
        }
        line_break_spans.push(TextSpan::new(start, text.len()));

        Self {
            messages: Cell::new(vec![]),
            line_break_spans,
        }
    }

    pub fn has_errors(&self) -> bool {
        let messages = self.messages.take();
        let result = messages.len() > 0;
        self.messages.set(messages);
        result
    }

    fn find_line_number(&self, pos: usize) -> usize {
        let mut lower = 0;
        let mut upper = self.line_break_spans.len();

        while lower < upper {
            let middle = (lower + upper) / 2;
            let span = self.line_break_spans[middle];

            if pos < span.start {
                upper = middle;
            } else if pos > span.end {
                lower = middle;
            } else {
                lower = middle;
                break;
            }
        }

        return lower + 1;
    }

    fn make_diagnostic(&self, message: String, span: TextSpan) -> DiagnosticMessage {
        let line_number = self.find_line_number(span.start);

        DiagnosticMessage {
            message,
            span,
            line_number,
        }
    }

    fn add_message(&self, message: String, span: TextSpan) {
        let diagnostic = self.make_diagnostic(message, span);
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_line_number() {
        let text = r#"foo
                     bar
                     baz
                     biz
                     boz
                     foo again
                     $ looking for that
                     more stuff
                     more
                     "#;
        let bag = DiagnosticBag::new(text);
        let target_pos = text.find('$').unwrap();
        let result = bag.find_line_number(target_pos);

        assert_eq!(result, 7);
    }

    #[test]
    fn test_line_number_none() {
        let text = " ";
        let bag = DiagnosticBag::new(text);
        let target = 0;

        assert_eq!(bag.find_line_number(target), 1);
    }
}
