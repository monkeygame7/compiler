use std::fmt::Display;

#[derive(Copy, Clone)]
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
