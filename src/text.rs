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

    pub fn length(&self) -> usize {
        self.end - self.start
    }
}
