use std::str::FromStr;

use ascii::{AsAsciiStrError, AsciiChar, AsciiStr, AsciiString, Chars};

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

#[derive(Debug, PartialEq, Eq)]
struct TextLine {
    start: usize,
    length: usize,
    line_break_length: usize,
}

impl TextLine {
    #[inline]
    fn end_with_line_break(&self) -> usize {
        self.end() + self.line_break_length
    }

    #[inline]
    fn end(&self) -> usize {
        self.start + self.length
    }
}

pub struct SourceText {
    pub text: AsciiString,
    lines: Vec<TextLine>,
}

impl SourceText {
    pub fn from(text: &str) -> Result<SourceText, AsAsciiStrError> {
        let text = AsciiString::from_str(text)?;
        let lines = SourceText::parse_lines(&text);
        let src = SourceText { text, lines };

        Ok(src)
    }

    pub fn chars(&self) -> Chars<'_> {
        self.text.chars()
    }

    pub fn relative_span(&self, original: TextSpan, line_num: usize) -> TextSpan {
        assert!(line_num > 0, "Line numbers must start at 1");
        let line = &self.lines[line_num - 1];
        TextSpan::new(original.start - line.start, original.end - line.start)
    }

    fn parse_lines(text: &AsciiStr) -> Vec<TextLine> {
        let mut lines = vec![];
        let mut chars = text.chars().enumerate().peekable();
        let mut line_start = 0;

        while let Some((mut pos, ch)) = chars.next() {
            let next_ch = chars.peek();
            let line_break_length = match (ch, next_ch) {
                (AsciiChar::CarriageReturn, Some((_, AsciiChar::LineFeed))) => {
                    chars.next();
                    2
                }
                (AsciiChar::LineFeed, _) => 1,
                (_, None) => {
                    pos += 1;
                    0
                }
                _ => continue,
            };
            let length = pos - line_start;
            lines.push(TextLine {
                start: line_start,
                length,
                line_break_length,
            });
            line_start = pos + line_break_length;
        }

        lines
    }

    pub fn get_line_number(&self, pos: usize) -> usize {
        let mut lower = 0;
        let mut upper = self.lines.len() - 1;

        while lower <= upper {
            let middle = (lower + upper) / 2;
            let line = &self.lines[middle];

            if line.start > pos {
                upper = middle - 1;
                // this is probably dangerous...integer underflow
            } else if line.end_with_line_break() - 1 < pos {
                lower = middle + 1;
            } else {
                return middle + 1;
            }
        }

        // was not found, just return the last index
        self.lines.len()
    }

    pub fn get_line(&self, line_num: usize) -> &AsciiStr {
        assert!(line_num > 0, "Line numbers must start at 1");
        let line = &self.lines[line_num - 1];
        return &self.text[line.start..line.end()];
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_source_text_empty() {
        let src = SourceText::from("").unwrap();

        assert_eq!(src.lines.len(), 0);
    }

    #[test]
    fn test_source_text_only_newline() {
        let src = SourceText::from("\r\n").unwrap();

        assert_eq!(
            src.lines,
            vec![TextLine {
                start: 0,
                length: 0,
                line_break_length: 2
            }]
        );
    }

    #[test]
    fn test_source_text_carriage_return_newline() {
        let text = "foo\r\nbar";
        let src = SourceText::from(text).unwrap();

        assert_eq!(
            src.lines,
            vec![
                TextLine {
                    start: 0,
                    length: 3,
                    line_break_length: 2
                },
                TextLine {
                    start: text.find("bar").unwrap(),
                    length: 3,
                    line_break_length: 0,
                }
            ]
        )
    }

    #[test]
    fn test_source_text_no_newline() {
        let src = SourceText::from("test").unwrap();

        assert_eq!(
            src.lines,
            vec![TextLine {
                start: 0,
                length: src.text.len(),
                line_break_length: 0
            }]
        );
    }

    #[test]
    fn test_source_text_one_line() {
        let src = SourceText::from("test\n").unwrap();

        assert_eq!(
            src.lines,
            vec![TextLine {
                start: 0,
                length: src.text.trim().len(),
                line_break_length: 1
            }]
        );
    }

    #[test]
    fn test_source_text_multiple_lines() {
        let multiline = r#"test
new
lines
no line"#;
        let expected_lines = multiline
            .lines() //raw_lines
            //.iter()
            .map(|l| TextLine {
                start: multiline.find(l).unwrap(),
                length: l.len(),
                line_break_length: if multiline.ends_with(l) { 0 } else { 1 },
            })
            .collect::<Vec<_>>();

        let src = SourceText::from(&multiline).unwrap();

        assert_eq!(src.lines, expected_lines, "{}", src.text);
    }

    #[test]
    fn test_get_line() {
        let multiline = r#"
            test
            lines
            should find

            any char in any
            line\r\ncarriageline

            edge"#;

        let src = SourceText::from(multiline).unwrap();
        let lines = multiline.lines().collect::<Vec<_>>();

        let mut base = 0;
        for line in lines {
            for (pos, _) in line.char_indices() {
                let line_num = src.get_line_number(base + pos);
                assert_eq!(
                    src.get_line(line_num),
                    line,
                    "failed to find line for pos {}",
                    pos
                )
            }
            base += line.len() + 1; // extra 1 for new line
        }
    }
}
