use std::fmt::Display;

use crate::diagnostics::{DiagnosticBag, TextSpan};

pub struct Lexer {
    chars: Vec<char>,
    current_position: usize,
    pub diagnostics: DiagnosticBag,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    EOF,
    BadToken(String),
    WhiteSpace(String),
    Integer(i32),
    PlusToken,
    DashToken,
    StarToken,
    SlashToken,
    LeftParenthesisToken,
    RightParenthesisToken,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokenKind::EOF => "<EOF>".to_owned(),
            TokenKind::BadToken(s) => s.to_owned(),
            TokenKind::WhiteSpace(s) => s.to_owned(),
            TokenKind::Integer(i) => i.to_string(),
            TokenKind::PlusToken => "+".to_owned(),
            TokenKind::DashToken => "-".to_owned(),
            TokenKind::StarToken => "*".to_owned(),
            TokenKind::SlashToken => "/".to_owned(),
            TokenKind::LeftParenthesisToken => "(".to_owned(),
            TokenKind::RightParenthesisToken => ")".to_owned(),
        };
        f.write_str(&s)
    }
}

#[derive(Clone)]
pub struct SyntaxToken {
    pub kind: TokenKind,
    pub span: TextSpan,
}

impl Display for SyntaxToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.kind))
    }
}

impl Lexer {
    pub fn new(text: String) -> Lexer {
        let chars = text.chars().collect();
        Lexer {
            chars,
            current_position: 0,
            diagnostics: DiagnosticBag::new(),
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.current_position).copied()
    }

    fn next(&mut self) -> Option<char> {
        let current = self.chars.get(self.current_position);
        if self.current_position < self.chars.len() {
            self.current_position += 1;
        }
        current.copied()
    }

    pub fn next_token(&mut self) -> SyntaxToken {
        let previous_position = self.current_position;
        let kind = match self.next() {
            None => TokenKind::EOF,
            Some(c) => match c {
                '+' => TokenKind::PlusToken,
                '-' => TokenKind::DashToken,
                '*' => TokenKind::StarToken,
                '/' => TokenKind::SlashToken,
                '(' => TokenKind::LeftParenthesisToken,
                ')' => TokenKind::RightParenthesisToken,
                c if c.is_whitespace() => self.read_whitespace(c),
                c if c.is_numeric() => self.read_integer(c, previous_position),
                unrecognized => {
                    let span = TextSpan::new(previous_position, self.current_position);
                    self.diagnostics
                        .report_unrecognized_symbol(unrecognized.to_string(), span);
                    TokenKind::BadToken(unrecognized.to_string())
                }
            },
        };
        SyntaxToken {
            kind,
            span: TextSpan::new(previous_position, self.current_position),
        }
    }

    fn read_whitespace(&mut self, first_whitespace_char: char) -> TokenKind {
        let mut whitespace = first_whitespace_char.to_string();

        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                whitespace.push(self.next().unwrap());
            } else {
                break;
            }
        }
        TokenKind::WhiteSpace(whitespace)
    }

    fn read_integer(&mut self, first_integer_char: char, start_position: usize) -> TokenKind {
        let mut integer_string = first_integer_char.to_string();

        while let Some(c) = self.peek() {
            if c.is_numeric() {
                integer_string.push(self.next().unwrap());
            } else {
                break;
            }
        }

        let integer = integer_string.parse();
        match integer {
            Ok(i) => TokenKind::Integer(i),
            Err(_) => {
                let span = TextSpan::new(start_position, self.current_position);
                self.diagnostics
                    .report_unrecognized_symbol(integer_string.to_owned(), span);
                TokenKind::BadToken(integer_string)
            }
        }
    }
}
