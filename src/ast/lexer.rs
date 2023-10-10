use std::fmt::Display;

use crate::diagnostics::{DiagnosticBag, TextSpan};

pub struct Lexer {
    chars: Vec<char>,
    current_position: usize,
    pub diagnostics: DiagnosticBag,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    // randos
    EOF,
    BadToken(String),
    WhiteSpace(String),

    // literals
    Integer(i32),
    Boolean(bool),
    Identifier(String),

    // int operators
    Plus,
    Dash,
    Star,
    Slash,

    // bool operators
    Ampersand,
    AmpersandAmpersand,
    Pipe,
    PipePipe,
    Bang,
    // mixed operators
    EqualsEquals,
    BangEquals,
    LeftAngleBracket,
    RightAngleBracket,
    LeftAngleEquals,
    RightAngleEquals,

    //organizational
    LeftParenthesis,
    RightParenthesis,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokenKind::EOF => "<EOF>".to_owned(),
            TokenKind::BadToken(s) => s.to_owned(),
            TokenKind::WhiteSpace(s) => s.to_owned(),
            TokenKind::Integer(i) => i.to_string(),
            TokenKind::Boolean(b) => b.to_string(),
            TokenKind::Identifier(s) => s.to_owned(),
            TokenKind::Plus => "+".to_owned(),
            TokenKind::Dash => "-".to_owned(),
            TokenKind::Star => "*".to_owned(),
            TokenKind::Slash => "/".to_owned(),
            TokenKind::Ampersand => "&".to_owned(),
            TokenKind::AmpersandAmpersand => "&&".to_owned(),
            TokenKind::Pipe => "|".to_owned(),
            TokenKind::PipePipe => "||".to_owned(),
            TokenKind::Bang => "!".to_owned(),
            TokenKind::EqualsEquals => "==".to_owned(),
            TokenKind::BangEquals => "!=".to_owned(),
            TokenKind::LeftAngleBracket => "<".to_owned(),
            TokenKind::RightAngleBracket => ">".to_owned(),
            TokenKind::LeftAngleEquals => "<=".to_owned(),
            TokenKind::RightAngleEquals => ">=".to_owned(),
            TokenKind::LeftParenthesis => "(".to_owned(),
            TokenKind::RightParenthesis => ")".to_owned(),
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
                '+' => TokenKind::Plus,
                '-' => TokenKind::Dash,
                '*' => TokenKind::Star,
                '/' => TokenKind::Slash,
                '(' => TokenKind::LeftParenthesis,
                ')' => TokenKind::RightParenthesis,
                '&' => self.match_potential_double(
                    '&',
                    TokenKind::AmpersandAmpersand,
                    TokenKind::Ampersand,
                ),
                '|' => self.match_potential_double('|', TokenKind::PipePipe, TokenKind::Pipe),
                '!' => self.match_potential_double('=', TokenKind::Bang, TokenKind::BangEquals),
                '=' => match self.next() {
                    Some('=') => TokenKind::EqualsEquals,
                    Some(c) => TokenKind::BadToken(c.to_string()),
                    None => TokenKind::BadToken("=".to_string()),
                },
                '<' => self.match_potential_double(
                    '=',
                    TokenKind::LeftAngleEquals,
                    TokenKind::LeftAngleBracket,
                ),
                '>' => self.match_potential_double(
                    '=',
                    TokenKind::RightAngleEquals,
                    TokenKind::RightAngleBracket,
                ),
                c if c.is_whitespace() => self.read_whitespace(c),
                c if c.is_numeric() => self.read_integer(c),
                c if c.is_alphabetic() => self.read_literal(c),
                unrecognized => TokenKind::BadToken(unrecognized.to_string()),
            },
        };

        if let TokenKind::BadToken(s) = &kind {
            let span = TextSpan::new(previous_position, self.current_position);
            self.diagnostics
                .report_unrecognized_symbol(s.to_owned(), span);
        }

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

    fn read_integer(&mut self, first_integer_char: char) -> TokenKind {
        let integer_string = self.match_continuous(first_integer_char, |c| c.is_numeric());

        let integer = integer_string.parse();
        match integer {
            Ok(i) => TokenKind::Integer(i),
            Err(_) => TokenKind::BadToken(integer_string),
        }
    }

    fn read_literal(&mut self, first_char: char) -> TokenKind {
        let literal_string = self.match_continuous(first_char, |c| c.is_alphanumeric() || c == '_');

        match literal_string.as_str() {
            "true" => TokenKind::Boolean(true),
            "false" => TokenKind::Boolean(false),
            _ => TokenKind::Identifier(literal_string),
        }
    }

    fn match_continuous<F>(&mut self, first_char: char, test: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut string = first_char.to_string();

        while let Some(c) = self.peek() {
            if test(c) {
                string.push(self.next().unwrap())
            } else {
                break;
            }
        }

        string
    }

    fn match_potential_double(
        &mut self,
        extra_match: char,
        double_symbol: TokenKind,
        single_symbol: TokenKind,
    ) -> TokenKind {
        match self.peek() {
            Some(c) if c == extra_match => {
                self.next();
                double_symbol
            }
            _ => single_symbol,
        }
    }
}
