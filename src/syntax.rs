use std::{str::Chars, fmt::Display, iter::Peekable};

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    position: usize,
    done: bool,
}

pub enum TokenKind {
    BadToken,
    EOF,
    Plus,
    Dash,
    Star,
    Slash,
    Number(i32),
    WhiteSpace,
    LeftParen,
    RigthParen,
}

pub struct SyntaxToken {
    pub kind: TokenKind,
    start_position: usize,
}

impl Display for SyntaxToken {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind {
            TokenKind::BadToken => f.write_str("BAD TOKEN")?,
            TokenKind::EOF => f.write_str("EOF")?,
            TokenKind::Plus => f.write_str("+")?,
            TokenKind::Dash => f.write_str("-")?,
            TokenKind::Star => f.write_str("*")?,
            TokenKind::Slash => f.write_str("/")?,
            TokenKind::LeftParen => f.write_str("(")?,
            TokenKind::RigthParen => f.write_str(")")?,
            TokenKind::Number(n) => f.write_str(&format!("{}", n))?,
            TokenKind::WhiteSpace => f.write_str("\" \"")?,
        };
        Ok(())
    }
}

impl Iterator for Lexer<'_> {
    type Item = SyntaxToken;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            None
        } else {
            let start_position = self.position;
            let kind = self.next_token().map(|s| self.match_next(s)).unwrap_or_else(|| {
                self.done = true;
                TokenKind::EOF
            });
            Some(SyntaxToken {
                kind,
                start_position,
            })
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn parse(text: &'a str) -> Self {
        Self {
            chars: text.chars().peekable(),
            position: 0,
            done: false,
        }
    }

    fn current(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn next_token(&mut self) -> Option<char> {
        self.position += 1;
        self.chars.next()
    }

    fn match_next(&mut self, last_char: char) -> TokenKind {
        match last_char {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Dash,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RigthParen,
            c if c.is_whitespace() => self.match_whitespace(),
            c if c.is_numeric() => self.match_number(c),
            _ => TokenKind::BadToken,
        }
    }

    fn match_whitespace(&mut self) -> TokenKind {
        while self.current().map(|c| c.is_whitespace()).unwrap_or(false) {
            self.next_token();
        }
        TokenKind::WhiteSpace
    }

    fn match_number(&mut self, first_char: char) -> TokenKind {
        let mut number_string = String::new();
        number_string.push(first_char);

        while self.current().map(|c| c.is_numeric()).unwrap_or(false) {
            number_string.push(self.next_token().unwrap())
        }

        let number = number_string.parse();

        match number {
            Ok(n) => TokenKind::Number(n),
            Err(_) => {
                todo!("error handling");
            }
        }
    }
}

