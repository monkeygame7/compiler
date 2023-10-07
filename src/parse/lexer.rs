use std::fmt::Display;

pub struct Lexer {
    chars: Vec<char>,
    current_position: usize,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    EOF,
    BadToken(String),
    WhiteSpace(String),
    Integer(i32),
    PlusToken(String),
    DashToken(String),
    StarToken(String),
    SlashToken(String),
    LeftParenthesisToken(String),
    RightParenthesisToken(String),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokenKind::EOF => "".to_owned(),
            TokenKind::BadToken(s) => s.clone(),
            TokenKind::WhiteSpace(s) => s.clone(),
            TokenKind::Integer(i) => i.to_string(),
            TokenKind::PlusToken(s) => s.clone(),
            TokenKind::DashToken(s) => s.clone(),
            TokenKind::StarToken(s) => s.clone(),
            TokenKind::SlashToken(s) => s.clone(),
            TokenKind::LeftParenthesisToken(s) => s.clone(),
            TokenKind::RightParenthesisToken(s) => s.clone(),
        };
        f.write_str(&s)
    }
}

#[derive(Clone)]
pub struct SyntaxToken {
    pub kind: TokenKind,
    pub position: usize,
}

impl Lexer {
    pub fn new(text: String) -> Lexer {
        let chars = text.chars().collect();
        Lexer {
            chars,
            current_position: 0,
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
        let position = self.current_position;
        let kind = match self.next() {
            None => TokenKind::EOF,
            Some(c) => match c {
                '+' => TokenKind::PlusToken(c.to_string()),
                '-' => TokenKind::DashToken(c.to_string()),
                '*' => TokenKind::StarToken(c.to_string()),
                '/' => TokenKind::SlashToken(c.to_string()),
                '(' => TokenKind::LeftParenthesisToken(c.to_string()),
                ')' => TokenKind::RightParenthesisToken(c.to_string()),
                c if c.is_whitespace() => self.read_whitespace(c),
                c if c.is_numeric() => self.read_integer(c),
                unrecognized => TokenKind::BadToken(unrecognized.to_string()),
            },
        };
        SyntaxToken { kind, position }
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
            Err(_) => TokenKind::BadToken(integer_string),
        }
    }
}
