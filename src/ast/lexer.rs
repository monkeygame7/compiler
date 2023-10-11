use std::fmt::Display;

use crate::text::TextSpan;

pub struct Lexer {
    chars: Vec<char>,
    current_position: usize,
}

#[derive(PartialEq, Eq, Debug, Clone)]
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
    LeftCurly,
    RightCurly,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::EOF => "<EOF>".to_owned(),
            Self::BadToken(s) => s.to_owned(),
            Self::WhiteSpace(s) => s.to_owned(),
            Self::Integer(i) => i.to_string(),
            Self::Boolean(b) => b.to_string(),
            Self::Identifier(s) => s.to_owned(),
            Self::Plus => "+".to_owned(),
            Self::Dash => "-".to_owned(),
            Self::Star => "*".to_owned(),
            Self::Slash => "/".to_owned(),
            Self::Ampersand => "&".to_owned(),
            Self::AmpersandAmpersand => "&&".to_owned(),
            Self::Pipe => "|".to_owned(),
            Self::PipePipe => "||".to_owned(),
            Self::Bang => "!".to_owned(),
            Self::EqualsEquals => "==".to_owned(),
            Self::BangEquals => "!=".to_owned(),
            Self::LeftAngleBracket => "<".to_owned(),
            Self::RightAngleBracket => ">".to_owned(),
            Self::LeftAngleEquals => "<=".to_owned(),
            Self::RightAngleEquals => ">=".to_owned(),
            Self::LeftParenthesis => "(".to_owned(),
            Self::RightParenthesis => ")".to_owned(),
            Self::LeftCurly => "{".to_owned(),
            Self::RightCurly => "}".to_owned(),
        };
        f.write_str(&s)
    }
}

#[derive(Clone, Debug)]
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
                '{' => TokenKind::LeftCurly,
                '}' => TokenKind::RightCurly,
                '&' => self.match_potential_double(
                    '&',
                    TokenKind::AmpersandAmpersand,
                    TokenKind::Ampersand,
                ),
                '|' => self.match_potential_double('|', TokenKind::PipePipe, TokenKind::Pipe),
                '!' => self.match_potential_double('=', TokenKind::BangEquals, TokenKind::Bang),
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

#[cfg(test)]
mod test {
    use super::*;
    use TokenKind::*;

    fn lex_tokens(text: String) -> Vec<SyntaxToken> {
        let mut lexer = Lexer::new(text);
        let mut tokens = vec![];
        loop {
            let token = lexer.next_token();
            if matches!(&token.kind, EOF) {
                break;
            }
            tokens.push(token);
        }

        tokens
    }

    #[test]
    fn test_lexes_single_character() {
        single_token_cases()
            .iter()
            .chain(separators().iter())
            .for_each(|(text, expected_kind)| {
                let tokens = lex_tokens(text.to_string());

                assert!(tokens.len() == 1, "{:?}", tokens);
                let result = &tokens[0];
                assert_eq!(
                    &result.kind, expected_kind,
                    "{} did not match {}",
                    text, expected_kind
                );
                assert_eq!(result.span, TextSpan::new(0, text.len()), "unexpected span");
            })
    }

    #[test]
    fn test_lexes_token_pairs() {
        token_pair_cases()
            .into_iter()
            .for_each(|(text1, expected_kind1, text2, expected_kind2)| {
                let full_text = text1.to_owned() + &text2;

                let tokens = lex_tokens(full_text.to_owned());

                assert!(
                    tokens.len() == 2,
                    "expected 2 tokens in '{}' but found {:?}",
                    full_text,
                    tokens,
                );
                let t1 = &tokens[0];
                let t2 = &tokens[1];
                assert_eq!(
                    t1.kind, expected_kind1,
                    "{} did not match {} for '{}'",
                    text1, expected_kind1, full_text
                );
                assert_eq!(t1.span, TextSpan::new(0, text1.len()), "unexpected span");
                assert_eq!(
                    t2.kind, expected_kind2,
                    "{} did not match {} for '{}'",
                    text2, expected_kind2, full_text
                );
                assert_eq!(
                    t2.span,
                    TextSpan::new(text1.len(), text1.len() + text2.len()),
                    "unexpected span"
                );
            })
    }

    #[test]
    fn test_lexes_token_pairs_with_separator() {
        token_pair_separator_cases().iter().for_each(
            |(text1, expected_kind1, text2, expected_kind2)| {
                separators()
                    .into_iter()
                    .for_each(|(whitespace, expected_whitespace_token)| {
                        let full_text = text1.to_owned() + &whitespace + text2;

                        let tokens = lex_tokens(full_text.to_owned());

                        assert!(
                            tokens.len() == 3,
                            "expected 3 tokens in '{}' but found {:?}:\n'{}' + '{}' + '{}'",
                            full_text,
                            tokens,
                            text1,
                            whitespace,
                            text2
                        );
                        let t1 = &tokens[0];
                        let whitespace_t = &tokens[1];
                        let t2 = &tokens[2];
                        assert_eq!(
                            &t1.kind, expected_kind1,
                            "{} did not match {} for '{}'",
                            text1, expected_kind1, full_text
                        );
                        assert_eq!(
                            t1.span,
                            TextSpan::new(0, text1.len()),
                            "unexpected span for {} in '{}'",
                            t1,
                            full_text
                        );

                        assert_eq!(
                            whitespace_t.kind, expected_whitespace_token,
                            "{} did not match {} for '{}'",
                            whitespace, whitespace_t, full_text
                        );
                        assert_eq!(
                            whitespace_t.span,
                            TextSpan::new(text1.len(), text1.len() + whitespace.len()),
                            "unexpected span for {} in '{}'",
                            whitespace_t,
                            full_text
                        );

                        assert_eq!(
                            &t2.kind, expected_kind2,
                            "{} did not match {} for '{}'",
                            text2, expected_kind2, full_text
                        );
                        assert_eq!(
                            t2.span,
                            TextSpan::new(text1.len() + whitespace.len(), full_text.len()),
                            "unexpected span for {} in '{}'",
                            t2,
                            full_text,
                        );
                    })
            },
        )
    }

    fn separators() -> Vec<(String, TokenKind)> {
        vec![
            (" ".to_string(), WhiteSpace(" ".to_string())),
            ("  ".to_string(), WhiteSpace("  ".to_string())),
            ("\t".to_string(), WhiteSpace("\t".to_string())),
            ("\t ".to_string(), WhiteSpace("\t ".to_string())),
            ("\r".to_string(), WhiteSpace("\r".to_string())),
            ("\r\n".to_string(), WhiteSpace("\r\n".to_string())),
            ("\n".to_string(), WhiteSpace("\n".to_string())),
        ]
    }

    fn single_token_cases() -> Vec<(String, TokenKind)> {
        vec![
            ("$".to_string(), BadToken("$".to_string())),
            ("+".to_string(), Plus),
            ("-".to_string(), Dash),
            ("*".to_string(), Star),
            ("/".to_string(), Slash),
            ("&".to_string(), Ampersand),
            ("&&".to_string(), AmpersandAmpersand),
            ("|".to_string(), Pipe),
            ("||".to_string(), PipePipe),
            ("!".to_string(), Bang),
            ("==".to_string(), EqualsEquals),
            ("!=".to_string(), BangEquals),
            ("<".to_string(), LeftAngleBracket),
            (">".to_string(), RightAngleBracket),
            ("<=".to_string(), LeftAngleEquals),
            (">=".to_string(), RightAngleEquals),
            ("(".to_string(), LeftParenthesis),
            (")".to_string(), RightParenthesis),
            ("{".to_string(), LeftCurly),
            ("}".to_string(), RightCurly),
            ("1".to_string(), Integer(1)),
            ("123".to_string(), Integer(123)),
            ("true".to_string(), Boolean(true)),
            ("false".to_string(), Boolean(false)),
            ("a".to_string(), Identifier("a".to_string())),
            ("foo_bar".to_string(), Identifier("foo_bar".to_string())),
        ]
        .into_iter()
        .collect()
    }

    fn token_pair_cases() -> Vec<(String, TokenKind, String, TokenKind)> {
        single_token_cases()
            .iter()
            .map(|(text1, kind1)| {
                single_token_cases()
                    .into_iter()
                    .map(|(text2, kind2)| (text1.clone(), kind1.clone(), text2, kind2))
            })
            .flatten()
            .filter(|(_, kind1, _, kind2)| !requires_separator(kind1, kind2))
            .collect()
    }

    fn token_pair_separator_cases() -> Vec<(String, TokenKind, String, TokenKind)> {
        single_token_cases()
            .iter()
            .map(|(text1, kind1)| {
                single_token_cases()
                    .into_iter()
                    .map(|(text2, kind2)| (text1.clone(), kind1.clone(), text2, kind2))
            })
            .flatten()
            .filter(|(_, kind1, _, kind2)| requires_separator(kind1, kind2))
            .collect()
    }

    fn requires_separator(t1_kind: &TokenKind, t2_kind: &TokenKind) -> bool {
        match (t1_kind, t2_kind) {
            (&Integer(_), &Integer(_))
            | (&Boolean(_), &Integer(_))
            | (&Boolean(_), &Identifier(_))
            | (&Boolean(_), &Boolean(_))
            | (&Identifier(_), &Boolean(_))
            | (&Identifier(_), &Integer(_))
            | (&Identifier(_), &Identifier(_))
            | (&Ampersand, &Ampersand)
            | (&Ampersand, &AmpersandAmpersand)
            | (&Pipe, &Pipe)
            | (&Pipe, &PipePipe)
            | (&Bang, &EqualsEquals)
            | (&LeftAngleBracket, &EqualsEquals)
            | (&RightAngleBracket, &EqualsEquals) => true,

            _ => false,
        }
    }
}
