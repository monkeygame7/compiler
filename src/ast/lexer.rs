use std::fmt::Display;

use ascii::{AsciiChar, AsciiString};

use crate::text::{SourceText, TextSpan};

pub struct Lexer<'a> {
    src: &'a SourceText,
    chars: Vec<AsciiChar>,
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
    Identifier,
    Let,
    If,
    Else,

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
    Equals,
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

#[derive(Clone, Debug)]
pub struct SyntaxToken {
    pub kind: TokenKind,
    pub span: TextSpan,
    pub literal: String,
}

impl Display for SyntaxToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.literal)
    }
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a SourceText) -> Lexer {
        let chars = src.chars().collect();
        Lexer {
            src,
            chars,
            current_position: 0,
        }
    }

    fn peek(&self) -> Option<AsciiChar> {
        self.chars.get(self.current_position).copied()
    }

    fn next(&mut self) -> Option<AsciiChar> {
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
            Some(ch) => match ch {
                AsciiChar::Plus => TokenKind::Plus,
                AsciiChar::Minus => TokenKind::Dash,
                AsciiChar::Asterisk => TokenKind::Star,
                AsciiChar::Slash => TokenKind::Slash,
                AsciiChar::ParenOpen => TokenKind::LeftParenthesis,
                AsciiChar::ParenClose => TokenKind::RightParenthesis,
                AsciiChar::CurlyBraceOpen => TokenKind::LeftCurly,
                AsciiChar::CurlyBraceClose => TokenKind::RightCurly,
                AsciiChar::Ampersand => self.match_potential_double(
                    AsciiChar::Ampersand,
                    TokenKind::AmpersandAmpersand,
                    TokenKind::Ampersand,
                ),
                AsciiChar::VerticalBar => self.match_potential_double(
                    AsciiChar::VerticalBar,
                    TokenKind::PipePipe,
                    TokenKind::Pipe,
                ),
                AsciiChar::Exclamation => self.match_potential_double(
                    AsciiChar::Equal,
                    TokenKind::BangEquals,
                    TokenKind::Bang,
                ),
                AsciiChar::Equal => self.match_potential_double(
                    AsciiChar::Equal,
                    TokenKind::EqualsEquals,
                    TokenKind::Equals,
                ),
                AsciiChar::LessThan => self.match_potential_double(
                    AsciiChar::Equal,
                    TokenKind::LeftAngleEquals,
                    TokenKind::LeftAngleBracket,
                ),
                AsciiChar::GreaterThan => self.match_potential_double(
                    AsciiChar::Equal,
                    TokenKind::RightAngleEquals,
                    TokenKind::RightAngleBracket,
                ),
                ch if ch.is_whitespace() => self.read_whitespace(ch),
                ch if ch.is_ascii_digit() => self.read_integer(ch),
                ch if ch.is_alphabetic() => self.read_literal(ch),
                unrecognized => TokenKind::BadToken(unrecognized.to_string()),
            },
        };

        SyntaxToken {
            kind,
            span: TextSpan::new(previous_position, self.current_position),
            literal: self.src.text[previous_position..self.current_position].to_string(),
        }
    }

    fn read_whitespace(&mut self, first_whitespace_char: AsciiChar) -> TokenKind {
        let mut whitespace = AsciiString::from(first_whitespace_char);

        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                whitespace.push(self.next().unwrap());
            } else {
                break;
            }
        }
        TokenKind::WhiteSpace(whitespace.to_string())
    }

    fn read_integer(&mut self, first_integer_char: AsciiChar) -> TokenKind {
        let integer_string = self.match_continuous(first_integer_char, |ch| ch.is_ascii_digit());

        let integer = integer_string.parse();
        match integer {
            Ok(i) => TokenKind::Integer(i),
            Err(_) => TokenKind::BadToken(integer_string),
        }
    }

    fn read_literal(&mut self, first_char: AsciiChar) -> TokenKind {
        let literal_string = self.match_continuous(first_char, |ch| {
            ch.is_alphanumeric() || ch == AsciiChar::UnderScore
        });

        match literal_string.as_str() {
            "true" => TokenKind::Boolean(true),
            "false" => TokenKind::Boolean(false),
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            _ => TokenKind::Identifier,
        }
    }

    fn match_continuous<F>(&mut self, first_char: AsciiChar, test: F) -> String
    where
        F: Fn(AsciiChar) -> bool,
    {
        let mut string = AsciiString::from(first_char);

        while let Some(ch) = self.peek() {
            if test(ch) {
                string.push(self.next().unwrap())
            } else {
                break;
            }
        }

        string.to_string()
    }

    fn match_potential_double(
        &mut self,
        extra_match: AsciiChar,
        double_symbol: TokenKind,
        single_symbol: TokenKind,
    ) -> TokenKind {
        match self.peek() {
            Some(ch) if ch == extra_match => {
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
        let src = SourceText::from(&text).unwrap();
        let mut lexer = Lexer::new(&src);
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
                    "{} did not match {:?}",
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
                    "{} did not match {:?} for '{}'",
                    text1, expected_kind1, full_text
                );
                assert_eq!(t1.span, TextSpan::new(0, text1.len()), "unexpected span");
                assert_eq!(
                    t2.kind, expected_kind2,
                    "{} did not match {:?} for '{}'",
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
                            "{} did not match {:?} for '{}'",
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
                            "{} did not match {:?} for '{}'",
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
            ("a".to_string(), Identifier),
            ("foo_bar".to_string(), Identifier),
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
            | (&Boolean(_), &Identifier)
            | (&Boolean(_), &Boolean(_))
            | (&Identifier, &Boolean(_))
            | (&Identifier, &Integer(_))
            | (&Identifier, &Identifier)
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
