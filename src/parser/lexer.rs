use expr_parser::Span;
use regex_syntax::is_word_character;
use smol_str::SmolStr;

use super::{error::LexerError, Position};

type Result<T, E = LexerError> = std::result::Result<T, E>;

pub type Token = expr_parser::token::Token<TokenKind, Position>;

pub struct Lexer<'a> {
    input: &'a str,
    position: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            position: Position::default(),
        }
    }

    pub fn current_position(&self) -> Position {
        self.position
    }

    fn get_next_token(&mut self) -> Result<Option<Token>> {
        self.skip_whitespace_and_comments()?;
        if let Some(c) = self.next_char() {
            let start = self.current_position();
            let kind = match c.into() {
                CharCat::Number => self.parse_number(),
                CharCat::Dot => self.parse_dot(),
                CharCat::Quote => self.parse_string(),
                CharCat::Singleton => {
                    self.advance_one();
                    Ok(TokenKind::singleton(c).unwrap())
                }
                cat => Ok(TokenKind::Tag(self.parse_tag(cat).into())),
            }?;
            let end = self.current_position();
            Ok(Some(Token {
                span: Span { start, end },
                kind,
            }))
        } else {
            Ok(None)
        }
    }

    fn rest(&self) -> &'a str {
        &self.input[self.position.byte_idx..]
    }

    fn next_char(&self) -> Option<char> {
        self.rest().chars().next()
    }

    fn skip_whitespace_and_comments(&mut self) -> Result<()> {
        while let Some(c) = self.next_char() {
            match c {
                // rest of line is a comment
                '#' => self.skip_line(),
                c if c.is_whitespace() => {
                    self.advance_while(char::is_whitespace);
                }
                // stop skipping
                _ => break,
            }
        }
        Ok(())
    }

    fn skip_line(&mut self) {
        let rest = self.rest();
        if let Some(offset) = rest.find('\n') {
            self.position.byte_idx += offset;
            self.position.line += 1;
            self.position.column = 0;
        } else {
            // no more newlines, we're at the end of the file
            self.position.byte_idx = self.input.len();
            self.position.column += rest.chars().count();
        }
    }

    fn advance_one(&mut self) {
        match self.next_char() {
            Some('\n') => {
                self.position.byte_idx += 1;
                self.position.line += 1;
                self.position.column = 0;
            }
            Some(c) => {
                self.position.byte_idx += c.len_utf8();
                self.position.column += 1;
            }
            None => {}
        }
    }

    fn advance_while(&mut self, mut pred: impl FnMut(char) -> bool) -> &'a str {
        let start_idx = self.position.byte_idx;
        let byte_len = self
            .rest()
            .char_indices()
            .find_map(|(num_bytes, ch)| {
                if pred(ch) {
                    if ch == '\n' {
                        self.position.line += 1;
                        self.position.column = 0;
                    } else {
                        self.position.column += 1;
                    }
                    None
                } else {
                    Some(num_bytes)
                }
            })
            .unwrap_or(self.rest().len());
        self.position.byte_idx += byte_len;
        self.slice_from(start_idx)
    }

    fn slice_from(&self, start_idx: usize) -> &'a str {
        &self.input[start_idx..self.position.byte_idx]
    }

    fn parse_tag(&mut self, cat: CharCat) -> &str {
        assert!(cat.is_tag());
        match cat {
            CharCat::Word => self.parse_word(),
            cat => self.parse_other(cat),
        }
    }

    fn parse_word(&mut self) -> &str {
        self.advance_while(is_word_character)
    }

    fn parse_string(&mut self) -> Result<TokenKind> {
        let mut s = String::new();
        self.advance_one();
        loop {
            s.push_str(self.advance_while(|c| !matches!(c, '\\' | '"')));
            match self.next_char() {
                Some('"') => {
                    self.advance_one();
                    return Ok(TokenKind::String(s));
                }
                Some('\\') => {
                    self.advance_one();
                    match self.next_char() {
                        Some('n') => {
                            self.advance_one();
                            s.push('\n');
                        }
                        Some(c) => {
                            self.advance_one();
                            s.push(c);
                        }
                        None => break,
                    }
                }
                Some(_) => {}
                None => break,
            }
        }
        Err(LexerError::UnterminatedString(self.position))
    }

    fn parse_dot(&mut self) -> Result<TokenKind> {
        let start_idx = self.position.byte_idx;
        self.advance_while(|c| c == '.');
        if let Some('0'..='9') = self.next_char() {
            // the dot is part of a number
            self.position.byte_idx -= 1;
            self.position.column -= 1;
        }
        let next_cat = self.next_char().map(CharCat::from);
        match (self.position.byte_idx - start_idx, next_cat) {
            (0, _) => self.parse_number(),
            (1, Some(cat)) if cat.is_tag() => Ok(TokenKind::DotTag(self.parse_tag(cat).into())),
            _ => Ok(TokenKind::Tag(self.slice_from(start_idx).into())),
        }
    }

    fn parse_number(&mut self) -> Result<TokenKind> {
        let start_idx = self.position.byte_idx;
        self.advance_while(|c| matches!(c, '0'..='9' | '_'));
        if let Some('.') = self.next_char() {
            self.advance_one();
            self.advance_while(|c| matches!(c, '0'..='9' | '_'));
        }
        let mut s = std::borrow::Cow::Borrowed(self.slice_from(start_idx));
        if s.contains('_') {
            s.to_mut().retain(|c| c != '_');
        }
        let val: f64 = s.parse().unwrap();
        Ok(TokenKind::Number(val))
    }

    fn parse_other(&mut self, cat: CharCat) -> &str {
        self.advance_while(|c| CharCat::from(c) == cat)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.get_next_token().transpose()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CharCat {
    Word,
    Number,
    Dot,
    Quote,
    Singleton,
    LeftBracket,
    RightBracket,
    Comment,
    Whitespace,
    Comparison,
    Other,
}

impl CharCat {
    fn is_tag(self) -> bool {
        matches!(
            self,
            Self::Word
                | Self::Dot
                | Self::LeftBracket
                | Self::RightBracket
                | Self::Comparison
                | Self::Other
        )
    }
}

impl From<char> for CharCat {
    fn from(c: char) -> CharCat {
        use self::CharCat::*;
        match c {
            '0'..='9' => Number,
            '.' => Dot,
            '"' => Quote,
            '(' | ')' | ',' | ';' => Singleton,
            '[' => LeftBracket,
            ']' => RightBracket,
            '#' => Comment,
            '<' | '=' | '>' => Comparison,
            c if is_word_character(c) => Word,
            c if c.is_whitespace() => Whitespace,
            _ => Other,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    /// A tag
    Tag(SmolStr),
    /// A tag preceded by a single dot
    DotTag(SmolStr),
    /// A literal number
    Number(f64),
    /// A literal string
    String(String),
    /// A left parenthesis
    LeftParen,
    /// A right parenthesis
    RightParen,
    /// A comma
    Comma,
    /// A semicolon
    Semicolon,
}

impl TokenKind {
    fn singleton(c: char) -> Option<Self> {
        match c {
            '(' => Some(Self::LeftParen),
            ')' => Some(Self::RightParen),
            ',' => Some(Self::Comma),
            ';' => Some(Self::Semicolon),
            _ => None,
        }
    }

    pub fn as_tag(&self) -> Option<&str> {
        match self {
            TokenKind::Tag(tag) => Some(tag),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use test_case::test_case;

    use super::{Lexer, TokenKind};

    fn tag(s: &str) -> TokenKind {
        TokenKind::Tag(s.into())
    }

    fn dot(s: &str) -> TokenKind {
        TokenKind::DotTag(s.into())
    }

    fn string(s: &str) -> TokenKind {
        TokenKind::String(s.into())
    }

    fn num(x: f64) -> TokenKind {
        TokenKind::Number(x)
    }

    #[test_case(" \n\t\u{a0}", []; "all whitespace")]
    #[test_case("# abc. 12345 //\"\na", [tag("a")]; "comment")]
    #[test_case("a,b.c.123", [tag("a"), TokenKind::Comma, tag("b"), dot("c"), num(0.123)]; "tokens")]
    #[test_case("1.1.1", [num(1.1), num(0.1)]; "number dot")]
    #[test_case("...5...a", [tag(".."), num(0.5), tag("..."), tag("a")]; "dots")]
    #[test_case(r#""abc" "\"\\""#, [string("abc"), string(r#""\"#)]; "strings")]
    #[test_case("\"foo\nbar\"", [string("foo\nbar")]; "string multiline")]
    #[test_case("a=b", [tag("a"), tag("="), tag("b")]; "equal")]
    #[test_case("a==b", [tag("a"), tag("=="), tag("b")]; "equal 2")]
    fn test_valid_tokens<const N: usize>(input: &str, expected: [TokenKind; N]) {
        let lexer = Lexer::new(input);
        let actual = lexer
            .map(|res| res.map(|tok| tok.kind))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(actual, expected);
    }

    #[test_case(r#""\"#, [Err(())]; "string trailing backslash")]
    #[test_case(r#"""#, [Err(())]; "string unterminated")]
    fn test_errors<const N: usize>(input: &str, expected: [Result<TokenKind, ()>; N]) {
        let lexer = Lexer::new(input);
        let actual = lexer
            .map(|res| res.map(|tok| tok.kind).map_err(|_| ()))
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);
    }
}
