use std::{ops, str::Utf8Chunk};

use expr_parser::Span;
use regex_syntax::is_word_character;
use smol_str::SmolStr;

use crate::{error::LexerError, parser::Position};

type Result<T, E = LexerError> = std::result::Result<T, E>;

pub type Token = expr_parser::token::Token<TokenKind, Position>;

pub struct Lexer<'a> {
    input: &'a [u8],
    chunk: Chunk<'a>,
    position: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        let chunk = input.utf8_chunks().next().into();
        Lexer {
            input,
            chunk,
            position: Position::default(),
        }
    }

    pub fn current_position(&self) -> Position {
        self.position
    }

    fn get_next_token(&mut self) -> Result<Option<Token>> {
        self.skip_whitespace_and_comments()?;
        if let Some(c) = self.get_current_char()? {
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

    fn skip_whitespace_and_comments(&mut self) -> Result<()> {
        while let Some(c) = self.get_current_char()? {
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
        if let Some(idx) = self.chunk.find('\n') {
            self.increment_byte_index(idx);
            self.position.line += 1;
            self.position.column = 0;
        } else {
            let rest = &self.input[self.position.byte_idx..];
            if let Some(line) = rest.split(|&b| b == b'\n').next() {
                self.increment_byte_index(line.len());
                self.position.line += 1;
                self.position.column = 0;
            } else {
                // no more newlines, we're at the end of the file
                self.position.byte_idx = self.input.len();
                self.chunk = Chunk::default();
                self.position.column += len_utf8_lossy(rest);
            }
        }
    }

    fn increment_byte_index(&mut self, count: usize) {
        self.position.byte_idx += count;
        self.chunk.byte_idx += count;
        if self.chunk.byte_idx >= self.chunk.total_len() {
            self.chunk = self.input[self.position.byte_idx..]
                .utf8_chunks()
                .next()
                .into();
        }
    }

    fn advance_one(&mut self) {
        if self.input[self.position.byte_idx] == b'\n' {
            self.increment_byte_index(1);
            self.position.line += 1;
            self.position.column = 0;
        } else {
            let Some(next_char) = self.get_char_or_width().transpose() else {
                return;
            };
            self.increment_byte_index(next_char.map_or_else(|len| len, char::len_utf8));
            self.position.column += 1;
        }
    }

    fn advance_while(&mut self, mut pred: impl FnMut(char) -> bool) -> &'a str {
        let mut num_chars = 0;
        let mut num_lines = 0;
        let byte_len = self
            .chunk
            .char_indices()
            .find_map(|(num_bytes, ch)| {
                if pred(ch) {
                    num_chars += 1;
                    if ch == '\n' {
                        num_lines += 1;
                        num_chars = 0;
                    }
                    None
                } else {
                    Some(num_bytes)
                }
            })
            .unwrap_or(self.chunk.len());
        let s = &self.chunk.as_str()[..byte_len];
        self.increment_byte_index(byte_len);
        if num_lines > 0 {
            self.position.line += num_lines;
            self.position.column = 0;
        }
        self.position.column += num_chars;
        s
    }

    fn slice_from(&self, start_idx: usize) -> &'a str {
        std::str::from_utf8(&self.input[start_idx..self.position.byte_idx]).unwrap()
    }

    fn get_char_or_width(&self) -> Result<Option<char>, usize> {
        if let Some(c) = self.chunk.chars().next() {
            Ok(Some(c))
        } else if !self.chunk.invalid.is_empty() {
            Err(self.chunk.invalid.len())
        } else {
            Ok(None)
        }
    }

    fn get_current_char(&self) -> Result<Option<char>> {
        self.get_char_or_width()
            .map_err(|_| LexerError::Unicode(self.position))
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
            match self.get_current_char()? {
                Some('"') => {
                    self.advance_one();
                    return Ok(TokenKind::String(s));
                }
                Some('\\') => {
                    self.advance_one();
                    match self.get_current_char()? {
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
        if let Some('0'..='9') = self.get_current_char()? {
            // the dot is part of a number
            self.chunk.byte_idx -= 1;
            self.position.byte_idx -= 1;
            self.position.column -= 1;
        }
        let next_cat = self.get_current_char()?.map(CharCat::from);
        match (self.position.byte_idx - start_idx, next_cat) {
            (0, _) => self.parse_number(),
            (1, Some(cat)) if cat.is_tag() => Ok(TokenKind::DotTag(self.parse_tag(cat).into())),
            _ => Ok(TokenKind::Tag(self.slice_from(start_idx).into())),
        }
    }

    fn parse_number(&mut self) -> Result<TokenKind> {
        let start_idx = self.position.byte_idx;
        self.advance_while(|c| matches!(c, '0'..='9' | '_'));
        if let Some('.') = self.get_current_char()? {
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

#[derive(Clone, Copy, Default)]
struct Chunk<'a> {
    valid: &'a str,
    invalid: &'a [u8],
    byte_idx: usize,
}

impl<'a> Chunk<'a> {
    fn total_len(&self) -> usize {
        self.valid.len() + self.invalid.len()
    }

    fn as_str(&self) -> &'a str {
        &self.valid[self.byte_idx..]
    }
}

impl ops::Deref for Chunk<'_> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl<'a> From<Option<Utf8Chunk<'a>>> for Chunk<'a> {
    fn from(chunk: Option<Utf8Chunk<'a>>) -> Self {
        chunk
            .map(|chunk| Chunk {
                valid: chunk.valid(),
                invalid: chunk.invalid(),
                byte_idx: 0,
            })
            .unwrap_or_default()
    }
}

fn len_utf8_lossy(bytes: &[u8]) -> usize {
    bytes
        .utf8_chunks()
        .map(|chunk| chunk.valid().chars().count() + chunk.invalid().len().min(1))
        .sum()
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
    use super::Lexer;

    #[test]
    fn all_whitespace() {
        let mut lexer = Lexer::new(" \n\t\u{a0}".as_bytes());
        assert!(lexer.next().is_none());
    }

    #[test]
    fn comment() {
        let mut lexer = Lexer::new("# abc. 12345 //\"".as_bytes());
        assert!(lexer.next().is_none());
    }

    #[test]
    fn tokens() {
        let lexer = Lexer::new("a,b.c.123".as_bytes());
        let tokens = lexer
            .map(|res| res.map(|tok| tok.kind))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(
            tokens,
            [
                token!(#"a"),
                token!(,),
                token!(#"b"),
                token!(."c"),
                token!(0.123)
            ]
        );
    }

    #[test]
    fn number_dot() {
        let lexer = Lexer::new("1.1.1".as_bytes());
        let tokens = lexer
            .map(|res| res.map(|tok| tok.kind))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(tokens, [token!(1.1), token!(0.1)]);
    }

    #[test]
    fn dots() {
        let lexer = Lexer::new("...5...a".as_bytes());
        let tokens = lexer
            .map(|res| res.map(|tok| tok.kind))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(
            tokens,
            [token!(#".."), token!(0.5), token!(#"..."), token!(#"a")]
        );
    }

    #[test]
    fn strings() {
        let lexer = Lexer::new(r#""abc" "\"\\""#.as_bytes());
        let tokens = lexer
            .map(|res| res.map(|tok| tok.kind))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(tokens, [token!(@"abc"), token!(@r#""\"#)]);
    }

    #[test]
    fn string_trailing_backslash() {
        let mut lexer = Lexer::new(r#""\"#.as_bytes());
        assert!(lexer.next().unwrap().is_err());
    }

    #[test]
    fn string_unterminated() {
        let mut lexer = Lexer::new(r#"""#.as_bytes());
        assert!(lexer.next().unwrap().is_err());
    }

    #[test]
    fn string_multiline() {
        let lexer = Lexer::new("\"foo\nbar\"".as_bytes());
        let tokens = lexer
            .map(|res| res.map(|tok| tok.kind))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(tokens, [token!(@"foo\nbar")]);
    }

    #[test]
    fn equal() {
        let lexer = Lexer::new("a=b".as_bytes());
        let tokens = lexer
            .map(|res| res.map(|tok| tok.kind))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(tokens, [token!(#"a"), token!(#"="), token!(#"b")]);
    }

    #[test]
    fn equal_2() {
        let lexer = Lexer::new("a==b".as_bytes());
        let tokens = lexer
            .map(|res| res.map(|tok| tok.kind))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(tokens, [token!(#"a"), token!(#"=="), token!(#"b")]);
    }
}
