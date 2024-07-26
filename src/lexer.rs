use std::io::BufRead;

use expr_parser::Span;
use itertools::Itertools;
use regex_syntax::is_word_character;
use smol_str::SmolStr;

use crate::{
    error::{LexerError, Result},
    parser::{LexerExt, Position},
};

pub type Token = expr_parser::token::Token<TokenKind>;

pub struct Lexer<R> {
    input: R,
    buffer: String,
    /// Byte index into `buffer`
    byte_idx: usize,
    /// Character index into the file, used for token spans.
    char_idx: usize,
    /// Character indices of the start of each line, used for mapping spans to (line, column)
    /// pairs.
    lines: Vec<usize>,
}

impl<R: BufRead> Lexer<R> {
    pub fn new(input: R) -> Lexer<R> {
        Lexer {
            input,
            buffer: String::new(),
            byte_idx: 0,
            char_idx: 0,
            lines: vec![0],
        }
    }

    pub fn column(&self) -> usize {
        self.char_idx - self.lines.last().unwrap()
    }

    fn get_next_token(&mut self) -> Result<Option<Token>> {
        self.skip_whitespace_and_comments()?;
        if let Some(c) = self.get()? {
            let start = self.char_idx;
            let kind = match c.into() {
                CharCat::Number => self.parse_number(),
                CharCat::Dot => self.parse_dot(),
                CharCat::Quote => self.parse_string(),
                CharCat::Singleton => {
                    self.advance_one();
                    Ok(TokenKind::singleton(c).unwrap())
                }
                cat => self.parse_tag(cat).map(|s| TokenKind::Tag(s.into())),
            }?;
            let end = self.char_idx;
            Ok(Some(Token {
                span: Span { start, end },
                kind,
            }))
        } else {
            Ok(None)
        }
    }

    fn skip_whitespace_and_comments(&mut self) -> Result<()> {
        while let Some(c) = self.get()? {
            match c {
                // rest of line is a comment; retrieve new line
                '#' => {
                    self.char_idx += 1;
                    self.fill_buffer()?;
                }
                c if c.is_whitespace() => self.advance_while(char::is_whitespace),
                // stop skipping
                _ => break,
            }
        }
        Ok(())
    }

    fn fill_buffer(&mut self) -> Result<()> {
        self.buffer.clear();
        self.byte_idx = 0;
        self.input
            .read_line(&mut self.buffer)
            .map_err(|err| LexerError::from_io(err, self.line()))?;
        if !self.buffer.is_empty() {
            self.lines.push(self.char_idx);
        }
        Ok(())
    }

    fn advance_one(&mut self) {
        if let Some(c) = self.get_current_char() {
            self.char_idx += 1;
            self.byte_idx += c.len_utf8();
        }
    }

    fn retreat_one(&mut self) {
        if let Some(c) = self.buffer[..self.byte_idx].chars().last() {
            self.char_idx -= 1;
            self.byte_idx -= c.len_utf8();
        }
    }

    fn advance_while(&mut self, mut pred: impl FnMut(char) -> bool) {
        let mut num_chars = 0;
        let byte_idx = self.buffer[self.byte_idx..]
            .char_indices()
            .find_map(|(num_bytes, ch)| {
                if pred(ch) {
                    num_chars += 1;
                    None
                } else {
                    Some(self.byte_idx + num_bytes)
                }
            })
            .unwrap_or(self.buffer.len());
        self.byte_idx = byte_idx;
        self.char_idx += num_chars;
    }

    fn slice_from(&self, start_idx: usize) -> &str {
        &self.buffer[start_idx..self.byte_idx]
    }

    fn get_current_char(&self) -> Option<char> {
        self.buffer[self.byte_idx..].chars().next()
    }

    /// Returns the current character, calling `fill_buffer` as necessary.
    ///
    /// If the buffer is still empty after `fill_buffer`, returns `None`, indicating end of input.
    fn get(&mut self) -> Result<Option<char>> {
        Ok(match self.get_current_char() {
            // if we've got a character, here it is
            Some(c) => Some(c),
            // if we don't, check if we can get a new buffer
            None => {
                self.fill_buffer()?;
                // and return the new character, if any
                self.get_current_char()
            }
        })
    }

    fn parse_tag(&mut self, cat: CharCat) -> Result<&str> {
        assert!(cat.is_tag());
        match cat {
            CharCat::Word => self.parse_word(),
            cat => self.parse_other(cat),
        }
    }

    fn parse_word(&mut self) -> Result<&str> {
        let start_idx = self.byte_idx;
        self.advance_while(is_word_character);
        Ok(self.slice_from(start_idx))
    }

    fn parse_string(&mut self) -> Result<TokenKind> {
        let mut s = String::new();
        self.advance_one();
        loop {
            let start_idx = self.byte_idx;
            self.advance_while(|c| !matches!(c, '\\' | '"'));
            s.push_str(self.slice_from(start_idx));
            match self.get()? {
                Some('"') => {
                    self.advance_one();
                    return Ok(TokenKind::String(s));
                }
                Some('\\') => {
                    self.advance_one();
                    match self.get()? {
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
        Err(LexerError::UnterminatedString(self.line()).into())
    }

    fn parse_dot(&mut self) -> Result<TokenKind> {
        let start_idx = self.byte_idx;
        self.advance_while(|c| c == '.');
        if let Some('0'..='9') = self.get_current_char() {
            // the dot is part of a number
            self.retreat_one();
        }
        let next_cat = self.get_current_char().map(CharCat::from);
        match (self.byte_idx - start_idx, next_cat) {
            (0, _) => self.parse_number(),
            (1, Some(cat)) if cat.is_tag() => {
                self.parse_tag(cat).map(|s| TokenKind::DotTag(s.into()))
            }
            _ => Ok(TokenKind::Tag(self.slice_from(start_idx).into())),
        }
    }

    fn parse_number(&mut self) -> Result<TokenKind> {
        let start_idx = self.byte_idx;
        self.advance_while(|c| matches!(c, '0'..='9' | '_'));
        if let Some('.') = self.get_current_char() {
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

    fn parse_other(&mut self, cat: CharCat) -> Result<&str> {
        let start_idx = self.byte_idx;
        self.advance_while(|c| CharCat::from(c) == cat);
        Ok(self.slice_from(start_idx))
    }
}

impl<R: BufRead> Iterator for Lexer<R> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.get_next_token().transpose()
    }
}

impl<R: BufRead> LexerExt for Lexer<R> {
    fn line(&self) -> usize {
        self.lines.len()
    }

    fn line_column(&self, idx: usize) -> Position {
        // we're almost always going to be calling this on a recent index, so it's probably fastest
        // to do a linear search from the end.
        let (lines_from_end, line_start) = self
            .lines
            .iter()
            .rev()
            .find_position(|start| **start <= idx)
            .unwrap();
        Position {
            line: self.lines.len() - lines_from_end,
            column: idx + 1 - line_start,
        }
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
