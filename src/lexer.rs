use std::io::BufRead;

use regex_syntax::is_word_character;

use crate::error::{LexerError, Result as EResult};
use crate::parser::LexerExt;

pub struct Lexer<R> {
    input: R,
    str_buffer: String,
    vec_buffer: Vec<char>,
    pos: usize,
    line: usize,
    put_back: Option<Token>,
}

impl<R: BufRead> Lexer<R> {
    pub fn new(input: R) -> Lexer<R> {
        Lexer {
            input,
            str_buffer: String::new(),
            vec_buffer: Vec::new(),
            pos: 0,
            line: 0,
            put_back: None,
        }
    }

    fn get_current_char(&self) -> Option<char> {
        self.vec_buffer.get(self.pos).cloned()
    }

    fn get_next_char(&self) -> Option<char> {
        self.vec_buffer.get(self.pos + 1).cloned()
    }

    fn get_next_token(&mut self) -> EResult<Option<Token>> {
        self.skip_whitespace_and_comments()?;
        if let Some(c) = self.get()? {
            match c.into() {
                CharCat::Letter | CharCat::Underscore => self.parse_word(),
                CharCat::Number => self.parse_number(),
                CharCat::Dot => self.parse_dot(),
                CharCat::Quote => self.parse_string(),
                CharCat::Singleton => {
                    self.pos += 1;
                    Ok(Token::singleton(c).unwrap())
                }
                cat => self.parse_other(cat),
            }
            .map(Some)
        } else {
            Ok(None)
        }
    }

    fn fill_buffer(&mut self) -> EResult<usize> {
        self.str_buffer.clear();
        self.vec_buffer.clear();
        self.input
            .read_line(&mut self.str_buffer)
            .map_err(|err| LexerError::from_io(err, self.line))?;
        self.vec_buffer.extend(self.str_buffer.chars());
        self.pos = 0;
        if !self.vec_buffer.is_empty() {
            self.line += 1;
        }
        Ok(self.vec_buffer.len())
    }

    /// Returns the current character, calling `fill_buffer` as necessary.
    ///
    /// If the buffer is still empty after `fill_buffer`, returns `None`, indicating end of input.
    fn get(&mut self) -> EResult<Option<char>> {
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

    fn skip_whitespace_and_comments(&mut self) -> EResult<()> {
        while let Some(c) = self.get()? {
            match c.into() {
                // move along
                CharCat::Whitespace => self.pos += 1,
                // rest of line is a comment; retrieve new line
                CharCat::Comment => {
                    self.fill_buffer()?;
                }
                // stop skipping
                _ => break,
            }
        }
        Ok(())
    }

    fn parse_word(&mut self) -> EResult<Token> {
        let mut word = String::new();
        while let Some(c) = self.get()? {
            if is_word_character(c) {
                word.push(c);
                self.pos += 1;
            } else {
                break;
            }
        }
        Ok(Token::Tag(word))
    }

    fn parse_string(&mut self) -> EResult<Token> {
        self.pos += 1;
        let mut s = String::new();
        while let Some(c) = self.get()? {
            self.pos += 1;
            match c {
                '"' => return Ok(Token::String(s)),
                '\\' => {
                    match self.get()? {
                        Some('n') => s.push('\n'),
                        Some(c) => s.push(c),
                        None => return Err(LexerError::UnterminatedString(self.line).into()),
                    }
                    self.pos += 1;
                }
                c => s.push(c),
            }
        }
        Err(LexerError::UnterminatedString(self.line).into())
    }

    fn parse_dot(&mut self) -> EResult<Token> {
        match self.get_next_char().map(CharCat::from) {
            // next char is a number, parse this dot as part of that number
            Some(CharCat::Number) => self.parse_number(),
            // next char is a dot, parse this dot as part of a sequence of dots
            Some(CharCat::Dot) => {
                // we necessarily have at least two dots.
                let mut n = 2;
                self.pos += 2;
                while let Some(c) = self.get()? {
                    match CharCat::from(c) {
                        // a dot? increment and continue
                        CharCat::Dot => {
                            self.pos += 1;
                            n += 1;
                        }
                        // a number? rewind, the last dot should be parsed as part of that number
                        CharCat::Number => {
                            self.pos -= 1;
                            n -= 1;
                            break;
                        }
                        // something else? the dots have ended, we're done
                        _ => break,
                    }
                }
                Ok(Token::Tag(".".repeat(n)))
            }
            // next char is something else, this is a solo dot
            _ => {
                self.pos += 1;
                Ok(Token::Dot)
            }
        }
    }

    fn parse_number(&mut self) -> EResult<Token> {
        let mut mantissa = 0_u64;
        let mut sigfigs = 0_u8;
        let mut exponent = 0_i32;
        let mut has_decimal = false;
        // parse the first 17 sigfigs. any more sigfigs after that are irrelevant
        while sigfigs < 17 {
            if let Some(c) = self.get()? {
                match c.into() {
                    CharCat::Number => {
                        self.pos += 1;
                        mantissa *= 10;
                        mantissa += c as u64 - '0' as u64;
                        sigfigs += 1;
                        // if we've matched a decimal, but we're still matching sigfigs, decrease
                        // the exponent
                        if has_decimal {
                            exponent -= 1;
                        }
                    }
                    // ignore underscores
                    CharCat::Underscore => {
                        self.pos += 1;
                    }
                    // if we haven't matched a decimal yet, well now we have. otherwise it's a
                    // second dot, and we should quit
                    CharCat::Dot if !has_decimal => {
                        self.pos += 1;
                        has_decimal = true;
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }
        while let Some(c) = self.get()? {
            match c.into() {
                CharCat::Number => {
                    self.pos += 1;
                    // if we haven't matched a decimal yet, we're adding insignificant digits
                    // before the decimal, so we're increasing the exponent
                    if !has_decimal {
                        exponent += 1;
                    }
                }
                // ignore underscores
                CharCat::Underscore => {
                    self.pos += 1;
                }
                // if we haven't matched a decimal yet, well now we have. otherwise it's a
                // second dot, and we should quit
                CharCat::Dot if !has_decimal => {
                    self.pos += 1;
                    has_decimal = true;
                }
                _ => break,
            }
        }

        if mantissa == 0 {
            Ok(Token::Number(0.0))
        } else if exponent == 0 {
            Ok(Token::Number(mantissa as f64))
        } else {
            Ok(Token::Number(10.0_f64.powi(exponent) * mantissa as f64))
        }
    }

    fn parse_other(&mut self, cat: CharCat) -> EResult<Token> {
        let mut tag = String::new();
        while let Some(c) = self.get()? {
            if CharCat::from(c) == cat {
                self.pos += 1;
                tag.push(c);
            } else {
                break;
            }
        }
        match tag.as_ref() {
            "=" => Ok(Token::Equal),
            _ => Ok(Token::Tag(tag)),
        }
    }
}

impl<R: BufRead> Iterator for Lexer<R> {
    type Item = EResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.put_back.take() {
            Some(item) => Some(Ok(item)),
            None => self.get_next_token().transpose(),
        }
    }
}

impl<R: BufRead> LexerExt for Lexer<R> {
    fn put_back(&mut self, next: Token) {
        self.put_back = Some(next)
    }

    fn line(&self) -> usize {
        self.line
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CharCat {
    Letter,
    Underscore,
    Number,
    Dot,
    Quote,
    Singleton,
    LeftBracket,
    RightBracket,
    Comment,
    Whitespace,
    Other,
}

impl From<char> for CharCat {
    fn from(c: char) -> CharCat {
        use self::CharCat::*;
        match c {
            '0'..='9' => Number,
            '.' => Dot,
            '_' => Underscore,
            '"' => Quote,
            '(' | ')' | ',' | ';' => Singleton,
            '[' => LeftBracket,
            ']' => RightBracket,
            '#' => Comment,
            c if is_word_character(c) => Letter,
            c if c.is_whitespace() => Whitespace,
            _ => Other,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// A tag
    Tag(String),
    /// A literal number
    Number(f64),
    /// A literal string
    String(String),
    /// A single dot
    Dot,
    /// A left parenthesis
    LeftParen,
    /// A right parenthesis
    RightParen,
    /// A comma
    Comma,
    /// A semicolon
    Semicolon,
    /// A single equals sign
    Equal,
}

impl Token {
    fn singleton(c: char) -> Option<Token> {
        match c {
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            ',' => Some(Token::Comma),
            ';' => Some(Token::Semicolon),
            _ => None,
        }
    }

    pub fn as_tag(&self) -> Option<&String> {
        match self {
            Token::Tag(tag) => Some(tag),
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
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(
            tokens,
            [
                token!(#"a"),
                token!(,),
                token!(#"b"),
                token!(.),
                token!(#"c"),
                token!(0.123)
            ]
        );
    }

    #[test]
    fn number_dot() {
        let lexer = Lexer::new("1.1.1".as_bytes());
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens, [token!(1.1), token!(0.1)]);
    }

    #[test]
    fn dots() {
        let lexer = Lexer::new("...5...a".as_bytes());
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(
            tokens,
            [token!(#".."), token!(0.5), token!(#"..."), token!(#"a")]
        );
    }

    #[test]
    fn strings() {
        let lexer = Lexer::new(r#""abc" "\"\\""#.as_bytes());
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();
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
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens, [token!(@"foo\nbar")]);
    }

    #[test]
    fn equal() {
        let lexer = Lexer::new("a=b".as_bytes());
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens, [token!(#"a"), token!(=), token!(#"b")]);
    }

    #[test]
    fn equal_2() {
        let lexer = Lexer::new("a==b".as_bytes());
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens, [token!(#"a"), token!(#"=="), token!(#"b")]);
    }
}
