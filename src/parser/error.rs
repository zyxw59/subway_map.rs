use std::fmt;

use thiserror::Error;

use super::{expression::ExpressionError, Position, Span};

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Error)]
pub struct Errors<'a> {
    input: &'a str,
    errors: Vec<Error>,
}

impl<'a> Errors<'a> {
    pub(super) fn new(input: &'a str, errors: Vec<Error>) -> Self {
        Self { input, errors }
    }

    fn index(&self, span: Span) -> &'a str {
        &self.input[span.start.byte_idx..span.end.byte_idx]
    }
}

impl fmt::Display for Errors<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for err in &self.errors {
            // TODO(#27): these could be formatted nicer probably
            match err {
                Error::EndOfInput => f.write_str("Unexpected end of input\n")?,
                Error::Token(span) => writeln!(
                    f,
                    "Unexpected token {:?} at {}",
                    self.index(*span),
                    span.start
                )?,
                Error::Parentheses(span) => {
                    writeln!(f, "Unclosed parentheses starting at {}", span.start)?
                }
                Error::Argument(span) => writeln!(
                    f,
                    "Repeated argument {:?} in function definition at {}",
                    self.index(*span),
                    span
                )?,
                Error::MarkerArgument(span) => writeln!(
                    f,
                    "Repeated argument {:?} to marker at {}",
                    self.index(*span),
                    span
                )?,
                Error::Lexer(err) => writeln!(f, "{err}")?,
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Error {
    EndOfInput,
    Token(Span),
    Parentheses(Span),
    Argument(Span),
    MarkerArgument(Span),
    Lexer(LexerError),
}

impl From<LexerError> for Error {
    fn from(error: LexerError) -> Error {
        Error::Lexer(error)
    }
}

impl From<ExpressionError> for Error {
    fn from(error: ExpressionError) -> Self {
        use super::expression::UnexpectedToken;
        use expr_parser::ParseErrorKind as E;
        match error.kind {
            E::EndOfInput { .. } => Error::EndOfInput,
            E::UnexpectedToken { .. } => Error::Token(error.span),
            // TODO(#27): mismatched delimiter
            E::MismatchedDelimiter { .. } => Error::Token(error.span),
            // TODO(#27): unmatched right delimiter
            E::UnmatchedRightDelimiter => Error::Token(error.span),
            E::UnmatchedLeftDelimiter => Error::Parentheses(error.span),
            E::Parser(UnexpectedToken) => Error::Token(error.span),
            E::Tokenizer(lexer_error) => lexer_error.into(),
        }
    }
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unterminated string at {0}")]
    UnterminatedString(Position),
}

pub(super) trait ResultExt<T> {
    fn or_push(self, errors: &mut Vec<Error>) -> Option<T>;
}

impl<T, E> ResultExt<T> for Result<T, E>
where
    Error: From<E>,
{
    fn or_push(self, errors: &mut Vec<Error>) -> Option<T> {
        self.map_err(|err| errors.push(err.into())).ok()
    }
}

impl<T, E> ResultExt<T> for Option<Result<T, E>>
where
    Error: From<E>,
{
    fn or_push(self, errors: &mut Vec<Error>) -> Option<T> {
        self.and_then(|res| res.or_push(errors))
    }
}
