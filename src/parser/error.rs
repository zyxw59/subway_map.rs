use expr_parser::Span;
use thiserror::Error;

use super::{Position, TokenKind, Variable};

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexpected end of input")]
    EndOfInput,
    #[error("Unexpected token {0:?} at {1}")]
    Token(TokenKind, Span<Position>),
    #[error("Unclosed parentheses starting at {0}")]
    Parentheses(Span<Position>),
    #[error("Repeated argument {argument} in function definition at {span}")]
    Argument {
        argument: Variable,
        span: Span<Position>,
    },
    #[error("Repeated argument {argument} to marker at {span}")]
    MarkerArgument {
        argument: Variable,
        span: Span<Position>,
    },
    #[error(transparent)]
    Lexer(#[from] LexerError),
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
