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
    #[error("Repeated argument {argument} to function {function} at {span}")]
    Argument {
        argument: Variable,
        function: Variable,
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
