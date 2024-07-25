use std::{io, result};

use thiserror::Error;

use crate::{expressions::Variable, lexer::TokenKind, values::Value};

pub type Result<T, E = Error> = result::Result<T, E>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Lexer error: {0}")]
    Lexer(#[from] LexerError),
    #[error("Parser error: {0}")]
    Parser(#[from] ParserError),
    #[error("Evaluator error: {0}")]
    Evaluator(#[from] EvaluatorError),
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected end of input on line {0}")]
    EndOfInput(usize),
    #[error("Unexpected token {0:?} on line {1}")]
    Token(TokenKind, usize),
    #[error("Unclosed parentheses starting on line {0}")]
    Parentheses(usize),
    #[error(
        "Too many items in parenthesized list starting on line {1} (got {0}, expected 1 or 2)"
    )]
    ParenList(usize, usize),
    #[error("Repeated argument {argument} to function {function} on line {line}")]
    Argument {
        argument: Variable,
        function: Variable,
        line: usize,
    },
}

#[derive(Error, Debug)]
pub enum EvaluatorError {
    #[error("Math error on line {1}: {0}")]
    Math(#[source] MathError, usize),
    #[error(
        "Point ({name}) redefined on line {line} (originally defined on line {original_line})"
    )]
    PointRedefinition {
        name: Variable,
        line: usize,
        original_line: usize,
    },
    #[error(
        "Route ({name}) redefined on line {line} (originally defined on line {original_line})"
    )]
    RouteRedefinition {
        name: Variable,
        line: usize,
        original_line: usize,
    },
    #[error("Undefined stop marker {name} on line {line}")]
    UndefinedStopMarker { name: Variable, line: usize },
    #[error("Invalid argument {arg} to stop marker of type {marker} on line {line}: {error}")]
    InvalidMarkerArg {
        marker: Variable,
        arg: &'static str,
        line: usize,
        #[source]
        error: MathError,
    },
    #[error("Missing argument {arg} to stop marker of type {marker} on line {line}")]
    MissingMarkerArg {
        marker: Variable,
        arg: &'static str,
        line: usize,
    },
    #[error("IO error during output: {0}")]
    Io(#[from] io::Error),
    #[error("Error during debug output: {0}")]
    DebugOutput(#[from] serde_json::Error),
}

#[derive(Error, Debug)]
pub enum MathError {
    #[error("Type error: expected {0:?}, got {0:?}")]
    Type(Type, Type),
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Intersection of parallel lines")]
    ParallelIntersection,
    #[error("Domain error: {0}")]
    Domain(String),
    #[error("Undefined variable {0}")]
    Variable(Variable),
    #[error("Undefined function {0}")]
    Function(Variable),
    #[error("Incorrect number of arguments to function {name}: expected {expected}, got {actual}")]
    Arguments {
        name: Variable,
        expected: usize,
        actual: usize,
    },
}

#[derive(Debug)]
pub enum Type {
    Number,
    Point,
    Line,
    String,
    List,
    Struct,
    Function,
}

impl From<Value> for Type {
    fn from(expr: Value) -> Type {
        (&expr).into()
    }
}

impl From<&'_ Value> for Type {
    fn from(expr: &Value) -> Type {
        match expr {
            Value::Number(_) => Type::Number,
            Value::Point(..) => Type::Point,
            Value::Line(..) => Type::Line,
            Value::String(..) => Type::String,
            Value::List(..) => Type::List,
            Value::Struct(..) => Type::Struct,
            Value::Function(..) => Type::Function,
        }
    }
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unterminated string at line {0}")]
    UnterminatedString(usize),
    #[error("Invalid UTF-8 at line {0}")]
    Unicode(usize),
    #[error("IO error while reading line {1}: {0}")]
    Io(#[source] io::Error, usize),
}

impl LexerError {
    pub fn from_io(err: io::Error, line: usize) -> LexerError {
        match err.kind() {
            io::ErrorKind::InvalidData => LexerError::Unicode(line),
            _ => LexerError::Io(err, line),
        }
    }
}
