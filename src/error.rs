use std::{io, result};

use thiserror::Error;

use crate::{expressions::Variable, values::Value};

pub type Result<T, E = Error> = result::Result<T, E>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Evaluator error: {0}")]
    Evaluator(#[from] EvaluatorError),
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
    #[error("Type error: expected {0:?}, got {1:?}")]
    Type(Type, Type),
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Intersection of parallel lines")]
    ParallelIntersection,
    #[error("Domain error: {0}")]
    Domain(String),
    #[error("Undefined variable {0}")]
    Variable(Variable),
    #[error("Undefined field {0}")]
    Field(Variable),
    #[error("Undefined function {0}")]
    Function(Variable),
    #[error("Incorrect number of arguments to function: expected {expected}, got {actual}")]
    Arguments { expected: usize, actual: usize },
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
