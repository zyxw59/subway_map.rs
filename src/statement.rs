use std::convert::TryFrom;

use crate::expressions::{Expression, Function, Variable};

/// A statement, annotated with a line number.
#[derive(Clone, Debug)]
pub struct Statement {
    /// The statement.
    pub statement: StatementKind,
    /// The line number.
    pub line: usize,
}

/// A statement.
#[derive(Clone, Debug, PartialEq)]
pub enum StatementKind {
    /// An empty statement.
    Null,
    /// A function declaration.
    Function(Variable, Function),
    /// A variable assignment.
    Variable(Variable, Expression),
    /// A declaration of a single point.
    PointSingle(Variable, Expression),
    /// A declaration of a sequence of points, using the `from` ... `spaced` syntax.
    PointSpaced {
        from: Variable,
        spaced: Expression,
        points: Vec<(Option<Expression>, Variable)>,
    },
    /// A declaration of a sequence of points, using the `from` ... `to` syntax or the
    /// `from` ... `past` syntax.
    PointExtend {
        from: Variable,
        to: (Option<Expression>, Variable),
        points: Vec<(Option<Expression>, Variable)>,
        is_past: bool,
    },
    /// A declaration of a route.
    Route {
        /// The name of the route.
        name: Variable,
        /// The style of the route.
        styles: Vec<Variable>,
        /// The sequence of segments the route follows.
        segments: Vec<Segment>,
    },
    /// A declaration of a stop.
    Stop(Stop),
    /// A stylesheet declaration.
    Style(String),
    /// A document title declaration.
    Title(String),
}

/// A segment in a route.
#[derive(Clone, Debug, PartialEq)]
pub struct Segment {
    /// The start point of the segment.
    pub start: Variable,
    /// The end point of the segment.
    pub end: Variable,
    /// The offset of the segment.
    pub offset: Expression,
}

/// A stop marker.
#[derive(Clone, Debug, PartialEq)]
pub struct Stop {
    /// The location of the stop.
    pub point: Expression,
    /// The style of the stop.
    pub styles: Vec<Variable>,
}

/// A label for a stop.
#[derive(Clone, Debug, PartialEq, serde::Serialize)]
pub struct Label {
    /// The label text.
    pub text: String,
    /// The positioning of the label.
    pub position: LabelPosition,
}

/// The position of a stop label.
#[derive(Clone, Debug, PartialEq, serde::Serialize)]
pub enum LabelPosition {
    End,
    Above,
    Below,
    Left,
    Right,
}

impl TryFrom<String> for LabelPosition {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_ref() {
            "end" => Ok(LabelPosition::End),
            "above" => Ok(LabelPosition::Above),
            "below" => Ok(LabelPosition::Below),
            "left" => Ok(LabelPosition::Left),
            "right" => Ok(LabelPosition::Right),
            _ => Err(value),
        }
    }
}
