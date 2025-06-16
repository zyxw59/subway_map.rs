use std::collections::HashMap;

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
    /// A function declaration.
    Function(Variable, Function),
    /// A variable assignment.
    Variable(Variable, Vec<Variable>, Expression),
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
    /// The type of marker to place at the stop.
    pub marker_type: Variable,
    /// The parameters for the marker.
    pub marker_parameters: HashMap<Variable, Expression>,
}
