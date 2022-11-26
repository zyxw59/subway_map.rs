use std::collections::BTreeMap;

use crate::values::Point;

/// A stop.
#[derive(Debug)]
pub struct Stop {
    /// Location of the stop
    pub point: Point,
    /// Styles applying to the stop
    pub style: Vec<String>,
    /// Type of marker to use for the stop
    pub marker: MarkerType,
}

/// Parameters for a stop marker
#[derive(Debug)]
pub enum MarkerType {
    Circle {
        radius: f64,
    },
    Tick {
        length: f64,
        angle: f64,
    },
    DoubleTick {
        length: f64,
        angle: f64,
    },
    Label {
        text: f64,
        angle: f64,
    },
    Other {
        name: String,
        params: BTreeMap<String, String>,
    },
}

#[derive(Debug, Default)]
pub struct StopCollection {
    pub stops: Vec<Stop>,
}

impl StopCollection {
}
