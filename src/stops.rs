use svg::node::{
    element::{Circle, Group, Path},
    Node, Text,
};

use crate::{
    document::Document,
    error::EvaluatorError,
    values::{Point, Value},
};

/// A stop.
#[derive(Debug)]
pub struct Stop {
    /// Location of the stop
    pub point: Point,
    /// Styles applying to the stop
    pub styles: Vec<String>,
    /// Type of marker to use for the stop
    pub marker_type: String,
    /// The parameters for the marker
    pub marker_parameters: Vec<Value>,
    /// The input line the stop is defined on
    pub input_line: usize,
}

impl Stop {
    pub fn draw(&self, document: &mut Document) -> Result<(), EvaluatorError> {
        let mut group = Group::new().set("class", self.styles.join(" "));
        match &*self.marker_type {
            "circle" => {
                let radius = match &*self.marker_parameters {
                    &[ref radius] => {
                        f64::try_from(radius).map_err(|error| EvaluatorError::InvalidMarkerArgs {
                            name: self.marker_type.clone(),
                            line: self.input_line,
                            error,
                        })
                    }
                    _ => Err(EvaluatorError::invalid_marker_args_len(
                        self.marker_type.clone(),
                        self.input_line,
                        1,
                        self.marker_parameters.len(),
                    )),
                }?;
                let circ = Circle::new()
                    .set("cx", self.point.0)
                    .set("cy", self.point.1)
                    .set("r", radius);
                group.append(circ);
            }
            _ => {
                return Err(EvaluatorError::UndefinedStopMarker {
                    name: self.marker_type.clone(),
                    line: self.input_line,
                })
            }
        }
        document.add_stop(group);
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct StopCollection {
    pub stops: Vec<Stop>,
}

impl StopCollection {
    pub fn push(&mut self, stop: Stop) {
        self.stops.push(stop);
    }

    pub fn draw(&self, document: &mut Document) -> Result<(), EvaluatorError> {
        for stop in &self.stops {
            stop.draw(document)?;
        }
        Ok(())
    }
}
