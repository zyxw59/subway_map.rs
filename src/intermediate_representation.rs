use svg::node::element::{path::Data, Path as SvgPath, SVG};

use crate::{
    corner::{calculate_longitudinal_offsets, calculate_tan_half_angle},
    error::Result,
    expressions::Variable,
    stops::Stop,
    values::{Point, UnitVector},
};

#[derive(Debug, serde::Serialize)]
pub struct Document {
    pub title: Option<String>,
    pub view_box: (f64, f64, f64, f64),
    pub stylesheets: Vec<String>,
    pub routes: Vec<Path>,
    pub stops: Vec<Stop>,
}

impl Document {
    pub fn to_svg(&self) -> Result<SVG> {
        let mut document = crate::svg::Document::new();
        if let Some(title) = &self.title {
            document.set_title(title);
        }
        document.add_stylesheets(&self.stylesheets);
        document.set_view_box(self.view_box);
        for route in &self.routes {
            document.add_route(route);
        }
        for stop in &self.stops {
            document.add_stop(stop.to_svg()?);
        }
        Ok(document.compile())
    }
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct Path {
    pub operations: Vec<Operation>,
    pub name: Variable,
    pub style: String,
}

impl Path {
    pub fn new(name: Variable, style: String) -> Self {
        Self {
            name,
            style,
            operations: Vec::new(),
        }
    }

    pub fn to_svg(&self) -> SvgPath {
        SvgPath::new()
            .set("id", format!("route-{}", self.name))
            .set("class", format!("route {}", self.style))
            .set(
                "d",
                self.operations
                    .iter()
                    .fold(Data::new(), |data, op| op.to_svg(data)),
            )
    }
}

#[derive(Clone, Copy, Debug, serde::Serialize)]
pub struct Operation {
    pub point: Point,
    #[serde(flatten)]
    pub kind: OperationKind,
}

impl Operation {
    #[doc = include_str!("doc-operation-new_corner.svg")]
    pub fn new_corner(
        from: Point,
        corner: Point,
        to: Point,
        offset_in: f64,
        offset_out: f64,
        arc_width: f64,
    ) -> Self {
        let direction_in = (corner - from).unit();
        let direction_out = (to - corner).unit();
        let tan = calculate_tan_half_angle(-direction_in, direction_out);
        Operation {
            point: corner,
            kind: OperationKind::Corner {
                direction_in,
                direction_out,
                offset_in,
                offset_out,
                radius: arc_width * tan,
            },
        }
    }

    pub fn to_svg(self, data: Data) -> Data {
        let base_point = self.point + self.kind.base_point_offset();
        match self.kind {
            OperationKind::Start { .. } => data.move_to(base_point),
            OperationKind::End { .. } => data.line_to(base_point),
            OperationKind::Corner {
                direction_in,
                direction_out,
                radius,
                ..
            } => {
                let arc_width = radius / calculate_tan_half_angle(-direction_in, direction_out);
                let cross = direction_in.cross(*direction_out);
                data.line_to(direction_in.mul_add(-arc_width, base_point))
                    .elliptical_arc_to((
                        radius,
                        radius,
                        0,
                        0,
                        (cross < 0.0) as u8,
                        direction_out.mul_add(arc_width, base_point),
                    ))
            }
            OperationKind::UTurn {
                direction,
                offset_in,
                offset_out,
                ..
            } => {
                let sweep = offset_in < -offset_out;
                let radius = offset_in + offset_out;
                data.line_to(direction.perp().mul_add(offset_in, base_point))
                    .elliptical_arc_to((
                        radius,
                        radius,
                        0,
                        0,
                        sweep as u8,
                        (-direction.perp()).mul_add(offset_out, base_point),
                    ))
            }
            OperationKind::Shift {
                direction,
                offset_in,
                offset_out,
                longitudinal_in,
                longitudinal_out,
            } => {
                let width = (offset_in - offset_out)
                    .abs()
                    .max((longitudinal_out + longitudinal_in) / 2.0);
                data.line_to(base_point + direction.basis(-width, offset_in))
                    .cubic_curve_to((
                        direction.perp().mul_add(offset_in, base_point),
                        direction.perp().mul_add(offset_out, base_point),
                        base_point + direction.basis(width, offset_out),
                    ))
            }
        }
    }
}

#[derive(Clone, Copy, Debug, serde::Serialize)]
pub enum OperationKind {
    Start {
        direction: UnitVector,
        offset: f64,
    },
    End {
        direction: UnitVector,
        offset: f64,
    },
    Corner {
        direction_in: UnitVector,
        offset_in: f64,
        direction_out: UnitVector,
        offset_out: f64,
        radius: f64,
    },
    UTurn {
        direction: UnitVector,
        offset_in: f64,
        offset_out: f64,
        longitudinal: f64,
    },
    Shift {
        direction: UnitVector,
        offset_in: f64,
        offset_out: f64,
        longitudinal_in: f64,
        longitudinal_out: f64,
    },
}

impl OperationKind {
    #[inline]
    fn base_point_offset(self) -> Point {
        match self {
            Self::Start { direction, offset } | Self::End { direction, offset } => {
                offset * direction.perp()
            }
            OperationKind::Corner {
                direction_in,
                offset_in,
                direction_out,
                offset_out,
                ..
            } => {
                let (longitudinal_in, _) = calculate_longitudinal_offsets(
                    direction_in,
                    direction_out,
                    offset_in,
                    offset_out,
                );
                direction_in.basis(longitudinal_in, offset_in)
            }
            OperationKind::UTurn {
                direction,
                longitudinal,
                ..
            } => longitudinal * *direction,
            OperationKind::Shift {
                direction,
                longitudinal_in,
                longitudinal_out,
                ..
            } => *direction * (longitudinal_out - longitudinal_in),
        }
    }
}
