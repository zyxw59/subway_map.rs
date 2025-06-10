use svg::node::element::path::Data;

use crate::values::{Point, UnitVector};

#[derive(Clone, Debug)]
pub struct Path {
    pub operations: Vec<Operation>,
}

#[derive(Clone, Copy, Debug)]
pub struct Operation {
    pub point: Point,
    pub kind: OperationKind,
}

impl Operation {
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
            OperationKind::Start { .. } => {
                data.move_to(base_point)
            }
            OperationKind::End { .. } => {
                data.line_to(base_point)
            }
            OperationKind::Corner {
                direction_in,
                direction_out,
                radius,
                ..
            } => {
                let arc_width = radius / calculate_tan_half_angle(-direction_in, direction_out);
                let cross = direction_out.cross(*direction_in);
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
            } => {
                let sweep = offset_in < -offset_out;
                let radius = offset_in + offset_out;
                data.line_to(direction.perp().mul_add(offset_in, self.point))
                    .elliptical_arc_to((
                        radius,
                        radius,
                        0,
                        0,
                        sweep as u8,
                        (-direction.perp()).mul_add(offset_out, self.point),
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

#[derive(Clone, Copy, Debug)]
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
                let cross = direction_out.cross(*direction_in);
                direction_in.basis(
                    direction_in.dot(direction_out.basis(offset_in * cross, offset_out)),
                    offset_in,
                )
            }
            OperationKind::UTurn { .. } => Point(0.0, 0.0),
            OperationKind::Shift {
                direction,
                longitudinal_in,
                longitudinal_out,
                ..
            } => *direction * (longitudinal_out - longitudinal_in),
        }
    }
}

/// Calculate `|tan(θ/2)|`, where `θ` is the angle formed by the two vectors.
#[inline]
pub fn calculate_tan_half_angle(in_dir: UnitVector, out_dir: UnitVector) -> f64 {
    let cos = in_dir.dot(*out_dir);
    ((1.0 - cos) / (1.0 + cos)).sqrt()
}
