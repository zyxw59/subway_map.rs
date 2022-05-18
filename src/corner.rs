use svg::node::element::path::Data;

use crate::values::Point;

/// The information required to draw a rounded corner.
pub struct Corner {
    /// The vertex of the corner (as if it weren't rounded.
    pub corner: Point,
    /// The radius of the corner.
    pub radius: f64,
    /// The distance between the corner and the endpoints of the arc.
    pub arc_width: f64,
    /// Whether the arc runs clockwise (`true`) or counterclockwise (`false`).
    pub sweep: bool,
    /// The start point of the arc.
    pub start: Point,
    /// The end point of the arc.
    pub end: Point,
}

impl Corner {
    /// Constructs a new `Corner` with the given parameters.
    pub fn new(from: Point, corner: Point, to: Point, arc_width: f64) -> Corner {
        let in_dir = (from - corner).unit();
        let out_dir = (to - corner).unit();
        let tan = calculate_tan_half_angle(in_dir, out_dir);
        let radius = arc_width * tan;
        Corner {
            corner,
            radius,
            arc_width,
            // positive cross product => clockwise; negative cross product => counterclockwise
            sweep: in_dir.cross(out_dir) < 0.0,
            start: in_dir.mul_add(arc_width, corner),
            end: out_dir.mul_add(arc_width, corner),
        }
    }

    /// Returns the point at the midpoint of the arc.
    pub fn midpoint(self) -> Point {
        let vector = (self.end - self.start).unit();
        let sine_sq = ((self.end - self.start) / 2.0).norm2();
        let sine = sine_sq.sqrt();
        let versine = self.radius - (self.radius.mul_add(self.radius, sine_sq)).sqrt();
        self.start
            + if self.sweep {
                vector.basis(sine, -versine)
            } else {
                vector.basis(sine, versine)
            }
    }

    /// Offsets the values of a `Corner` by the specified parallel distances.
    pub fn offset(self, transverse_in: f64, transverse_out: f64) -> Corner {
        let in_dir = (self.start - self.corner).unit();
        let out_dir = (self.end - self.corner).unit();
        let (longitudinal_in, _) =
            calculate_longitudinal_offsets(in_dir, out_dir, -transverse_in, transverse_out);
        let offset = in_dir.basis(longitudinal_in, transverse_in);
        Corner {
            corner: self.corner + offset,
            start: self.start + offset,
            end: self.end + offset,
            ..self
        }
    }

    /// Constructs a `Corner` representing a 180-degree turn around the corner.
    ///
    /// Because the line is traversed in reverse on the way out, `offset_out` is oriented in the
    /// opposite direction as `offset_in`.
    pub fn u_turn(
        from: Point,
        corner: Point,
        offset_in: f64,
        offset_out: f64,
        shift: f64,
    ) -> Corner {
        let vector = (from - corner).unit();
        Corner {
            corner,
            radius: (offset_in + offset_out).abs() / 2.0,
            arc_width: 0.0,
            sweep: offset_in < -offset_out,
            start: corner + vector.basis(shift, offset_in),
            end: corner + vector.basis(shift, -offset_out),
        }
    }

    /// Appends the corner to the given `Data`.
    pub fn apply(&self, data: Data) -> Data {
        data.line_to(self.start).elliptical_arc_to((
            self.radius,
            self.radius,
            0,
            0,
            self.sweep as u8,
            self.end,
        ))
    }
}

/// The information required to draw a shift between two offsets on the same line as a cubic Bézier
/// curve.
pub struct ParallelShift(pub Point, pub Point, pub Point, pub Point);

impl ParallelShift {
    pub fn new(
        transverse_in: f64,
        transverse_out: f64,
        longitudinal_in: f64,
        longitudinal_out: f64,
        dir: Point,
        at: Point,
    ) -> ParallelShift {
        let dir = dir.unit();
        let longitudinal_width = longitudinal_in + longitudinal_out;
        let transverse_width = (transverse_out - transverse_in).abs();
        let at = dir.mul_add(longitudinal_out - longitudinal_in, at);
        let width = transverse_width.max(longitudinal_width / 2.0);
        ParallelShift(
            at + dir.basis(-width, -transverse_in),
            at + dir.basis(0.0, -transverse_in),
            at + dir.basis(0.0, -transverse_out),
            at + dir.basis(width, -transverse_out),
        )
    }

    /// Appends the shift to the given `Data`.
    pub fn apply(&self, data: Data) -> Data {
        data.line_to(self.0)
            .cubic_curve_to((self.1, self.2, self.3))
    }
}

/// Calculates the offsets from the corner along each ray of a corner, with the given transverse
/// offsets on each ray. Assumes the input vectors are unit vectors.
///
/// Positive transverse offsets correspond to shifting the ray to the right relative to their
/// respective rays.
///
/// Positive longitudinal offsets correspond to shifting the corner along the respective rays.
#[inline]
pub fn calculate_longitudinal_offsets(
    in_dir: Point,
    out_dir: Point,
    transverse_in: f64,
    transverse_out: f64,
) -> (f64, f64) {
    // positive if in_dir is clockwise of out_dir
    let cross = out_dir.cross(in_dir);
    let dot = in_dir * out_dir;
    let transverse_in = -transverse_in;
    (
        transverse_in.mul_add(dot, transverse_out) / cross,
        transverse_out.mul_add(dot, transverse_in) / cross,
    )
}

/// Calculate `tan(θ/2)`, where `θ` is the angle formed by the two vectors.
#[inline]
pub fn calculate_tan_half_angle(in_dir: Point, out_dir: Point) -> f64 {
    let magnitude = in_dir.norm() * out_dir.norm();
    let dot = in_dir * out_dir;
    ((magnitude - dot) / (magnitude + dot)).sqrt()
}
