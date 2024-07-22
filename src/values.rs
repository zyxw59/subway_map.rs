use std::{fmt, ops, result};

use serde::Serialize;
use svg::node::element::path::Parameters;

use crate::{
    error::{MathError, Type},
    evaluator::EvaluationContext,
    points::PointId,
};

pub type Result<T = Value, E = MathError> = result::Result<T, E>;

macro_rules! numeric_fn {
    (($x:ident, $y:ident) => $val:expr) => {
        numeric_fn!(($x, $y) as ($x, $y) => $val)
    };
    ($x:ident => $val:expr) => {
        numeric_fn!(($x) as $x => $val)
    };
    (($ex:expr, $ey:expr) as ($x:ident, $y:ident) => $val:expr) => {
        match ($ex, $ey) {
            (Value::Number($x), Value::Number($y)) => $val,
            (Value::Number(_), y) => Err(MathError::Type(Type::Number, y.into())),
            (x, _) => Err(MathError::Type(Type::Number, x.into())),
        }
    };
    (($ex:expr) as $x:ident => $val:expr) => {
        match $ex {
            Value::Number($x) => $val,
            x => Err(MathError::Type(Type::Number, x.into())),
        }
    };
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub struct Point(pub f64, pub f64);

impl Point {
    pub fn norm(self) -> f64 {
        self.0.hypot(self.1)
    }

    pub fn norm2(self) -> f64 {
        self * self
    }

    /// Returns a unit vector in the direction of the point.
    pub fn unit(self) -> UnitVector {
        UnitVector(self / self.norm())
    }

    /// Rotates `self` 90 degrees clockwise.
    pub fn perp(self) -> Point {
        Point(self.1, -self.0)
    }

    /// Positive if `other` is clockwise of `self`.
    pub fn cross(self, other: Point) -> f64 {
        self.0 * other.1 - self.1 * other.0
    }

    /// Fused multiply-add. Computes `(self * a) + b with only one rounding error, yielding a more
    /// accurate result than an unfused multiply-add.
    pub fn mul_add(self, a: f64, b: Point) -> Point {
        Point(self.0.mul_add(a, b.0), self.1.mul_add(a, b.1))
    }

    /// Constructs a new point equal to `a * self + b * self.perp()`.
    pub fn basis(self, a: f64, b: f64) -> Point {
        self.mul_add(a, b * self.perp())
    }

    /// Constructs a new point with each coordinate being the maximum of the two points.
    pub fn max(self, other: Point) -> Point {
        Point(self.0.max(other.0), self.1.max(other.1))
    }

    /// Constructs a new point with each coordinate being the minimum of the two points.
    pub fn min(self, other: Point) -> Point {
        Point(self.0.min(other.0), self.1.min(other.1))
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:.4},{:.4}", self.0, self.1)
    }
}

impl ops::Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point(self.0 + other.0, self.1 + other.1)
    }
}

impl ops::Sub for Point {
    type Output = Point;

    fn sub(self, other: Point) -> Point {
        Point(self.0 - other.0, self.1 - other.1)
    }
}

impl ops::Neg for Point {
    type Output = Point;

    fn neg(self) -> Point {
        Point(-self.0, -self.1)
    }
}

impl ops::Mul for Point {
    type Output = f64;

    fn mul(self, other: Point) -> f64 {
        self.0 * other.0 + self.1 * other.1
    }
}

impl ops::Mul<f64> for Point {
    type Output = Point;

    fn mul(self, other: f64) -> Point {
        Point(self.0 * other, self.1 * other)
    }
}

impl ops::Mul<Point> for f64 {
    type Output = Point;

    fn mul(self, other: Point) -> Point {
        Point(self * other.0, self * other.1)
    }
}

impl ops::Div<f64> for Point {
    type Output = Point;

    fn div(self, other: f64) -> Point {
        Point(self.0 / other, self.1 / other)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UnitVector(Point);

impl UnitVector {
    pub const NORTH: UnitVector = UnitVector(Point(0.0, -1.0));
    pub const SOUTH: UnitVector = UnitVector(Point(0.0, 1.0));
    pub const EAST: UnitVector = UnitVector(Point(-1.0, 0.0));
    pub const WEST: UnitVector = UnitVector(Point(1.0, 0.0));

    /// Unit vector in the given direction in degrees, with 0 being up the page, and increasing
    /// clockwise.
    pub fn dir(angle: f64) -> UnitVector {
        UnitVector(Point(sin_deg(angle), -cos_deg(angle)))
    }
}

impl ops::Deref for UnitVector {
    type Target = Point;

    fn deref(&self) -> &Point {
        &self.0
    }
}

impl TryFrom<Value> for Point {
    type Error = MathError;

    fn try_from(value: Value) -> Result<Point> {
        match value {
            Value::Point(p, _) => Ok(p),
            _ => Err(MathError::Type(Type::Point, value.into())),
        }
    }
}

impl TryFrom<Value> for (Point, PointProvenance) {
    type Error = MathError;

    fn try_from(value: Value) -> Result<(Point, PointProvenance)> {
        match value {
            Value::Point(point, provenance) => Ok((point, provenance)),
            _ => Err(MathError::Type(Type::Point, value.into())),
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = MathError;

    fn try_from(value: Value) -> Result<f64> {
        match value {
            Value::Number(x) => Ok(x),
            _ => Err(MathError::Type(Type::Number, value.into())),
        }
    }
}

impl TryFrom<&'_ Value> for f64 {
    type Error = MathError;

    fn try_from(value: &Value) -> Result<f64> {
        match value {
            Value::Number(x) => Ok(*x),
            _ => Err(MathError::Type(Type::Number, value.into())),
        }
    }
}

impl TryFrom<Value> for String {
    type Error = MathError;

    fn try_from(value: Value) -> Result<String> {
        match value {
            Value::String(s) => Ok(s),
            _ => Err(MathError::Type(Type::String, value.into())),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a str {
    type Error = MathError;

    fn try_from(value: &'a Value) -> Result<&'a str> {
        match value {
            Value::String(s) => Ok(s),
            _ => Err(MathError::Type(Type::String, value.into())),
        }
    }
}

impl From<Point> for Value {
    fn from(point: Point) -> Value {
        Value::Point(point, PointProvenance::None)
    }
}

impl From<Point> for Parameters {
    fn from(point: Point) -> Parameters {
        Parameters::from((point.0, point.1))
    }
}

/// An enum representing the provenance of a point.
#[derive(Clone, Copy, Debug)]
pub enum PointProvenance {
    /// The point is defined numerically.
    None,
    /// The point is a named point.
    Named(PointId),
    /// The point is the intersection  of two lines.
    Intersection(Option<(PointId, PointId)>, Option<(PointId, PointId)>),
}

impl PointProvenance {
    pub fn line(self, other: PointProvenance) -> Option<(PointId, PointId)> {
        match (self, other) {
            (PointProvenance::Named(id1), PointProvenance::Named(id2)) => Some((id1, id2)),
            _ => None,
        }
    }
}

impl PartialEq for PointProvenance {
    fn eq(&self, other: &Self) -> bool {
        use self::PointProvenance::*;
        match (*self, *other) {
            (Named(id1), Named(id2)) => id1 == id2,
            // we could check intersections for equality, but there's a lot of different
            // combinations to check for, and even that would miss cases where points are defined
            // as the same intersection but with the lines referred to by different points.
            // this is probably a rare enough case that simply handling it with numerical
            // comparison is fine.
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Point(Point, PointProvenance),
    Line(Point, Point, Option<(PointId, PointId)>),
    String(String),
    List(Vec<Value>),
    Function(crate::expressions::Function),
}

impl Value {
    pub fn into_number(self) -> Option<f64> {
        match self {
            Value::Number(x) => Some(x),
            _ => None,
        }
    }

    pub fn point(x: Value, y: Value) -> Result {
        numeric_fn!((x, y) => Ok(Value::Point(Point(x, y), PointProvenance::None)))
    }

    pub fn hypot(self, other: Value) -> Result {
        numeric_fn!((self, other) as (x, y) => Ok(Value::Number(x.hypot(y))))
    }

    pub fn hypot_sub(self, other: Value) -> Result {
        numeric_fn!((self, other) as (x, y) => {
            let x = x.abs();
            let y = y.abs();
            if y > x {
                Err(MathError::Domain(format!("{x} +-+ {y}")))
            } else {
                Ok(Value::Number((x*x - y*y).sqrt()))
            }
        })
    }

    pub fn pow(self, other: Value) -> Result {
        numeric_fn!((self, other) as (x, y) => Ok(Value::Number(x.powf(y))))
    }

    /// Cosine of the number in degrees
    pub fn cos(self) -> Result {
        numeric_fn!((self) as x => Ok(Value::Number(cos_deg(x))))
    }

    /// Sine of the number in degrees
    pub fn sin(self) -> Result {
        numeric_fn!((self) as x => Ok(Value::Number(sin_deg(x))))
    }

    /// Unit vector in the given direction in degrees, with 0 being up the page, and increasing
    /// clockwise.
    pub fn dir(self) -> Result {
        numeric_fn!((self) as x => Ok(Value::Point(*UnitVector::dir(x), PointProvenance::None)))
    }

    /// Angle of given vector in degrees
    pub fn angle(self) -> Result {
        match self {
            Value::Point(Point(x, y), _) => Ok(Value::Number(y.atan2(x).to_degrees())),
            x => Err(MathError::Type(Type::Point, x.into())),
        }
    }

    /// x value of the given point
    pub fn xpart(self) -> Result {
        match self {
            Value::Point(Point(x, _), _) => Ok(Value::Number(x)),
            x => Err(MathError::Type(Type::Point, x.into())),
        }
    }

    /// y value of the given point
    pub fn ypart(self) -> Result {
        match self {
            Value::Point(Point(_, y), _) => Ok(Value::Number(y)),
            x => Err(MathError::Type(Type::Point, x.into())),
        }
    }

    /// Line between two points
    pub fn line_between(self, rhs: Value) -> Result {
        match (&self, rhs) {
            (&Value::Point(p1, id1), Value::Point(p2, id2)) => {
                Ok(Value::Line(p1, p2 - p1, id1.line(id2)))
            }
            _ => Err(MathError::Type(Type::Point, self.into())),
        }
    }

    /// Line from point and vector
    pub fn line_vector(self, rhs: Value) -> Result {
        match (&self, rhs) {
            (&Value::Point(p1, _), Value::Point(p2, _)) => Ok(Value::Line(p1, p2, None)),
            _ => Err(MathError::Type(Type::Point, self.into())),
        }
    }

    pub fn intersect(self, rhs: Value) -> Result {
        match (&self, rhs) {
            (&Value::Line(p1, d1, ids1), Value::Line(p2, d2, ids2)) => {
                match intersect(p1, d1, p2, d2) {
                    Some(intersection) => Ok(Value::Point(
                        intersection,
                        PointProvenance::Intersection(ids1, ids2),
                    )),
                    None => Err(MathError::ParallelIntersection),
                }
            }
            _ => Err(MathError::Type(Type::Line, self.into())),
        }
    }

    pub fn line_offset(self, rhs: Value) -> Result {
        match (self, rhs) {
            (Value::Line(p, d, _), Value::Number(dist)) => {
                Ok(Value::Line(d.unit().perp().mul_add(dist, p), d, None))
            }
            (Value::Line(..), rhs) => Err(MathError::Type(Type::Number, rhs.into())),
            (lhs, _) => Err(MathError::Type(Type::Number, lhs.into())),
        }
    }

    fn eq_bool(&self, other: &Value) -> Result<bool> {
        match (self, other) {
            (&Value::Number(x), &Value::Number(y)) => Ok(float_eq(x, y)),
            (&Value::Point(p1, id1), &Value::Point(p2, id2)) => {
                Ok(id1 == id2 || point_float_eq(p1, p2))
            }
            (&Value::Line(p1, d1, ids1), &Value::Line(p2, d2, ids2)) => Ok(ids1 == ids2
                || vector_parallel_float_eq(d1, d2) && vector_parallel_float_eq(p2 - p1, d1)),
            (Value::String(s1), Value::String(s2)) => Ok(s1 == s2),
            _ => Err(MathError::Type(self.into(), other.into())),
        }
    }

    pub fn eq(self, other: Value) -> Result {
        self.eq_bool(&other).map(Value::from)
    }

    pub fn ne(self, other: Value) -> Result {
        self.eq_bool(&other).map(|x| Value::from(!x))
    }

    pub fn lt(self, other: Value) -> Result {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::from(x < y)),
            (Value::String(x), Value::String(y)) => Ok(Value::from(x < y)),
            (bad, good @ (Value::Number(..) | Value::String(..)))
            | (good @ (Value::Number(..) | Value::String(..)), bad) => {
                Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => Err(MathError::Type(Type::Number, bad.into())),
        }
    }

    pub fn le(self, other: Value) -> Result {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::from(x <= y)),
            (Value::String(x), Value::String(y)) => Ok(Value::from(x <= y)),
            (bad, good @ (Value::Number(..) | Value::String(..)))
            | (good @ (Value::Number(..) | Value::String(..)), bad) => {
                Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => Err(MathError::Type(Type::Number, bad.into())),
        }
    }

    pub fn gt(self, other: Value) -> Result {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::from(x > y)),
            (Value::String(x), Value::String(y)) => Ok(Value::from(x > y)),
            (bad, good @ (Value::Number(..) | Value::String(..)))
            | (good @ (Value::Number(..) | Value::String(..)), bad) => {
                Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => Err(MathError::Type(Type::Number, bad.into())),
        }
    }

    pub fn ge(self, other: Value) -> Result {
        match (self, other) {
            (Value::Number(x), Value::Number(y)) => Ok(Value::from(x >= y)),
            (Value::String(x), Value::String(y)) => Ok(Value::from(x >= y)),
            (bad, good @ (Value::Number(..) | Value::String(..)))
            | (good @ (Value::Number(..) | Value::String(..)), bad) => {
                Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => Err(MathError::Type(Type::Number, bad.into())),
        }
    }

    pub fn max(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Number(x.max(y))),
            (Point(p1, _), Point(p2, _)) => Ok(Point(p1.max(p2), PointProvenance::None)),
            (String(x), String(y)) => Ok(String(x.max(y))),
            (bad, good @ (Number(..) | Point(..) | String(..)))
            | (good @ (Number(..) | Point(..) | String(..)), bad) => {
                Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => Err(MathError::Type(Type::Number, bad.into())),
        }
    }

    pub fn min(self, other: Value) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Number(x.min(y))),
            (Point(p1, _), Point(p2, _)) => Ok(Point(p1.min(p2), PointProvenance::None)),
            (String(x), String(y)) => Ok(String(x.min(y))),
            (bad, good @ (Number(..) | Point(..) | String(..)))
            | (good @ (Number(..) | Point(..) | String(..)), bad) => {
                Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => Err(MathError::Type(Type::Number, bad.into())),
        }
    }

    pub fn comma(self, other: Value) -> Result {
        Ok(match self {
            Self::List(mut values) => {
                values.push(other);
                Self::List(values)
            }
            _ => Self::List(vec![self, other]),
        })
    }

    pub fn comma_unary(self) -> Result {
        Ok(self)
    }

    pub fn paren_unary(self) -> Result {
        match self {
            Self::List(mut values) => {
                match <[_; 1]>::try_from(values) {
                    Ok([value]) => return Ok(value),
                    Err(err) => values = err,
                }
                match <[_; 2]>::try_from(values) {
                    Ok([x, y]) => Self::point(x, y),
                    Err(_err) => todo!(),
                }
            }
            _ => Ok(self),
        }
    }

    pub fn add(self, rhs: Value) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a + b),
            (Point(p1, _), Point(p2, _)) => Point(p1 + p2, PointProvenance::None),
            (String(s1), String(s2)) => String(s1 + &s2),
            (bad, good @ (Number(..) | Point(..) | String(..)))
            | (good @ (Number(..) | Point(..) | String(..)), bad) => {
                return Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => return Err(MathError::Type(Type::Number, bad.into())),
        })
    }

    pub fn sub(self, rhs: Value) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a - b),
            (Point(p1, _), Point(p2, _)) => Point(p1 - p2, PointProvenance::None),
            (bad, good @ (Number(..) | Point(..))) | (good @ (Number(..) | Point(..)), bad) => {
                return Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => return Err(MathError::Type(Type::Number, bad.into())),
        })
    }

    pub fn mul(self, rhs: Value) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a * b),
            (Number(a), Point(p, _)) => Point(a * p, PointProvenance::None),
            (Point(p, _), Number(a)) => Point(p * a, PointProvenance::None),
            (Point(p1, _), Point(p2, _)) => Number(p1 * p2),
            (bad, good @ (Number(..) | Point(..))) | (good @ (Number(..) | Point(..)), bad) => {
                return Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => return Err(MathError::Type(Type::Number, bad.into())),
        })
    }

    pub fn div(self, rhs: Value) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (_, Number(x)) if x == 0.0 => return Err(MathError::DivisionByZero),
            (Number(a), Number(b)) => Number(a / b),
            (Point(p, _), Number(a)) => Point(p / a, PointProvenance::None),
            (bad, Number(..)) | (_, bad) => return Err(MathError::Type(Type::Number, bad.into())),
        })
    }

    pub fn neg(self) -> Result {
        use self::Value::*;
        Ok(match self {
            Number(x) => Number(-x),
            Point(p, _) => Point(-p, PointProvenance::None),
            _ => return Err(MathError::Type(Type::Number, self.into())),
        })
    }

    pub fn fn_call(self, args: Self, ctx: &dyn EvaluationContext) -> Result {
        match (self, args) {
            (Self::Function(f), Self::List(args)) => f.apply(args, ctx),
            (Self::Function(f), arg) => f.apply(vec![arg], ctx),
            (bad, _) => Err(MathError::Type(Type::Function, bad.into())),
        }
    }

    pub fn fn_call_unary(self, ctx: &dyn EvaluationContext) -> Result {
        match self {
            Self::Function(f) => f.apply(Vec::new(), ctx),
            bad => Err(MathError::Type(Type::Function, bad.into())),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        self.eq_bool(other).unwrap_or(false)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Number(f64::from(b as u8))
    }
}

fn cos_deg(x: f64) -> f64 {
    // since cos is even, we can take |x|, which guarantees that |x| % 360 is nonnegative
    let x = x.abs() % 360.0;
    if float_eq(x, 0.0) {
        1.0
    } else if float_eq(x, 180.0) {
        -1.0
    } else if float_eq(x, 90.0) || float_eq(x, 270.0) {
        0.0
    } else {
        x.to_radians().cos()
    }
}

fn sin_deg(x: f64) -> f64 {
    let x = x % 360.0;
    if float_eq(x, 0.0) || float_eq(x, 180.0) || float_eq(x, -180.0) {
        0.0
    } else if float_eq(x, 90.0) || float_eq(x, -270.0) {
        1.0
    } else if float_eq(x, -90.0) || float_eq(x, 270.0) {
        -1.0
    } else {
        x.to_radians().sin()
    }
}

pub fn intersect(p1: Point, d1: Point, p2: Point, d2: Point) -> Option<Point> {
    let cross = d1.cross(d2);
    // if `cross ~= 0`, the lines are (approximately) parallel; in this case there is no
    // intersection.
    if cross.abs() < f64::EPSILON {
        None
    } else {
        Some(d1.mul_add((p2 - p1).cross(d2) / cross, p1))
    }
}

pub fn float_eq(x: f64, y: f64) -> bool {
    float_eq::float_eq!(x, y, rmax <= f64::EPSILON)
}

pub fn point_float_eq(p1: Point, p2: Point) -> bool {
    float_eq((p1 - p2).norm(), 0.0)
}

/// Returns true if the two vectors are parallel or antiparallel
fn vector_parallel_float_eq(v1: Point, v2: Point) -> bool {
    float_eq(v1.cross(v2), 0.0)
}

#[cfg(test)]
mod tests {
    // TODO: these tests probably want to live in `evaluator.rs`
    macro_rules! assert_eval {
        (($($expr:tt)+), ($($val:tt)*)) => {{
            let expr = expression!($($expr)+);
            assert_eq!(expr.evaluate(&()).unwrap(), value!($($val)*));
        }};
        (($($expr:tt)+), $($val:tt)*) => {{
            let expr = expression!($($expr)+);
            assert_eq!(expr.evaluate(&()).unwrap(), value!($($val)*));
        }};
    }

    #[test]
    fn basic_arithmetic() {
        // 1 + 2 * 3 + 4 == 11
        assert_eval!(("+", ("+", 1, ("*", 2, 3)), 4), 11)
    }

    #[test]
    fn basic_arithmetic_2() {
        // 1 - 2 * 3 + 4 == -1
        assert_eval!(("+", ("-", 1, ("*", 2, 3)), 4), -1);
    }

    #[test]
    fn basic_arithmetic_3() {
        // 1 - 3 / 2 * 5 == -6.5
        assert_eval!(("-", 1, ("*", ("/", 3, 2), 5)), -6.5);
    }

    #[test]
    fn hypot() {
        // 3 ++ 4 == 5
        assert_eval!(("++", 3, 4), 5);
    }

    #[test]
    fn hypot_sub() {
        // 5 +-+ 3 == 4
        assert_eval!(("+-+", 5, 3), 4);
    }

    #[test]
    fn pow() {
        // 3 ^ 4 == 81
        assert_eval!(("^", 3, 4), 81);
    }

    #[test]
    fn points() {
        // (1, 2) + (3, 4) == (4, 6)
        assert_eval!(("+", (@1, 2), (@3, 4)), (4, 6));
    }

    #[test]
    fn dot_product() {
        // (1, 2) * (3, 4) == 11
        assert_eval!(("*", (@1, 2), (@3, 4)), 11);
    }

    #[test]
    fn scalar_product() {
        // 3 * (1, 2) == (3, 6)
        assert_eval!(("*", 3, (@1, 2)), (3, 6));
    }

    #[test]
    fn angle() {
        // angle (3, 3) == 45
        assert_eval!(("angle", (@3, 3)), 45);
    }

    #[test]
    fn unary_minus() {
        // 3 * -2 == -6
        assert_eval!(("*", 3, ("-", 2)), -6);
    }

    #[test]
    fn unary_minus_2() {
        // -2 * 3 == -6
        assert_eval!(("*", ("-", 2), 3), -6);
    }

    #[test]
    fn unary_minus_3() {
        // -(1, 2) * (3, 4) == -11
        assert_eval!(("-", ("*", (@1, 2), (@3, 4))), -11);
    }

    #[test]
    fn unary_cos() {
        // cos 90 == 0
        assert_eval!(("cos", 90), 0);
    }

    #[test]
    fn unary_sin() {
        // sin 90 == 1
        assert_eval!(("sin", 90), 1);
    }

    #[test]
    fn intersect() {
        assert_eval!(("&", ("<>", (@1, 2), (@3, 4)), ("<>", (@1, 4), (@3, 2))), (2, 3))
    }

    #[test]
    fn offset() {
        assert_eval!(("^^", ("<>", (@0, 0), (@3, 4)), 5), ((4, -3) -> (7, 1)))
    }

    #[test]
    fn min() {
        // (2, 4) min (3, 1) == (2, 1)
        assert_eval!(("min", (@2, 4), (@3, 1)), (2, 1))
    }

    #[test]
    fn string_concat() {
        assert_eval!(("+", (@"foo"), (@"bar")), @"foobar");
    }

    #[test]
    fn string_max() {
        assert_eval!(("max", (@"a"), (@"b")), @"b");
    }
}
