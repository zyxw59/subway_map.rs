use std::{
    collections::{hash_map::Entry, HashMap},
    fmt, ops,
    rc::Rc,
    result,
};

use serde::Serialize;
use svg::node::element::path::Parameters;

use crate::{
    error::{MathError, Type},
    evaluator::EvaluationContext,
    expressions::Variable,
    points::{LineId, PointId},
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
        Point(-self.1, self.0)
    }

    pub fn dot(self, other: Point) -> f64 {
        self * other
    }

    /// Positive if `self` is clockwise of `other`.
    /// Equal to `self.dot(other.perp())`
    pub fn cross(self, other: Point) -> f64 {
        self.dot(other.perp())
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

#[derive(Clone, Copy, Debug, serde::Serialize)]
#[serde(transparent)]
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

impl ops::Neg for UnitVector {
    type Output = Self;

    fn neg(self) -> Self {
        Self(-self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub struct Line {
    pub origin: Point,
    pub direction: Point,
}

impl Line {
    pub fn between(p1: Point, p2: Point) -> Self {
        Self {
            origin: p1,
            direction: p2 - p1,
        }
    }

    pub fn intersect(self, other: Self) -> Option<Point> {
        let cross = self.direction.cross(other.direction);
        // if `cross ~= 0`, the lines are (approximately) parallel; in this case there is no
        // intersection.
        if cross.abs() < f64::EPSILON {
            None
        } else {
            Some(self.direction.mul_add(
                (other.origin - self.origin).cross(other.direction) / cross,
                self.origin,
            ))
        }
    }

    fn float_eq(self, other: Self) -> bool {
        vector_parallel_float_eq(self.direction, other.direction)
            && vector_parallel_float_eq(other.origin - self.origin, self.direction)
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

impl TryFrom<Value> for (Point, PointId) {
    type Error = MathError;

    fn try_from(value: Value) -> Result<(Point, PointId)> {
        match value {
            Value::Point(point, id) => Ok((point, id)),
            _ => Err(MathError::Type(Type::Point, value.into())),
        }
    }
}

impl TryFrom<Value> for (Line, LineId) {
    type Error = MathError;

    fn try_from(value: Value) -> Result<(Line, LineId)> {
        match value {
            Value::Line(line, id) => Ok((line, id)),
            _ => Err(MathError::Type(Type::Line, value.into())),
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

impl<'a> TryFrom<&'a Value> for &'a str {
    type Error = MathError;

    fn try_from(value: &'a Value) -> Result<&'a str> {
        match value {
            Value::String(s) => Ok(s),
            _ => Err(MathError::Type(Type::String, value.into())),
        }
    }
}

impl From<Point> for Parameters {
    fn from(point: Point) -> Parameters {
        Parameters::from((point.0, point.1))
    }
}

#[derive(Clone, Debug, serde::Serialize)]
pub enum Value {
    Number(f64),
    Point(Point, PointId),
    Line(Line, LineId),
    String(Rc<String>),
    List(Rc<Vec<Value>>),
    Struct(Rc<HashMap<Variable, Value>>),
    Function(#[serde(skip)] crate::expressions::Function),
}

impl Value {
    pub fn into_number(self) -> Option<f64> {
        match self {
            Value::Number(x) => Some(x),
            _ => None,
        }
    }

    pub fn new_struct() -> Self {
        Self::Struct(Default::default())
    }

    pub fn slot(&mut self, field: Variable) -> Result<Entry<Variable, Value>> {
        match self {
            Self::Struct(fields) => Ok(Rc::make_mut(fields).entry(field)),
            bad => Err(MathError::Type(Type::Struct, (&*bad).into())),
        }
    }

    pub fn point(x: Value, y: Value, ctx: &mut dyn EvaluationContext) -> Result {
        numeric_fn!((x, y) => {
            Ok(Self::make_point(Point(x, y), ctx))
        })
    }

    fn make_point(p: Point, ctx: &mut dyn EvaluationContext) -> Self {
        let id = ctx.point_collection().insert_point_get_id(p);
        Self::Point(p, id)
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
    pub fn dir(self, ctx: &mut dyn EvaluationContext) -> Result {
        numeric_fn!((self) as x => {
            let p = UnitVector::dir(x).0;
            Ok(Self::make_point(p, ctx))
        })
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
    pub fn line_between(self, rhs: Value, ctx: &mut dyn EvaluationContext) -> Result {
        match (&self, rhs) {
            (&Value::Point(p1, id1), Value::Point(p2, id2)) => {
                let line_id = ctx.point_collection().get_or_insert_line(id1, id2);
                Ok(Value::Line(Line::between(p1, p2), line_id))
            }
            _ => Err(MathError::Type(Type::Point, self.into())),
        }
    }

    /// Line from point and vector
    pub fn line_vector(self, rhs: Value, ctx: &mut dyn EvaluationContext) -> Result {
        match (&self, rhs) {
            (&Value::Point(origin, origin_id), Value::Point(direction, _)) => {
                let line_id = ctx.point_collection().new_line(origin_id, direction);
                Ok(Value::Line(Line { origin, direction }, line_id))
            }
            _ => Err(MathError::Type(Type::Point, self.into())),
        }
    }

    pub fn intersect(self, rhs: Value, ctx: &mut dyn EvaluationContext) -> Result {
        match (&self, rhs) {
            (&Value::Line(l1, id1), Value::Line(l2, id2)) => match l1.intersect(l2) {
                Some(intersection) => {
                    let point_id = ctx.point_collection().intersect(id1, id2, intersection);
                    Ok(Value::Point(intersection, point_id))
                }
                None => Err(MathError::ParallelIntersection),
            },
            _ => Err(MathError::Type(Type::Line, self.into())),
        }
    }

    pub fn line_offset(self, rhs: Value, ctx: &mut dyn EvaluationContext) -> Result {
        match (self, rhs) {
            (Value::Line(line, _), Value::Number(dist)) => {
                let direction = line.direction;
                let origin = direction.unit().perp().mul_add(dist, line.origin);
                let origin_id = ctx.point_collection().insert_point_get_id(origin);
                let line_id = ctx.point_collection().new_line(origin_id, direction);
                Ok(Value::Line(Line { origin, direction }, line_id))
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
            (&Value::Line(l1, id1), &Value::Line(l2, id2)) => Ok(id1 == id2 || l1.float_eq(l2)),
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

    pub fn max(self, other: Value, ctx: &mut dyn EvaluationContext) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Number(x.max(y))),
            (Point(p1, _), Point(p2, _)) => Ok(Self::make_point(p1.max(p2), ctx)),
            (String(x), String(y)) => Ok(String(x.max(y))),
            (bad, good @ (Number(..) | Point(..) | String(..)))
            | (good @ (Number(..) | Point(..) | String(..)), bad) => {
                Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => Err(MathError::Type(Type::Number, bad.into())),
        }
    }

    pub fn min(self, other: Value, ctx: &mut dyn EvaluationContext) -> Result {
        use self::Value::*;
        match (self, other) {
            (Number(x), Number(y)) => Ok(Number(x.min(y))),
            (Point(p1, _), Point(p2, _)) => Ok(Self::make_point(p1.min(p2), ctx)),
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
                Rc::make_mut(&mut values).push(other);
                Self::List(values)
            }
            _ => Self::List(Rc::new(vec![self, other])),
        })
    }

    pub fn comma_unary(self) -> Result {
        Ok(self)
    }

    pub fn paren_unary(self, ctx: &mut dyn EvaluationContext) -> Result {
        match self {
            Self::List(values) => match &**values {
                [value] => Ok(value.clone()),
                [x, y] => Self::point(x.clone(), y.clone(), ctx),
                _ => Err(MathError::Arguments {
                    expected: 2,
                    actual: values.len(),
                }),
            },
            _ => Ok(self),
        }
    }

    pub fn add(self, rhs: Value, ctx: &mut dyn EvaluationContext) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a + b),
            (Point(p1, _), Point(p2, _)) => Self::make_point(p1 + p2, ctx),
            (String(s1), String(s2)) => String(Rc::new(Rc::unwrap_or_clone(s1) + &s2)),
            (bad, good @ (Number(..) | Point(..) | String(..)))
            | (good @ (Number(..) | Point(..) | String(..)), bad) => {
                return Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => return Err(MathError::Type(Type::Number, bad.into())),
        })
    }

    pub fn sub(self, rhs: Value, ctx: &mut dyn EvaluationContext) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a - b),
            (Point(p1, _), Point(p2, _)) => Self::make_point(p1 - p2, ctx),
            (bad, good @ (Number(..) | Point(..))) | (good @ (Number(..) | Point(..)), bad) => {
                return Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => return Err(MathError::Type(Type::Number, bad.into())),
        })
    }

    pub fn mul(self, rhs: Value, ctx: &mut dyn EvaluationContext) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (Number(a), Number(b)) => Number(a * b),
            (Number(a), Point(p, _)) => Self::make_point(a * p, ctx),
            (Point(p, _), Number(a)) => Self::make_point(p * a, ctx),
            (Point(p1, _), Point(p2, _)) => Number(p1 * p2),
            (bad, good @ (Number(..) | Point(..))) | (good @ (Number(..) | Point(..)), bad) => {
                return Err(MathError::Type(good.into(), bad.into()))
            }
            (bad, _) => return Err(MathError::Type(Type::Number, bad.into())),
        })
    }

    pub fn div(self, rhs: Value, ctx: &mut dyn EvaluationContext) -> Result {
        use self::Value::*;
        Ok(match (self, rhs) {
            (_, Number(0.0)) => return Err(MathError::DivisionByZero),
            (Number(a), Number(b)) => Number(a / b),
            (Point(p, _), Number(a)) => Self::make_point(p / a, ctx),
            (bad, Number(..)) | (_, bad) => return Err(MathError::Type(Type::Number, bad.into())),
        })
    }

    pub fn neg(self, ctx: &mut dyn EvaluationContext) -> Result {
        use self::Value::*;
        Ok(match self {
            Number(x) => Number(-x),
            Point(p, _) => Self::make_point(-p, ctx),
            _ => return Err(MathError::Type(Type::Number, self.into())),
        })
    }

    pub fn fn_call(self, args: Self, ctx: &mut dyn EvaluationContext) -> Result {
        match (self, args) {
            (Self::Function(f), Self::List(args)) => f.apply(&args, ctx),
            (Self::Function(f), arg) => f.apply(&[arg], ctx),
            (bad, _) => Err(MathError::Type(Type::Function, bad.into())),
        }
    }

    pub fn fn_call_unary(self, ctx: &mut dyn EvaluationContext) -> Result {
        match self {
            Self::Function(f) => f.apply(&[], ctx),
            bad => Err(MathError::Type(Type::Function, bad.into())),
        }
    }

    pub fn field_access(self, name: Variable) -> Result {
        match self {
            Self::Struct(fields) => fields.get(&name).cloned().ok_or(MathError::Field(name)),
            bad => Err(MathError::Type(Type::Struct, bad.into())),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        self.eq_bool(other).unwrap_or(false)
    }
}

impl PartialEq<f64> for Value {
    fn eq(&self, other: &f64) -> bool {
        if let Self::Number(this) = self {
            float_eq(*this, *other)
        } else {
            false
        }
    }
}

impl PartialEq<Point> for Value {
    fn eq(&self, other: &Point) -> bool {
        if let Self::Point(this, _) = self {
            point_float_eq(*this, *other)
        } else {
            false
        }
    }
}

impl PartialEq<&str> for Value {
    fn eq(&self, other: &&str) -> bool {
        if let Self::String(this) = self {
            **this == *other
        } else {
            false
        }
    }
}

impl PartialEq<Line> for Value {
    fn eq(&self, other: &Line) -> bool {
        if let Self::Line(this, _) = self {
            this.float_eq(*other)
        } else {
            false
        }
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
pub(crate) mod tests {
    use test_case::test_case;

    use crate::{
        evaluator::{evaluate_expression, Evaluator},
        expressions::tests::{b, expression_full, t, u, Expr},
        operators::{COMMA, PAREN_UNARY},
        values::{Line, Point, Value},
    };

    // 1 + 2 * 3 + 4 == 11
    #[test_case([t(1), t(2), t(3), b("*"), b("+"), t(4), b("+")], 11.0; "basic arithmetic")]
    // 1 - 2 * 3 + 4 == -1
    #[test_case([t(1), t(2), t(3), b("*"), b("-"), t(4), b("+")], -1.0; "basic arithmetic 2")]
    // 1 - 3 / 2 * 5 == -6.5
    #[test_case([t(1), t(3), t(2), b("/"), t(5), b("*"), b("-")], -6.5; "basic arithmetic 3")]
    // 3 ++ 4 == 5
    #[test_case([t(3), t(4), b("++")], 5.0; "hypot")]
    // 5 +-+ 3 == 4
    #[test_case([t(5), t(3), b("+-+")], 4.0; "hypot sub")]
    // 3 ^ 4 == 81
    #[test_case([t(3), t(4), b("^")], 81.0; "pow")]
    // (1, 2) + (3, 4) == (4, 6)
    #[test_case([t(1), t(2), b(COMMA), u(PAREN_UNARY), t(3), t(4), b(COMMA), u(PAREN_UNARY), b("+")], Point(4.0, 6.0); "vector addition")]
    // (1, 2) * (3, 4) == 11
    #[test_case([t(1), t(2), b(COMMA), u(PAREN_UNARY), t(3), t(4), b(COMMA), u(PAREN_UNARY), b("*")], 11.0; "dot product")]
    // 3 * (1, 2) == (3, 6)
    #[test_case([t(3), t(1), t(2), b(COMMA), u(PAREN_UNARY), b("*")], Point(3.0, 6.0); "scalar product")]
    // angle (3, 3) == 45
    #[test_case([t(3), t(3), b(COMMA), u(PAREN_UNARY), u("angle")], 45.0; "angle")]
    // 3 * - 2 == -6
    #[test_case([t(3), t(2), u("-"), b("*")], -6.0; "unary minus")]
    // - 2 * 3 == -6
    #[test_case([t(2), u("-"), t(3), b("*")], -6.0; "unary minus 2")]
    // -(1, 2) * (3, 4) == -11
    #[test_case([t(1), t(2), b(COMMA), u(PAREN_UNARY), u("-"), t(3), t(4), b(COMMA), u(PAREN_UNARY), b("*")], -11.0; "unary minus 3")]
    // cos 90 == 0
    #[test_case([t(90), u("cos")], 0.0; "unary cos")]
    // sin 90 == 1
    #[test_case([t(90), u("sin")], 1.0; "unary sin")]
    // (1, 2) <> (3, 4) & (1, 4) <> (3, 2) == (2, 3)
    #[test_case([
        t(1), t(2), b(COMMA), u(PAREN_UNARY), t(3), t(4), b(COMMA), u(PAREN_UNARY), b("<>"),
        t(1), t(4), b(COMMA), u(PAREN_UNARY), t(3), t(2), b(COMMA), u(PAREN_UNARY), b("<>"), b("&")
    ], Point(2.0, 3.0); "intersect")]
    // (0, 0) <> (3, 4) ^^ 5 == (4, -3) -> (7, 1)
    #[test_case([
        t(0), t(0), b(COMMA), u(PAREN_UNARY), t(3), t(4), b(COMMA), u(PAREN_UNARY), b("<>"),
        t(5), b("^^")
    ], Line::between(Point(-4.0, 3.0), Point(-1.0, 7.0)); "offset")]
    // (2, 4) min (3, 1) == (2, 1)
    #[test_case([t(2), t(4), b(COMMA), u(PAREN_UNARY), t(3), t(1), b(COMMA), u(PAREN_UNARY), b("min")], Point(2.0, 1.0); "min")]
    // "foo" + "bar" == "foobar"
    #[test_case([t("foo"), t("bar"), b("+")], "foobar"; "string concat")]
    // "a" max "b" == "b"
    #[test_case([t("a"), t("b"), b("max")], "b"; "string max")]
    fn eval<T: std::fmt::Debug, const N: usize>(expression: [Expr; N], expected: T)
    where
        Value: PartialEq<T>,
    {
        assert_eq!(
            evaluate_expression(&mut Evaluator::default(), expression_full(expression)).unwrap(),
            expected
        );
    }
}
