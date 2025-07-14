use std::{cmp::Ordering, collections::BTreeSet, fmt};

use serde::Serialize;

use super::{PointId, PointInfoLite};
use crate::values::{Line, Point};

#[derive(Clone, Debug, Serialize)]
pub struct LineInfo {
    pub value: Line,
    pub id: LineId,
    points: BTreeSet<LinePoint>,
}

impl LineInfo {
    /// Create a new line between the two points.
    pub fn from_pair(id: LineId, p1: PointInfoLite, p2: PointInfoLite) -> Self {
        let mut points = BTreeSet::new();
        points.insert(LinePoint {
            distance: 0.0,
            id: p1.id,
        });
        points.insert(LinePoint {
            distance: 1.0,
            id: p2.id,
        });
        Self {
            id,
            value: Line::between(p1.value, p2.value),
            points,
        }
    }

    /// Create a new line with the given origin and direction.
    pub fn from_origin_direction(id: LineId, origin: PointInfoLite, direction: Point) -> Self {
        let mut points = BTreeSet::new();
        points.insert(LinePoint {
            distance: 0.0,
            id: origin.id,
        });
        Self {
            id,
            value: Line {
                origin: origin.value,
                direction,
            },
            points,
        }
    }

    pub fn points(&self) -> impl DoubleEndedIterator<Item = &LinePoint> + Clone + '_ {
        self.points.iter()
    }

    /// Calculate the distance along the line for the specified point.
    pub fn distance(&self, p: Point) -> f64 {
        self.relative_distance(self.value.origin, p)
    }

    /// Calculate the distance between the two points along the line.
    pub fn relative_distance(&self, p1: Point, p2: Point) -> f64 {
        (p2 - p1) * self.value.direction / self.value.direction.norm2()
    }

    /// Calculate the location of a point a given distance along the line.
    pub fn point(&self, distance: f64) -> Point {
        self.value.direction.mul_add(distance, self.value.origin)
    }

    /// Returns a `LinePoint` corresponding to the given `PointInfoLite`.
    pub fn line_point(&self, point: PointInfoLite) -> LinePoint {
        LinePoint {
            distance: self.distance(point.value),
            id: point.id,
        }
    }

    pub fn add_point(&mut self, point: PointInfoLite) {
        self.points.insert(self.line_point(point));
    }

    /// Registers the given segment with the line.
    pub fn add_segment(
        &mut self,
        p1: PointInfoLite,
        p2: PointInfoLite,
    ) {
        self.points.insert(self.line_point(p1));
        self.points.insert(self.line_point(p2));
    }

    pub fn get_direction(&self, start: PointInfoLite, end: PointInfoLite) -> Option<LineDirection> {
        match self.line_point(start).cmp(&self.line_point(end)) {
            Ordering::Less => Some(LineDirection::Positive),
            Ordering::Equal => None,
            Ordering::Greater => Some(LineDirection::Negative),
        }
    }

    /// Returns the id of the point at the intersection of the two lines, if such a point exists.
    pub fn intersect(&self, other: &Self) -> Option<PointId> {
        // get the distance of the intersection point along this line.
        let distance = self.distance(self.value.intersect(other.value)?);
        // points to construct a range to search in. the `id` values are bogus, but we won't
        // actually be needing to do any equality comparisons.
        let start = LinePoint {
            distance: distance - 1.0,
            id: PointId(0),
        };
        let end = LinePoint {
            distance: distance + 1.0,
            id: PointId(0),
        };
        for line_point in self.points.range(start..end) {
            let point = self.point(line_point.distance);
            let id = line_point.id;
            let distance = other.distance(point);
            let line_point = LinePoint { distance, id };
            if other.points.contains(&line_point) {
                return Some(id);
            }
        }
        None
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum LineDirection {
    Positive,
    Negative,
}

#[derive(Clone, Copy, Debug, Serialize)]
pub struct LinePoint {
    pub distance: f64,
    pub id: PointId,
}

/// `LinePoint`s are equal iff they have the same `id`, regardless of their `distance` value.
impl PartialEq for LinePoint {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for LinePoint {}

/// If two `LinePoint`s have different `id`s, but the same `distance` value, they are ordered by
/// their `id`. This is arbitrary, but necessary for compatibility with `Eq`.
impl Ord for LinePoint {
    fn cmp(&self, other: &Self) -> Ordering {
        if self == other {
            Ordering::Equal
        } else if self.distance < other.distance {
            Ordering::Less
        } else if other.distance < self.distance {
            Ordering::Greater
        } else {
            self.id.cmp(&other.id)
        }
    }
}

impl PartialOrd for LinePoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct LineId(pub usize);

impl fmt::Debug for LineId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // print on a single line even with `#?` formatter
        write!(f, "LineId({})", self.0)
    }
}
