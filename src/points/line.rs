use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashSet},
    fmt,
};

use serde::Serialize;

use super::{PointId, PointInfoLite, RouteSegmentRef};
use crate::values::{intersect, Point};

#[derive(Clone, Debug, Serialize)]
pub struct Line {
    pub direction: Point,
    pub origin: Point,
    points: BTreeSet<LinePoint>,
    segments: Vec<Segment>,
}

impl Line {
    /// Create a new line between the two points.
    pub fn from_pair(p1: PointInfoLite, p2: PointInfoLite) -> Line {
        let mut points = BTreeSet::new();
        points.insert(LinePoint {
            distance: 0.0,
            id: p1.id,
        });
        points.insert(LinePoint {
            distance: 1.0,
            id: p2.id,
        });
        Line {
            direction: p2.value - p1.value,
            origin: p1.value,
            points,
            segments: Vec::new(),
        }
    }

    /// Create a new line with the given origin and direction.
    pub fn from_origin_direction(origin: PointInfoLite, direction: Point) -> Line {
        let mut points = BTreeSet::new();
        points.insert(LinePoint {
            distance: 0.0,
            id: origin.id,
        });
        Line {
            origin: origin.value,
            direction,
            points,
            segments: Vec::new(),
        }
    }

    pub fn points(&self) -> impl DoubleEndedIterator<Item = &LinePoint> + Clone + '_ {
        self.points.iter()
    }

    /// Calculate the distance along the line for the specified point.
    pub fn distance(&self, p: Point) -> f64 {
        self.relative_distance(self.origin, p)
    }

    /// Calculate the distance between the two points along the line.
    pub fn relative_distance(&self, p1: Point, p2: Point) -> f64 {
        (p2 - p1) * self.direction / self.direction.norm2()
    }

    /// Calculate the location of a point a given distance along the line.
    pub fn point(&self, distance: f64) -> Point {
        distance * self.direction + self.origin
    }

    /// Returns a `LinePoint` corresponding to the given `PointInfoLite`.
    pub fn line_point(&self, point: PointInfoLite) -> LinePoint {
        LinePoint {
            distance: self.distance(point.value),
            id: point.id,
        }
    }

    pub fn add_point(&mut self, id: PointId, distance: f64) {
        self.points.insert(LinePoint { id, distance });
    }

    /// Registers the given segment with the line.
    pub fn add_segment(
        &mut self,
        p1: PointInfoLite,
        p2: PointInfoLite,
        route_segment: RouteSegmentRef,
    ) {
        let mut p1 = self.line_point(p1);
        let mut p2 = self.line_point(p2);
        // if the two points are the same, we have nothing to update
        if p1 == p2 {
            return;
        }
        if p1 > p2 {
            // switch the order
            std::mem::swap(&mut p1, &mut p2);
        }
        // make immutable again
        let (p1, p2) = (p1, p2);

        // start_idx points to the segment which contains the start of the new segment, or the
        // segment after the start of the new segment
        let start_idx = self.segments.binary_search_by(|seg| seg.cmp(&p1));
        // end_idx points to the segment which contains the end of the new segment, or the segment
        // after the end of the new segment
        let end_idx = self.segments.binary_search_by(|seg| seg.cmp(&p2));

        let pre_split = match start_idx {
            // split the segment containing p1
            Ok(start) => self.segments[start].split(p1),
            // create a new segment
            Err(start) => {
                // get the start point of the next segment
                let end_point = if let Some(next_seg) = self.segments.get(start) {
                    next_seg.start.min(p2)
                } else {
                    p2
                };
                Some(Segment::new(p1, end_point, route_segment))
            }
        };

        let post_split = match end_idx {
            // split the segment containing p2
            Ok(end) => self.segments[end].split(p2),
            // create a new segment
            Err(end) => {
                // get the end point of the previous segment
                let start_point = if end > 0 {
                    self.segments[end - 1].end.max(p1)
                } else {
                    p1
                };
                // if the start point is equal to p2, we're be creating an empty segment, so return
                // None instead
                if start_point == p2 {
                    None
                } else {
                    Some(Segment::new(start_point, p2, route_segment))
                }
            }
        };

        let mut start = start_idx.unwrap_or_else(|err| err);
        let mut end = end_idx.unwrap_or_else(|err| err);

        // insert the new segments if necessary
        match (pre_split, post_split) {
            (Some(pre_split), Some(mut post_split)) => {
                // both endpoints of the new segment split an existing segment, or fell in a gap.

                // the special cases to consider here are if both endpoints split the same segment
                // or the same gap.
                if pre_split.start == post_split.start && pre_split.end == post_split.end {
                    // if they split the same gap, we will be left with two copies of the same
                    // segment, and we should insert just one of them.
                    assert_eq!(start, end);
                    assert_eq!(pre_split, post_split);
                    self.segments.insert(start, pre_split);
                // `start` and `end` both point to this newly inserted segment.
                } else if pre_split.start == post_split.start {
                    // if they split the same segment, we will be left with two segments which
                    // share a start point but not an end point.
                    // `pre_split` does not include the current offset and route; `post_split`
                    // does.
                    // here we want to insert both of them, but first change the start point of
                    // `post_split` to be `p1` (the end point of `pre_split`
                    assert_eq!(start, end);
                    assert_eq!(pre_split.end, p1);
                    post_split.start = p1;
                    self.segments.insert(end, post_split);
                    self.segments.insert(start, pre_split);
                    // increment `start` and `end` to point at the middle segment
                    start += 1;
                    end += 1;
                } else {
                    // otherwise, `p1` and `p2` split different segments or gaps.
                    self.segments.insert(end, post_split);
                    self.segments.insert(start, pre_split);

                    // if `p1` split a segment, increment `start` to point at the new location of that
                    // segment; otherwise, keep `start` as it is, to point at the newly inserted
                    // segment.
                    if start_idx.is_ok() {
                        start += 1;
                    }
                    // increment `end` to point at the newly inserted segment (which moved by one after
                    // the insertion of `pre_split`)
                    end += 1;
                }
            }
            (Some(pre_split), None) => {
                // `p1` split a segment or fell in a gap; `p2` was at an expressions segment
                // boundary.
                self.segments.insert(start, pre_split);

                // if `p1` split a segment, increment `start` to point at the new location of that
                // segment; otherwise, keep `start` as it is, to point at the newly inserted
                // segment.
                if start_idx.is_ok() {
                    start += 1;
                }

                // `end` pointed to the segment after the ending with `p2`; after the insertion of
                // `pre_split`, it now points to the segment ending with `p2`.
            }
            (None, Some(post_split)) => {
                // `p1` fell on an existing segment boundary; `p2` split a segment or fell in a
                // gap.
                self.segments.insert(end, post_split);
                // `start` points to the segment starting with `p1`

                // `end` points to the newly inserted segment (which ends with `p2`)
            }
            (None, None) => {
                // both `p1` and `p2` fell on existing segment boundaries.
                // `start` points to the segment starting with `p1`.
                // `end` points to the segment after the one ending with `p2`; we know that
                // `end > 0` because `end == 0` would imply that `p2` was at the start of the first
                // segment, which would require either `p1 == p2` (which was checked for at the
                // beginning of the function) or that `p1` fell in the gap before the first segment
                // (which would mean `pre_split` is `Some`). Thus, decrement `end` by 1 to point at
                // the segment ending at `p2`.
                end -= 1;
            }
        }
        // at this point, the new segments created from the start and end segments have been added.
        // `start` points to the segment whose start point is `p1`
        // `end` ponits to the segment whose end point is `p2`
        // `start` and `end` are both in the range 0 <= x < segments.len()

        // update the intermediate segments
        let mut idx = start;
        while idx < end {
            // update this segment
            self.segments[idx].update(route_segment);
            // add an intermediate segment if necessary
            let curr_end = self.segments[idx].end;
            let next_start = self.segments[idx + 1].start;
            if curr_end < next_start {
                let new_seg = Segment::new(curr_end, next_start, route_segment);
                self.segments.insert(idx + 1, new_seg);
                idx += 1;
                end += 1;
            }
            idx += 1;
        }
        // update the end segment
        self.segments[end].update(route_segment);
    }

    /// Returns the segment ending at `end`
    pub fn get_segment(&self, start: PointInfoLite, end: PointInfoLite) -> (bool, &Segment) {
        let start = self.line_point(start);
        let end = self.line_point(end);
        let reverse = start > end;
        let mut idx = self
            .segments
            .binary_search_by(|seg| seg.cmp(&end))
            .unwrap_or_else(|err| err);
        if !reverse {
            idx -= 1
        };
        (reverse, &self.segments[idx])
    }

    /// Returns the id of the point at the intersection of the two lines, if such a point exists.
    pub fn intersect(&self, other: &Line) -> Option<PointId> {
        // get the distance of the intersection point along this line.
        let distance = self.distance(intersect(
            self.origin,
            self.direction,
            other.origin,
            other.direction,
        )?);
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

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Segment {
    pub start: LinePoint,
    pub end: LinePoint,
    routes: HashSet<RouteSegmentRef>,
}

impl Segment {
    pub fn new(start: LinePoint, end: LinePoint, route_segment: RouteSegmentRef) -> Segment {
        Segment {
            start,
            end,
            routes: [route_segment].into_iter().collect(),
        }
    }

    /// Returns the first and last points of the segment. If `reverse` is true, they will be
    /// returned in reverse order.
    pub fn endpoints(&self, reverse: bool) -> (LinePoint, LinePoint) {
        if !reverse {
            (self.start, self.end)
        } else {
            (self.end, self.start)
        }
    }

    /// Split `self`, leaving the post-split segment in `self`'s place, returning the segment to
    /// be inserted before `self`. If the split point is equal to `self.start`, return `None`, as
    /// no new segment needs to be inserted.
    fn split(&mut self, point: LinePoint) -> Option<Segment> {
        if point == self.start {
            None
        } else {
            // the split is in the middle
            let mut pre_split = self.clone();
            pre_split.end = point;
            self.start = point;
            Some(pre_split)
        }
    }

    /// Update the segment with the given `offset` and `width`.
    fn update(&mut self, route_segment: RouteSegmentRef) {
        self.routes.insert(route_segment);
    }

    /// Compare the segment to a point.
    ///
    /// - `Ordering::Less`: The point is at or after the end of the segment.
    /// - `Ordering::Equal`: The point is in the interior of the segment, or at the start of
    ///   segment.
    /// - `Ordering::Greater`: The point is before the start of the segment.
    fn cmp(&self, other: &LinePoint) -> Ordering {
        if self.end <= *other {
            Ordering::Less
        } else if self.start > *other {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    }
}

impl PartialEq<LinePoint> for Segment {
    fn eq(&self, other: &LinePoint) -> bool {
        self.cmp(other) == Ordering::Equal
    }
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
