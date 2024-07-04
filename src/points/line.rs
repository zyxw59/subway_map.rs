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

    #[cfg(test)]
    pub fn get_segment_by_index(&self, index: usize) -> Option<&Segment> {
        self.segments.get(index)
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
        mut offset: isize,
        width: f64,
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
            offset = -offset;
        }
        // make immutable again
        let (p1, p2, offset) = (p1, p2, offset);

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
                Some(Segment::new(p1, end_point, offset, width, route_segment))
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
                    Some(Segment::new(start_point, p2, offset, width, route_segment))
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
            self.segments[idx].update(offset, width, route_segment);
            // add an intermediate segment if necessary
            let curr_end = self.segments[idx].end;
            let next_start = self.segments[idx + 1].start;
            if curr_end < next_start {
                let new_seg = Segment::new(curr_end, next_start, offset, width, route_segment);
                self.segments.insert(idx + 1, new_seg);
                idx += 1;
                end += 1;
            }
            idx += 1;
        }
        // update the end segment
        self.segments[end].update(offset, width, route_segment);
    }

    pub fn segments_between(
        &self,
        start: PointInfoLite,
        end: PointInfoLite,
    ) -> (bool, Box<dyn Iterator<Item = &Segment> + '_>) {
        let mut start = self.line_point(start);
        let mut end = self.line_point(end);
        let reverse = start > end;
        if reverse {
            std::mem::swap(&mut start, &mut end);
        }
        // make immutable again
        let (start, end) = (start, end);
        // this is only called with points forming a segment which has been added, so they must
        // match some segment.
        let mut start_seg = self
            .segments
            .binary_search_by(|seg| seg.cmp(&start))
            .unwrap();
        if self.segments[start_seg].end == start {
            // we matched the end of the previous segment
            start_seg += 1;
        }
        let end_seg = self
            .segments
            .binary_search_by(|seg| seg.cmp(&end))
            .unwrap_or_else(|err| err)
            - 1;
        if reverse {
            (
                reverse,
                Box::new(self.segments[start_seg..=end_seg].iter().rev()),
            )
        } else {
            (reverse, Box::new(self.segments[start_seg..=end_seg].iter()))
        }
    }

    /// Returns the segments on either side of `end`
    pub fn segments_at(
        &self,
        start: PointInfoLite,
        end: PointInfoLite,
    ) -> (bool, &Segment, &Segment) {
        let start = self.line_point(start);
        let end = self.line_point(end);
        let reverse = start > end;
        let idx = self.segments.binary_search_by(|seg| seg.cmp(&end)).unwrap();
        if reverse {
            (reverse, &self.segments[idx], &self.segments[idx - 1])
        } else {
            (reverse, &self.segments[idx - 1], &self.segments[idx])
        }
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

    /// Returns up to two segments. The first segment returned is the one ending at the given
    /// point, if it exists. The second segment returned is the segment starting with or
    /// encompassing the given point, if it exists.
    pub fn get_segments_containing_point(&self, point: PointInfoLite) -> [Option<&Segment>; 2] {
        let point = self.line_point(point);
        let idx = self
            .segments
            .binary_search_by(|seg| seg.cmp(&point))
            .unwrap_or_else(|idx| idx);
        [
            // the first segment returned is the segment ending at `point`, if it exists
            if idx > 0 && self.segments[idx - 1].end == point {
                Some(&self.segments[idx - 1])
            } else {
                None
            },
            // the second segment returned is the segment with
            // `segment.start <= point < segment.end`, if it exists
            if idx < self.segments.len() && self.segments[idx] == point {
                Some(&self.segments[idx])
            } else {
                None
            },
        ]
    }

    /// Returns whether the points are in reversed order relative to the line.
    pub fn are_reversed(&self, a: PointInfoLite, b: PointInfoLite) -> bool {
        let a = self.line_point(a);
        let b = self.line_point(b);
        a > b
    }

    /// True if the line is to the right of the specified point (looking in the direction of
    /// `line.direction`.
    pub fn right_of(&self, point: Point) -> bool {
        self.direction.cross(point - self.origin) < 0.0
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
    pos_offsets: Vec<Option<f64>>,
    neg_offsets: Vec<Option<f64>>,
}

impl Segment {
    pub fn new(
        start: LinePoint,
        end: LinePoint,
        offset: isize,
        width: f64,
        route_segment: RouteSegmentRef,
    ) -> Segment {
        Segment {
            start,
            end,
            routes: HashSet::new(),
            pos_offsets: Vec::new(),
            neg_offsets: Vec::new(),
        }
        .update_new(offset, width, route_segment)
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

    pub fn routes(&self) -> impl Iterator<Item = &RouteSegmentRef> + Clone + '_ {
        self.routes.iter()
    }

    pub fn contains_route(&self, route_segment: &RouteSegmentRef) -> bool {
        self.routes.contains(route_segment)
    }

    /// Calculates the specified offset from the center of the line.
    ///
    /// If `reverse` is set, the offset is calculated as if the line ran in the opposite direction.
    /// Regardless of whether `reverse` is set, the returned value will have the same sign as the
    /// input offset.
    pub fn calculate_offset(&self, offset: isize, reverse: bool, default_width: f64) -> f64 {
        if offset == 0 {
            0.0
        } else {
            let offset = if reverse { -offset } else { offset };
            let zero_offset = self
                .pos_offsets
                .first()
                .copied()
                .flatten()
                .unwrap_or(default_width);
            let value: f64 = if offset >= 0 {
                let offset = offset as usize;
                zero_offset / 2.0
                    + self.pos_offsets[1..offset]
                        .iter()
                        .map(|width| width.unwrap_or(default_width))
                        .sum::<f64>()
                    + self.pos_offsets[offset].unwrap_or(default_width) / 2.0
            } else {
                let offset = !offset as usize;
                -zero_offset / 2.0
                    - self.neg_offsets[..offset]
                        .iter()
                        .map(|width| width.unwrap_or(default_width))
                        .sum::<f64>()
                    - self.neg_offsets[offset].unwrap_or(default_width) / 2.0
            };
            if reverse {
                -value
            } else {
                value
            }
        }
    }

    pub fn calculate_offset_radius(
        &self,
        offset: isize,
        reverse: bool,
        turn_dir: bool,
        default_width: f64,
    ) -> (f64, f64) {
        let offset = self.calculate_offset(offset, reverse, default_width);
        // offset relative to the line
        let abs_offset = if reverse { -offset } else { offset };
        let radius = if turn_dir {
            abs_offset - self.offset_min(default_width)
        } else {
            self.offset_max(default_width) - abs_offset
        };
        (offset, radius.abs())
    }

    fn offset_max(&self, default_width: f64) -> f64 {
        if !self.pos_offsets.is_empty() {
            self.calculate_offset((self.pos_offsets.len() - 1) as isize, false, default_width)
        } else {
            let mut n = 0;
            for w in &self.neg_offsets {
                if w.is_some() {
                    break;
                } else {
                    n += 1;
                }
            }
            f64::from(n) * default_width
        }
    }

    fn offset_min(&self, default_width: f64) -> f64 {
        if !self.neg_offsets.is_empty() {
            self.calculate_offset(!(self.neg_offsets.len() - 1) as isize, false, default_width)
        } else if self.pos_offsets[0].is_none() {
            0.0
        } else {
            let mut n = 0;
            for w in &self.pos_offsets {
                if w.is_some() {
                    break;
                } else {
                    n += 1;
                }
            }
            f64::from(n) * default_width
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

    /// Update the segment with the given `offset` and `width`, returning the new segment
    fn update_new(mut self, offset: isize, width: f64, route_segment: RouteSegmentRef) -> Segment {
        self.update(offset, width, route_segment);
        self
    }

    /// Update the segment with the given `offset` and `width`.
    fn update(&mut self, offset: isize, width: f64, route_segment: RouteSegmentRef) {
        if offset >= 0 {
            let offset = offset as usize;
            if offset >= self.pos_offsets.len() {
                self.pos_offsets.resize_with(offset + 1, Default::default);
            }
            self.pos_offsets[offset] =
                Some(self.pos_offsets[offset].map_or(width, |old| old.max(width)));
        } else {
            let offset = !offset as usize;
            if offset >= self.neg_offsets.len() {
                self.neg_offsets.resize_with(offset + 1, Default::default);
            }
            self.neg_offsets[offset] =
                Some(self.neg_offsets[offset].map_or(width, |old| old.max(width)));
        }
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
