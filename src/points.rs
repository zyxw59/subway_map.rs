use std::{
    collections::{btree_map::Entry, BTreeMap, HashMap, HashSet},
    fmt,
    ops::{Index, IndexMut},
};

use itertools::Itertools;
use serde::Serialize;

use crate::{
    corner::{calculate_longitudinal_offsets, calculate_tan_half_angle},
    error::{Error, Result},
    expressions::Variable,
    intermediate_representation::{Operation, OperationKind, Path},
    values::Point,
};

mod line;
mod route_corner;

pub use line::{LineId, LineInfo};
use route_corner::{RouteCorners, RouteTurn};

#[derive(Debug, Serialize)]
pub struct PointCollection {
    points: Vec<PointInfo>,
    lines: Vec<LineInfo>,
    #[serde(skip)]
    pairs: HashMap<(PointId, PointId), LineId>,
    routes: Vec<Route>,
    route_ids: BTreeMap<Variable, RouteId>,
    default_width: f64,
    inner_radius: f64,
}

impl Default for PointCollection {
    fn default() -> Self {
        Self {
            points: Default::default(),
            lines: Default::default(),
            pairs: Default::default(),
            routes: Default::default(),
            route_ids: Default::default(),
            default_width: 1.0,
            inner_radius: 0.0,
        }
    }
}

impl PointCollection {
    pub fn set_default_width(&mut self, default_width: f64) {
        self.default_width = default_width;
    }

    pub fn set_inner_radius(&mut self, inner_radius: f64) {
        self.inner_radius = inner_radius;
    }

    /// Inserts a new, empty route, and returns the id of that route; or returns an error if a
    /// route with that name has already been defined.
    pub fn insert_route_get_id(
        &mut self,
        name: Variable,
        styles: Vec<Variable>,
        line_number: usize,
    ) -> Result<RouteId> {
        match self.route_ids.entry(name) {
            Entry::Occupied(e) => {
                let &id = e.get();
                Err(Error::RouteRedefinition {
                    name: e.key().clone(),
                    line: line_number,
                    original_line: self[id].line_number,
                })
            }
            Entry::Vacant(e) => {
                let id = RouteId(self.routes.len());
                self.routes
                    .push(Route::new(e.key().clone(), id, styles, line_number));
                e.insert(id);
                Ok(id)
            }
        }
    }

    pub fn insert_point_get_id(&mut self, value: Point) -> PointId {
        let id = PointId(
            self.points
                .len()
                .try_into()
                .expect("more than 65535 points not supported"),
        );
        self.points.push(PointInfo::new(value, id));
        id
    }

    /// Returns a `LineId` so that `self` isn't mutably borrowed
    pub fn get_or_insert_line(&mut self, p1: PointId, p2: PointId) -> LineId {
        if let Some(&line_id) = self.pairs.get(&(p1, p2)) {
            line_id
        } else {
            let line_id = LineId(self.lines.len());
            Self::add_pair(&mut self.pairs, &mut self.points, p1, p2, line_id);
            let p1 = &self[p1];
            let p2 = &self[p2];
            let new_line = LineInfo::from_pair(line_id, p1.info, p2.info);
            self.lines.push(new_line);
            line_id
        }
    }

    /// Returns a vector of the points of the line, in the same order as the specified points.
    #[cfg(test)]
    pub fn get_points_of_line(&self, p1: PointId, p2: PointId) -> Option<Vec<Point>> {
        let p1 = self[p1].info;
        let p2 = self[p2].info;
        let line = self
            .pairs
            .get(&(p1.id, p2.id))
            .and_then(|&LineId(id)| self.lines.get(id))?;
        if line.distance(p1.value) < line.distance(p2.value) {
            Some(line.points().map(|p| line.point(p.distance)).collect())
        } else {
            Some(
                line.points()
                    .rev()
                    .map(|p| line.point(p.distance))
                    .collect(),
            )
        }
    }

    pub fn intersect(&mut self, l1: LineId, l2: LineId, value: Point) -> PointId {
        if let Some(id) = self[l1].intersect(&self[l2]) {
            id
        } else {
            let id = self.insert_point_get_id(value);
            self.add_point_to_line(id, l1);
            self.add_point_to_line(id, l2);
            id
        }
    }

    pub fn add_point_on_line(&mut self, point: Point, line: LineId) -> PointId {
        let point_id = self.insert_point_get_id(point);
        self.add_point_to_line(point_id, line);
        point_id
    }

    fn add_point_to_line(&mut self, point_id: PointId, line: LineId) {
        for other in self.lines[line.0].points() {
            Self::add_pair(&mut self.pairs, &mut self.points, point_id, other.id, line);
        }
        let point = self[point_id].info;
        self[line].add_point(point);
    }

    /// This is an associated function so it can be used when another part of `self` is already
    /// borowed.
    fn add_pair(
        pairs: &mut HashMap<(PointId, PointId), LineId>,
        points: &mut [PointInfo],
        p1: PointId,
        p2: PointId,
        line: LineId,
    ) {
        pairs.insert((p1, p2), line);
        pairs.insert((p2, p1), line);
        points[p1.0 as usize].lines.insert(line);
        points[p2.0 as usize].lines.insert(line);
    }

    /// Creates a new line from a start point, and a direction.
    pub fn new_line(&mut self, start_id: PointId, direction: Point) -> LineId {
        let line_id = LineId(self.lines.len());
        let origin = self[start_id].info;
        self.lines
            .push(LineInfo::from_origin_direction(line_id, origin, direction));
        line_id
    }

    /// Appends a given segment to the given route.
    pub fn add_segment<'a>(
        &mut self,
        route: RouteId,
        start_id: PointId,
        end_id: PointId,
        offset: f64,
    ) -> Result<(), &'a str> {
        let offset = offset * self.default_width;
        let p1 = self[start_id].info;
        let p2 = self[end_id].info;
        self.get_or_insert_line(p1.id, p2.id);
        self[route].add_segment(p1.id, p2.id, offset);
        Ok(())
    }

    pub fn routes(&self) -> Vec<Path> {
        let mut route_corners = HashMap::new();
        for route in &self.routes {
            self.extend_route_corners(route, &mut route_corners);
        }

        self.routes
            .iter()
            .map(|route| self.route_to_path(route, &route_corners))
            .collect()
    }

    fn extend_route_corners(&self, route: &Route, route_corners: &mut RouteCorners) {
        for (current, next) in route.iter().tuple_windows() {
            if current.end == next.start
                && self.are_collinear(current.start, current.end, next.end)
                    == Collinearity::NotCollinear
            {
                let corner = route_corners.entry(current.end).or_default();
                let line_in = &self[(current.start, current.end)];
                let line_out = &self[(next.start, next.end)];
                let start_pt = self[current.start].info;
                let corner_pt = self[current.end].info;
                let end_pt = self[next.end].info;
                let line_dir_in = line_in
                    .get_direction(start_pt, corner_pt)
                    .expect("duplicate point in route");
                let line_dir_out = line_out
                    .get_direction(end_pt, corner_pt)
                    .expect("duplicate point in route");

                let off_in = current.offset;
                let off_out = next.offset;
                let in_dir = (start_pt.value - corner_pt.value).unit();
                let out_dir = (end_pt.value - corner_pt.value).unit();
                let arc_width =
                    self.inner_radius / calculate_tan_half_angle(in_dir, out_dir).sqrt();
                let (delta_in, delta_out) = calculate_longitudinal_offsets(
                    in_dir, out_dir,
                    // `off_in` is reversed because `in_dir` is pointing from `corner` to `start`,
                    // but `off_in` is calculated along the segment from `start` to `corner`
                    -off_in, off_out,
                );
                let perp_in = arc_width + delta_in;
                let perp_out = arc_width + delta_out;
                let turn_in = RouteTurn {
                    transverse: current.offset,
                    longitudinal: perp_in,
                };
                let turn_out = RouteTurn {
                    transverse: -next.offset,
                    longitudinal: perp_out,
                };
                // positive if out_dir is clockwise of in_dir
                // => positive == turning left
                if out_dir.cross(*in_dir) > 0.0 {
                    corner.insert_left(line_in.id, line_dir_in, turn_in);
                    corner.insert_right(line_out.id, line_dir_out, turn_out);
                } else {
                    corner.insert_right(line_in.id, line_dir_in, turn_in);
                    corner.insert_left(line_out.id, line_dir_out, turn_out);
                }
            }
        }
    }

    fn route_to_path(&self, route: &Route, route_corners: &RouteCorners) -> Path {
        let mut path = Path::new(route.name.clone(), route.style.clone());
        if let Some(segment) = route.first() {
            // the start of the route
            path.operations.push(self.segment_start(segment));
            for (current, next) in route.iter().tuple_windows() {
                if current.end == next.start {
                    let route_corner = route_corners.get(&current.end);
                    let line_in = &self[(current.start, current.end)];
                    let line_out = &self[(next.start, next.end)];
                    let start_pt = self[current.start].info;
                    let corner_pt = self[current.end].info;
                    let end_pt = self[next.end].info;
                    let line_dir_in = line_in
                        .get_direction(start_pt, corner_pt)
                        .expect("duplicate point in route");
                    let line_dir_out = line_out
                        .get_direction(end_pt, corner_pt)
                        .expect("duplicate point in route");
                    let longitudinal_in = route_corner
                        .and_then(|rc| rc.get(line_in.id, line_dir_in))
                        .unwrap_or(0.0);
                    let longitudinal_out = route_corner
                        .and_then(|rc| rc.get(line_out.id, line_dir_out))
                        .unwrap_or(0.0);
                    match self.are_collinear(current.start, current.end, next.end) {
                        Collinearity::Sequential => {
                            // parallel shift
                            if let Some(shift) = self.parallel_shift(
                                current,
                                next,
                                longitudinal_in,
                                longitudinal_out,
                            ) {
                                path.operations.push(shift);
                            }
                        }
                        Collinearity::NotSequential => {
                            if current.offset == -next.offset {
                                // no turn, just a straight line ending.
                                path.operations.push(self.segment_end(current));
                            } else {
                                // u-turn
                                path.operations
                                    .push(self.u_turn(current, next, longitudinal_in));
                            }
                        }
                        Collinearity::NotCollinear => {
                            // corner
                            path.operations.push(self.corner(
                                current,
                                next,
                                longitudinal_in,
                                longitudinal_out,
                            ));
                        }
                    }
                } else {
                    // end this line, and move to the start of the next.
                    path.operations.push(self.segment_end(current));
                    path.operations.push(self.segment_start(next));
                }
            }
            let current = route.last().unwrap();
            path.operations.push(self.segment_end(current));
        }
        path
    }

    pub fn parallel_shift(
        &self,
        segment_in: &RouteSegment,
        segment_out: &RouteSegment,
        longitudinal_in: f64,
        longitudinal_out: f64,
    ) -> Option<Operation> {
        let start_id = segment_in.start;
        let end_id = segment_in.end;
        let start = self[start_id].info;
        let end = self[end_id].info;
        let offset_in = segment_in.offset;
        let offset_out = segment_out.offset;
        if crate::values::float_eq(offset_in, offset_out) {
            None
        } else {
            let direction = (end.value - start.value).unit();
            Some(Operation {
                point: end.value,
                kind: OperationKind::Shift {
                    direction,
                    offset_in,
                    offset_out,
                    longitudinal_in,
                    longitudinal_out,
                },
            })
        }
    }

    pub fn u_turn(
        &self,
        segment_in: &RouteSegment,
        segment_out: &RouteSegment,
        longitudinal: f64,
    ) -> Operation {
        let start_id = segment_in.start;
        let end_id = segment_in.end;
        let start = self[start_id].info;
        let end = self[end_id].info;
        let direction = (end.value - start.value).unit();
        let offset_in = segment_in.offset;
        let offset_out = segment_out.offset;
        Operation {
            point: end.value,
            kind: OperationKind::UTurn {
                direction,
                longitudinal,
                offset_in,
                offset_out,
            },
        }
    }

    pub fn corner(
        &self,
        segment_in: &RouteSegment,
        segment_out: &RouteSegment,
        offset_long_in: f64,
        offset_long_out: f64,
    ) -> Operation {
        let start_id = segment_in.start;
        let corner_id = segment_in.end;
        let end_id = segment_out.end;
        let start = self[start_id].info;
        let corner = self[corner_id].info;
        let end = self[end_id].info;

        let transverse_in = segment_in.offset;
        let transverse_out = segment_out.offset;

        let (long_in, long_out) = calculate_longitudinal_offsets(
            (start.value - corner.value).unit(),
            (end.value - corner.value).unit(),
            -transverse_in,
            transverse_out,
        );
        let arc_width = (offset_long_in - long_in).max(offset_long_out - long_out);

        Operation::new_corner(
            start.value,
            corner.value,
            end.value,
            transverse_in,
            transverse_out,
            arc_width,
        )
    }

    pub fn segment_start(&self, segment: &RouteSegment) -> Operation {
        self.segment_start_or_end(segment, true)
    }

    pub fn segment_end(&self, segment: &RouteSegment) -> Operation {
        self.segment_start_or_end(segment, false)
    }

    fn segment_start_or_end(&self, segment: &RouteSegment, is_start: bool) -> Operation {
        let start_id = segment.start;
        let end_id = segment.end;
        let start = self[start_id].info;
        let end = self[end_id].info;
        let offset = segment.offset;
        let direction = (end.value - start.value).unit();
        Operation {
            point: if is_start { start } else { end }.value,
            kind: if is_start {
                OperationKind::Start { offset, direction }
            } else {
                OperationKind::End { offset, direction }
            },
        }
    }

    pub fn are_collinear(&self, p1: PointId, p2: PointId, p3: PointId) -> Collinearity {
        // this is only called on segments which have already been added, so we can be sure that
        // all the points mentioned are valid.
        if self.pairs[&(p1, p2)] != self.pairs[&(p2, p3)] {
            Collinearity::NotCollinear
        } else {
            let line = &self[(p1, p2)];
            let p1 = line.line_point(self[p1].info);
            let p2 = line.line_point(self[p2].info);
            let p3 = line.line_point(self[p3].info);
            if p1 <= p2 && p2 <= p3 || p3 <= p2 && p2 <= p1 {
                Collinearity::Sequential
            } else {
                Collinearity::NotSequential
            }
        }
    }
}

impl Index<PointId> for PointCollection {
    type Output = PointInfo;

    fn index(&self, PointId(idx): PointId) -> &PointInfo {
        &self.points[idx as usize]
    }
}

impl IndexMut<PointId> for PointCollection {
    fn index_mut(&mut self, PointId(idx): PointId) -> &mut PointInfo {
        &mut self.points[idx as usize]
    }
}

impl Index<LineId> for PointCollection {
    type Output = LineInfo;

    fn index(&self, LineId(idx): LineId) -> &LineInfo {
        &self.lines[idx]
    }
}

impl IndexMut<LineId> for PointCollection {
    fn index_mut(&mut self, LineId(idx): LineId) -> &mut LineInfo {
        &mut self.lines[idx]
    }
}

impl Index<(PointId, PointId)> for PointCollection {
    type Output = LineInfo;

    fn index(&self, (p1, p2): (PointId, PointId)) -> &LineInfo {
        &self[self.pairs[&(p1, p2)]]
    }
}

impl Index<RouteId> for PointCollection {
    type Output = Route;

    fn index(&self, RouteId(idx): RouteId) -> &Route {
        &self.routes[idx]
    }
}

impl IndexMut<RouteId> for PointCollection {
    fn index_mut(&mut self, RouteId(idx): RouteId) -> &mut Route {
        &mut self.routes[idx]
    }
}

/// The result of `are_collinear`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Collinearity {
    /// The three points are collinear, and in order.
    Sequential,
    /// The three points are collinear, but not in order.
    NotSequential,
    /// The three points are not collinear.
    NotCollinear,
}

#[derive(Clone, Debug, Serialize)]
pub struct PointInfo {
    #[serde(flatten)]
    info: PointInfoLite,
    lines: HashSet<LineId>,
}

#[derive(Clone, Copy, Debug, Serialize)]
pub struct PointInfoLite {
    value: Point,
    id: PointId,
}

impl PointInfo {
    pub fn new(value: Point, id: PointId) -> PointInfo {
        PointInfo {
            info: PointInfoLite { value, id },
            lines: HashSet::new(),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct PointId(u16);

impl fmt::Debug for PointId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // print on a single line even with `#?` formatter
        write!(f, "PointId({})", self.0)
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct Route {
    /// The name of the route.
    name: Variable,
    /// The id of the route.
    id: RouteId,
    /// The style (if any) for the route.
    style: String,
    /// The segments making up the route.
    segments: Vec<RouteSegment>,
    /// The number of the line where the route is defined.
    line_number: usize,
}

impl Route {
    fn new(name: Variable, id: RouteId, style: Vec<Variable>, line_number: usize) -> Route {
        Route {
            name,
            id,
            style: style.join(" "),
            segments: Vec::new(),
            line_number,
        }
    }

    fn add_segment(&mut self, start: PointId, end: PointId, offset: f64) -> RouteSegmentRef {
        let index = self.len();
        self.segments.push(RouteSegment { start, end, offset });
        RouteSegmentRef {
            route: self.id,
            index,
        }
    }

    fn first(&self) -> Option<&RouteSegment> {
        self.segments.first()
    }

    fn last(&self) -> Option<&RouteSegment> {
        self.segments.last()
    }

    fn len(&self) -> usize {
        self.segments.len()
    }

    fn iter(&self) -> ::std::slice::Iter<RouteSegment> {
        self.segments.iter()
    }
}

impl Index<usize> for Route {
    type Output = RouteSegment;

    fn index(&self, index: usize) -> &RouteSegment {
        &self.segments[index]
    }
}

impl IndexMut<usize> for Route {
    fn index_mut(&mut self, index: usize) -> &mut RouteSegment {
        &mut self.segments[index]
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct RouteId(usize);

impl fmt::Debug for RouteId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // print on a single line even with `#?` formatter
        write!(f, "RouteId({})", self.0)
    }
}

/// A segment of a route.
#[derive(Clone, Copy, Debug, Serialize)]
pub struct RouteSegment {
    pub start: PointId,
    pub end: PointId,
    pub offset: f64,
}

/// A reference to a segment of a route, referencing it by its `RouteId` and the index into that
/// route's list of segments.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize)]
pub struct RouteSegmentRef {
    pub route: RouteId,
    pub index: usize,
}
