use std::{
    collections::{btree_map::Entry, BTreeMap, HashMap, HashSet},
    fmt,
    ops::{Index, IndexMut},
};

use itertools::Itertools;
use serde::Serialize;

use crate::{
    corner::{calculate_longitudinal_offsets, calculate_tan_half_angle},
    error::{Error, MathError, Result},
    expressions::Variable,
    intermediate_representation::{Operation, OperationKind, Path},
    values::{Point, PointProvenance},
};

mod line;
mod route_corner;

use line::{Line, LineId};
use route_corner::{RouteCorners, RouteTurn};

#[derive(Debug, Serialize)]
pub struct PointCollection {
    points: Vec<PointInfo>,
    point_ids: BTreeMap<Variable, PointId>,
    lines: Vec<Line>,
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
            point_ids: Default::default(),
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
    pub fn contains(&self, k: &str) -> bool {
        self.point_ids.contains_key(k)
    }

    fn get_point_info(&self, k: &str) -> Option<&PointInfo> {
        self.point_ids
            .get(k)
            .and_then(|&PointId(id)| self.points.get(id as usize))
    }

    pub fn get_point_and_id(&self, k: &str) -> Option<(Point, PointId)> {
        self.point_ids.get(k).map(|&id| (self[id].info.value, id))
    }

    pub fn get_point_line_number(&self, k: &str) -> Option<usize> {
        self.get_point_info(k).map(|info| info.info.line_number)
    }

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

    fn insert_point_get_id(
        &mut self,
        name: Variable,
        value: Point,
        line_number: usize,
    ) -> Result<PointId> {
        match self.point_ids.entry(name) {
            Entry::Occupied(e) => {
                let &id = e.get();
                Err(Error::PointRedefinition {
                    name: e.key().clone(),
                    line: line_number,
                    original_line: self[id].info.line_number,
                })
            }
            Entry::Vacant(e) => {
                let id = PointId(
                    self.points
                        .len()
                        .try_into()
                        .expect("more than 65535 points not supported"),
                );
                self.points.push(PointInfo::new(value, id, line_number));
                e.insert(id);
                Ok(id)
            }
        }
    }

    fn insert_alias(&mut self, name: Variable, id: PointId, line_number: usize) -> Result<()> {
        match self.point_ids.entry(name) {
            Entry::Occupied(e) => {
                let &id = e.get();
                Err(Error::PointRedefinition {
                    name: e.key().clone(),
                    line: line_number,
                    original_line: self[id].info.line_number,
                })
            }
            Entry::Vacant(e) => {
                e.insert(id);
                Ok(())
            }
        }
    }

    /// Returns a `LineId` so that `self` isn't mutably borrowed
    fn get_or_insert_line(&mut self, p1: PointId, p2: PointId) -> LineId {
        if let Some(&line_id) = self.pairs.get(&(p1, p2)) {
            line_id
        } else {
            let line_id = LineId(self.lines.len());
            Self::add_pair(&mut self.pairs, &mut self.points, p1, p2, line_id);
            let p1 = &self[p1];
            let p2 = &self[p2];
            let new_line = Line::from_pair(p1.info, p2.info);
            self.lines.push(new_line);
            line_id
        }
    }

    /// Returns a vector of the points of the line, in the same order as the specified points.
    #[cfg(test)]
    pub fn get_points_of_line(&self, p1: &str, p2: &str) -> Option<Vec<Point>> {
        let p1 = self.get_point_info(p1)?.info;
        let p2 = self.get_point_info(p2)?.info;
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

    /// Inserts the point, or if the point already exists, return a point redefinition error.
    pub fn insert_point(
        &mut self,
        name: Variable,
        value: Point,
        provenance: PointProvenance,
        line_number: usize,
    ) -> Result<()> {
        match provenance {
            PointProvenance::None => {
                // just insert the point
                self.insert_point_get_id(name, value, line_number)?;
            }
            PointProvenance::Named(id) => {
                // this point is identical to an existing point; don't add a new point, just add a
                // new reference to the existing one.
                self.insert_alias(name, id, line_number)?;
            }
            PointProvenance::Intersection(pair1, pair2) => {
                let l1 = pair1.map(|(p1, p2)| self.get_or_insert_line(p1, p2));
                let l2 = pair2.map(|(p1, p2)| self.get_or_insert_line(p1, p2));
                if let (Some(l1), Some(l2)) = (l1, l2) {
                    if let Some(existing) = self[l1].intersect(&self[l2]) {
                        // this point already exists
                        self.insert_alias(name, existing, line_number)?;
                        return Ok(());
                    }
                }
                // this is a new point on the intersection of two lines; update both of
                // those lines if they exist.
                let id = self.insert_point_get_id(name, value, line_number)?;
                if let Some(l1) = l1 {
                    for p in self.lines[l1.0].points() {
                        Self::add_pair(&mut self.pairs, &mut self.points, id, p.id, l1);
                    }
                }
                if let Some(l2) = l2 {
                    for p in self.lines[l2.0].points() {
                        Self::add_pair(&mut self.pairs, &mut self.points, id, p.id, l2);
                    }
                }
            }
        }
        Ok(())
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

    /// Creates a new line from a start point, a direction, and (name, distance) pairs along the
    /// line for new points to create.
    ///
    /// Because only one existing point is referenced, this necessarily creates a new line.
    pub fn new_line(
        &mut self,
        start_point: Variable,
        direction: Point,
        points: impl IntoIterator<Item = (Variable, f64)>,
        line_number: usize,
    ) -> Result<()> {
        let line_id = LineId(self.lines.len());
        let &start_id = self
            .point_ids
            .get(&start_point)
            .ok_or(Error::Math(MathError::Variable(start_point), line_number))?;
        let origin = self[start_id].info;
        let mut line = Line::from_origin_direction(origin, direction);
        let mut total_distance = 0.0;
        for (name, distance) in points {
            total_distance += distance;
            let point = total_distance * direction + origin.value;
            let id = self.insert_point_get_id(name, point, line_number)?;
            line.add_point(id, total_distance);
        }
        for (&p1, &p2) in line.points().tuple_combinations() {
            Self::add_pair(&mut self.pairs, &mut self.points, p1.id, p2.id, line_id);
        }
        self.lines.push(line);
        Ok(())
    }

    /// Extends the line specified by the given points, with intermediate points indicated as a
    /// fraction of the distance between the given points.
    pub fn extend_line(
        &mut self,
        start_point: Variable,
        end_point: Variable,
        points: impl IntoIterator<Item = (Variable, f64)>,
        line_number: usize,
    ) -> Result<()> {
        let start_id = *self
            .point_ids
            .get(&start_point)
            .ok_or(Error::Math(MathError::Variable(start_point), line_number))?;
        let end_id = *self
            .point_ids
            .get(&end_point)
            .ok_or(Error::Math(MathError::Variable(end_point), line_number))?;
        let line_id = self.get_or_insert_line(start_id, end_id);
        let start = self[start_id].info.value;
        let end = self[end_id].info.value;
        let direction = end - start;
        let (start_distance, distance_scale) = {
            let line = &self[line_id];
            (line.distance(start), line.relative_distance(start, end))
        };
        for (name, distance) in points {
            let id = self.insert_point_get_id(name, distance * direction + start, line_number)?;
            self[line_id].add_point(id, distance * distance_scale + start_distance);
        }
        for (&p1, &p2) in self.lines[line_id.0].points().tuple_combinations() {
            Self::add_pair(&mut self.pairs, &mut self.points, p1.id, p2.id, line_id);
        }
        Ok(())
    }

    /// Appends a given segment to the given route.
    pub fn add_segment<'a>(
        &mut self,
        route: RouteId,
        start: &'a str,
        end: &'a str,
        offset: f64,
    ) -> Result<(), &'a str> {
        let offset = offset * self.default_width;
        let p1 = self.get_point_info(start).ok_or(start)?.info;
        let p2 = self.get_point_info(end).ok_or(end)?.info;
        let line_id = self.get_or_insert_line(p1.id, p2.id);
        let route_segment = self[route].add_segment(p1.id, p2.id, offset);
        self[line_id].add_segment(p1, p2, route_segment);
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

                let (rev_in, seg_in) = line_in.get_segment(start_pt, corner_pt);
                let (rev_out, seg_out) = line_out.get_segment(corner_pt, end_pt);
                let off_in = current.offset;
                let off_out = next.offset;
                let (start_pt, _) = seg_in.endpoints(rev_in);
                let (_, end_pt) = seg_out.endpoints(rev_out);
                let start_pt = self[start_pt.id].info;
                let end_pt = self[end_pt.id].info;
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
                    corner.insert_left(start_pt.id, turn_in);
                    corner.insert_right(end_pt.id, turn_out);
                } else {
                    corner.insert_right(start_pt.id, turn_in);
                    corner.insert_left(end_pt.id, turn_out);
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
                    let in_pt = self.get_other_segment_endpoint(current.start, current.end);
                    let out_pt = self.get_other_segment_endpoint(next.end, next.start);
                    let longitudinal_in = route_corner.and_then(|rc| rc.get(in_pt)).unwrap_or(0.0);
                    let longitudinal_out =
                        route_corner.and_then(|rc| rc.get(out_pt)).unwrap_or(0.0);
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

    /// Finds the segment along the line from `far` to `near` which contains `near`, and returns
    /// the endpoint of that segment closest to `far`.
    fn get_other_segment_endpoint(&self, far: PointId, near: PointId) -> PointId {
        let (rev, seg) = self[(far, near)].get_segment(self[far].info, self[near].info);
        seg.endpoints(rev).0.id
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
        let line = &self[(start_id, end_id)];
        let (this, other) = if is_start { (start, end) } else { (end, start) };
        let (reverse, _) = line.get_segment(other, this);
        // if `is_start`, we reversed the start and end points already, so flip `reverse`
        let reverse = reverse ^ is_start;
        let offset = segment.offset;
        let direction = if reverse {
            -line.direction.unit()
        } else {
            line.direction.unit()
        };
        Operation {
            point: this.value,
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
    type Output = Line;

    fn index(&self, LineId(idx): LineId) -> &Line {
        &self.lines[idx]
    }
}

impl IndexMut<LineId> for PointCollection {
    fn index_mut(&mut self, LineId(idx): LineId) -> &mut Line {
        &mut self.lines[idx]
    }
}

impl Index<(PointId, PointId)> for PointCollection {
    type Output = Line;

    fn index(&self, (p1, p2): (PointId, PointId)) -> &Line {
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
    line_number: usize,
}

impl PointInfo {
    pub fn new(value: Point, id: PointId, line_number: usize) -> PointInfo {
        PointInfo {
            info: PointInfoLite {
                value,
                id,
                line_number,
            },
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extend_line_in_order() {
        let mut points = PointCollection::default();
        points
            .insert_point("A".into(), Point(0.0, 0.0), PointProvenance::None, 0)
            .unwrap();
        points
            .insert_point("B".into(), Point(2.0, 0.0), PointProvenance::None, 1)
            .unwrap();
        points
            .extend_line(
                "A".into(),
                "B".into(),
                vec![("C".into(), 0.1), ("D".into(), 0.5)],
                2,
            )
            .unwrap();
        let a_id = points.point_ids["A"];
        let b_id = points.point_ids["B"];
        let line = &points[(a_id, b_id)];
        assert_eq!(line.direction, Point(2.0, 0.0));
        assert_eq!(
            line.points().map(|p| p.distance).collect::<Vec<_>>(),
            &[0.0, 0.1, 0.5, 1.0]
        );
    }

    #[test]
    fn extend_line_in_reverse_order() {
        let mut points = PointCollection::default();
        points
            .insert_point("A".into(), Point(0.0, 0.0), PointProvenance::None, 0)
            .unwrap();
        points
            .insert_point("B".into(), Point(2.0, 0.0), PointProvenance::None, 1)
            .unwrap();
        points
            .extend_line(
                "A".into(),
                "B".into(),
                vec![("C".into(), 0.5), ("D".into(), 0.1)],
                2,
            )
            .unwrap();
        let a_id = points.point_ids["A"];
        let b_id = points.point_ids["B"];
        let line = &points[(a_id, b_id)];
        assert_eq!(line.direction, Point(2.0, 0.0));
        assert_eq!(
            line.points().map(|p| p.distance).collect::<Vec<_>>(),
            &[0.0, 0.1, 0.5, 1.0]
        );
    }

    #[test]
    fn new_line() {
        let mut points = PointCollection::default();
        points
            .insert_point("A".into(), Point(0.0, 0.0), PointProvenance::None, 0)
            .unwrap();
        points
            .new_line(
                "A".into(),
                Point(2.0, 0.0),
                vec![("B".into(), 1.0), ("C".into(), 0.5), ("D".into(), 1.0)],
                2,
            )
            .unwrap();
        let a_id = points.point_ids["A"];
        let b_id = points.point_ids["B"];
        let line = &points[(a_id, b_id)];
        assert_eq!(line.direction, Point(2.0, 0.0));
        assert_eq!(
            line.points().map(|p| p.distance).collect::<Vec<_>>(),
            &[0.0, 1.0, 1.5, 2.5]
        );
    }
}
