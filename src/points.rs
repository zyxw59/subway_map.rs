use std::collections::{btree_map::Entry, BTreeMap, HashMap, HashSet};
use std::fmt;
use std::ops::{Index, IndexMut};

use itertools::Itertools;
use serde::Serialize;
use svg::node::{
    element::{path::Data, Path, Use},
    Node,
};

mod line;

use crate::corner::{Corner, ParallelShift};
use crate::document::Document;
use crate::error::{EvaluatorError, MathError};
use crate::expressions::Variable;
use crate::statement;
use crate::values::{Point, PointProvenance};

use line::{Line, LineId};

#[derive(Default, Debug, Serialize)]
pub struct PointCollection {
    points: Vec<PointInfo>,
    point_ids: BTreeMap<Variable, PointId>,
    lines: Vec<Line>,
    #[serde(skip)]
    pairs: HashMap<(PointId, PointId), LineId>,
    routes: Vec<Route>,
    route_ids: BTreeMap<Variable, RouteId>,
    stops: Vec<Stop>,
    default_width: f64,
    inner_radius: f64,
}

impl PointCollection {
    pub fn new() -> PointCollection {
        Default::default()
    }

    pub fn contains(&self, k: &str) -> bool {
        self.point_ids.contains_key(k)
    }

    pub fn point_iter(&self) -> impl Iterator<Item = &Point> {
        self.points.iter().map(|info| &info.info.value)
    }

    fn get_point_info(&self, k: &str) -> Option<&PointInfo> {
        self.point_ids
            .get(k)
            .and_then(|&PointId(id)| self.points.get(id))
    }

    pub fn get_point_and_id(&self, k: &str) -> Option<(Point, PointId)> {
        self.point_ids.get(k).map(|&id| (self[id].info.value, id))
    }

    pub fn get_point(&self, k: &str) -> Option<Point> {
        self.get_point_info(k).map(|info| info.info.value)
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
        width: f64,
        styles: Vec<Variable>,
        line_number: usize,
    ) -> Result<RouteId, EvaluatorError> {
        match self.route_ids.entry(name) {
            Entry::Occupied(e) => {
                let &id = e.get();
                Err(EvaluatorError::RouteRedefinition(
                    e.key().clone(),
                    line_number,
                    self[id].line_number,
                ))
            }
            Entry::Vacant(e) => {
                let id = RouteId(self.routes.len());
                self.routes
                    .push(Route::new(e.key().clone(), id, width, styles, line_number));
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
    ) -> Result<PointId, EvaluatorError> {
        match self.point_ids.entry(name) {
            Entry::Occupied(e) => {
                let &id = e.get();
                Err(EvaluatorError::PointRedefinition(
                    e.key().clone(),
                    line_number,
                    self[id].info.line_number,
                ))
            }
            Entry::Vacant(e) => {
                let id = PointId(self.points.len());
                self.points.push(PointInfo::new(value, id, line_number));
                e.insert(id);
                Ok(id)
            }
        }
    }

    fn insert_alias(
        &mut self,
        name: Variable,
        id: PointId,
        line_number: usize,
    ) -> Result<(), EvaluatorError> {
        match self.point_ids.entry(name) {
            Entry::Occupied(e) => {
                let &id = e.get();
                Err(EvaluatorError::PointRedefinition(
                    e.key().clone(),
                    line_number,
                    self[id].info.line_number,
                ))
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
    ) -> Result<(), EvaluatorError> {
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
        points[p1.0].lines.insert(line);
        points[p2.0].lines.insert(line);
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
    ) -> Result<(), EvaluatorError> {
        let line_id = LineId(self.lines.len());
        let &start_id = self
            .point_ids
            .get(&start_point)
            .ok_or(EvaluatorError::Math(
                MathError::Variable(start_point),
                line_number,
            ))?;
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
    ) -> Result<(), EvaluatorError> {
        let start_id = *self
            .point_ids
            .get(&start_point)
            .ok_or(EvaluatorError::Math(
                MathError::Variable(start_point),
                line_number,
            ))?;
        let end_id = *self.point_ids.get(&end_point).ok_or(EvaluatorError::Math(
            MathError::Variable(end_point),
            line_number,
        ))?;
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
        offset: isize,
    ) -> Result<(), &'a str> {
        let p1 = self.get_point_info(start).ok_or(start)?.info;
        let p2 = self.get_point_info(end).ok_or(end)?.info;
        let line_id = self.get_or_insert_line(p1.id, p2.id);
        let route_segment = self[route].add_segment(p1.id, p2.id, offset);
        let width = self[route].width;
        self[line_id].add_segment(p1, p2, offset, width, route_segment);
        Ok(())
    }

    pub fn add_stop(
        &mut self,
        statement::Stop {
            point,
            styles,
            routes,
            label,
        }: statement::Stop,
        line_number: usize,
    ) -> Result<(), EvaluatorError> {
        let point = *self.point_ids.get(&point).ok_or(EvaluatorError::Math(
            MathError::Variable(point),
            line_number,
        ))?;
        let routes = routes
            .map(|vec| {
                vec.into_iter()
                    .map(|rte| {
                        self.route_ids
                            .get(&rte)
                            .copied()
                            .ok_or(EvaluatorError::Math(MathError::Variable(rte), line_number))
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?;
        self.stops.push(Stop {
            point,
            style: styles.join(" "),
            routes,
            label,
        });
        Ok(())
    }

    pub fn draw_stops(&self, document: &mut Document) {
        for stop in &self.stops {
            self.draw_stop(stop, document);
        }
    }

    fn draw_stop(&self, stop: &Stop, document: &mut Document) {
        // just put a circle at the stop for now
        // TODO: More complicated stop icons
        // TODO: Stop labels
        // step 0: get the point info from the stop
        let point_id = stop.point;
        let point = &self[point_id];
        // step 1: get the set of route segments which should have stop markers at this point.
        let mut route_segments: HashSet<RouteSegmentRef> = HashSet::new();
        for &line_id in &point.lines {
            let line = &self[line_id];
            let segments = line.get_segments_containing_point(point.info);
            route_segments.extend(segments.iter().flatten().flat_map(|seg| {
                seg.routes().filter(|seg_ref| {
                    stop.routes
                        .as_ref()
                        .map_or(true, |routes| routes.contains(&seg_ref.route))
                })
            }));
        }
        // step 2: calculate stop marker locations, consolidating them as necessary.
        let mut points_and_routes = Vec::new();
        for &seg_ref in &route_segments {
            let route = &self[seg_ref.route];
            let segment = &route[seg_ref.index];
            if point_id != segment.start && point_id != segment.end {
                // case 1: the point is somewhere in the middle of the segment; we don't need to
                // check for other segments.
                points_and_routes.push(self.calculate_stop_simple(point_id, seg_ref));
            } else if point_id == segment.start {
                // case 2: the point is at the start of the segment...
                match route.get_previous(seg_ref.index) {
                    // case 2a: ... and the end of the previous segment in the line.
                    Some(prev_seg) if point_id == prev_seg.end => {
                        match self.are_collinear(prev_seg.start, point_id, segment.end) {
                            Collinearity::Sequential => {
                                // parallel shift; take the average of the two offsets.
                                points_and_routes
                                    .push(self.calculate_stop_sequential(point_id, seg_ref));
                            }
                            Collinearity::NotSequential => {
                                // u-turn; place the marker at the top of the arc.
                                let corner = self.u_turn(prev_seg, segment);
                                points_and_routes.push((corner.midpoint(), seg_ref.route));
                            }
                            Collinearity::NotCollinear => {
                                // corner; place the marker at the midpoint of the curve.
                                let corner = self.corner(prev_seg, segment);
                                points_and_routes.push((corner.midpoint(), seg_ref.route));
                            }
                        }
                    }
                    // case 2b: ... but not at the end of a previous segment.
                    _ => points_and_routes.push(self.calculate_stop_simple(point_id, seg_ref)),
                }
            } else {
                // case 3: the point is at the end of the segment...
                match route.get_next(seg_ref.index) {
                    // case 3a: ... and at the start of the next segment in the line.
                    // we don't need to do anything here, since this case is handled by case 2a.
                    Some(next_seg) if point_id == next_seg.start => {}
                    // case 3b: ... but not at the start of a following segment.
                    _ => points_and_routes.push(self.calculate_stop_simple(point_id, seg_ref)),
                }
            }
        }
        // step 2: draw the stop icons
        for (point, route) in points_and_routes {
            let route = &self[route];
            let marker = Use::new()
                .set("href", "#stop")
                .set("x", point.0)
                .set("y", point.1)
                .set(
                    "class",
                    format!("stop {} {} {}", route.name, stop.style, route.style),
                );
            document.add_stop(marker);
        }
    }

    /// Calculates the location for a stop marker for a point on a single route segment.
    fn calculate_stop_simple(
        &self,
        point_id: PointId,
        seg_ref: RouteSegmentRef,
    ) -> (Point, RouteId) {
        let point = &self[point_id];
        let route_seg = &self[seg_ref.route][seg_ref.index];
        let line = &self[(route_seg.start, route_seg.end)];
        let reversed = line.are_reversed(self[route_seg.start].info, self[route_seg.end].info);
        let [seg1, seg2] = line.get_segments_containing_point(point.info);
        let offset1 = seg1
            .filter(|seg| seg.contains_route(&seg_ref))
            .map(|seg| seg.calculate_offset(route_seg.offset, reversed, self.default_width));
        let offset2 = seg2
            .filter(|seg| seg.contains_route(&seg_ref))
            .map(|seg| seg.calculate_offset(route_seg.offset, reversed, self.default_width));
        let offset = match (offset1, offset2) {
            (Some(offset1), Some(offset2)) => (offset1 + offset2) / 2.0,
            (Some(offset), None) | (None, Some(offset)) => offset,
            (None, None) => unreachable!(),
        };
        // calculated offset it relative to the course of the route; we want relative to the line.
        let offset = if reversed { -offset } else { offset };
        let dir = line.direction;
        (
            (-dir.unit().perp()).mul_add(offset, point.info.value),
            seg_ref.route,
        )
    }

    /// Calculates the location for a stop marker for a point at the dividing point between two
    /// segments along the same line.
    ///
    /// `seg_ref` refers to the segment after the point.
    fn calculate_stop_sequential(
        &self,
        point_id: PointId,
        seg_ref: RouteSegmentRef,
    ) -> (Point, RouteId) {
        let point = &self[point_id];
        let next_seg = &self[seg_ref.route][seg_ref.index];
        let prev_seg = &self[seg_ref.route][seg_ref.index - 1];
        let line = &self[(next_seg.start, next_seg.end)];
        let reversed = line.are_reversed(self[next_seg.start].info, self[next_seg.end].info);
        let [seg1, seg2] = line.get_segments_containing_point(point.info);
        // if the route is reversed relative to the line, `seg1` will match with `next_seg`, and
        // `seg2` with `prev_seg`; otherwise `seg1` will match with `prev_seg` and `seg2` with
        // `next_seg`.
        // After swapping, `seg1` matches `prev_seg` and `seg2` matches `next_seg`
        let [seg1, seg2] = if reversed { [seg2, seg1] } else { [seg1, seg2] };
        let offset1 = seg1
            .map(|seg| seg.calculate_offset(prev_seg.offset, reversed, self.default_width))
            .unwrap();
        let offset2 = seg2
            .map(|seg| seg.calculate_offset(next_seg.offset, reversed, self.default_width))
            .unwrap();
        let offset = (offset1 + offset2) / 2.0;
        // calculated offset it relative to the course of the route; we want relative to the line.
        let offset = if reversed { -offset } else { offset };
        let dir = line.direction;
        (
            (-dir.unit().perp()).mul_add(offset, point.info.value),
            seg_ref.route,
        )
    }

    pub fn draw_routes(&self, document: &mut Document) {
        for route in &self.routes {
            let path = self.route_to_path(route);
            document.add_route(&route.name, &route.style, path);
        }
    }

    fn route_to_path(&self, route: &Route) -> Path {
        let mut path = Path::new()
            .set("id", format!("route-{}", route.name))
            .set("class", format!("route {}", route.style));
        if let Some(segment) = route.first() {
            // the start of the route
            let mut data = Data::new().move_to(self.segment_start(segment));
            for (current, next) in route.iter().tuple_windows() {
                // process `current` in the loop; `next` will be handled on the next iteration, or
                // after the loop, for the last segment.
                for shift in self.parallel_shifts(current) {
                    data = shift.apply(data);
                }
                if current.end == next.start {
                    match self.are_collinear(current.start, current.end, next.end) {
                        Collinearity::Sequential => {
                            // parallel shift
                            if let Some(shift) = self.parallel_shift(current, next) {
                                data = shift.apply(data);
                            }
                        }
                        Collinearity::NotSequential => {
                            if current.offset == -next.offset {
                                // no turn, just a straight line ending.
                                data = data.line_to(self.segment_end(current));
                            } else {
                                // u-turn
                                data = self.u_turn(current, next).apply(data);
                            }
                        }
                        Collinearity::NotCollinear => {
                            // corner
                            data = self.corner(current, next).apply(data);
                        }
                    }
                } else {
                    // end this line, and move to the start of the next.
                    data = data
                        .line_to(self.segment_end(current))
                        .move_to(self.segment_start(next));
                }
            }
            let current = route.last().unwrap();
            for shift in self.parallel_shifts(current) {
                data = shift.apply(data);
            }
            data = data.line_to(self.segment_end(current));
            path.assign("d", data);
        }
        path
    }

    pub fn parallel_shifts(
        &self,
        segment: &RouteSegment,
    ) -> impl Iterator<Item = ParallelShift> + '_ {
        // this is only called on segments which have already been added, so we can be sure that
        // all the points mentioned are valid.
        let start_id = segment.start;
        let end_id = segment.end;
        let start = self[start_id].info;
        let end = self[end_id].info;
        let line = &self[(start_id, end_id)];
        let (reverse, segments) = line.segments_between(start, end);
        let offset = segment.offset;
        segments.tuple_windows().filter_map(move |(prev, next)| {
            let offset_in = prev.calculate_offset(offset, reverse, self.default_width);
            let offset_out = next.calculate_offset(offset, reverse, self.default_width);
            if crate::values::float_eq(offset_in, offset_out) {
                None
            } else {
                let (dir, at) = if reverse {
                    (-line.direction, line.point(prev.start.distance))
                } else {
                    (line.direction, line.point(prev.end.distance))
                };
                Some(ParallelShift::new(offset_in, offset_out, dir, at))
            }
        })
    }

    pub fn parallel_shift(
        &self,
        segment_in: &RouteSegment,
        segment_out: &RouteSegment,
    ) -> Option<ParallelShift> {
        let start_id = segment_in.start;
        let end_id = segment_in.end;
        let start = self[start_id].info;
        let end = self[end_id].info;
        let line = &self[(start_id, end_id)];
        let (reverse, prev, next) = line.segments_at(start, end);
        let offset_in = prev.calculate_offset(segment_in.offset, reverse, self.default_width);
        let offset_out = next.calculate_offset(segment_out.offset, reverse, self.default_width);
        if crate::values::float_eq(offset_in, offset_out) {
            None
        } else {
            let dir = if reverse {
                -line.direction
            } else {
                line.direction
            };
            Some(ParallelShift::new(offset_in, offset_out, dir, end.value))
        }
    }

    pub fn u_turn(&self, segment_in: &RouteSegment, segment_out: &RouteSegment) -> Corner {
        let start_id = segment_in.start;
        let end_id = segment_in.end;
        let start = self[start_id].info;
        let end = self[end_id].info;
        let line = &self[(start_id, end_id)];
        let (reverse, seg) = line.get_segment(start, end);
        let offset_in = seg.calculate_offset(segment_in.offset, reverse, self.default_width);
        let offset_out = seg.calculate_offset(segment_out.offset, !reverse, self.default_width);
        Corner::u_turn(start.value, end.value, offset_in, offset_out)
    }

    pub fn corner(&self, segment_in: &RouteSegment, segment_out: &RouteSegment) -> Corner {
        let start_id = segment_in.start;
        let corner_id = segment_in.end;
        let end_id = segment_out.end;
        let start = self[start_id].info;
        let corner = self[corner_id].info;
        let end = self[end_id].info;
        let line_in = &self[(start_id, corner_id)];
        let line_out = &self[(corner_id, end_id)];
        let (reverse_in, seg_in) = line_in.get_segment(start, corner);
        let (reverse_out, seg_out) = line_out.get_segment(end, corner);
        // true => to the left (line_in is right of end)
        // false => to the righht (line_in is left of end)
        let turn_in = line_in.right_of(end.value);
        let (offset_in, radius_in) = seg_in.calculate_offset_radius(
            segment_in.offset,
            reverse_in,
            turn_in,
            self.default_width,
        );
        let turn_out = line_out.right_of(start.value);
        let (offset_out, radius_out) = seg_out.calculate_offset_radius(
            segment_out.offset,
            !reverse_out,
            turn_out,
            self.default_width,
        );
        Corner::new(
            start.value,
            corner.value,
            end.value,
            self.inner_radius,
            radius_in.max(radius_out),
        )
        .offset(offset_in, offset_out)
    }

    pub fn segment_start(&self, segment: &RouteSegment) -> Point {
        self.segment_start_or_end(segment, true)
    }

    pub fn segment_end(&self, segment: &RouteSegment) -> Point {
        self.segment_start_or_end(segment, false)
    }

    fn segment_start_or_end(&self, segment: &RouteSegment, is_start: bool) -> Point {
        let start_id = segment.start;
        let end_id = segment.end;
        let mut start = self[start_id].info;
        let mut end = self[end_id].info;
        let line = &self[(start_id, end_id)];
        if is_start {
            std::mem::swap(&mut start, &mut end);
        }
        let (mut reverse, seg) = line.get_segment(start, end);
        // if `is_start`, we reversed the start and end points already, so flip `reverse`
        reverse ^= is_start;
        let mut offset = seg.calculate_offset(segment.offset, reverse, self.default_width);
        // we want absoolute offset.
        if reverse {
            offset = -offset;
        }
        line.direction.unit().perp().mul_add(-offset, end.value)
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
        &self.points[idx]
    }
}

impl IndexMut<PointId> for PointCollection {
    fn index_mut(&mut self, PointId(idx): PointId) -> &mut PointInfo {
        &mut self.points[idx]
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
#[derive(Clone, Copy, Debug)]
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
pub struct PointId(usize);

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
    /// The width of the route line.
    width: f64,
    /// The style (if any) for the route.
    style: String,
    /// The segments making up the route.
    segments: Vec<RouteSegment>,
    /// The number of the line where the route is defined.
    line_number: usize,
}

impl Route {
    fn new(
        name: Variable,
        id: RouteId,
        width: f64,
        style: Vec<Variable>,
        line_number: usize,
    ) -> Route {
        Route {
            name,
            id,
            width,
            style: style.join(" "),
            segments: Vec::new(),
            line_number,
        }
    }

    fn add_segment(&mut self, start: PointId, end: PointId, offset: isize) -> RouteSegmentRef {
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

    fn get(&self, index: usize) -> Option<&RouteSegment> {
        self.segments.get(index)
    }

    fn get_previous(&self, index: usize) -> Option<&RouteSegment> {
        index.checked_sub(1).and_then(|index| self.get(index))
    }

    fn get_next(&self, index: usize) -> Option<&RouteSegment> {
        index.checked_add(1).and_then(|index| self.get(index))
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
    pub offset: isize,
}

/// A reference to a segment of a route, referencing it by its `RouteId` and the index into that
/// route's list of segments.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize)]
pub struct RouteSegmentRef {
    pub route: RouteId,
    pub index: usize,
}

/// A stop.
#[derive(Clone, Debug, Serialize)]
pub struct Stop {
    /// The location of the stop.
    pub point: PointId,
    /// The style of the stop.
    pub style: String,
    /// The set of routes which stop at the stop, or `None` if all lines stop.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub routes: Option<Vec<RouteId>>,
    /// The label.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<statement::Label>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extend_line_in_order() {
        let mut points = PointCollection::new();
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
        let mut points = PointCollection::new();
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
        let mut points = PointCollection::new();
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

    #[test]
    fn segments_containing_point() {
        let mut points = PointCollection::new();
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
        let route = points
            .insert_route_get_id("red".into(), 1.0, Vec::new(), 3)
            .unwrap();
        points.add_segment(route, "A", "B", 0).unwrap();
        points.add_segment(route, "B", "D", 0).unwrap();
        let a_id = points.point_ids["A"];
        let b_id = points.point_ids["B"];
        let c_id = points.point_ids["C"];
        let d_id = points.point_ids["D"];
        let line = &points[(a_id, b_id)];
        assert_eq!(
            line.get_segments_containing_point(points[a_id].info),
            [None, line.get_segment_by_index(0)]
        );
        assert_eq!(
            line.get_segments_containing_point(points[b_id].info),
            [line.get_segment_by_index(0), line.get_segment_by_index(1)]
        );
        assert_eq!(
            line.get_segments_containing_point(points[c_id].info),
            [None, line.get_segment_by_index(1)]
        );
        assert_eq!(
            line.get_segments_containing_point(points[d_id].info),
            [line.get_segment_by_index(1), None]
        );
    }
}
