use std::{cmp::Ordering, collections::HashMap, fmt};

use super::PointId;

#[derive(Default)]
pub struct RouteCorner {
    // point: PointId,
    turns: HashMap<PointId, RouteTurns>,
}

impl RouteCorner {
    //pub fn new(point: PointId) -> Self {
    //    RouteCorner {
    //        point,
    //        turns: HashMap::new(),
    //    }
    //}

    pub fn insert_left(&mut self, point: PointId, turn: RouteTurn) {
        self.turns.entry(point).or_default().insert_left(turn)
    }

    pub fn insert_right(&mut self, point: PointId, turn: RouteTurn) {
        self.turns.entry(point).or_default().insert_right(turn)
    }

    pub fn get(&self, point: PointId) -> Option<f64> {
        self.turns.get(&point).and_then(RouteTurns::get)
    }
}

impl fmt::Debug for RouteCorner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.turns, f)
    }
}

#[derive(Default)]
struct RouteTurns {
    left: Option<RouteTurn>,
    right: Option<RouteTurn>,
}

impl RouteTurns {
    fn insert_left(&mut self, new: RouteTurn) {
        if let Some(existing) = &mut self.left {
            *existing = existing.min(new)
        } else {
            self.left = Some(new)
        }
    }

    fn insert_right(&mut self, new: RouteTurn) {
        if let Some(existing) = &mut self.right {
            *existing = existing.max(new)
        } else {
            self.right = Some(new)
        }
    }

    fn get(&self) -> Option<f64> {
        match (self.left, self.right) {
            (Some(left), Some(right)) => Some(left.longitudinal.max(right.longitudinal)),
            (Some(x), None) | (None, Some(x)) => Some(x.longitudinal),
            (None, None) => None,
        }
    }
}

impl fmt::Debug for RouteTurns {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = f.debug_struct("RouteTurns");
        if let Some(left) = &self.left {
            s.field("left", left);
        }
        if let Some(right) = &self.right {
            s.field("right", right);
        }
        s.finish()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RouteTurn {
    pub transverse: isize,
    pub longitudinal: f64,
}

impl RouteTurn {
    /// Select the `RouteTurn` with the smaller `transverse`. If they are equal, select the *larger*
    /// `longitudinal`.
    fn min(self, other: Self) -> Self {
        match self.transverse.cmp(&other.transverse) {
            Ordering::Less => self,
            Ordering::Equal => Self {
                transverse: self.transverse,
                longitudinal: self.longitudinal.max(other.longitudinal),
            },
            Ordering::Greater => other,
        }
    }

    /// Select the `RouteTurn` with the larger `transverse`. If they are equal, select the *larger*
    /// `longitudinal`.
    fn max(self, other: Self) -> Self {
        match self.transverse.cmp(&other.transverse) {
            Ordering::Less => other,
            Ordering::Equal => Self {
                transverse: self.transverse,
                longitudinal: self.longitudinal.max(other.longitudinal),
            },
            Ordering::Greater => self,
        }
    }
}
