use std::{cmp::Ordering, collections::HashMap, fmt};

use super::{line::LineDirection, LineId, PointId};

pub type RouteCorners = HashMap<PointId, RouteCorner>;

#[derive(Default)]
pub struct RouteCorner {
    turns: HashMap<(LineId, LineDirection), RouteTurns>,
}

impl RouteCorner {
    pub fn insert_left(&mut self, line: LineId, direction: LineDirection, turn: RouteTurn) {
        self.turns
            .entry((line, direction))
            .or_default()
            .insert_left(turn)
    }

    pub fn insert_right(&mut self, line: LineId, direction: LineDirection, turn: RouteTurn) {
        self.turns
            .entry((line, direction))
            .or_default()
            .insert_right(turn)
    }

    pub fn get(&self, line: LineId, direction: LineDirection) -> Option<f64> {
        self.turns.get(&(line, direction)).and_then(RouteTurns::get)
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
    pub transverse: f64,
    pub longitudinal: f64,
}

impl RouteTurn {
    /// Select the `RouteTurn` with the smaller `transverse`. If they are equal, select the *larger*
    /// `longitudinal`.
    fn min(self, other: Self) -> Self {
        match self.transverse.partial_cmp(&other.transverse) {
            Some(Ordering::Less) => self,
            Some(Ordering::Equal) | None => Self {
                transverse: self.transverse,
                longitudinal: self.longitudinal.max(other.longitudinal),
            },
            Some(Ordering::Greater) => other,
        }
    }

    /// Select the `RouteTurn` with the larger `transverse`. If they are equal, select the *larger*
    /// `longitudinal`.
    fn max(self, other: Self) -> Self {
        match self.transverse.partial_cmp(&other.transverse) {
            Some(Ordering::Less) => other,
            Some(Ordering::Equal) | None => Self {
                transverse: self.transverse,
                longitudinal: self.longitudinal.max(other.longitudinal),
            },
            Some(Ordering::Greater) => self,
        }
    }
}
