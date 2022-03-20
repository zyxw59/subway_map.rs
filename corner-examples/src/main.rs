use std::f64::consts::FRAC_1_SQRT_2;
use std::{cmp, fmt, ops};

use fixed::{
    types::extra::{Unsigned, U22 as AngleFrac},
    FixedI32,
};
use serde::{Deserialize, Deserializer};

type AngleFixed = FixedI32<AngleFrac>;

const FRAC_SQRT_3_2: f64 = 0.8660254037844386467637231707529361834714026269051903;
const FRAC_1_SQRT_3: f64 = 0.5773502691896257645091487805019574556476017512701268;
const SQRT_3: f64 = 1.7320508075688772935274463415058723669428052538103806;
const ONE_EIGHTY: AngleFixed = AngleFixed::from_bits(180i32 << AngleFrac::USIZE);
const THREE_SIXTY: AngleFixed = AngleFixed::from_bits(360i32 << AngleFrac::USIZE);

const LINE_EXTENT: f64 = 150.0;

const HEADER: &str = r#"<?xml version="1.0" encoding="utf-8" ?>
<svg height="1000" width="1000" xmlns="http://www.w3.org/2000/svg">
<style>
@import url(base.css);
</style>
<rect height="100%" width="100%" fill="white" />
<g stroke="black">
"#;

fn main() -> anyhow::Result<()> {
    println!("{}", HEADER);
    let mut corners = Vec::new();
    for document in serde_yaml::Deserializer::from_reader(std::io::stdin()) {
        let corner = Corner::deserialize(document)?;
        corners.push(corner.calculate_points(LINE_EXTENT));
    }
    for corner in &corners {
        println!(r#"<path class="route-bg" d="{}" />"#, corner.arc_path());
        print!(r#"<path class="route" d="{}" "#, corner.arc_path());
        if let Some(ref color) = corner.color {
            print!(r#"stroke="{}" "#, color);
        }
        println!("/>");
    }
    println!("</g>");
    for corner in &corners {
        if corner.guides {
            println!(r#"<path class="ideal" d="{}" />"#, corner.ideal_path());
            println!(r#"<path class="guide" d="{}" />"#, corner.guide_path());
        }
    }
    println!("</svg>");
    Ok(())
}

#[derive(Clone, Debug, Deserialize)]
pub struct Corner {
    pub x: f64,
    pub y: f64,
    pub in_theta: Angle,
    pub out_theta: Angle,
    #[serde(default)]
    pub in_offset: f64,
    #[serde(default)]
    pub out_offset: f64,
    #[serde(default)]
    pub radius: f64,
    #[serde(default)]
    pub radius_adjust: f64,
    #[serde(default)]
    pub color: Option<String>,
    #[serde(default = "default_true")]
    pub guides: bool,
}

fn default_true() -> bool {
    true
}

impl Corner {
    pub fn calculate_points(self, extent: f64) -> CornerPoints {
        let angle = self.in_theta.supplementary() - self.out_theta;
        let half_angle = angle / 2;

        let orig_corner = Point(self.x, self.y);
        let in_vec = self.in_theta.unit();
        let out_vec = self.out_theta.unit();

        let start = orig_corner + in_vec.basis(-extent, self.in_offset);
        let end = orig_corner + out_vec.basis(extent, self.out_offset);

        let (arc, corner) = if self.in_theta != self.out_theta {
            let tan = half_angle.tan().abs();
            let radius = self.radius * tan.sqrt() + self.radius_adjust;
            let width = radius / tan;
            let sweep = angle.is_positive();

            let corner = orig_corner
                + in_vec.basis(
                    self.in_offset.mul_add(in_vec.dot(out_vec), self.out_offset)
                        / in_vec.cross(out_vec),
                    self.in_offset,
                );
            let start = in_vec.mul_add(-width, corner);
            let end = out_vec.mul_add(width, corner);
            let center = start + in_vec.basis(0.0, -radius);
            (
                Some(ArcPoints {
                    start,
                    end,
                    radius,
                    sweep,
                    center,
                }),
                corner,
            )
        } else {
            (None, orig_corner + in_vec.basis(0.0, self.in_offset))
        };

        CornerPoints {
            start,
            corner,
            end,
            arc,
            color: self.color,
            guides: self.guides,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CornerPoints {
    start: Point,
    corner: Point,
    end: Point,
    arc: Option<ArcPoints>,
    color: Option<String>,
    guides: bool,
}

#[derive(Clone, Copy, Debug)]
struct ArcPoints {
    start: Point,
    end: Point,
    center: Point,
    radius: f64,
    sweep: bool,
}

impl fmt::Display for ArcPoints {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ArcPoints {
            start,
            end,
            radius,
            sweep,
            ..
        } = self;
        let sweep = *sweep as u8;
        write!(f, "{start} A {radius},{radius} 0 0 {sweep} {end}")
    }
}

struct ArcPath<'a>(&'a CornerPoints);

impl fmt::Display for ArcPath<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CornerPoints {
            start, end, arc, ..
        } = self.0;
        if let Some(arc) = arc {
            write!(f, "M {start} L {arc} L {end}")
        } else {
            write!(f, "M {start} {end}")
        }
    }
}

struct IdealPath<'a>(&'a CornerPoints);

impl fmt::Display for IdealPath<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CornerPoints {
            start, corner, end, ..
        } = self.0;
        write!(f, "M {start} L {corner} {end}")
    }
}

struct GuidePath<'a>(&'a CornerPoints);

impl fmt::Display for GuidePath<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ArcPoints {
            start, center, end, ..
        }) = self.0.arc
        {
            write!(f, "M {start} L {center} {end}")
        } else {
            Ok(())
        }
    }
}

impl CornerPoints {
    fn arc_path(&self) -> impl fmt::Display + '_ {
        ArcPath(self)
    }

    fn ideal_path(&self) -> impl fmt::Display + '_ {
        IdealPath(self)
    }

    fn guide_path(&self) -> impl fmt::Display + '_ {
        GuidePath(self)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Point(f64, f64);

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{},{}", self.0, self.1)
    }
}

impl Point {
    /// Constructs a new point equal to `a * self + b * self.perp()`.
    pub fn basis(self, a: f64, b: f64) -> Point {
        Point(
            self.0.mul_add(a, self.1 * b),
            self.1.mul_add(a, -self.0 * b),
        )
    }

    /// Fused multiply-add. Computes `(self * a) + b with only one rounding error, yielding a more
    /// accurate result than an unfused multiply-add.
    pub fn mul_add(self, a: f64, b: Point) -> Point {
        Point(self.0.mul_add(a, b.0), self.1.mul_add(a, b.1))
    }

    pub fn dot(self, other: Self) -> f64 {
        self.0 * other.0 + self.1 * other.1
    }

    /// Positive if `other` is clockwise of `self`.
    pub fn cross(self, other: Self) -> f64 {
        self.0 * other.1 - self.1 * other.0
    }
}

impl ops::Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Point(self.0 + other.0, self.1 + other.1)
    }
}

#[derive(Clone, Copy)]
pub struct Angle(pub AngleFixed);

impl Angle {
    pub fn is_positive(self) -> bool {
        self.0.rem_euclid(THREE_SIXTY) < 180
    }

    fn to_fixed(self) -> FixedAngle {
        let x = self.0.to_num::<i16>();
        if self.0.frac() != 0 {
            return FixedAngle::Other(self.0.to_num());
        }
        let quadrant = match x {
            0..=89 => Quadrant::PosPos,
            90..=179 => Quadrant::NegPos,
            180..=269 => Quadrant::NegNeg,
            270..=359 => Quadrant::PosNeg,
            _ => unreachable!("x should be between 0 and 360"),
        };
        match x % 90 {
            0 => FixedAngle::Zero(quadrant),
            30 => FixedAngle::OneThird(quadrant),
            45 => FixedAngle::Half(quadrant),
            60 => FixedAngle::TwoThirds(quadrant),
            _ => FixedAngle::Other(self.0.into()),
        }
    }

    pub fn sin(self) -> f64 {
        self.to_fixed().sin()
    }
    pub fn cos(self) -> f64 {
        self.to_fixed().cos()
    }
    pub fn tan(self) -> f64 {
        self.to_fixed().tan()
    }

    pub fn supplementary(self) -> Self {
        Angle(ONE_EIGHTY) - self
    }

    pub fn unit(self) -> Point {
        Point(self.cos(), self.sin())
    }
}

impl fmt::Debug for Angle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl cmp::PartialEq for Angle {
    fn eq(&self, other: &Self) -> bool {
        self.0.rem_euclid(THREE_SIXTY) == other.0.rem_euclid(THREE_SIXTY)
    }
}

impl ops::Add for Angle {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let s = self.0.rem_euclid(THREE_SIXTY);
        let o = other.0.rem_euclid(THREE_SIXTY);
        Angle((s + o).rem_euclid(THREE_SIXTY))
    }
}

impl ops::Div<i32> for Angle {
    type Output = Self;

    fn div(self, other: i32) -> Self {
        Angle(self.0 / other)
    }
}

impl ops::Neg for Angle {
    type Output = Self;

    fn neg(self) -> Self {
        Angle(-self.0)
    }
}

impl ops::Sub for Angle {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        self + (-other)
    }
}

impl<'de> Deserialize<'de> for Angle {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use serde::de::Error;
        AngleFixed::checked_from_num(f64::deserialize(de)?)
            .ok_or_else(|| D::Error::custom("Invalid number"))
            .map(Angle)
    }
}

enum FixedAngle {
    /// {0, 90, 180, 270}°
    Zero(Quadrant),
    /// {30, 120, 210, 300}°
    OneThird(Quadrant),
    /// {45, 135, 225, 315}°
    Half(Quadrant),
    /// {60, 150, 240, 330}°
    TwoThirds(Quadrant),
    Other(f64),
}

impl FixedAngle {
    fn sin(self) -> f64 {
        use self::{FixedAngle::*, Quadrant::*};
        match self {
            // 0, 180
            Zero(PosPos | NegNeg) => 0.0,
            // 90
            Zero(NegPos) => 1.0,
            // 270
            Zero(PosNeg) => -1.0,
            // 30, 150
            OneThird(PosPos) | TwoThirds(NegPos) => 0.5,
            // 210, 330
            OneThird(NegNeg) | TwoThirds(PosNeg) => -0.5,
            // 60, 120
            TwoThirds(PosPos) | OneThird(NegPos) => FRAC_SQRT_3_2,
            // 240, 300
            TwoThirds(NegNeg) | OneThird(PosNeg) => -FRAC_SQRT_3_2,
            // 45, 135
            Half(PosPos | NegPos) => FRAC_1_SQRT_2,
            // 225, 315
            Half(NegNeg | PosNeg) => -FRAC_1_SQRT_2,
            Other(x) => x.to_radians().sin(),
        }
    }

    fn cos(self) -> f64 {
        use self::{FixedAngle::*, Quadrant::*};
        match self {
            // 0
            Zero(PosPos) => 1.0,
            // 90, 270
            Zero(NegPos | PosNeg) => 0.0,
            // 180
            Zero(NegNeg) => -1.0,
            // 30, 330
            OneThird(PosPos) | TwoThirds(PosNeg) => FRAC_SQRT_3_2,
            // 150, 210
            TwoThirds(NegPos) | OneThird(NegNeg) => -FRAC_SQRT_3_2,
            // 60, 300
            TwoThirds(PosPos) | OneThird(PosNeg) => 0.5,
            // 120, 240
            OneThird(NegPos) | TwoThirds(NegNeg) => -0.5,
            // 45, 315
            Half(PosPos | PosNeg) => FRAC_1_SQRT_2,
            // 135, 225
            Half(NegPos | NegNeg) => -FRAC_1_SQRT_2,
            Other(x) => x.to_radians().cos(),
        }
    }

    fn tan(self) -> f64 {
        use self::{FixedAngle::*, Quadrant::*};
        match self {
            // 0, 180
            Zero(PosPos | NegNeg) => 0.0,
            // 90, 270
            Zero(NegPos | PosNeg) => f64::INFINITY,
            // 30, 210
            OneThird(PosPos | NegNeg) => FRAC_1_SQRT_3,
            // 60, 240
            TwoThirds(PosPos | NegNeg) => SQRT_3,
            // 120, 300
            OneThird(NegPos | PosNeg) => -SQRT_3,
            // 150, 330
            TwoThirds(NegPos | PosNeg) => -FRAC_1_SQRT_3,
            // 45, 225
            Half(PosPos | NegNeg) => 1.0,
            // 135, 315
            Half(NegPos | PosNeg) => -1.0,
            Other(x) => x.to_radians().tan(),
        }
    }
}

enum Quadrant {
    PosPos,
    NegPos,
    NegNeg,
    PosNeg,
}
