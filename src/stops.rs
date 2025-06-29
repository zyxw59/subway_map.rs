use std::collections::HashMap;

use smol_str::SmolStr;
use svg::node::{
    element::{Circle, Group, Text},
    Node,
};

use crate::{
    error::{Error, MathError, Result},
    values::{Point, Value},
};

/// A stop.
#[derive(Debug, serde::Serialize)]
pub struct Stop {
    /// Location of the stop
    pub point: Point,
    /// Styles applying to the stop
    pub styles: Vec<SmolStr>,
    /// Type of marker to use for the stop
    pub marker_type: SmolStr,
    /// The parameters for the marker
    pub marker_parameters: HashMap<SmolStr, Value>,
    /// The input line the stop is defined on
    pub input_line: usize,
}

impl Stop {
    pub fn to_svg(&self) -> Result<Group> {
        let mut group = Group::new().set(
            "class",
            format!("stop {} {}", self.marker_type, self.styles.join(" ")),
        );
        match &*self.marker_type {
            "circle" => {
                let radius = self.get_parameter_typed::<f64>("r")?;
                let circ = Circle::new()
                    .set("cx", self.point.0)
                    .set("cy", self.point.1)
                    .set("r", radius);
                group.append(circ);
            }
            "text" => {
                let text = self.get_parameter_typed::<&str>("text")?;
                let anchor = self
                    .get_optional_parameter_typed::<Anchor>("anchor")?
                    .unwrap_or_default();
                let angle = self
                    .get_optional_parameter_typed::<f64>("angle")?
                    .unwrap_or(0.0);
                // TODO(#15): handle multi-line text
                let text_el = Text::new(text)
                    .set("x", self.point.0)
                    .set("y", self.point.1)
                    .set("text-anchor", anchor.horizontal_anchor())
                    .set("dominant-baseline", anchor.vertical_anchor())
                    .set(
                        "transform",
                        format!("rotate({} {} {})", angle, self.point.0, self.point.1),
                    );
                group.append(text_el);
            }
            // TODO(#16): more marker types?
            // TODO(#17): user-defined marker types?
            _ => {
                return Err(Error::UndefinedStopMarker {
                    name: self.marker_type.clone(),
                    line: self.input_line,
                })
            }
        }
        Ok(group)
    }

    fn get_parameter(&self, arg: &'static str) -> Result<&Value> {
        self.marker_parameters
            .get(arg)
            .ok_or_else(|| Error::MissingMarkerArg {
                marker: self.marker_type.clone(),
                arg,
                line: self.input_line,
            })
    }

    fn get_parameter_typed<'a, T>(&'a self, arg: &'static str) -> Result<T>
    where
        T: TryFrom<&'a Value, Error = MathError>,
    {
        self.get_parameter(arg)
            .and_then(|val| T::try_from(val).map_err(|err| self.invalid_arg_error(arg, err)))
    }

    fn get_optional_parameter_typed<'a, T>(&'a self, arg: &'static str) -> Result<Option<T>>
    where
        T: TryFrom<&'a Value, Error = MathError>,
    {
        self.marker_parameters
            .get(arg)
            .map(|val| T::try_from(val).map_err(|err| self.invalid_arg_error(arg, err)))
            .transpose()
    }

    fn invalid_arg_error(&self, arg: &'static str, error: MathError) -> Error {
        Error::InvalidMarkerArg {
            marker: self.marker_type.clone(),
            arg,
            line: self.input_line,
            error,
        }
    }
}

macro_rules! anchor_enum {
    ($(#[$attrs:meta])*
     $vis:vis enum $ty_name:ident {
         $($(#[$var_attrs:meta])* $parse:literal => $variant:ident),* $(,)?
    }) => {
        $(#[$attrs])*
        $vis enum $ty_name {
            $($(#[$var_attrs])* $variant,)*
        }

        impl $ty_name {
            $vis const VARIANTS: &'static [&'static str] = &[$($parse,)*];
        }

        impl<'a> TryFrom<&'a str> for $ty_name {
            type Error = UnknownAnchor<'a>;

            fn try_from(s: &'a str) -> Result<Self, Self::Error> {
                match s {
                    $($parse => Ok(Self::$variant),)*
                    actual => Err(UnknownAnchor {
                        actual,
                        expected: Self::VARIANTS,
                    }),
                }
            }
        }
    };
}

anchor_enum! {
    #[derive(Copy, Clone, Debug, Default)]
    pub enum Anchor {
        #[default]
        "bottom-left" => BottomLeft,
        "bottom" => Bottom,
        "bottom-right" => BottomRight,
        "left" => Left,
        "center" => Center,
        "right" => Right,
        "top-left" => TopLeft,
        "top" => Top,
        "top-right" => TopRight,
    }
}

impl Anchor {
    pub fn horizontal_anchor(self) -> &'static str {
        match self {
            Self::BottomLeft | Self::Left | Self::TopLeft => "start",
            Self::Bottom | Self::Center | Self::Top => "middle",
            Self::BottomRight | Self::Right | Self::TopRight => "end",
        }
    }

    pub fn vertical_anchor(self) -> &'static str {
        match self {
            Self::BottomLeft | Self::Bottom | Self::BottomRight => "alphabetic",
            Self::Left | Self::Center | Self::Right => "central",
            Self::TopLeft | Self::Top | Self::TopRight => "hanging",
        }
    }
}

impl TryFrom<&'_ Value> for Anchor {
    type Error = MathError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        <&str>::try_from(value)?
            .try_into()
            .map_err(|e: UnknownAnchor| MathError::Domain(e.to_string()))
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Unknown anchor value: {actual} (expected one of {expected:?})")]
pub struct UnknownAnchor<'a> {
    actual: &'a str,
    expected: &'static [&'static str],
}
