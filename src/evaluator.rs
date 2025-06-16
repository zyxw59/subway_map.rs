use std::{
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use expr_parser::{evaluate::Evaluator as EvaluatorTrait, Span};

use crate::{
    error::{Error, MathError, Result},
    expressions::{ExpressionBit, Term, Variable},
    intermediate_representation::Document,
    operators::{BinaryOperator, UnaryOperator},
    parser::Position,
    points::PointCollection,
    statement::{Statement, StatementKind},
    stops::Stop,
    values::{Point, PointProvenance, Value},
};

pub trait EvaluationContext {
    fn get_variable(&self, name: &str) -> Option<Value>;

    fn get_fn_arg(&self, idx: usize) -> Option<Value>;
}

pub fn evaluate_expression(
    ctx: &dyn EvaluationContext,
    expr: impl IntoIterator<Item = ExpressionBit>,
) -> Result<Value, MathError> {
    EvaluatorTrait::evaluate(ctx, expr)
}

impl EvaluatorTrait<Position, BinaryOperator, UnaryOperator, Term> for dyn EvaluationContext + '_ {
    type Value = Value;
    type Error = MathError;

    fn evaluate_binary_operator(
        &self,
        _span: Span<Position>,
        operator: BinaryOperator,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value, MathError> {
        (operator.function)(lhs, rhs, self)
    }

    fn evaluate_unary_operator(
        &self,
        _span: Span<Position>,
        operator: UnaryOperator,
        argument: Value,
    ) -> Result<Value, MathError> {
        operator.call(argument, self)
    }

    fn evaluate_term(&self, _span: Span<Position>, term: Term) -> Result<Value, MathError> {
        match term {
            Term::Number(x) => Ok(Value::Number(x)),
            Term::String(s) => Ok(Value::String(Rc::new(s))),
            Term::Variable(v) => self.get_variable(&v).ok_or(MathError::Variable(v)),
            Term::FnArg(idx) => Ok(self
                .get_fn_arg(idx)
                .expect("invalid function argument index")),
        }
    }
}

const LINE_SEP: Variable = Variable::new_static("line_sep");
const INNER_RADIUS: Variable = Variable::new_static("inner_radius");

#[derive(Default, Debug)]
pub struct Evaluator {
    variables: HashMap<Variable, Value>,
    points: PointCollection,
    stops: Vec<Stop>,
    stylesheets: Vec<String>,
    title: Option<String>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Default::default()
    }

    pub fn evaluate_all(&mut self, parser: impl IntoIterator<Item = Statement>) -> Result<()> {
        parser
            .into_iter()
            .try_for_each(|statement| self.evaluate(statement))
    }

    fn evaluate(&mut self, Statement { statement, line }: Statement) -> Result<()> {
        match statement {
            StatementKind::Null => {}
            StatementKind::Variable(name, fields, expr) => {
                let value =
                    evaluate_expression(self, expr).map_err(|err| Error::Math(err, line))?;
                if fields.is_empty() {
                    if name == LINE_SEP {
                        let value = f64::try_from(&value).map_err(|err| Error::Math(err, line))?;
                        self.points.set_default_width(value);
                    } else if name == INNER_RADIUS {
                        let value = f64::try_from(&value).map_err(|err| Error::Math(err, line))?;
                        self.points.set_inner_radius(value);
                    }
                    // named points can't be redefined, since lines are defined in terms of them
                    if let Some(original_line) = self.points.get_point_line_number(&name) {
                        return Err(Error::PointRedefinition {
                            name,
                            line,
                            original_line,
                        });
                    }
                }
                let mut slot = self.variables.entry(name.clone());
                for field in fields {
                    slot = slot
                        .or_insert(Value::new_struct())
                        .slot(field)
                        .map_err(|err| Error::Math(err, line))?
                }
                match slot {
                    Entry::Occupied(mut entry) => {
                        entry.insert(value);
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(value);
                    }
                }
            }
            StatementKind::Function(name, function) => {
                self.variables.insert(name, Value::Function(function));
            }
            StatementKind::PointSingle(name, expr) => {
                // named points can't be redefined, since lines are defined in terms of them
                if let Some(original_line) = self.points.get_point_line_number(&name) {
                    return Err(Error::PointRedefinition {
                        name,
                        line,
                        original_line,
                    });
                }
                let (point, provenance) = evaluate_expression(self, expr)
                    .and_then(<(Point, PointProvenance)>::try_from)
                    .map_err(|err| Error::Math(err, line))?;
                self.points.insert_point(name, point, provenance, line)?;
            }
            StatementKind::PointSpaced {
                from,
                spaced,
                points,
            } => {
                let spacing = evaluate_expression(self, spaced)
                    .and_then(Point::try_from)
                    .map_err(|err| Error::Math(err, line))?;
                if !self.points.contains(&from) {
                    return Err(Error::Math(MathError::Variable(from), line));
                }
                let points = points
                    .into_iter()
                    .map(|(multiplier, name)| {
                        multiplier
                            .map(|expr| evaluate_expression(self, expr).and_then(f64::try_from))
                            .unwrap_or(Ok(1.0))
                            .map_err(|err| Error::Math(err, line))
                            .map(|distance| (name, distance))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                self.points.new_line(from, spacing, points, line)?;
            }
            StatementKind::PointExtend {
                from,
                to: (to_multiplier, to),
                points,
                is_past,
            } => {
                let mut total_distance = 0.0;
                let points = points
                    .into_iter()
                    .map(|(multiplier, name)| {
                        total_distance += multiplier
                            .map(|expr| evaluate_expression(self, expr).and_then(f64::try_from))
                            .unwrap_or(Ok(1.0))
                            .map_err(|err| Error::Math(err, line))?;
                        Ok((name, total_distance))
                    })
                    .collect::<Result<Vec<_>>>()?;
                if is_past {
                    let past_multiplier = to_multiplier
                        .map(|expr| evaluate_expression(self, expr).and_then(f64::try_from))
                        .unwrap_or(Ok(1.0))
                        .map_err(|err| Error::Math(err, line))?;
                    self.points.extend_line(
                        from,
                        to,
                        points
                            .into_iter()
                            .map(|(name, distance)| (name, distance / past_multiplier + 1.0)),
                        line,
                    )?;
                } else {
                    total_distance += to_multiplier
                        .map(|expr| evaluate_expression(self, expr).and_then(f64::try_from))
                        .unwrap_or(Ok(1.0))
                        .map_err(|err| Error::Math(err, line))?;
                    self.points.extend_line(
                        from,
                        to,
                        points
                            .into_iter()
                            .map(|(name, distance)| (name, distance / total_distance)),
                        line,
                    )?;
                }
            }
            StatementKind::Route {
                name,
                styles,
                segments,
            } => {
                let width = styles
                    .iter()
                    // if a style has a distinct line_sep, get the appropriate line_sep
                    // take the line_sep of the first listed style with a defined line_sep
                    .find_map(|style| {
                        self.get_variable(style)
                            .and_then(|st| st.field_access(LINE_SEP).ok())
                    })
                    // otherwise, get the default line_sep
                    .or_else(|| self.get_variable(&LINE_SEP))
                    // convert value to number
                    .and_then(Value::into_number)
                    // if it wasn't found, or wasn't a number, default to 1
                    .unwrap_or(1.0);
                let route = self.points.insert_route_get_id(name, width, styles, line)?;
                for segment in segments {
                    let offset = evaluate_expression(self, segment.offset)
                        .and_then(f64::try_from)
                        .map_err(|err| Error::Math(err, line))?;
                    self.points
                        .add_segment(route, &segment.start, &segment.end, offset)
                        .map_err(|name| Error::Math(MathError::Variable(name.into()), line))?;
                }
            }
            StatementKind::Stop(stop) => {
                let point = evaluate_expression(self, stop.point)
                    .and_then(Point::try_from)
                    .map_err(|err| Error::Math(err, line))?;
                let marker_parameters = stop
                    .marker_parameters
                    .into_iter()
                    .map(|(key, expr)| Ok((key, evaluate_expression(self, expr)?)))
                    .collect::<Result<_, _>>()
                    .map_err(|err| Error::Math(err, line))?;
                self.stops.push(Stop {
                    point,
                    marker_parameters,
                    styles: stop.styles,
                    marker_type: stop.marker_type,
                    input_line: line,
                })
            }
            StatementKind::Style(style) => self.stylesheets.push(style),
            StatementKind::Title(title) => self.title = Some(title),
        }
        Ok(())
    }

    pub fn write_debug(&self, file: impl std::io::Write) -> Result<()> {
        serde_json::to_writer_pretty(file, &self.points).map_err(Into::into)
    }

    pub fn into_document(self) -> Document {
        Document {
            view_box: self.view_box(),
            title: self.title,
            stylesheets: self.stylesheets,
            routes: self.points.routes(),
            stops: self.stops,
        }
    }

    fn view_box(&self) -> (f64, f64, f64, f64) {
        let top = self
            .get_variable("top")
            .and_then(Value::into_number)
            .unwrap_or(0.0);
        let left = self
            .get_variable("left")
            .and_then(Value::into_number)
            .unwrap_or(0.0);
        let bottom = self
            .get_variable("bottom")
            .and_then(Value::into_number)
            .unwrap_or(0.0);
        let right = self
            .get_variable("right")
            .and_then(Value::into_number)
            .unwrap_or(0.0);
        (top, left, bottom, right)
    }
}

impl EvaluationContext for Evaluator {
    fn get_variable(&self, name: &str) -> Option<Value> {
        if let Some((point, id)) = self.points.get_point_and_id(name) {
            Some(Value::Point(point, PointProvenance::Named(id)))
        } else {
            self.variables.get(name).cloned()
        }
    }

    fn get_fn_arg(&self, _idx: usize) -> Option<Value> {
        None
    }
}

impl EvaluationContext for () {
    fn get_variable(&self, _name: &str) -> Option<Value> {
        None
    }

    fn get_fn_arg(&self, _idx: usize) -> Option<Value> {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::parse,
        values::{tests::value, Point, Value},
    };

    use super::{EvaluationContext, Evaluator};

    #[test]
    fn variables_set() {
        let mut evaluator = Evaluator::new();
        evaluator.evaluate_all(parse("x = 1;").unwrap()).unwrap();
        assert_eq!(evaluator.variables.get("x"), Some(&Value::Number(1.0)));
    }

    #[test]
    fn variables_get() {
        let mut evaluator = Evaluator::new();
        evaluator
            .evaluate_all(parse("x = 1; z = x * 2;").unwrap())
            .unwrap();
        assert_eq!(evaluator.variables.get("x"), Some(&Value::Number(1.0)));
        assert_eq!(evaluator.variables.get("z"), Some(&Value::Number(2.0)));
    }

    #[test]
    fn functions() {
        let mut evaluator = Evaluator::new();
        evaluator
            .evaluate_all(parse("fn f(x) = x + 1; y = f(3);").unwrap())
            .unwrap();
        assert_eq!(evaluator.variables.get("y"), Some(&Value::Number(4.0)));
    }

    #[test]
    fn functions_2() {
        let mut evaluator = Evaluator::new();
        evaluator
            .evaluate_all(parse("fn f(x, y) = x * y; z = f(3, 2);").unwrap())
            .unwrap();
        assert_eq!(evaluator.variables.get("z"), Some(&Value::Number(6.0)));
    }

    #[test]
    fn point_single() {
        let mut evaluator = Evaluator::new();
        evaluator
            .evaluate_all(parse("point a = (1, 1);").unwrap())
            .unwrap();
        assert_eq!(evaluator.get_variable("a"), Some(value!(1, 1)));
    }

    macro_rules! points_multiple {
        ($str:expr,
         $first:ident: ($first_x:expr, $first_y:expr),
         $($name:ident: ($x:expr, $y:expr)),*;
         $last:ident: ($last_x:expr, $last_y:expr)) => {
            let mut evaluator = Evaluator::new();
            evaluator.evaluate_all(parse($str).unwrap()).unwrap();
            assert_eq!(
                evaluator.points.get_points_of_line(stringify!($first), stringify!($last)),
                Some(vec![
                     Point($first_x as f64, $first_y as f64),
                     $(Point($x as f64, $y as f64)),*,
                     Point($last_x as f64, $last_y as f64)
                ]),
            );
            for (name, value) in [
                 (stringify!($first), value!($first_x, $first_y)),
                 $((stringify!($name), value!($x, $y))),*,
                 (stringify!($last), value!($last_x, $last_y)),
            ] {
                assert_eq!(evaluator.get_variable(name), Some(value));
            }
        }
    }

    #[test]
    fn points_spaced() {
        points_multiple!(
            "point a = (1, 1); points from a spaced (1, 1): (0.5) b, c, d, (0.5) e;",
            a: (1, 1),
            b: (1.5, 1.5),
            c: (2.5, 2.5),
            d: (3.5, 3.5);
            e: (4, 4)
        );
    }

    #[test]
    fn points_between() {
        points_multiple!(
            "point a = (1, 1); point e = (4, 4); points from a to (0.5) e: (0.5) b, c, d;",
            a: (1, 1),
            b: (1.5, 1.5),
            c: (2.5, 2.5),
            d: (3.5, 3.5);
            e: (4, 4)
        );
    }

    #[test]
    fn points_between_2() {
        points_multiple!(
            "point a = (1, 1);
             point e = (4, 4);
             points from a to (0.5) e: (0.5) b, c, d;
             points from a to e: f, g;",
            a: (1, 1),
            b: (1.5, 1.5),
            f: (2, 2),
            c: (2.5, 2.5),
            g: (3, 3),
            d: (3.5, 3.5);
            e: (4, 4)
        );
    }
}
