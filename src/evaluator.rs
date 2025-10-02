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
    points::{PointCollection, PointId},
    statement::{Statement, StatementKind},
    stops::Stop,
    values::{Point, Value},
};

pub trait EvaluationContext {
    fn get_variable(&self, name: &str) -> Option<Value>;

    fn get_fn_arg(&self, idx: usize) -> Option<Value>;

    fn point_collection(&mut self) -> &mut PointCollection;
}

pub fn evaluate_expression(
    ctx: &mut dyn EvaluationContext,
    expr: impl IntoIterator<Item = ExpressionBit>,
) -> Result<Value, MathError> {
    EvaluatorTrait::evaluate(ctx, expr)
}

impl EvaluatorTrait<Position, BinaryOperator, UnaryOperator, Term> for dyn EvaluationContext + '_ {
    type Value = Value;
    type Error = MathError;

    fn evaluate_binary_operator(
        &mut self,
        _span: Span<Position>,
        operator: BinaryOperator,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value, MathError> {
        (operator.function)(lhs, rhs, self)
    }

    fn evaluate_unary_operator(
        &mut self,
        _span: Span<Position>,
        operator: UnaryOperator,
        argument: Value,
    ) -> Result<Value, MathError> {
        operator.call(argument, self)
    }

    fn evaluate_term(&mut self, _span: Span<Position>, term: Term) -> Result<Value, MathError> {
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

const LINE_WIDTH: Variable = Variable::new_static("line_width");
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
            StatementKind::Variable(name, fields, expr) => {
                let value =
                    evaluate_expression(self, expr).map_err(|err| Error::Math(err, line))?;
                if fields.is_empty() {
                    if name == LINE_WIDTH {
                        let value = f64::try_from(&value).map_err(|err| Error::Math(err, line))?;
                        self.points.set_default_width(value);
                    } else if name == INNER_RADIUS {
                        let value = f64::try_from(&value).map_err(|err| Error::Math(err, line))?;
                        self.points.set_inner_radius(value);
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
                // NOTE: this statement is almost identical to a normal variable assignment, except
                // that it type-checks that the expression evaluates to a point.
                let value = evaluate_expression(self, expr)
                    .and_then(|value| {
                        if !matches!(value, Value::Point(..)) {
                            Err(MathError::Type(crate::error::Type::Point, value.into()))
                        } else {
                            Ok(value)
                        }
                    })
                    .map_err(|err| Error::Math(err, line))?;
                self.variables.insert(name, value);
            }
            StatementKind::PointSpaced {
                from,
                spaced,
                points,
            } => {
                let spacing = evaluate_expression(self, spaced)
                    .and_then(Point::try_from)
                    .map_err(|err| Error::Math(err, line))?;
                let (from_point, from_id) = self.get_point(from, line)?;
                let line_id = self.points.new_line(from_id, spacing);
                let mut distance = 0.0;
                for (multiplier, name) in points {
                    let multiplier = multiplier
                        .map(|expr| evaluate_expression(self, expr).and_then(f64::try_from))
                        .unwrap_or(Ok(1.0))
                        .map_err(|err| Error::Math(err, line))?;
                    distance += multiplier;
                    let point = spacing.mul_add(distance, from_point);
                    let id = self.points.add_point_on_line(point, line_id);
                    self.variables.insert(name, Value::Point(point, id));
                }
            }
            StatementKind::PointExtend {
                from,
                to: (to_multiplier, to),
                points,
                is_past,
            } => {
                let (from_point, from_id) = self.get_point(from, line)?;
                let (to_point, to_id) = self.get_point(to, line)?;
                let to_multiplier = to_multiplier
                    .map(|expr| evaluate_expression(self, expr).and_then(f64::try_from))
                    .unwrap_or(Ok(1.0))
                    .map_err(|err| Error::Math(err, line))?;
                let mut total_distance = 0.0;
                if is_past {
                    total_distance += to_multiplier;
                }
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
                let line_id = self.points.get_or_insert_line(from_id, to_id);
                let distance_quotient = if is_past {
                    to_multiplier
                } else {
                    total_distance + to_multiplier
                };
                let spacing = (to_point - from_point) / distance_quotient;
                for (name, distance) in points {
                    let point = spacing.mul_add(distance, from_point);
                    let id = self.points.add_point_on_line(point, line_id);
                    self.variables.insert(name, Value::Point(point, id));
                }
            }
            StatementKind::Route {
                name,
                styles,
                segments,
            } => {
                let route = self
                    .points
                    .insert_route_get_id(name.clone(), styles, line)?;
                for segment in segments {
                    let (_, start_id) = self.get_point(segment.start.clone(), line)?;
                    let (_, end_id) = self.get_point(segment.end.clone(), line)?;
                    let offset = evaluate_expression(self, segment.offset)
                        .and_then(f64::try_from)
                        .map_err(|err| Error::Math(err, line))?;
                    self.points
                        .add_segment(route, start_id, end_id, offset)
                        .map_err(|()| Error::DuplicatePointInRoute {
                            route: name.clone(),
                            start: segment.start,
                            end: segment.end,
                            line,
                        })?;
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

    fn get_point(&self, name: Variable, line_no: usize) -> Result<(Point, PointId)> {
        self.get_variable(&name)
            .ok_or(MathError::Variable(name))
            .and_then(TryFrom::try_from)
            .map_err(|err| Error::Math(err, line_no))
    }
}

impl EvaluationContext for Evaluator {
    fn get_variable(&self, name: &str) -> Option<Value> {
        self.variables.get(name).cloned()
    }

    fn get_fn_arg(&self, _idx: usize) -> Option<Value> {
        None
    }

    fn point_collection(&mut self) -> &mut PointCollection {
        &mut self.points
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::parse,
        points::{LineId, PointId},
        values::Point,
    };

    use super::{EvaluationContext, Evaluator};

    #[test]
    fn variables_set() {
        let mut evaluator = Evaluator::new();
        evaluator.evaluate_all(parse("x = 1;").unwrap()).unwrap();
        assert_eq!(*evaluator.variables.get("x").unwrap(), 1.0);
    }

    #[test]
    fn variables_get() {
        let mut evaluator = Evaluator::new();
        evaluator
            .evaluate_all(parse("x = 1; z = x * 2;").unwrap())
            .unwrap();
        assert_eq!(*evaluator.variables.get("x").unwrap(), 1.0);
        assert_eq!(*evaluator.variables.get("z").unwrap(), 2.0);
    }

    #[test]
    fn functions() {
        let mut evaluator = Evaluator::new();
        evaluator
            .evaluate_all(parse("fn f(x) = x + 1; y = f(3);").unwrap())
            .unwrap();
        assert_eq!(*evaluator.variables.get("y").unwrap(), 4.0);
    }

    #[test]
    fn functions_2() {
        let mut evaluator = Evaluator::new();
        evaluator
            .evaluate_all(parse("fn f(x, y) = x * y; z = f(3, 2);").unwrap())
            .unwrap();
        assert_eq!(*evaluator.variables.get("z").unwrap(), 6.0);
    }

    #[test]
    fn point_single() {
        let mut evaluator = Evaluator::new();
        evaluator
            .evaluate_all(parse("point a = (1, 1);").unwrap())
            .unwrap();
        assert_eq!(evaluator.get_variable("a").unwrap(), Point(1.0, 1.0));
    }

    #[test]
    fn grid() {
        let mut evaluator = Evaluator::new();
        let input = "
N = dir 0;
E = dir 90;

n0 = (0 * N) >> E;
e0 = (0 * E) >> N;
e1 = (1 * E) >> N;
e2 = (2 * E) >> N;

point n0e0 = n0 & e0;
point n0e1 = n0 & e1;
point n0e2 = n0 & e2;
";
        evaluator.evaluate_all(parse(input).unwrap()).unwrap();
        let (_, n0e0): (_, PointId) = evaluator.variables["n0e0"].clone().try_into().unwrap();
        let (_, n0e1): (_, PointId) = evaluator.variables["n0e1"].clone().try_into().unwrap();
        let (_, n0e2): (_, PointId) = evaluator.variables["n0e2"].clone().try_into().unwrap();
        let (_, n0): (_, LineId) = evaluator.variables["n0"].clone().try_into().unwrap();
        assert_eq!(evaluator.points.get_or_insert_line(n0e0, n0e1), n0);
        assert_eq!(evaluator.points.get_or_insert_line(n0e0, n0e2), n0);
        assert_eq!(evaluator.points.get_or_insert_line(n0e1, n0e2), n0);
    }

    macro_rules! points_multiple {
        ($str:expr,
         $first:ident: ($first_x:expr, $first_y:expr),
         $($name:ident: ($x:expr, $y:expr)),*;
         $last:ident: ($last_x:expr, $last_y:expr)) => {
            let mut evaluator = Evaluator::new();
            evaluator.evaluate_all(parse($str).unwrap()).unwrap();
            let (_, start_id) = evaluator.get_point(stringify!($first).into(), 0).unwrap();
            let (_, end_id) = evaluator.get_point(stringify!($last).into(), 0).unwrap();
            assert_eq!(
                evaluator.points.get_points_of_line(start_id, end_id).unwrap(),
                [
                     Point($first_x as f64, $first_y as f64),
                     $(Point($x as f64, $y as f64)),*,
                     Point($last_x as f64, $last_y as f64)
                ],
            );
            for (name, value) in [
                 (stringify!($first), Point($first_x as f64, $first_y as f64)),
                 $((stringify!($name), Point($x as f64, $y as f64))),*,
                 (stringify!($last), Point($last_x as f64, $last_y as f64)),
            ] {
                assert_eq!(evaluator.get_variable(name).unwrap(), value);
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
