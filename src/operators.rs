use std::cmp::PartialEq;
use std::fmt;

use crate::error::MathError;
use crate::expressions::Expression;
use crate::values::Value;

type EResult<T> = Result<T, MathError>;

enum Precedence {
    /// Comparison operators, such as `==`, `>`, `<>`, etc.
    Comparison,
    /// Additive operators, such as `+`, `-`, `++`, etc.
    Additive,
    /// Multiplicative operators, such as `*`, `/`, `&`, etc., as well as unary minus.
    Multiplicative,
    /// Exponential operators, such as `^`, as well as unary sine and cosine.
    Exponential,
}

mod builtins {
    use std::ops;

    use crate::values::Value;

    use super::{BinaryOperator, Precedence, UnaryOperator};

    macro_rules! bin_op {
        ($name:ident ( $prec:ident, $fun:path, $debug:literal )) => {
            pub const $name: BinaryOperator<'static> = BinaryOperator {
                precedence: Precedence::$prec as usize,
                function: &$fun,
                name: $debug,
            };
        };
    }

    bin_op! {EQ(Comparison, Value::eq, "==")}
    bin_op! {NE(Comparison, Value::ne, "!=")}
    bin_op! {LT(Comparison, Value::lt, ">")}
    bin_op! {LE(Comparison, Value::le, ">=")}
    bin_op! {GT(Comparison, Value::gt, "<")}
    bin_op! {GE(Comparison, Value::ge, "<=")}
    bin_op! {MAX(Comparison, Value::max, "max")}
    bin_op! {MIN(Comparison, Value::min, "min")}
    bin_op! {ADD(Additive, ops::Add::add, "+")}
    bin_op! {SUB(Additive, ops::Sub::sub, "-")}
    bin_op! {HYPOT(Additive, Value::hypot, "++")}
    bin_op! {HYPOT_SUB(Additive, Value::hypot_sub, "+-+")}
    bin_op! {MUL(Multiplicative, ops::Mul::mul, "*")}
    bin_op! {DIV(Multiplicative, ops::Div::div, "/")}
    bin_op! {POW(Exponential, Value::pow, "^")}
    bin_op! {LINE_BETWEEN(Exponential, Value::line_between, "<>")}
    bin_op! {LINE_VECTOR(Exponential, Value::line_vector, ">>")}
    bin_op! {INTERSECT(Multiplicative, Value::intersect, "&")}
    bin_op! {LINE_OFFSET(Exponential, Value::line_offset, "^^")}

    macro_rules! unary_op {
        ($name:ident ( $prec:ident, $fun:path, $debug:literal )) => {
            pub const $name: UnaryOperator<'static> = UnaryOperator {
                precedence: Precedence::$prec as usize,
                function: &$fun,
                name: $debug,
            };
        };
    }

    unary_op! {NEG(Multiplicative, ops::Neg::neg, "-")}
    unary_op! {COS(Exponential, Value::cos, "cos")}
    unary_op! {SIN(Exponential, Value::sin, "sin")}
    unary_op! {DIR(Exponential, Value::dir, "dir")}
    unary_op! {ANGLE(Exponential, Value::angle, "angle")}
    unary_op! {XPART(Multiplicative, Value::xpart, "xpart")}
    unary_op! {YPART(Multiplicative, Value::ypart, "ypart")}
}

pub struct BinaryBuiltins;

impl BinaryBuiltins {
    pub fn get(&self, key: &str) -> Option<&'static BinaryOperator<'static>> {
        match key {
            "==" => Some(&builtins::EQ),
            "!=" => Some(&builtins::NE),
            "<" => Some(&builtins::LT),
            "<=" => Some(&builtins::LE),
            ">" => Some(&builtins::GT),
            ">=" => Some(&builtins::GE),
            "+" => Some(&builtins::ADD),
            "-" => Some(&builtins::SUB),
            "++" => Some(&builtins::HYPOT),
            "+-+" => Some(&builtins::HYPOT_SUB),
            "*" => Some(&builtins::MUL),
            "/" => Some(&builtins::DIV),
            "^" => Some(&builtins::POW),
            "max" => Some(&builtins::MAX),
            "min" => Some(&builtins::MIN),
            "<>" => Some(&builtins::LINE_BETWEEN),
            ">>" => Some(&builtins::LINE_VECTOR),
            "&" => Some(&builtins::INTERSECT),
            "^^" => Some(&builtins::LINE_OFFSET),
            _ => None,
        }
    }
}

pub struct UnaryBuiltins;

impl UnaryBuiltins {
    pub fn get(&self, key: &str) -> Option<&'static UnaryOperator<'static>> {
        match key {
            "-" => Some(&builtins::NEG),
            "cos" => Some(&builtins::COS),
            "sin" => Some(&builtins::SIN),
            "dir" => Some(&builtins::DIR),
            "angle" => Some(&builtins::ANGLE),
            "xpart" => Some(&builtins::XPART),
            "ypart" => Some(&builtins::YPART),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct BinaryOperator<'a> {
    pub precedence: usize,
    function: &'a dyn Fn(Value, Value) -> EResult<Value>,
    name: &'static str,
}

impl<'a> BinaryOperator<'a> {
    pub fn apply(&self, lhs: Value, rhs: Value) -> EResult<Value> {
        (self.function)(lhs, rhs)
    }

    pub fn expression(&'static self, lhs: Expression, rhs: Expression) -> Expression {
        Expression::BinaryOperator(self, Box::new((lhs, rhs)))
    }
}

impl<'a> fmt::Debug for BinaryOperator<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a> PartialEq for BinaryOperator<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.precedence == other.precedence && self.name == other.name
    }
}

#[derive(Clone)]
pub struct UnaryOperator<'a> {
    pub precedence: usize,
    function: &'a dyn Fn(Value) -> EResult<Value>,
    name: &'static str,
}

impl<'a> UnaryOperator<'a> {
    pub fn apply(&self, argument: Value) -> EResult<Value> {
        (self.function)(argument)
    }

    pub fn expression(&'static self, argument: Expression) -> Expression {
        Expression::UnaryOperator(self, Box::new(argument))
    }
}

impl<'a> fmt::Debug for UnaryOperator<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a> PartialEq for UnaryOperator<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.precedence == other.precedence && self.name == other.name
    }
}
