use std::fmt;

use crate::{
    evaluator::EvaluationContext,
    values::{Result, Value},
};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Precedence {
    /// The `,` operator
    Comma,
    /// Comparison operators, such as `==`, `>`, `<>`, etc.
    Comparison,
    /// Additive operators, such as `+`, `-`, `++`, etc.
    Additive,
    /// Multiplicative operators, such as `*`, `/`, `&`, etc., as well as unary minus.
    Multiplicative,
    /// Exponential operators, such as `^`, as well as unary sine and cosine.
    Exponential,
    /// The `.` operator
    Dot,
}

macro_rules! as_binary_operator {
    ($fn:path) => {{
        fn _f(a: Value, b: Value, _: &dyn EvaluationContext) -> Result {
            $fn(a, b)
        }
        _f
    }};
}

macro_rules! as_unary_operator {
    ($fn:path) => {{
        fn _f(a: Value, _: &dyn EvaluationContext) -> Result {
            $fn(a)
        }
        _f
    }};
}

pub const COMMA: BinaryOperator = BinaryOperator {
    function: as_binary_operator!(Value::comma),
    name: ",",
};

pub const COMMA_UNARY: UnaryOperator = UnaryOperator {
    function: as_unary_operator!(Value::comma_unary),
    name: ",",
};

pub const PAREN_UNARY: UnaryOperator = UnaryOperator {
    function: as_unary_operator!(Value::comma_unary),
    name: "()",
};

macro_rules! get_binary_builtin {
    (match $key:ident { $($name:literal => ($prec:ident, $fn:ident)),* $(,)? }) => {
        match $key {
            $($name => Some((Precedence::$prec, Operator {
                function: as_binary_operator!(Value::$fn),
                name: $name,
            })),)*
            _ => None,
        }
    }
}

macro_rules! get_unary_builtin {
    (match $key:ident { $($name:literal => ($prec:ident, $fn:ident)),* $(,)? }) => {
        match $key {
            $($name => Some((Precedence::$prec, Operator {
                function: as_unary_operator!(Value::$fn),
                name: $name,
            })),)*
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Operator<F> {
    function: F,
    name: &'static str,
}

impl<F> fmt::Debug for Operator<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.name)
    }
}

pub type BinaryOperator = Operator<fn(Value, Value, &dyn EvaluationContext) -> Result>;

impl BinaryOperator {
    pub fn get(key: &str) -> Option<(Precedence, Self)> {
        get_binary_builtin!(match key {
            "==" => (Comparison, eq),
            "!=" => (Comparison, ne),
            "<" => (Comparison, lt),
            "<=" => (Comparison, le),
            ">" => (Comparison, gt),
            ">=" => (Comparison, ge),
            "max" => (Comparison, max),
            "min" => (Comparison, min),
            "+" => (Additive, add),
            "-" => (Additive, sub),
            "++" => (Additive, hypot),
            "+-+" => (Additive, hypot_sub),
            "*" => (Multiplicative, mul),
            "/" => (Multiplicative, div),
            "&" => (Multiplicative, intersect),
            "^" => (Exponential, pow),
            "<>" => (Exponential, line_between),
            ">>" => (Exponential, line_vector),
            "^^" => (Exponential, line_offset),
        })
    }
}

pub type UnaryOperator = Operator<fn(Value, &dyn EvaluationContext) -> Result>;

impl UnaryOperator {
    pub fn get(key: &str) -> Option<(Precedence, Self)> {
        get_unary_builtin!(match key {
            "-" => (Multiplicative, neg),
            "xpart" => (Multiplicative, xpart),
            "ypart" => (Multiplicative, ypart),
            "cos" => (Exponential, cos),
            "sin" => (Exponential, sin),
            "dir" => (Exponential, dir),
            "angle" => (Exponential, angle),
        })
    }
}
